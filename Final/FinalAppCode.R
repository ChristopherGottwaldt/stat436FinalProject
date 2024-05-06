library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(DT)
library(tidyr)
library(ggplot2)

births_url <- 'https://uwmadison.box.com/shared/static/pesmuru6zcar4q590jnbv0ntbs3xw915.csv'
births_data <- read_csv(births_url, show_col_types = FALSE) %>%
  mutate(across(-county, ~ifelse(. == "<5", "0", as.character(.)))) %>%
  mutate(across(-county, as.numeric))


counties <- sf::st_read("https://uwmadison.box.com/shared/static/6u48cq1lyir9flbfecv8xte0li3s0cw3.geojson", quiet = TRUE)
stats <- read_csv("https://uwmadison.box.com/shared/static/oln9vtfskaj7hp0l0q2o94m1vzlos77k.csv", show_col_types = FALSE)

ui <- navbarPage(title = div(style = "display: flex;",
                             img(src = 'https://upload.wikimedia.org/wikipedia/commons/thumb/3/31/Seal_of_Wisconsin.svg/2048px-Seal_of_Wisconsin.svg.png', style = "height: 55px; margin-top: -18px; margin-left: -10px; margin-right: 10px"),
                             "Wisconsin Births Analysis"),
                 theme = shinytheme("flatly"),
                 tabPanel("Map",
                          fluidPage(
                            leafletOutput("map", width = "100%", height = "600px"),  
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          top = 50, right = 20, width = 300, style = "opacity: 1; background-color: rgba(255, 255, 255, 0.8); padding: 10px;",
                                          h4("Filter Options"),
                                          selectInput("colorBy",
                                                      "Color Map By:",
                                                      choices = c("Total Births", "Female Births", "Male Births", "Crude Birth Rate", "Fertility Rate", "Home Births"),
                                                      selected = "Total Births"),
                                          uiOutput("info")
                            )
                          )),
                 tabPanel("Age distribution",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("mother_age", "Select Mother Age:",
                                          choices = names(stats)[grepl("Mother Age", names(stats))])
                            ),
                            mainPanel(
                              h4(textOutput("ageTableTitle"), style = "text-align: center;"),
                              DTOutput("ageTable")
                            )
                          ),
                          tags$hr(),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selected_county", "Select County:",
                                          choices = unique(stats$County))
                            ),
                            mainPanel(
                              h4(textOutput("agePlotTitle"), style = "text-align: center;"),
                              plotOutput("ageDistributionPlot")
                            )
                          )),
                 tabPanel("Ethnic distribution",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selectedEthnicity", "Select an Ethnicity:", choices = setdiff(colnames(births_data), "county"))
                            ),
                            mainPanel(
                              h4(textOutput("ethnicityTableTitle"), style = "text-align: center;"),
                              DTOutput("ethnicityComparisonTable")
                            )
                          ),
                          tags$hr(),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selectedCounty", "Select a County:", choices = unique(births_data$county))
                            ),
                            mainPanel(
                              h4(textOutput("ethnicityPlotTitle"), style = "text-align: center;"),
                              plotOutput("ethnicityPlot")
                            )
                          )),
                 tabPanel("Education distribution",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("mother_education", "Select Education Level:",
                                          choices = names(stats)[grep("Mother Education", names(stats))])
                            ),
                            mainPanel(
                              h4(textOutput("educationTableTitle"), style = "text-align: center;"),
                              DTOutput("table")
                            )
                          ),
                          tags$hr(),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selected_countys", "Select County:",
                                          choices = unique(stats$County))
                            ),
                            mainPanel(
                              h4(textOutput("educationPlotTitle"), style = "text-align: center;"),
                              plotOutput("educationDistributionPlot")
                            ))
                 )
)

server <- function(input, output) {
  selected_stats <- reactiveVal("<em>No county selected.</em>")
  
  counties_stats <- reactive({
    counties %>%
      left_join(stats, by = c("COUNTY_NAME" = "County"))
  })

  output$ethnicityPlotTitle <- renderText({  
    if (input$selectedCounty == "All") {
      paste("Distribution of Births by Ethnicity in", input$selectedCounty, "Counties")
    } else {
      paste("Distribution of Births by Ethnicity in", input$selectedCounty, "County")
    }
  })
  
  output$ethnicityTableTitle <- renderText({  
    paste("Number of", input$selectedEthnicity, "Baby Births in each County")
  })
  
  output$agePlotTitle <- renderText({  
    if (input$selected_county == "All") {
      paste("Distribution of Births by Age in", input$selected_county, "Counties")
    } else {
      paste("Distribution of Births by Age in", input$selected_county, "County")
    }
  })
  
  output$ageTableTitle <- renderText({  
    paste("Number of Birth", input$mother_age, "in each County")
  })
  
  output$educationTableTitle <- renderText({  
    paste("Number of", input$mother_education, "in each County")
  })
  
  output$educationPlotTitle <- renderText({  
    if (input$selected_countys == "All") {
    paste("Distribution of Mothers Education in", input$selected_countys, "Counties")
    } else {
      paste("Distribution of Mothers Education in", input$selected_countys, "County")
    }
  })
  
  output$table <- renderDT({
    stats %>%
      group_by(County) %>%
      summarise(Count = sum(get(input$mother_education)), .groups = 'drop') %>%
      arrange(desc(Count)) %>%
      datatable()
  })
  
  output$ageTable <- renderDT({
    stats %>%
      group_by(County) %>%
      summarise(Count = sum(get(input$mother_age)), .groups = 'drop') %>%
      arrange(desc(Count)) %>%
      datatable()
  })
  
  output$educationDistributionPlot <- renderPlot({
    filtered_data <- stats %>%
      filter(County == input$selected_countys) %>%
      summarise(
        '< Highschool' = sum(`Mother Education < Highschool`, na.rm = TRUE),
        'Highschool Diploma/GED' = sum(`Mother Education Highschool Diploma/GED`, na.rm = TRUE),
        'Some College No Degree' = sum(`Mother Education Some College No Degree`, na.rm = TRUE),
        'Associates Degree' = sum(`Mother Education Associates Degree`, na.rm = TRUE),
        'Some College/Associates' = sum(`Mother Education Some College/Associates`, na.rm = TRUE),
        'Bachelors Degree' = sum(`Mother Education Bachelors Degree`, na.rm = TRUE),
        'Masters Degree' = sum(`Mother Education Masters Degree`, na.rm = TRUE),
        'Doctorate or Professional Degree' = sum(`Mother Education Doctorate or Professional Degree`, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Education Level", values_to = "Count") %>%
      mutate(`Education Level` = factor(`Education Level`, levels = c('Doctorate or Professional Degree', 
                                                                      'Masters Degree', 
                                                                      'Bachelors Degree', 
                                                                      'Some College/Associates', 
                                                                      'Associates Degree', 
                                                                      'Some College No Degree', 
                                                                      'Highschool Diploma/GED', 
                                                                      '< Highschool')))
    
    ggplot(filtered_data, aes(x = `Education Level`, y = Count)) +
      geom_bar(stat = "identity", fill = "#2C3E50") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Education Level", y = "Count of Mothers") +
      theme(axis.title.x = element_text(size = 12),  
            axis.title.y = element_text(size = 12), 
            axis.text.x = element_text(size = 12),  
            plot.title = element_text(size = 12, hjust = 0.5),
            legend.position = "none")
  })
  
  output$ageDistributionPlot <- renderPlot({
    filtered_data <- stats %>%
      filter(County == input$selected_county) %>%
      summarise(`<15` = sum(`Mother Age < 15`, na.rm = TRUE),
                `15-17` = sum(`Mother Age 15-17`, na.rm = TRUE),
                `18-19` = sum(`Mother Age 18-19`, na.rm = TRUE),
                `20-24` = sum(`Mother Age 20-24`, na.rm = TRUE),
                `25-29` = sum(`Mother Age 25-29`, na.rm = TRUE),
                `30-34` = sum(`Mother Age 30-34`, na.rm = TRUE),
                `35-39` = sum(`Mother Age 35-39`, na.rm = TRUE),
                `40-44` = sum(`Mother Age 40-44`, na.rm = TRUE),
                `45+` = sum(`Mother Age 45+`, na.rm = TRUE)) %>%
      pivot_longer(cols = everything(), names_to = "Age Group", values_to = "Count") %>%
      mutate(`Age Group` = factor(`Age Group`, c('45+',
                                                 '40-44',
                                                 '35-39', 
                                                 '30-34', 
                                                 '25-29', 
                                                 '20-24', 
                                                 '18-19', 
                                                 '15-17', 
                                                 '<15')))
    
    ggplot(filtered_data, aes(x = `Age Group`, y = Count)) +
      geom_bar(stat = "identity", fill = "#2C3E50") +
      theme_minimal() +
      coord_flip() +
      labs(x = "Age Group", y = "Count of Mothers") + 
      theme(axis.title.x = element_text(size = 12),  
            axis.title.y = element_text(size = 12), 
            axis.text.y = element_text(size = 12),  
            plot.title = element_text(size = 12, hjust = 0.5),
            legend.position = "None") 
  })
  
  output$ethnicityPlot <- renderPlot({
    filtered_data <- filter(births_data, county == input$selectedCounty)
    
    long_data <- pivot_longer(filtered_data, 
                              cols = -county, 
                              names_to = "Ethnicity", 
                              values_to = "Births")
    
    ggplot(long_data, aes(y = Ethnicity, x = Births)) +
      geom_bar(stat = "identity", fill = "#2C3E50") +
      theme_minimal() +
      labs( y = "Ethnicity", x = "Number of Births") +
      theme(axis.title.x = element_text(size = 12),  
            axis.title.y = element_text(size = 12), 
            axis.text.y = element_text(size = 12),  
            plot.title = element_text(size = 12, hjust = 0.5),
            legend.position = "None") +
      scale_x_continuous(labels = scales::comma) +
      scale_fill_brewer(palette = "Blues")
  })
  
  output$ethnicityComparisonTable <- renderDT({
    data_to_show <- births_data %>%
      dplyr::select(county, all_of(input$selectedEthnicity)) %>%
      rename(County = county, Births = all_of(input$selectedEthnicity)) %>%
      arrange(desc(Births))
    data_to_show
  }, options = list(pageLength = 10))
  
  output$map <- renderLeaflet({
    leaflet(data = counties_stats()) %>%
      addTiles() %>%  
      addPolygons(
        fillColor = ~colorQuantile("Blues", as.numeric(as.character(get(input$colorBy))))(as.numeric(as.character(get(input$colorBy)))),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.9,
        highlight = highlightOptions(
          weight = 5,
          color = '#666',
          dashArray = '',
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~as.character(COUNTY_NAME),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        popup = ~as.character(COUNTY_NAME),
        layerId = ~as.character(COUNTY_NAME)
      )
  })
  
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id)) {
      county_data <- counties_stats() %>% filter(COUNTY_NAME == click$id)
      html_output <- paste("<h3>", click$id, "County", "</h3>",
                           "<strong>Total Births:</strong> ", county_data$`Total Births`, "<br>",
                           "<strong>Female Births:</strong> ", county_data$`Female Births`, "<br>",
                           "<strong>Male Births:</strong> ", county_data$`Male Births`, "<br>",
                           "<strong>Crude Birth Rate:</strong> ", county_data$`Crude Birth Rate`, "%<br>",
                           "<strong>Fertility Rate:</strong> ", county_data$`Fertility Rate`, "%<br>",
                           "<strong>Home Births:</strong> ", county_data$`Home Births`, "<br>",
                           "<strong>Population:</strong> ", county_data$Population)
      selected_stats(html_output)
    }
  })
  
  output$info <- renderUI({
    HTML(selected_stats())
  })
}

shinyApp(ui = ui, server = server)


