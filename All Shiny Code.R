library(readxl)
library(ggplot2)
library(dplyr)
library(sf)
library(shiny)
library(plotly)
library(tidyr)
library(readr)
library(grDevices)

## Maps Section
# Load geometric data
wi_counties <- st_read("https://uwmadison.box.com/shared/static/iezuh9n53402jfz10vldy58r1rcihmym.geojson")
wi_counties <- wi_counties %>% 
  arrange(COUNTY_NAME)

# Load birth data
birth <- read.csv("https://uwmadison.box.com/shared/static/b0k2jqpexlbtx23onrb5v8cumybcj9jk.csv")
birth <- birth[-1, ]  # Remove header
# Perform an inner join based on the COUNTY_NAME and county columns
merged_data <- merge(birth, wi_counties, by.x = "county", by.y = "COUNTY_NAME")
# Convert to sf
merged_data <- st_as_sf(merged_data)


## Mother Ethnicity Section
# Define a base set of pastel colors
base_pastel_colors <- c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2")
# Create a function to interpolate between these colors
pastel_palette <- colorRampPalette(base_pastel_colors)
# Generate 72 pastel colors
pastel_colors_72 <- pastel_palette(72)

# Load the new dataset
births <- read_csv('https://uwmadison.box.com/shared/static/xjci0nm6qpbxa0d4o9mnf2q1jqhz2j06.csv')
births <- births %>%
  mutate(across(-county, ~ifelse(. == "<5", "0", as.character(.)))) %>%
  mutate(across(-county, as.numeric))

## Barplot
births2 = read.csv("https://raw.githubusercontent.com/ChristopherGottwaldt/stat436FinalProject/main/2022birthdataWiscIteration2.csv?token=GHSAT0AAAAAACNUA6QGL7HZNETXTQSCY7O6ZQCFZYA")

births2 = births2 %>% na.omit()

# Rename the columns before pivoting
births2 = rename(births2, Female = `f_births`, Male = `m_births`)

# Pivot longer
births_long = pivot_longer(births2, 
                           cols = c(Male, Female), 
                           names_to = "Gender", 
                           values_to = "Births")

# Remove "Total" column
births2 = births2[-1,]


# UI
ui <- fluidPage(
  titlePanel("Visualizing Birth Rates in Wisconsin"),
  tabsetPanel(
    tabPanel("Map",
             radioButtons("variable", "Select Variable to Display:",
                          choices = c("Total Births" = "total_births",
                                      "Female Births" = "f_births",
                                      "Male Births" = "m_births",
                                      "Crude Birth Rate" = "crude_birth_rate",
                                      "Fertility Rate" = "fertility_rate",
                                      "women_18+_<HS_diploma" ="women_18._.HS_diploma",
                                      "women_18+_HS_diploma/GED"="women_18._HS_diploma.GED",
                                      "women_18+_some_college_associates"="women_18._some_college_associates",
                                      "women_18+_bachelors+"="women_18._bachelors."
                          ), 
                          selected = "total_births"),  # Default selection
             plotlyOutput("map")
    ),
    tabPanel("Ethnicity Plots",
             fluidRow(
               column(
                 6,
                 selectInput("selectedCounty", "Select a County:", choices = unique(births$county))
               ),
               column(
                 6,
                 selectInput("selectedEthnicity", "Select an Ethnicity:",
                             choices = setdiff(colnames(births), "county"))
               )
             ),
             fluidRow(
               column(
                 6,
                 plotOutput("ethnicityPlot")
               ),
               column(
                 6,
                 plotOutput("ethnicityComparisonPlot", height = "800px")
               )
             )
    ),
    tabPanel("Births Data",
             fluidRow(
               column(
                 6,
                 selectInput("gender", "Gender", choices = c("Male", "Female"), selected = c("Male", "Female"), multiple = TRUE)
               ),
               column(
                 6,
                 selectInput("county", "County", choices = unique(births$county), selected = unique(births$county), multiple = TRUE)
               )
             ),
             fluidRow(
               column(
                 12,
                 plotOutput("birth_counts")
               )
             )
    )
  )
)


# Server
server <- function(input, output) {
  
  # Create map
  output$map <- renderPlotly({
    # Extract selected variable
    variable <- input$variable
    
    # Create tooltip text with county names and variable values
    tooltip_text <- paste(
      "County: ", merged_data$county, "<br>",
      variable, ": ", format(merged_data[[variable]], nsmall = 2)
    )
    
    # Plot map
    p <- ggplot() +
      geom_sf(data = merged_data, aes(fill = !!as.name(variable), text = tooltip_text)) +  # Modify tooltip
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      ggtitle(paste("Map of Wisconsin by", variable)) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")  # Ensure tooltip displays custom text
  })
  
  output$ethnicityPlot <- renderPlot({
    filtered_data <- filter(births, county == input$selectedCounty)
    
    long_data <- pivot_longer(filtered_data, 
                              cols = -county, 
                              names_to = "Ethnicity", 
                              values_to = "Births")
    
    ggplot(long_data, aes(x = Ethnicity, y = Births, fill = Ethnicity)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = paste("Distribution of Births by Ethnicity in", input$selectedCounty),
           x = "Ethnicity",
           y = "Number of Births") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = base_pastel_colors) # Use pastel colors
  })
  
  output$ethnicityComparisonPlot <- renderPlot({
    ethnicity_col <- input$selectedEthnicity
    
    comparison_data <- births %>%
      filter(county != "All") %>%
      group_by(county) %>%
      summarize(Total_Births = sum(.data[[ethnicity_col]], na.rm = TRUE)) %>%
      filter(Total_Births > 0) 
    
    ggplot(comparison_data, aes(x = reorder(county, -Total_Births), y = Total_Births, fill = county)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) +
      theme_minimal() +
      labs(title = paste("Total Births for", ethnicity_col, "Ethnicity Across Counties"),
           x = "County", y = "Number of Births") +
      theme(legend.position = "none") +
      scale_fill_manual(values = pastel_colors_72) # Use pastel colors
  })
  
  output$birth_counts <- renderPlot({
    # Filter births_long based on selected counties
    filtered_births_long <- births_long %>%
      filter(county %in% input$county)
    
    # Filter births based on selected counties
    filtered_births <- births2 %>%
      filter(county %in% input$county)
    
    if ("Male" %in% input$gender && "Female" %in% input$gender) {
      print("Displaying combined plot")
      combined <- ggplot(filtered_births_long) +
        geom_col(aes(x = county, y = Births, fill = Gender), position = "dodge") +
        scale_fill_manual(values = c("Male" = "lightblue", "Female" = "lightpink")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(fill = "Gender")
      return(combined)
    } else if ("Male" %in% input$gender) {
      print("Displaying males plot")
      males <- ggplot(filtered_births) +
        geom_col(aes(x = county, y = Male), fill = "lightblue") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      return(males)
    } else if ("Female" %in% input$gender) {
      print("Displaying females plot")
      females <- ggplot(filtered_births) +
        geom_col(aes(x = county, y = Female), fill = "lightpink") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      return(females)
    } else {
      # If neither Male nor Female is selected, display an empty plot
      print("No gender selected, displaying empty plot")
      return(plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", main = ""))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
