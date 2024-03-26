library(ggplot2)
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(grDevices)

# Define a base set of pastel colors
base_pastel_colors <- c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2")
# Create a function to interpolate between these colors
pastel_palette <- colorRampPalette(base_pastel_colors)
# Generate 72 pastel colors
pastel_colors_72 <- pastel_palette(72)

# Load the new dataset
births = read_csv('https://uwmadison.box.com/shared/static/xjci0nm6qpbxa0d4o9mnf2q1jqhz2j06.csv')
births <- births %>%
  mutate(across(-county, ~ifelse(. == "<5", "0", as.character(.)))) %>%
  mutate(across(-county, as.numeric))

ui <- fluidPage(
  titlePanel("Births by Mother's Ethnicity"),
  fluidRow(column(6,selectInput("selectedCounty", "Select a County:", choices = unique(births$county))),
           column(6,selectInput("selectedEthnicity", "Select an Ethnicity:",
                                choices = setdiff(colnames(births), "county")))),
  fluidRow(column(6,plotOutput("ethnicityPlot")), column(6,plotOutput("ethnicityComparisonPlot", height = "800px")))
  )

server <- function(input, output) {
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
           x = "Ethnicity", y = "Number of Births") +
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
}

shinyApp(ui, server)
