library(ggplot2)
library(dplyr)
library(sf)
library(shiny)
library(plotly)

# geometric data
wi_counties <- st_read("https://uwmadison.box.com/shared/static/iezuh9n53402jfz10vldy58r1rcihmym.geojson")
wi_counties <- wi_counties %>% 
  arrange(COUNTY_NAME)

# birth data
birth <- read.csv("https://uwmadison.box.com/shared/static/b0k2jqpexlbtx23onrb5v8cumybcj9jk.csv")
birth<- birth[-1, ]  #remove header
# Perform an inner join based on the COUNTY_NAME and county columns
merged_data <- merge(birth, wi_counties, by.x = "county", by.y = "COUNTY_NAME")
# convert to sf
merged_data <- st_as_sf(merged_data)

# UI
ui <- fluidPage(
  titlePanel("County Births Map"),
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
    ggplot() +
      geom_sf(data = merged_data, aes(fill = !!as.name(variable), text = tooltip_text)) +  # Modify tooltip
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      ggtitle(paste("Map of Wisconsin by", variable)) +
      theme_minimal() +
      theme(legend.position = "bottom") -> p
    
    ggplotly(p, tooltip = "text")  # Ensure tooltip displays custom text
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
