library(shiny)
library(dplyr)
library(plotly)  
library(highcharter)

olympic_data <- read.csv("data/olympic.csv", header = TRUE, stringsAsFactors = FALSE)
olympic_data <- na.omit(olympic_data)
unique_sports <- select(olympic_data, Sport) %>% distinct()

# Exclude sports with few data
filtered_data <- olympic_data[olympic_data$Sport  !=  "Art Competitons" & olympic_data$Sport  !=  "Larcrosse"
                              & olympic_data$Sport  !=  "Golf", ]

# Find the start year and end year of the games in dataset
start_year <- min(filtered_data$Year)
end_year <- max(filtered_data$Year)


# Revise team name in order to match the country name for the map
filtered_data$Team <- replace(filtered_data$Team, filtered_data$Team == "United States" | 
                                filtered_data$Team == "United States-1" | filtered_data$Team == "United States-2",
                                   "United States of America")

my_server <- function(input, output) {
  data_reactive <- reactive({ 
    if(input$trait == "trait") {
      olympic_data %>% filter(Sport == input$sport) 
    } else {
      olympic_data %>% filter(Sport == input$sport) 
    }
  })
  
  
  output$chart <- renderPlotly({
    olympic_data <- data_reactive()
    trait <- 0
    if(input$trait == "Age") {
      trait <- 4
    } else if(input$trait == "Height") {
      trait <- 5
    } else {
      trait <- 6
    }
    
    interval <- round((max(olympic_data[,trait]) - min(olympic_data[,trait])) / 8,0)
    min <- min(olympic_data[,trait])
    max <- max(olympic_data[,trait])
    
    data <- data.frame(
      Range = c(paste(min, "-", min + interval), paste(min + interval, "-", min + 2*interval), 
                paste(min + 2*interval, "-", min + 3*interval),
                paste(min + 3*interval, "-", min + 4*interval),
                paste(min + 4*interval, "-", min + 5*interval),
                paste(min + 5*interval, "-", min + 6*interval),
                paste(min + 6*interval, "-", min + 7*interval),
                paste(min + 7*interval, "-", min + 8*interval)),
      Medals = c(nrow(olympic_data[olympic_data[,trait] <= min + interval,]), 
                 nrow(olympic_data[olympic_data[,trait] > min + interval & olympic_data[,trait] <= min + 2*interval,]),
                 nrow(olympic_data[olympic_data[,trait] > min + 2*interval & olympic_data[,trait] <= min + 3*interval,]),
                 nrow(olympic_data[olympic_data[,trait] > min + 3*interval & olympic_data[,trait] <= min + 4*interval,]),
                 nrow(olympic_data[olympic_data[,trait] > min + 4*interval & olympic_data[,trait] <= min + 5*interval,]),
                 nrow(olympic_data[olympic_data[,trait] > min + 5*interval & olympic_data[,trait] <= min + 6*interval,]),
                 nrow(olympic_data[olympic_data[,trait] > min + 6*interval & olympic_data[,trait] <= min + 7*interval,]),
                 nrow(olympic_data[olympic_data[,trait] > min + 7*interval & olympic_data[,trait] <= min + 8*interval,]))
    )
    
    xTitle <- ""
    if(input$trait == "Age") {
      xTitle <- "Age (years)"
    } else if(input$trait == "Height") {
      xTitle <- "Height (cm)"
    } else {
      xTitle <- "Weight (kg)"
    }
    
    plot_ly(data, x = ~Range, y = ~Medals, type = 'bar', 
            text = ~Medals, textposition = 'auto', 
            marker = list(color = "#305f72")
    ) %>%
      layout(title = "Athelete Traits vs Medals Won",
             xaxis = list(title = xTitle),
             yaxis = list(title = "Medals Won")) 
    
  })
  
  # Generate a map that maps the total number of medal won by each country.
  output$overview_map <- renderHighchart({
    country_and_medals <- select(filtered_data, Team)
    country_and_medals <- data.frame(table(country_and_medals))
    colnames(country_and_medals) <- c("Country", "Freq")
    max_freq <- max(country_and_medals$Freq)
    Overview_map <- hcmap('custom/world', data = country_and_medals, 
                          name = paste0("The amount of medal won between ", start_year, " and ", end_year), 
                          value = "Freq", borderColor = "black", joinBy = c("name", "Country")) %>%
      hc_colorAxis(dataClasses = color_classes(c(0, 10, 50, 100, 500, 1000, 2000, 3000, 4000, 5000), 
                                               colors = c("#ADD8E6", "#ef3674")))
  })
  
  # Generate a map that maps the number of medal won by each country.
  output$map <- renderHighchart({
    data_for_the_sport <- filtered_data %>% filter(Sport == input$sports) %>% select(Team)
    table <- data.frame(table(data_for_the_sport$Team))
    colnames(table) <- c("Country", "Freq")
    map_viz <- hcmap('custom/world', data = table, 
                     name = paste0("Number of Medal for ", input$sports), 
                     value = "Freq", borderColor = "black", joinBy = c("name", "Country")) %>%
      hc_colorAxis(dataClasses = color_classes(c(seq(0, max(table$Freq), by = max(table$Freq)/5)), 
                                               colors = c("#ADD8E6", "#0000ff")))
  })
  
}