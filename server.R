library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(highcharter)

original_data <- read.csv("data/olympic.csv", header = TRUE, stringsAsFactors = FALSE)

# Giving a dataset, the data_cleaning function will return a cleaned verison of the given dataset.
data_cleaning <- function(dataset) {
  # Remove sports with few data
  dataset <- dataset[dataset$Sport  !=  "Art Competitons" & dataset$Sport  !=  "Larcrosse"
                                & dataset$Sport  !=  "Golf", ]
  # Remove non alphabetic character and non space character in team name(country)
  dataset$Team <- gsub("(-[0-9]*)$", "", dataset$Team)
  
  # Revise team name in order to match the country name for the map
  dataset$Team <- replace(dataset$Team, dataset$Team == "United States", "United States of America")
  dataset$Team <- replace(dataset$Team, dataset$Team == "Congo (Kinshasa)" | dataset$Team == "Congo (Brazzaville)", "Congo")
  return(dataset)
} 

olympic_data <- data_cleaning(na.omit(original_data))
with_nonmedal_record <- data_cleaning(original_data)
unique_sports <- select(olympic_data, Sport) %>% distinct()

# Find the start year and end year of the games in dataset
start_year <- min(olympic_data$Year)
end_year <- max(olympic_data$Year)

# filtered datasets for later use in Page 3
country_and_medals <- data.frame(table(select(olympic_data, Team)))
colnames(country_and_medals) <- c("Country", "Freq")
sports_and_country <- with_nonmedal_record %>% select(Team, Sport)
sports_and_country <- sports_and_country[(!is.na(sports_and_country$Team))|(!is.na(sports_and_country$Sport)),]
sports_and_country_summary <- data.frame(table(sports_and_country)) %>% filter(Freq != 0) %>% select(Team, Sport)
sports_and_country_summary <- data.frame(table(sports_and_country_summary$Team))
colnames(sports_and_country_summary) <- c("Country", "Freq")

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
    Overview_map <- hcmap('custom/world', data = country_and_medals, 
                          name = paste0("The amount of medal won between ", start_year, " and ", end_year), 
                          value = "Freq", borderColor = "black", joinBy = c("name", "Country")) %>%
      hc_colorAxis(dataClasses = color_classes(c(0, 10, 50, 100, 500, 1000, 2000, 3000, 4000, 5000), 
                                               colors = c("#ADD8E6", "#ef3674")))
  })
  
  
  # Generate a map that displays the total number of sports participated by each country.
  output$sports_map <- renderHighchart({
    sports_map <- hcmap('custom/world', data = sports_and_country_summary, 
                        name = "Number of sports' types participated by each country", value = "Freq",
                        borderColor = "black", joinBy = c("name", "Country")) %>% 
      hc_colorAxis(dataClasses = color_classes(seq(0, 60, by = 10),colors = c("#ADD8E6", "#ef3674")))
  })
  
  # Generate a scatter plot that shows the number of sports paticipated and the number of medal won.
  output$sport_and_medal <- renderPlotly({
    # Join "country_and_medals" and "sports_and_country" by country name
    sports_country_medal <- sports_and_country_summary
    sports_country_medal$medals <- "0"
    for (i in 1:nrow(sports_country_medal)) {
      country_name <- as.character(sports_country_medal[i, 1])
      if(is.element(country_name, country_and_medals$Country)) {
        sports_country_medal[i, 3] <- country_and_medals[country_and_medals$Country == country_name,][[2]]
      }
    }
    sports_country_medal$medals <- as.numeric(sports_country_medal$medals)
    colnames(sports_country_medal) <- c("Country", "Sports", "Medals")
    if (input$checkbox) {
      sports_country_medal$Medals <- log(sports_country_medal$Sports, 10)
      View(sports_country_medal)
    } 
    sport_and_medal <- plot_ly(data = sports_country_medal, x = ~Sports, y = ~Medals, text = ~Country, 
                               color = ~Medals, size = ~Medals) %>% 
      layout(title = "NUmber of Sports Versus Amount of Medals Won")
  })
  
  # Generate a map that maps the number of medal won by each country.
  output$map <- renderHighchart({
    data_for_the_sport <- olympic_data %>% filter(Sport == input$sports) %>% select(Team)
    table <- data.frame(table(data_for_the_sport$Team))
    colnames(table) <- c("Country", "Freq")
    map_viz <- hcmap('custom/world', data = table,
                     name = paste0("Number of Medal for ", input$sports),
                     value = "Freq", borderColor = "black", joinBy = c("name", "Country")) %>%
      hc_colorAxis(dataClasses = color_classes(c(seq(0, max(table$Freq), by = max(table$Freq)/5)),
                                               colors = c("#ADD8E6", "#0000ff")))
  })
  
}