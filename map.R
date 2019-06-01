library(shiny)
library(highcharter)

olympic_data <- read.csv("data/olympic.csv", header = TRUE, stringsAsFactors = FALSE)
olympic_data <- na.omit(olympic_data)

# Remove sports that contains too few data.
filtered_data <- olympic_data[olympic_data$Sport  !=  "Art Competitons" & olympic_data$Sport  !=  "Larcrosse"
                              & olympic_data$Sport  !=  "Golf", ]
unique_sports <- select(filtered_data, Sport) %>% distinct()

my_ui <- fluidPage(
  titlePanel("The relationship between Height and medals won"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sports", 
        label = h3("Select a Sport:"),
        choices = unique(filtered_data$Sport)
      ),
      selectInput("Medal", 
                  label = h3("Select a Medal Type:"),
                  choices = unique(filtered_data$Medal)
      ),
      div(id = "note", # Adding Note
          p("This bar chart helps to explore relationships between atheletes' physical traits and medals won in all Olympic games")  
      )
    ),
    
    mainPanel(
      highchartOutput("map")
    )
  )
)

my_server <- function(input, output) {
  output$map <- renderHighchart({
    data_for_the_sport <- filtered_data %>% filter(Sport == input$sports) %>% select(Team, Medal) %>% 
      filter(Medal == input$Medal)
    table <- data.frame(table(data_for_the_sport))
    dem_viz <- hcmap('custom/world', data = data_for_the_sport, 
                     name = paste0(input$Medal, " Medal of the ", input$Medal), 
                     value = table$Freq, borderColor = "black", joinBy = c("name", "Team")) %>%
      hc_colorAxis(dataClasses = color_classes(c(seq(0, 100, by = 20)), colors = c("#ADD8E6", "#0000ff")))
  })
}

shinyApp(ui = my_ui, server = my_server)