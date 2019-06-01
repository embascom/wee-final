library(shiny)
library(dplyr)
library(plotly)  

olympic_data <- read.csv("data/olympic.csv", header = TRUE, stringsAsFactors = FALSE)
olympic_data <- na.omit(olympic_data)
unique_sports <- select(olympic_data, Sport) %>% distinct()

# Define UI for application that renders the map and table
my_ui <- fluidPage(
  navbarPage("Menu",
             
    # Add Main Page
    tabPanel("Athelete Traits and Medals Won",
      includeCSS("style_1.css"), 
      div(id = "container",
        img(src = "logo.png", id = "logo"),
        
        # Application title
        titlePanel("Athelete Traits and Medals Won"), # Title of the app
        
        # Sidebar with a selectInput for the variable for analysis
        sidebarLayout(
          sidebarPanel(
            selectInput( # Widget 1: Shape selections 
              inputId = "sport",
              label = "Sports",
              choices = unique_sports
            ),
            selectInput( # Widget 1: Shape selections 
              inputId = "trait",
              label = "Traits",
              selected = "Height",
              choices = c("Age", "Height", "Weight")
            ),
            div(id = "note", # Adding Note
                p("This bar chart helps to explore relationships between atheletes' physical traits and medals won in all Olympic games")  
            )
          ),
          
          mainPanel(
            plotlyOutput("chart") # reactive output provided by leaflet
          )
        )
      )
    ),
    
    # Add Summary Page
    tabPanel("Summary",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    )
  )
)

# Define server that renders a map and a table
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
}

shinyApp(ui = my_ui, server = my_server)
