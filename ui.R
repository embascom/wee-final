library(shiny)
library(dplyr)
library(plotly)
library(highcharter)

olympic_data <- read.csv("data/olympic.csv", header = TRUE, stringsAsFactors = FALSE)
olympic_data <- na.omit(olympic_data)
unique_sports <- select(olympic_data, Sport) %>% distinct()
filtered_data <- olympic_data[olympic_data$Sport  !=  "Art Competitons" & olympic_data$Sport  !=  "Larcrosse"
                              & olympic_data$Sport  !=  "Golf", ]

# Define the first page content
main_page <- tabPanel(
  "Main", # label for the tab in the navbar
  includeCSS("style_1.css"), 
  div(id = "container",
    div(id = "content",
      h2("Project Authors"),
      p("Wei Fan, Emily Bascom, Eric Kim"),
      h2("Project Description"),
      p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna 
        aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
        Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint 
        occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
      h2("Dataset"),
      p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna 
        aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
        Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint 
        occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
    )    
  )
)

# Define content for the second page
trait_page <- tabPanel(
  "Traits", # label for the tab in the navbar
  includeCSS("style_1.css"), 
  div(id = "container",
      # img(src = "logo.png", id = "logo"),
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
)

# Define content for the third page
page_three <- tabPanel(
  "Page 3", # label for the tab in the navbar
  includeCSS("style_1.css"),
  div(id = "container",
    # img(src = "logo.png", id = "logo"),
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
)

# Define content for the fourth page
page_four <- tabPanel(
  "Page 4" # label for the tab in the navbar
  # ...more content would go here...
)

# Pass each page to a multi-page layout (`navbarPage`)
ui <- fluidPage(
  includeCSS("style_1.css"),
  div(id = "mainHeader",
    img(src = "logo.png", id = "logo"),
    titlePanel("TITLE TITLE TITLE TITLE") # show with a displayed title
  ),
  navbarPage(
    "GROUP WEE", # application title
    main_page,         # include the first page content
    trait_page,         # include the second page content
    page_three,  # include the third page content
    page_four  # include the four page content
  )
)