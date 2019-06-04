library(shiny)
library(dplyr)
library(plotly)
library(highcharter)
library(DT)

olympic_data <- read.csv("data/olympic.csv", header = TRUE, stringsAsFactors = FALSE)
olympic_data <- na.omit(olympic_data)
filtered_data <- olympic_data[olympic_data$Sport != "Art Competitions" & olympic_data$Sport != "Lacrosse"
                              & olympic_data$Sport != "Golf", ]
unique_sports <- select(filtered_data, Sport) %>% distinct()
olympic_table <- select(olympic_data, ID, Name, Sex, Age, Height, Team, Games, Sport, Event)

# Define the first page content
main_page <- tabPanel(
  "Main", # label for the tab in the navbar
  includeCSS("style_1.css"), 
  div(id = "container",
    div(id = "content",
      h2("Project Authors"),
      p("Wei Fan, Emily Bascom, Eric Kim"),
      h2("The Olympic Games"),
      p("The Olympic Games are leading international sporting events featuring summer and winter sports competitions in which 
        thousands of athletes from around the world participate in a variety of competitions. The Olympics are held every four 
        years with the summer and winter games alternating by occuring every four years but two years apart. Their creation was 
        inspired by the ancient Olympics Games, which were held in Olympia, Greece, from the 8th century BC to the 4th century 
        AD."),
      h2("Project Description"),
      p(""),
      h2("Dataset"),
      p("The dataset that was used for this project contains information about all the athletes and results from the 
        1896 Athens to 2016 Rio Olympic games. The data was collected by Randi H. Griffin, a data scientist and a lecturer at the 
        Northeastern University. He scraped historical olympic records from sports-reference.com, a company based in Pennsylvania, 
        that delivers sports statistics. Their primary data providers are Gracenote, Sportradar, and Delta Sports Group. You can
        get the dataset ", a("here.", href="https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results")),
      h2("Visualizations"),
      p("Countries: This page displays the the number of medals won by each country. *takes time to load*", br(),
      "Athletes: This page contains all of the individual entries for every Olympic Games.",br(), 
      "Traits: This page explores the relationship between atheletes' traits and the total number of medals won.")
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
            selected = "Swimming",
            choices = unique_sports
          ),
          selectInput( # Widget 1: Shape selections 
            inputId = "trait",
            label = "Traits",
            selected = "Height",
            choices = c("Age", "Height", "Weight")
          )
        ),
        mainPanel(
          plotlyOutput("chart") # reactive output provided by leaflet
        )
      ),
      div(id = "content",
          p("This bar chart helps to explore relationships between athletes' physical traits and medals won in all Olympic games.
            Though it is difficult to claim and justify correlations with high certainty for the given physical traits and Olympic games, this
            chart provides a valuable insight into how a certain physical characteristic may have helped in athletes' performance in winning
            their Olympic medals.")
      )
  )
)

# Define content for the third page: Analysis of Number of Medal won by country
countries <- tabPanel(
  "Countries", # label for the tab in the navbar
  includeCSS("style_1.css"),
  div(id = "container",
    # img(src = "logo.png", id = "logo"),
    titlePanel("Country Participation Information and Medals Won"),
    
    h3("Total Number of Medals Won by Country from 1896 to 2016"),
    highchartOutput("overview_map"),
    div(id = "content",
        p("The Map displays the total number of medals won. From the graph, we know United States won the most medals (4357).
          The number of medal won is ranged from 0 to 4357. There is a huge difference between the number of medals won in 
          differenct countried. One possible guess is that some countries are participating in more sports than other countries.
          Also, some countries might join Olympic game later than other countries.Let's examine the realtionship between the 
          number of medals won and the number of sports the country participated in.")),
    
    h3("Number of Sports Participated in by Country"),
    highchartOutput("sports_map"),
    div(id = "content",
        p("From the graph above, countries that won more medals tend to have participated in more type of sports. However, 
          participating in more sports does not guarantee more medals won. ")
    ),
    

    h3("Relationship Between Sports and Medals Won"),
    div(id = "content",
        p("Check the box below to see the scatter plot using the log10 version of the sports participated in and the medals
          won. Uncheck the box below to see the scatter plot of the unchanged version. The color in the graph indicates how 
          many lines of records we have."),
        p("Be careful, 0's will not be changed by the log version graph, and they will still display as 0's.")
    ),
    checkboxInput("checkbox", label = "Log version"),
    plotlyOutput("sport_and_medal"),
    div(id = "content",
        p("The above graph demonstrates the relationship between the number of sports a given country has participated in and how
          many medals they have won.")
        ),
    
    h3("Number of Medal Won by Sport"),
    sidebarLayout(
      sidebarPanel(
        selectInput("sports",
                    label = h3("Select a Sport:"),
                    choices = unique(filtered_data$Sport)
        ),
        div(id = "note", # Adding Note
            p("The map displays the number of medals won by countries for the sport selected.")
        )
      ),

      mainPanel(
        highchartOutput("map")
      )
    ),
    div(id = "content",
        p("The above graph shows the number of medals each country has won in a given sport.")
    )
  )
)

# Define content for the fourth page
athletes <- tabPanel(
  "Athletes",
  includeCSS("style_1.css"),
  div(id = "container",
      titlePanel("Athlete Entry Information"),
      sidebarPanel(
        checkboxGroupInput("show_vars", "Columns to show:", names(olympic_table), selected = names(olympic_table))
      ),
      mainPanel(
        div(id = "content",
            h2("Description"),
            p("The following table ")
            ),
        DT::dataTableOutput("table")
      )
    )
)


# Pass each page to a multi-page layout (`navbarPage`)
ui <- fluidPage(
  includeCSS("style_1.css"),
  div(id = "mainHeader",
    img(src = "logo.png", id = "logo"),
    titlePanel("THE OLYMPIC GAMES") # show with a displayed title
  ),
  navbarPage(
    "GROUP WEE", # application title
    main_page,         # include the first page content
    countries,  # include the second page content
    athletes,  # include the third page content
    trait_page       # include the fourth page content
  )
)