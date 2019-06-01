library(shiny)
library(dplyr)
library(plotly)  

source("ui.R")
source("server.R")

shinyApp(ui = my_ui, server = my_server)
