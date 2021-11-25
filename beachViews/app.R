#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(shinythemes)

beaches <- read_csv("../Data/beachData.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(
        h1("Beach Locations",h3("Click on beach markers to view photo-spheres"))
    ),
    theme = shinytheme("superhero"),

    mainPanel(
        leafletOutput("beachMap")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$beachMap <- renderLeaflet({
        leaflet(data = beaches) %>%
            addTiles() %>%
            addMarkers(popup = beaches$link)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
