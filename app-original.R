##Shiny App for Beaches Environmental Project

# Load necessary packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(vegan)
#library(fivethirtyeight)

# Import data
#IMPORT DATA HEREEEEEEEEEEE
beaches <- read_csv("Data/beachData.csv")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################


# Group contribution breakdown:
# We each took the lead on one tab. David did the nutrient tab, Caroline did the
# comparison of treatments, and Luis did the scatterplot. Luis took the lead
# on data wrangling, and Caroline took the lead on the shiny app framework.
# David helped out on both parts, and he coordinated moving the data between the
# two parts of the project.


#HISTOGRAM widgets:

## For selectInput, 'choices' object should be a NAMED LIST

# For TAB 2 widgets:

## vectors for beaches
beaches_choice_values <- c("Samil", "America_1", "America_2", "Barra", "Nerga", "Lanzada", "Corrubedo", "Carnota")
beaches_choice_names <- c("Samil", "America 1", "America 2", "Barra", "Nerga", "Lanzada", "Corrubedo", "Carnota")
names(beaches_choice_values) <- beaches_choice_names

##vectors for treatments
treatment_choice_values <- c("controlDiversity", "tenMinDiversity", "twentyMinDiversity")
treatment_choice_names <- c("Control", "10 Minutes", "20 Minutes")
names(treatment_choice_values) <- treatment_choice_names

#vectors for sediments
sediment_choice_values <- c("po4SedMean", "po4WaterIntMean", "po4WaterSurfMean",
                            "no2no3SedMean", "no2no3WaterIntMean", 
                            "no2no3WaterSurfMean", "nh4SedMean", "nh4WaterIntMean",
                            "nh4WaterSurfMean", "totalSedMean", "totalWaterIntMean",
                            "totalWaterSurfMean")
sediment_choice_names <- c("Mean Sediment PO4", "Mean Interstitial Water PO4", 
                           "Mean Surf Water PO4", "Mean Sediment NO2+NO3",
                           "Mean Interstitial Water NO2+NO3",
                           "Mean Surf Water NO2+NO3", "Mean Sediment NH4",
                           "Mean Interstitial Water NH4", "Mean Surf Water NH4",
                           "Mean Sediment Total Nutrient Content",
                           "Mean Interstitial Water Total Nutrient Content",
                           "Mean Surf Water Total Nutrient Content")
names(sediment_choice_values) <- sediment_choice_names

#choices for diversity indices
diversityChoices <- c("controlDiversity", "speciesRichness")
diversityNames <- c("Shannon Diversity Index", "Species Richness")
names(diversityChoices) <- diversityNames



# For TAB 3 TABLE widgets: 

############
#    ui    #
############
ui <- navbarPage(
  title = "Beaches in Galicia",
  theme = shinytheme("superhero"),
  # Tab 1: Sediment Histogram
  tabPanel(
    ##levels of nutrients in sediments on each beach
    ##sediments being evaluated can be picked by user
    title = "Sediment Nutrients Histogram",
    
    sidebarLayout(
      sidebarPanel(
        
        selectInput(inputId = "histSed",
                    label = "Choose which nutrients to evaluate:",
                    choices = sediment_choice_values,
                    selected = "po4SedMean")
      ),
      
      mainPanel(plotOutput(outputId = "hist"))
    )
  ),
  
  # Tab 2: Bar graph of fauna diversity (x axis is experimental treatment,
  # and y axis is the diversity of fauna)
  #x axis is treatment and y axis is species diversity, and each plot is a beach
  tabPanel(
    title = "Fauna Diversity Bar Graph",
    
    sidebarLayout(
      
      sidebarPanel(
        
        ##color changes for scatter plot
        radioButtons(inputId = "beachChoice",
                     label = "Select Beach to Evaluate",
                     choices = beaches_choice_values,
                     selected = "Samil")
      ),
      
      mainPanel(plotOutput(outputId = "bar"))
    )
  ),
  
  # Tab 3: Scatterplot of fauna diversity vs algae percent cover
  # x axis is percent cover and y is diversity, and each point is a beach
  tabPanel(
    title = "Diversity vs Algal Cover Scatterplot",
    
    sidebarLayout(
      
      sidebarPanel(
        
        ##color changes for scatter plot
        radioButtons(inputId = "diversityIndex",
                     label = "Select Diversity Index",
                     choices = diversityChoices,
                     selected = "controlDiversity")
      ),
      
      mainPanel(plotOutput(outputId = "scatter"))
    )
  )
  
)


############
# server   #
############
server <- function(input, output){
  
  # TAB 1: SEDIMENT HISTOGRAM
  data_for_hist <- reactive({
    ##probably filter sediment, beaches
    data <- beaches %>%
      select(site, input$histSed)
  })
  
  output$hist <- renderPlot({
    ggplot(data = data_for_hist(), aes_string(x = input$histSed)) +
      geom_histogram(color = "#2c7fb8", fill = "#7fcdbb", alpha = 0.7,
                     binwidth = case_when(
                       input$histSed == "po4SedMean" ~ 0.25,
                       input$histSed == "po4WaterIntMean" ~ 75,
                       input$histSed == "po4WaterSurfMean" ~ 30,
                       input$histSed == "no2no3SedMean" ~ 0.25,
                       input$histSed == "no2no3WaterSurfMean" ~ 10,
                       input$histSed == "nh4SedMean" ~ 5,
                       input$histSed == "totalSedMean" ~ 7.5,
                       TRUE ~ 25
                     )) +
      labs(title = 'Distribution of Nutrient Content on Beaches',
           y = "Number of Beaches",
           x = "Average PO4 Content (ug/g Sediment)")
  })
  
  # TAB 2: INTERACTIVE BAR GRAPH OF BIODIVERSITY VS TREATMENT TYPE
  #x axis is the experimental treatment and y is the fauna diversity found
  output$bar <- renderPlot({
    diversityData <- read_csv("data/diversityData.csv")%>%
      filter(site %in% input$beachChoice)
    ggplot(data = diversityData, aes(x = treatment, y = diversity)) +
      scale_color_brewer()+
      geom_col(aes(fill = treatment)) +
      labs(title = 'Fauna Diversity by Treatment', x = "Treatment", y = "Diversity")
    
    
  })
  
  # TAB 3: INTERACTIVE SCATTERPLOT OF FAUNA DIVERSITY VS ALGAL COVER
  #Filter data based on input
  data_for_diversity <- reactive({
    data <- beaches %>%
      select(site, algaePercentCover, input$diversityIndex) %>%
      rename(diversity = input$diversityIndex)
  })
  
  #Make y axis label
  yName <- reactive({
    if(input$diversityIndex == "controlDiversity"){
      data <- "Shannon Diversity Index"
    } else{
      data <- "Species Richness"
    }
  })
  
  output$scatter <- renderPlot({
    ggplot(data = data_for_diversity(), aes(x = algaePercentCover, y = diversity)) +
      scale_color_brewer(type = "qual") +
      geom_point(size = 5, aes(color = site)) +
      geom_smooth(method = 'lm', color = "black") +
      labs(title = "Effect of Algal Cover on Fauna Diversity", x = "Algae Percent Cover", y = yName())
  })
  
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)