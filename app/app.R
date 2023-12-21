# Shiny app to visualize summaries of daily wind direction data

# Add code for the following
# 
# 'azmet-shiny-template.html': <!-- Google tag (gtag.js) -->
# 'azmet-shiny-template.html': <!-- CSS specific to this AZMet Shiny app -->

# Edit the following [in square brackets]:
# 
# 'azmet-shiny-template.html': <title>[Web Application Title] | Arizona Meteorological Network</title>
# 'azmet-shiny-template.html': <h1 class="hidden title"><span class="field field--name-title field--type-string field--label-hidden">[Hidden Title]</span></h1>
# 'azmet-shiny-template.html': <article role="article" about="[/application-areas]" class="node node--type-az-flexible-page node--view-mode-full clearfix">
# 'azmet-shiny-template.html': <span class="lm-az"></span>
# 'azmet-shiny-template.html': <h1 class="mt-4 d-inline">[Web Tool Name]</h1>
# 'azmet-shiny-template.html': <h4 class="mb-0 mt-2">[High-level text summary]</h4>


# Libraries
library(azmetr)
library(dplyr)
library(ggplot2)
library(htmltools)
library(lubridate)
library(shiny)
library(vroom)

# Functions
#source("./R/fxnABC.R", local = TRUE)

# Scripts
#source("./R/scr##DEF.R", local = TRUE)


# UI --------------------

ui <- htmltools::htmlTemplate(
  
  "azmet-shiny-template.html",
  
  sidebarLayout = sidebarLayout(
    position = "left",
    
    sidebarPanel(
      id = "sidebarPanel",
      width = 4,
      
      verticalLayout(
        helpText(em(
          "Select an AZMet station and specify the month or months of interest. Then, click or tap 'VISUALIZE DATA'."
        )),
        
        br(),
        selectInput(
          inputId = "azmetStation", 
          label = "AZMet Station",
          choices = stationNames[order(stationNames$stationName), ]$stationName,
          selected = stationNames[order(stationNames$stationName), ]$stationName[1]
        ),
        
        #br(),
        #selectInput(
        #  inputId = "monthsOfInterest",
        #  label = "Months",
        #  choices = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
        #  selected = NULL,
        #  multiple = TRUE,
        #  selectize = TRUE
        #),
        
        br(),
        actionButton(
          inputId = "visualizeData", 
          label = "Visualize Data",
          class = "btn btn-block btn-blue"
        )
      )
    ), # sidebarPanel()
    
    mainPanel(
      id = "mainPanel",
      width = 8,
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureTitle"))
      ), 
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureMainSubtitle"))
      ), 
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, plotOutput(outputId = "figureMain"))
      ), 
      br(),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureFacetsSubtitle"))
      ), 
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, plotOutput(outputId = "figureFacets"))
      ), 
      br()
    ) # mainPanel()
  ) # sidebarLayout()
) # htmltools::htmlTemplate()


# Server --------------------

server <- function(input, output, session) {
  
  # Reactive events -----
  
  # AZMet data ELT
  dfAZMetData <- eventReactive(input$visualizeData, {
    idPreview <- showNotification(
      ui = "Preparing data visualization . . .", 
      action = NULL, 
      duration = NULL, 
      closeButton = FALSE,
      id = "idPreview",
      type = "message"
    )
    
    on.exit(removeNotification(id = idPreview), add = TRUE)
    
    fxnAZMetDataELT(
      azmetStation = input$azmetStation,
      timeStep = timeStep,
      startDate = databaseStart,
      endDate = databaseEnd)
  })
  
  # Build figure - faceted polar plots
  figureFacets <- eventReactive(input$visualizeData, {
    fxnFigureFacets(inData = dfAZMetData())
  })
  
  # Build faceted figure subtitle
  figureFacetsSubtitle <- eventReactive(input$visualizeData, {
    fxnFigureFacetsSubtitle(
      startDate = databaseStart,
      endDate = databaseEnd
    )
  })
  
  # Build figure - main polar plot
  figureMain <- eventReactive(input$visualizeData, {
    fxnFigureMain(inData = dfAZMetData())
  })
  
  # Build main figure subtitle
  figureMainSubtitle <- eventReactive(input$visualizeData, {
    fxnFigureMainSubtitle(
      startDate = databaseStart,
      endDate = databaseEnd)
  })
  
  # Build figure title
  figureTitle <- eventReactive(input$visualizeData, {
    fxnFigureTitle(
      azmetStation = input$azmetStation, 
      timeStep = timeStep
    )
  })
  
  # Outputs -----
  
  output$figureFacets <- renderPlot({
    figureFacets()
  }, res = 48)
  
  output$figureFacetsSubtitle <- renderUI({
    figureFacetsSubtitle()
  })
  
  output$figureMain <- renderPlot({
    figureMain()
  }, res = 96)
  
  output$figureMainSubtitle <- renderUI({
    figureMainSubtitle()
  })
  
  output$figureTitle <- renderUI({
    figureTitle()
  })
}

# Run --------------------

shinyApp(ui = ui, server = server)
