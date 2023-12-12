

# <App description>

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
        selectInput("dataset", label = "Dataset", choices = ls("package:datasets"))
      )
    ), # sidebarPanel()
    
    mainPanel(
      id = "mainPanel",
      width = 8,
      
      verbatimTextOutput("summary"),
      tableOutput("table")
    ) # mainPanel()
  ) # sidebarLayout()
) # htmltools::htmlTemplate()


# Server --------------------

server <- function(input, output, session) {
  
  # Reactive events -----
  
  # Outputs -----
  
  output$summary <- renderPrint({
    dataset <- get(input$dataset, "package:datasets")
    summary(dataset)
  })
  
  output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    dataset
  })
}

# Run --------------------

shinyApp(ui = ui, server = server)