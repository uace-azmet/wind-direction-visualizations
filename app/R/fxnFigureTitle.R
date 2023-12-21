#' `fxnFigureTitle.R` - Build title for HTML table based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param timeStep - AZMet data time step
#' @return `figureTitle` - Table title for HTML table based on user input


fxnFigureTitle <- function(azmetStation, timeStep) {
  figureTitle <- 
    htmltools::h4(
      htmltools::HTML(
        paste(
          "Visualization of", timeStep, "Wind Direction Data from the AZMet", azmetStation, "station", 
          sep = " "
        ),
      ),
      
      class = "figure-title"
    )
  
  return(figureTitle)
}
