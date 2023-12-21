#' `fxnFigureMainSubtitle.R` - Build subtitle for main figure based on current date
#' 
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `figureMainSubtitle` - Subtitle for main wind direction figure based on current date


fxnFigureMainSubtitle <- function(startDate, endDate) {
  figureMainSubtitle <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          "All months from", gsub(" 0", " ", format(startDate, "%B %d, %Y")), "through", gsub(" 0", " ", format(endDate, "%B %d, %Y")), 
          sep = " "
        )
      ), 
      
      class = "figure-main-subtitle"
    )
  
  return(figureMainSubtitle)
}