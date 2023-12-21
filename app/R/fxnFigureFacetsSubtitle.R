#' `fxnFigureFacetsSubtitle.R` - Build subtitle for faceted figure based on current date
#' 
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `figureFacetsSubtitle` - Subtitle for faceted wind direction figure based on current date


fxnFigureFacetsSubtitle <- function(startDate, endDate) {
  figureFacetsSubtitle <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          "By month from", gsub(" 0", " ", format(startDate, "%B %d, %Y")), "through", gsub(" 0", " ", format(endDate, "%B %d, %Y")), 
          sep = " "
        )
      ), 
      
      class = "figure-facets-subtitle"
    )
  
  return(figureFacetsSubtitle)
}