library(dplyr)
library(erify)
library(ggplot2)
library(purrr)
library(rlang)
library(scales)

source("app/R/coord-radial.R")
source("app/R/compat-types-check.R")
source("app/R/scale-expansion.R")

"%|W|%" <- function(a, b) { 
  if (!is.waive(a)) a else b 
} 

#' Internal test to see if an object is a waiver
#'
#' This function tests if the object was created by the `waiver` function. This
#' function is functionally identical to `ggplot2:::is.waive()`.
#'
#' @param x Object to be tested.
#'
#' @return `TRUE` for objects that are waivers, `FALSE` otherwise.
#' @md
#' @keywords internal
is.waive <- function(x){ return(inherits(x, "waiver")) }

#' Identity transformation (do nothing)
#'
#' @export
#' @examples
#' plot(identity_trans(), xlim = c(-1, 1))
identity_trans <- function() {
  trans_new("identity", "force", "force")
}


databaseStart <- lubridate::as_date("2021-01-01")
databaseEnd <- lubridate::today() - 1

df <- azmetr::az_daily(station_id = "az01", start_date = databaseStart, end_date = databaseEnd)

# Search: r graphics wind rose ggplot
# Search: r graphics wind rose plotly
# https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r
# https://rstudio-pubs-static.s3.amazonaws.com/284981_2e1c4c62b74446008d7ca449b744f7ab.html
# https://gist.github.com/eliocamp/20cc47b2a4b39af602a57f1e2cdd437f
# https://www.kuan-liu.com/posts/2022/02/creating-static-and-interactive-nightingale-rose-diagram-using-ggplot-and-plotly-in-r/
# https://ropenspain.github.io/climaemet/reference/ggwindrose.html
# https://github.com/tidyverse/ggplot2
# # Or the development version from GitHub:
# install.packages("pak")
#pak::pak("tidyverse/ggplot2")
#https://rdrr.io/github/hadley/ggplot2/src/R/coord-radial.R
# coord_radial() is a successor to coord_polar() with more customisation options. coord_radial() can: integrate with the new guide system via a dedicated guide_axis_theta() to display the angle coordinate. in addition to drawing full circles, also draw circle sectors by using the end argument. avoid data vanishing in the center of the plot by setting the donut argument. adjust the angle aesthetic of layers, such as geom_text(), to align with the coordinate system using the rotate_angle argument.




get_hist <- function(p) {
  d <- ggplot_build(p)$data[[1]]
  data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y)
}

# https://r-graphics.org/recipe-axes-polar
p <- ggplot(data = df, mapping = aes(x = wind_vector_dir)) +  
  geom_histogram(binwidth = 22.5, boundary = 0.0, colour = "white", fill = "#001c48", linewidth = .25) +
  coord_radial(expand = FALSE, r_axis_inside = FALSE, donut = 0) +
  scale_x_continuous(
    breaks = seq(from = 0, to = 360 - 1, by = 45),
    limits = c(0, 360)
  ) +
  #scale_y_continuous(
  #  breaks = seq(from = 0, to = max(df$wind_vector_dir, na.rm = TRUE), by = 50),
    #limits = c(0, max(df$wind_vector_dir, na.rm = TRUE))
  #) +
  #facet_wrap(facets = lubridate::month(df$datetime), ncol = 3)
  theme_minimal()
p

hist = get_hist(p)
hist$x
hist$y
hist$xmax
hist$xmin





#df <- df %>%
  dplyr::select(meta_station_name, wind_vector_dir) %>%
  dplyr::filter(is.na(wind_vector_dir) == FALSE) %>%
  dplyr::mutate(wind_vector_dir_category = dplyr::if_else(
    wind_vector_dir >= 0 & wind_vector_dir < 45, "N", dplyr::if_else(
      wind_vector_dir >= 45 & wind_vector_dir < 135, "E", dplyr::if_else(
        wind_vector_dir >= 135 & wind_vector_dir < 225, "S", dplyr::if_else(
          wind_vector_dir >= 225 & wind_vector_dir < 315, "W", "N"
          )
        )
      )
    )
  )

# filter out days with NA for either wind speed or direction