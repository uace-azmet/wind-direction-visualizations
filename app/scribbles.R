
# https://github.com/tidyverse/ggplot2 Or the development version from GitHub:
#install.packages("pak")
#pak::pak("tidyverse/ggplot2")

#https://rdrr.io/github/hadley/ggplot2/src/R/coord-radial.R
# coord_radial() is a successor to coord_polar() with more customisation options. coord_radial() can: integrate with the new guide system via a dedicated guide_axis_theta() to display the angle coordinate. in addition to drawing full circles, also draw circle sectors by using the end argument. avoid data vanishing in the center of the plot by setting the donut argument. adjust the angle aesthetic of layers, such as geom_text(), to align with the coordinate system using the rotate_angle argument.


library(dplyr)
library(ggplot2)
library(purrr)


databaseStart <- lubridate::as_date("2021-01-01")
databaseEnd <- lubridate::today() - 1

df <- azmetr::az_daily(station_id = "az01", start_date = databaseStart, end_date = databaseEnd)


#get_hist <- function(p) {
#  d <- ggplot_build(p)$data[[1]]
#  data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y)
#}

ggplot(data = df, mapping = aes(x = wind_vector_dir)) +  
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

ggsave(file = paste0("tuc-wind-direction", "-", Sys.Date(), ".png"))

#hist = get_hist(p)
#hist$x
#hist$y
#hist$xmax
#hist$xmin


# filter out days with NA for either wind speed or direction

#df <- df %>%
#  dplyr::select(meta_station_name, wind_vector_dir) %>%
#  dplyr::filter(is.na(wind_vector_dir) == FALSE) %>%
#  dplyr::mutate(wind_vector_dir_category = dplyr::if_else(
#    wind_vector_dir >= 0 & wind_vector_dir < 45, "N", dplyr::if_else(
#      wind_vector_dir >= 45 & wind_vector_dir < 135, "E", dplyr::if_else(
#        wind_vector_dir >= 135 & wind_vector_dir < 225, "S", dplyr::if_else(
#          wind_vector_dir >= 225 & wind_vector_dir < 315, "W", "N"
#          )
#        )
#      )
#    )
#  )
  
  
# https://www.kuan-liu.com/posts/2022/02/creating-static-and-interactive-nightingale-rose-diagram-using-ggplot-and-plotly-in-r/
library(DT)
  
set.seed(123)
dat <- tibble(
  Year = paste0("Y",rep(2016:2021, each=3)),
  Instrument = rep(c("physical","mental","social"),6),
  Score = sample(1:5, size = 3*6, replace = T))
  
dat %>% datatable(
  rownames = FALSE,
  options = list(
    columnDefs = list(list(className = 'dt-center', targets = 0:2))
  ))

library(plotly)
  
fig <- plot_ly() %>%
  add_trace(
    data = dat,
    r = ~Score, #radius 
    theta = ~Year, #angle
    type="barpolar", 
    color = ~Instrument,
    hovertemplate = paste('Score: %{r}', '<br>Year: %{theta}<br>')) %>%
  layout(
    legend=list(title=list(text='Health Instrument')), 
    polar = list(angularaxis = list(
      rotation = 90,
      direction = 'clockwise', #position the polar diagram
      period = 6)), #evenly distribute angles to host categorical var;
    margin = -0.5) 
fig

