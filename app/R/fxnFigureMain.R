#' `fxnFigureMain`: generates polar graph of summarized wind direction data
#' 
#' @param: `inData` - data table of daily AZMet station data
#' @return: `figureMain` - ggplot polar graph of summarized wind direction data
#' 
fxnFigureMain <- function(inData) {
  
  figureMain <- ggplot2::ggplot(data = inData, mapping = aes(x = wind_vector_dir)) +  
    geom_histogram(binwidth = 22.5, boundary = 0.0, colour = "#FFFFFF", fill = "#001c48", linewidth = .25) +
    #coord_radial(expand = FALSE, r_axis_inside = FALSE, donut = 0) +
    coord_polar() +
    
    scale_x_continuous(
      breaks = seq(from = 0, to = 360 - 1, by = 45),
      limits = c(0, 360)
    ) +
    
    theme_minimal() +
    
    theme( # https://ggplot2.tidyverse.org/reference/theme.html
      #line,
      #rect,
      text = element_text(family = "sans"),
      #title,
      #aspect.ratio,
      #axis.title,
      #axis.title.x = element_text(color = "#343a40", face = "plain", size = 9, hjust = 0.0),
      #axis.title.x.top,
      #axis.title.x.bottom,
      #axis.title.y = element_text(color = "#343a40", face = "plain", size = 9, angle = 0),
      #axis.title.y.left,
      #axis.title.y.right,
      #axis.text,
      axis.text.x = element_text(color = "#343a40", face = "plain", size = 9),
      #axis.text.x.top,
      #axis.text.x.bottom,
      axis.text.y = element_text(color = "#343a40", face = "plain", size = 9),
      #axis.text.y.left,
      #axis.text.y.right,
      #axis.ticks,
      #axis.ticks.x,
      #axis.ticks.x.top,
      #axis.ticks.x.bottom = element_line(color = "#343a40", linewidth = 0.25, linetype = "solid", lineend = "round", arrow = NULL, inherit.blank = FALSE),
      #axis.ticks.y,
      #axis.ticks.y.left = element_line(color = "#343a40", linewidth = 0.25, linetype = "solid", lineend = "round", arrow = NULL, inherit.blank = FALSE),
      #axis.ticks.y.right,
      #axis.ticks.length,
      #axis.ticks.length.x,
      #axis.ticks.length.x.top,
      #axis.ticks.length.x.bottom,
      #axis.ticks.length.y,
      #axis.ticks.length.y.left,
      #axis.ticks.length.y.right,
      #axis.line,
      #axis.line.x,
      #axis.line.x.top,
      #axis.line.x.bottom = element_line(color = "#343a40", linewidth = 0.25, linetype = "solid", lineend = "round", arrow = NULL, inherit.blank = FALSE),
      #axis.line.y,
      #axis.line.y.left = element_line(color = "#343a40", linewidth = 0.25, linetype = "solid", lineend = "round", arrow = NULL, inherit.blank = FALSE),
      #axis.line.y.right,
      #legend.background,
      #legend.margin,
      #legend.spacing,
      #legend.spacing.x,
      #legend.spacing.y,
      #legend.key,
      #legend.key.size,
      #legend.key.height,
      #legend.key.width,
      #legend.text,
      #legend.text.align,
      #legend.title,
      #legend.title.align,
      #legend.position,
      #legend.direction,
      #legend.justification,
      #legend.box,
      #legend.box.just,
      #legend.box.margin,
      #legend.box.background,
      #legend.box.spacing,
      #panel.background,
      #panel.border,
      #panel.spacing,
      #panel.spacing.x,
      #panel.spacing.y,
      #panel.grid,
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      #panel.grid.major.x,
      #panel.grid.major.y,
      #panel.grid.minor.x,
      #panel.grid.minor.y,
      #panel.ontop,
      #plot.background,
      #plot.title,
      #plot.title.position = "plot",
      #plot.subtitle = element_text(family = "sans", face = "plain", color = "#343a40", size = 9)
      #plot.caption,
      #plot.caption.position,
      #plot.tag,
      #plot.tag.position,
      #plot.margin,
      #strip.background,
      #strip.background.x,
      #strip.background.y,
      #strip.clip,
      #strip.placement,
      #strip.text,
      #strip.text.x,
      #strip.text.y,
      #strip.switch.pad.grid,
      #strip.switch.pad.wrap,
      #...,
      #complete = FALSE,
      #validate = TRUE
    )
  
  return(figureMain)
}
