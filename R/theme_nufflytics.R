theme_nufflytics <- function(...) {
  ggplot2::theme_linedraw(base_family = "Open Sans Condensed Bold") +
    ggplot2::theme(plot.subtitle = element_text(family = "Open Sans Condensed Light"),
          axis.text = element_text(family = "Open Sans Condensed Light"),
          legend.text = element_text(family="Open Sans Condensed Light"),
          legend.position = "bottom" )
}
