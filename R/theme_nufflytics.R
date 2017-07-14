#' Nufflytics theme
#'
#' @param ... extra parameters not currently used
#'
#' @return a modified \code{ggplot} theme for use on nufflytics.com
#' @export
#'
theme_nufflytics <- function(...) {
  ggplot2::theme_linedraw(base_family = "Open Sans Condensed Bold") +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(family = "Open Sans Condensed Light"),
          axis.text = ggplot2::element_text(family = "Open Sans Condensed Light"),
          legend.text = ggplot2::element_text(family="Open Sans Condensed Light"),
          legend.position = "bottom" )
}
