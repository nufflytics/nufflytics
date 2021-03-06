% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{bb_plot_addons}
\alias{bb_plot_addons}
\title{Blood Bowl}
\usage{
bb_plot_addons()
}
\value{
a list of \code{ggplot} functions
}
\description{
Utility function to add blood bowl pitch to any \code{ggplot} object
}
\details{
Combines the theme, coordinate, and scale modifications to create a blood bowl plot into a single callable function
}
\examples{
# p is a ggplot object
p + bb_plot_addons()
}
