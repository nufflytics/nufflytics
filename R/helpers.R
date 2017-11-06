#' Convert null values to specified value
#'
#' Since .bbrz files will sometimes have no value recorded for some things (examples seen include \code{x} position of ball, game turn, etc.), we get a NULL if we try to call them. Passing all calls for data from a replay file through here allows us to control the returned value if the data is unrecorded. For example, it appears that the Ball$IsHeld value is set to "1" when the ball is held, but is not reported at all if the ball is free.
#'
#' @param data value to be checked
#' @param null_val value to return if data is \code{NULL}
#'
#' @export
fill_nulls <- function(data, null_val = NA) {
  ifelse(anyNA(data) | is.null(data) | length(data) == 0,null_val, data)
}

#' Flip y values
#'
#' Cyanide's way of recording y values produces a mirror image of what is displayed on the pitch. This function mirrors y values around the pitch midpoint (7) to produce plots that look like what the CabalVision replay looks like
#'
#' @param y_vals integers to flip.
flip_y <- function(y_vals) {
  (y_vals - 7) * (-1) + 7
}

#' Blood Bowl theme
#'
#' Based on
#'
#' @param base_size text size
#' @param base_family text family
#' @export
theme_bb <- function(base_size = 12, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill="transparent",colour="transparent"),
      panel.background = ggplot2::element_rect(fill="transparent"),
      panel.grid.major = ggplot2::element_line(colour = "grey70"),
      panel.grid.minor = ggplot2::element_line(colour = "black", linetype="dotted",size=0.1),
      #panel.background = ggplot2::element_rect(colour = "#018E0E33"), # if you want a green background for the pitch :P
      axis.ticks = ggplot2::element_blank(),
      #axis.text = element_blank(),
      axis.title = ggplot2::element_blank()
    )
}

#' Position scales for blood bowl pitch
#'
#'
#'
#' @param ... arguments passed to \code{scale_(x|y)_continuous()}
#'
#' @export
scale_x_bb <- function(...) {
  ggplot2::scale_x_continuous(breaks = c(-0.5,0.5,12.5,25.5,26.5), minor_breaks = -0.5:26.5, labels = c("-1|0","0|1","12|13","25|26","26|27"),...)
}
scale_y_bb <- function(...) {
  ggplot2::scale_y_continuous(breaks = c(-1.5,3.5,10.5,14.5), minor_breaks = -0.5:14.5, labels = c("14","10\n11","3\n4","\n0"),...)
}

#' Blood bowl coordinate system
#'
#' Fixed aspect ratio coordinate system restricted to the on-pitch area
#'
#' @param ...
#'
#' @export
coord_bb <- function(...) {
  ggplot2::coord_fixed(xlim = c(-0.5,26.5), ylim = c(-0.5,14.5), expand = FALSE,...)
}

#' Blood bowl shape scale
#'
#' Defines the shapes to be plotted for different player classes
#'
#' @param ...
#'
#' @export
scale_shape_bb <- function(...) {
  ggplot2::scale_shape_manual("Player type",
    values = c("stunty" = 20,
               "high_strength" = 21,
               "big_guy" = 24,
               "blitzer" = 22,
               "positional" = 23,
               "lineman" = 18,
               "Unknown" = 18
               )
    ,...)
}

#' Blood Bowl
#'
#' Utility function to add blood bowl pitch to any \code{ggplot} object
#'
#' Combines the theme, coordinate, and scale modifications to create a blood bowl plot into a single callable function
#' @return a list of \code{ggplot} functions
#' @export
#'
#' @examples # p is a ggplot object
#' p + bb_plot_addons()
bb_plot_addons <- function() {
  list(
    theme_bb(),
    scale_x_bb(),
    scale_y_bb(),
    coord_bb(),
    scale_shape_bb()
    )
}

#' Parse arbitrary xml data
#'
#' @param nodeset an xml_nodest object to parse through step by step
#' @param data the xpath search string for the data to extract
#' @param data_type what format to convert the data into (eg. xml2::xml_integer)
#' @param null_val what to record null values as
#'
#' @export
parse_data <- function(nodeset, data, data_type, null_val = NA) {
  xml2::xml_find_first(nodeset,data) %>% data_type() %>% fill_nulls(null_val)
}

parse_multicomponent_data <- function(l, data, data_type, null_val = NA) {
  purrr::map(l, xml2::xml_find_first, data) %>%
    purrr::map(data_type) %>%
    purrr::map(fill_nulls, null_val = null_val) %>%
    purrr::map(~unlist(.))
}

parse_bracketed_list <- function(l) {
  stringr::str_replace_all(l,"[(|)]","") %>% stringr::str_split(",") %>% unlist %>% as.integer()
}



#' Find Star Players
#'
#' Utility to find star players to record them properly in mappings.R
#'
#' @param uuid UUID of match to check
#'
#' @return Prints a list of unrecorded star players and the team they were hired for
#' @export
#'
#' @examples
find_stars <- function(uuid) {
  stats = get_game_stats(uuid,"pc")
  Races <- rep(
    c(
      stats$RowMatch$idRacesHome %>% id_to_race(),
      stats$RowMatch$idRacesAway %>% id_to_race()
    ),
    times = c(
      length(stats$MatchResultDetails$coachResults[[1]]$teamResult$playerResults),
      length(stats$MatchResultDetails$coachResults[[2]]$teamResult$playerResults)
    )
  )

  c(stats$MatchResultDetails$coachResults[[1]]$teamResult$playerResults,stats$MatchResultDetails$coachResults[[2]]$teamResult$playerResults) %>%
    purrr::map(purrr::pluck,"playerData") %>%
    purrr::map(magrittr::extract,c("name","idPlayerTypes")) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(Race = Races) %>%
    dplyr::filter(grepl("PLAYER",name))
}

