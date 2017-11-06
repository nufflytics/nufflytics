#' Plot a BB2 game step
#'
#' Presents a diagnotic plot of the current board state of a \code{bb2_game_step} object. Likely to have been created from a replay file with \link{read_replay_step}.
#'
#' @param x a \code{bb2_game_step} object
#' @param ...
#'
#' @export
plot.bb2_game_step <- function(x,..., alpha = 1) {
  ggplot2::ggplot(x$board_state$player_state,ggplot2::aes(x=as.numeric(x),y=flip_y(as.numeric(y)))) +
    #ggplot2::geom_tile(ggplot2::aes(fill=can_act),alpha = 0.2) +
    ggplot2::geom_point(ggplot2::aes(fill = team,colour=team), size=5, alpha = alpha) +
    ggplot2::geom_point(data = x$board_state$ball_state, shape=20, size = 3, alpha = 0.6*alpha) +
    ggplot2::geom_point(data = function(x) {dplyr::filter(x,status==1)}, shape = 3, size = 4, alpha = alpha) +
    ggplot2::geom_point(data = function(x) {dplyr::filter(x,status==2)}, shape = 8, size = 4, alpha = alpha) +
    ggrepel::geom_text_repel(data = function(x) {dplyr::filter(x,x>-1&y>-1)}, ggplot2::aes(label=ID), alpha = alpha) +
    ggplot2::ggtitle(paste(x$replay$md$CoachHomeName,"v",x$replay$md$CoachAwayName,"\n","step",x$stepID)) +
    bb_plot_addons()
}
