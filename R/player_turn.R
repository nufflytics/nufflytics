#' Parses an entire player turn from a \code{bb2_replay} object
#'
#' @param replay A \code{bb2_replay} object
#' @param team Which team to work on. Must be either "home" or "away"
#' @param turn Game turn to extract. Must be a valid turn number for the replay
#'
#' @return A \code{player_turn} object
#' @export
parse_player_turn <- function(replay, team = c("home","away"), turn = 0) {
  turn_steps <- get_turn_steps(replay, team, turn)

  start_position <- read_replay_step(replay, turn_steps[1]-1) # board state at start of turn is in the replay step before this starts

  action_list <- purrr::map(turn_steps , ~classify_actions(replay, .x)) %>% dplyr::bind_rows(.id = "turn_step")

  action_list
}

#' Get replay steps for game turn
#'
#' Extracts the replay steps corresponding to a specified team's game turn. \code{add_turn_info()} must have been run on the replay object first.
#'
#' @param replay A \code{bb2_replay} object
#' @param team Which team to work on. Must be either "home" or "away"
#' @param turn Game turn to extract. Must be a valid turn number for the replay
#'
#' @return A vector of replay stepIDs for the team and game turn.
#' @export
get_turn_steps <- function(replay, team = c("home","away"), turn = 0) {
  if( ! "turn_data" %in% names(replay) ) {
    cat("Haven't yet processed turn data\nRunning add_turn_info()...\nCan take some time, so recommend running this once and then accessing the results\n(See examples in ?add_turn_info for more)")
    replay <- add_turn_info(replay)
  }
  if(turn < 0 | turn > max(replay$turn_data$current_turn)) {
    stop("The turn is '",turn,"'. This is probably an error?")
  }

  teams = c("home" = 1, "away" = 2)

  replay$turn_data %>%
    dplyr::filter(active_team == teams[team], current_turn == turn, current_phase != 6) %>%
    .$stepID
}

classify_actions <- function(r, stepID) {
  actions <- read_replay_step(r, stepID)$actions
  a_l <- list()
  if ( length(actions$board_action) > 0 ) {
    b_a <- parse_board_actions(actions$board_action)
    a_l <- append(a_l, b_a)
  }
  a_l
}

parse_board_actions <- function(b_a) {
  action_types    <- purrr::map2_chr(b_a, "./ActionType",parse_data, data_type = xml2::xml_text) %>% purrr::map(action_type) %>% unlist
  player_id       <- purrr::map2_int(b_a, "./PlayerId",parse_data, data_type = xml2::xml_integer) %>% unlist
  cell_to         <- purrr::map2(b_a, "./Order/CellTo/Cell", ~ xml2::xml_find_first(.x,.y) %>% xml2::as_list())
  cell_from       <- purrr::map2(b_a, "./Order/CellFrom", ~ xml2::xml_find_first(.x,.y) %>% xml2::as_list())

  #Can have multiple results in a single board action (eg, injury dice and cas dice)
  results <- purrr::map2(b_a, "./Results/BoardActionResult", ~xml2::xml_find_all(.x,.y))

  order_completed <- purrr::map(results, parse_multicomponent_data, "./IsOrderCompleted", data_type = xml2::xml_integer, null_val = 0) %>% purrr::map(as.logical)
  roll_type       <- purrr::map(results, ~parse_multicomponent_data(., "./RollType", data_type = xml2::xml_text, null_val = NA_character_) %>% purrr::map(roll_type)) %>% purrr::map(unlist)
  dice_rolled     <- purrr::map(results, parse_multicomponent_data, "./CoachChoices/ListDices", data_type = xml2::xml_text, null_val = NA_character_) %>% purrr::map(unlist)
  rerollable     <- purrr::map(results, parse_multicomponent_data, "./CoachChoices/Reroll", data_type = xml2::xml_integer, null_val = 0) %>% purrr::map(as.logical)
  roll_required   <- purrr::map(results, parse_multicomponent_data, "./Requirement", data_type = xml2::xml_integer) %>% purrr::map(unlist)

  d = list(
    action_types = action_types,
    player_id = player_id,
    cell_to_x = purrr::map(cell_to,"x"),
    cell_to_y = purrr::map(cell_to,"y"),
    cell_from_x = purrr::map(cell_from,"x"),
    cell_from_y = purrr::map(cell_from,"y"),
    order_completed = order_completed,
    roll_type = roll_type,
    dice_rolled = dice_rolled,
    roll_required = roll_required,
    rerollable = rerollable
  )
  #Post-processing some data
  d <- d %>%
    purrr::map_if(grepl("cell",names(.)), ~purrr::map(.,fill_nulls) %>% unlist %>% as.integer) %>% # convert x/y values to integers
    dplyr::as_data_frame()

  #Unnest it to deal with board action results which can have multiple per board action
  d <- tidyr::unnest(d)

  # Halve block dice rolled since a second set are reported
  d$dice_rolled <- purrr::map2(d$roll_type, d$dice_rolled, modify_rolled_dice)

  d
}


#' Modifies rolled dice types
#'
#' Removes the doubled up dice for block dice
#'
#' @param type string describing the roll type
#' @param dice_list string with the bracketed list of dice rolled
#'
#' @export
#'
modify_rolled_dice <- function(type, dice_list) {
  if (!type %in% c("Block")) {return(parse_bracketed_list(dice_list))}

  d = parse_bracketed_list(dice_list)
  midpoint = length(d)/2

  d[1:midpoint]
}
