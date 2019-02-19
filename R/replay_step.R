#' Read BB2 replay step
#'
#' Extracts a single replay step from a replay file
#'
#' @param replay A \code{bb2_replay} object created with read_replay
#' @param stepID Integer specifying the index of the replay step to extract
#'
#' @return A \code{bb2_game_step} object
#' @export
#'
read_replay_step <- function(replay, stepID) {
  stopifnot(stepID > 1, stepID < replay$replay_length)
  replay_step <- xml2::xml_child(replay$xml, stepID)

  game_state  <- extract_game_state(replay_step)
  current_board_state  <- extract_board_state(replay_step)
  actions <- parse_actions(replay_step)

  return(structure(
    list(
      replay = replay,
      stepID = stepID,
      game_state  = game_state,
      board_state = current_board_state,
      actions = actions
    ), class = c("bb2_game_step")))
}

extract_board_state <- function(replay_step) {
  teams <- xml2::xml_find_all(replay_step, "./BoardState/ListTeams/TeamState/ListPitchPlayers")
  home_team <- teams[[1]] %>% xml2::xml_children()
  away_team <- teams[[2]] %>% xml2::xml_children()

  home_players <- extract_player_data(home_team, team = "home")
  away_players <- extract_player_data(away_team, team = "away")
  player_state <- dplyr::bind_rows(home_players,away_players)

  ball_state   <- extract_ball(replay_step)
  return(list(player_state=player_state, ball_state=ball_state))
}

#' Extract game state
#'
#' Identifies active team and game phase/turn
#'
#' @param replay_step
#'
#' @export
extract_game_state <- function(replay_step) {
  active_team   <- replay_step %>% xml2::xml_find_first(".//ActiveTeam") %>% xml2::xml_integer() %>% fill_nulls(0) %>% +1
  current_phase <- replay_step %>% xml2::xml_find_first(".//CurrentPhase") %>% xml2::xml_integer() %>% fill_nulls(0)
  current_turn  <- replay_step %>% xml2::xml_find_all(".//GameTurn") %>% xml2::xml_integer()

  if(length(current_turn) > 1)  {current_turn = current_turn[active_team]}
  if(length(current_turn) == 0) {current_turn = fill_nulls(current_turn,0)}

  return(list(
    active_team = active_team,
    current_phase = current_phase,
    current_turn = current_turn
  ))
}

parse_actions <- function(replay_step) {
  coach_choice <- replay_step %>% xml2::xml_find_all(".//RulesEventCoachChoice")
  board_action <- replay_step %>% xml2::xml_find_all(".//RulesEventBoardAction")
  special_action <- replay_step %>% xml2::xml_find_all(".//RulesEventSpecialAction")
  kick_off_action <- replay_step %>% xml2::xml_find_all(".//RulesEventKickOffTable")

  return(list(
    coach_choice = coach_choice,
    board_action = board_action,
    special_action = special_action,
    kick_off_action = kick_off_action
  ))
}

extract_player_data <- function(players, team = NA) {
  ID <- purrr::map2_int(players, "./Id", parse_data, data_type = xml2::xml_integer)
  x <- purrr::map2_int(players, "./Cell/x", parse_data, data_type = xml2::xml_integer) %>% purrr::map(fill_nulls,0)
  y <- purrr::map2_int(players, "./Cell/y", parse_data, data_type = xml2::xml_integer) %>% purrr::map(fill_nulls,0)
  name <- purrr::map2_chr(players, "./Data/Name", parse_data, data_type = xml2::xml_text)
  skills <- purrr::map2_chr(players, "./Data/ListSkills", parse_data, data_type = xml2::xml_text) %>% purrr::map(parse_bracketed_list)
  player_type <- purrr::map2_chr(players, "./Data/IdPlayerTypes", parse_data, data_type = xml2::xml_text)
  status <- purrr::map2_int(players, "./Status", parse_data, data_type = xml2::xml_integer)
  can_act <- purrr::map2_int(players, "./CanAct", parse_data, data_type = xml2::xml_integer, null_val = as.integer(0)) %>% as.logical()
  dplyr::data_frame(ID=ID,x=x,y=y,name=name,player_type=player_type,skills=skills,team=team, status=status, can_act = can_act)
}



extract_ball <- function(replay_step) {
  x = xml2::xml_find_all(replay_step, "./BoardState/Ball/Cell/x") %>% xml2::xml_integer() %>% fill_nulls(0)
  y = xml2::xml_find_all(replay_step, "./BoardState/Ball/Cell/y") %>% xml2::xml_integer() %>% fill_nulls(0)
  return(data.frame(x=x,y=y,name="ball",player_type="ball"))
}
