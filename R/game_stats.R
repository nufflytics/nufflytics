#' Get game statistics
#'
#' @param uuid String of the uuid for the game
#'
#' @return A list from the JSON match stats returned from the Cyanide API
#' @export
#'
#' @examples
get_game_stats <- function(uuid) {
  #add a leading 1 to uuid if not present
  if(grepl("^0", uuid)) uuid <- stringr::str_c(1,uuid)

  httr::POST(paste0("http://app2-pc.bb2.cyanide-studio.com:21050/BB2/GetMatch/?matchUUID=1",uuid)) %>%
    httr::content() %>%
    structure(class = c("BBgame",class(.)))
}

#' Total match statistics
#'
#' Calculate the total match statistics for a team by adding the individual player statistics. Slightly slower than using the pre-summarised stats in .$RowMatch, but these are inaccurate - eg. crowdsurfs aren't recorded at all.
#' Available statistics are \code{"inflictedCasualties", "inflictedCatches", "inflictedDead", "inflictedInjuries", "inflictedInterceptions", "inflictedKO", "inflictedMetersPassing", "inflictedMetersRunning", "inflictedPasses", "inflictedPushOuts", "inflictedStuns", "inflictedTackles", "inflictedTouchdowns", "sustainedCasualties", "sustainedDead", "sustainedInjuries", "sustainedInterceptions", "sustainedKO", "sustainedStuns", "sustainedTackles"}
#'
#' @param match_data Match data object from \link{\code{get_game_stats}}
#' @param stat Name of the statistic to collect
#' @param team One of \code{"home"} or \code{"away"} to identify the team
#'
#' @return The sum of the individual player stats for the team
#' @export
#'
#' @examples m = get_game_stats(uuid = "1000304d15")
#' home_kills <- stat_total(m, "inflictedDead", "home")
stat_total <- function(match_data, stat, team) {
  team = c("home" = 1, "away" = 2)[[team]]

  match_data$MatchResultDetails$coachResults[[team]]$teamResult$playerResults %>%
    purrr::map("statistics") %>%
    purrr::map_dbl(stat) %>%
    sum
}

parse_skills <- function(match_data, player_skills) {
  all_skills <- match_data$MatchResultDetails$MatchPlayerSkillsInfos

  purrr::map(
    as.character(player_skills),
    ~all_skills[[.]] %>%
      magrittr::extract2("Name") %>%
      stringr::str_replace_all("([a-z])([A-Z])","\\1 \\2") %>%
      stringr::str_replace("Increase Movement", "+MA") %>%
      stringr::str_replace("Increase Armour", "+AV") %>%
      stringr::str_replace("Increase Agility", "+AG") %>%
      stringr::str_replace("Increase Strength", "+ST")
    )
}

#' Get team score
#'
#' @param match_data Match data object from \link{\code{get_game_stats}}
#' @param team One of \code{"home"} or \code{"away"} to identify the team
#'
#' @return Number of touchdowns scored by the specified team
#' @export
#'
#' @examples
score <- function(match_data, team) {
  score_string <- paste0(team,"Score")

  match_data$RowMatch[[score_string]]
}

#' Get Player Data
#'
#' Gets player data for a match. Recording injuries received and SPP gained.
#'
#' @param match_data Match data object from \link{\code{get_game_stats}}
#' @param team One of \code{"home"} or \code{"away"} to identify the team
#'
#' @return A \code{data_frame} containing summary information for players who played during the match
#' @export
#'
player_data <- function(match_data, team) {
  team = c("home" = 1, "away" = 2)[[team]]
  playerResults <- match_data$MatchResultDetails$coachResults[[team]]$teamResult$playerResults

  data_frame(
    name     = playerResults %>% purrr::map("playerData") %>% purrr::map("name") %>% purrr::map_chr(star_player_name),
    type     = playerResults %>% purrr::map("playerData") %>% purrr::map_int("idPlayerTypes") %>% purrr::map_chr(id_to_playertype),
    skills   = playerResults %>% purrr::map("playerData") %>% purrr::map("listSkills") %>% purrr::map2(list(match_data), ~parse_skills(.y,.x)) %>% purrr::map_chr(paste0,collapse=", "),
    injuries = playerResults %>% purrr::map_int("casualty1") %>% purrr::map_chr(id_to_casualty),
    SPP      = playerResults %>% purrr::map("playerData") %>% purrr::map_int("experience"),
    SPP_gain = playerResults %>% purrr::map_int("xp"),
    lvlup    = purrr::map2_lgl(SPP,SPP_gain, did_level_up)
  )
}

did_level_up <- function(spp_before, spp_gained) {
  level_triggers <- c(6,16,31,51,76,176)
  new_triggers <- level_triggers[level_triggers > spp_before]

  any(seq(spp_before, spp_before+spp_gained) %in% new_triggers)
}
