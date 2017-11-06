#' Read BB2 replay file (.bbrz)
#'
#' Reads a .bbrz BB2 replay file and extracts the relevant game metadata.
#'
#' @param replay_file_path path to replay file. Can be relative or absolute path.
#'
#' @return a \code{bb2_replay} object containing relevant metadata as well as the the full replay file xml data for further processing
#' @export
read_replay <- function(replay_file_path, zipped = T){
  if (zipped)  xml <- xml2::read_xml(unzip(zipfile = replay_file_path, exdir = "/tmp"))
  else xml <- xml2::read_xml(replay_file_path)

  client_version <- xml %>% xml2::xml_child(1) %>% xml2::xml_text()
  replay_length <- xml %>% xml2::xml_length()

  mtc <- metadata_to_collect() # from mappings.R
  metadata <- extract_replay_metadata(xml, replay_length, mtc)

  replay = list(
    xml = xml,
    client_version = client_version,
    replay_length = replay_length,
    md = metadata
  )
  bb2_replay <- structure(
    replay,
    class = c("bb2_replay", "list")
  )

  return(bb2_replay)
}

#' Extract replay metadata
#'
#' Extracts the metadata from an unzipped .bbrz file.
#'
#' Relevant metadata is found in the final xml entry, reporting the game results back to the server.
#'
#' @param xml_data an \code{xml_document} resulting from reading in a .bbrz file with \code{xml2::read_xml}
#' @param replay_length How many nodes \code{xml_data} contains. The match results are contained in the final entry
#'
#' @return a list of relevant metadata as strings
extract_replay_metadata <- function(xml_data, replay_length, metadata_required) {
  last_step <- xml2::xml_child(xml_data, replay_length)
  metadata <- xml2::xml_find_all(last_step, xpath = "./RulesEventGameFinished/MatchResult/Row")

  md <- purrr::map2(metadata, metadata_required, xml2::xml_find_all) %>%
    purrr::map(xml2::xml_text) %>%
    purrr::map(fill_nulls)

  names(md) <- metadata_required

  #
  concession_data <- was_game_conceded(metadata)
  md$was_conceded <- concession_data[1]
  md$conceding_coach_id <- concession_data[2]

  league_info <- get_league_info(xml_data)
  md$league <- league_info$league
  md$competition <- league_info$competition

  # Fix up some data that needs conversion
  md$Finished <- as.POSIXct(md$Finished) # set correct datatype for the match completion time
  md$HomeScore <- ifelse(is.na(md$HomeScore), 0, md$HomeScore)
  md$AwayScore <- ifelse(is.na(md$AwayScore), 0, md$AwayScore)

  return(md)
}

was_game_conceded <- function(metadata) {
  conceding_coach_id <- NA

  #Completion Status level above metadata in hierarchy
  completion_status <- xml2::xml_find_all(metadata, xpath = "../CompletionStatus")
  was_conceded <- ifelse(length(completion_status) == 1, TRUE, FALSE)

  if (was_conceded) {
    conceding_coach_id <- xml2::xml_find_all(metadata, xpath = "../CoachResults/CoachResult/CompletionStatus") %>%
      xml2::xml_parent() %>%
      xml2::xml_find_all("IdCoach") %>%
      xml2::xml_text()
  }

  return(c(was_conceded,conceding_coach_id))
}

get_league_info <- function(replay) {
  league <- xml2::xml_find_first(replay, xpath = "//RowLeague/Name") %>% xml2::xml_text()
  comp   <- xml2::xml_find_first(replay, xpath = "//RowCompetition/Name") %>% xml2::xml_text()

  return(list(league = league, competition = comp))
}

#' Format a \code{bb2_replay}
#'
#' @param x a \code{bb2_replay} object
#' @param ...
#'
#' @export
#'
format.bb2_replay <- function(x, ...) {
  md <- x$md

 # Add asterisk to score to indicate a concession
  if (md$was_conceded) {
    if (md$conceding_coach_id == md$IdCoachHome) {
      md$HomeScore = paste0("*",md$HomeScore)
    } else {
      md$AwayScore = paste0(md$AwayScore,"*")
    }
  }

  out <- paste0("Game between ", md$CoachHomeName, " and ", md$CoachAwayName,"\n",
                "Played in the ",md$league," (" ,md$competition,") at ",format(md$Finished,"%a %b %e %Y at %R"),"\n",
                md$TeamHomeName," [",md$HomeScore," v ",md$AwayScore,"] ",md$TeamAwayName,"\n")

  out
}

#' Print a \code{bb2_replay}
#'
#' @param x a \code{bb2_replay} object
#' @param ...
#'
#' @export
#'
print.bb2_replay <- function(x,...) {
  cat(format(x))
}

#' Add turn information
#'
#' Summarises the active team, game phase and game turn for each step of a replay. Useful if you then want to run analysis on only a per-player-turn basis
#'
#' @param replay a \code{bb2_replay} object
#'
#' @return the passed in object with turn information added
#' @export
#'
#' @examples
#' r = read_replay(test_file()) %>% add_turn_info()
add_turn_info <- function(replay) {
  game_turns <- 2:replay$replay_length-1
  pb <- txtProgressBar(min = 2, max = max(game_turns), initial = 2)
  turn_data  <- purrr::map(game_turns, ~xml2::xml_child(replay$xml,.x)) %>%
    purrr::map(extract_game_state) %>%
    purrr::map(dplyr::as_data_frame) %>%
    dplyr::bind_rows(.id = "stepID") %>%
    dplyr::mutate(stepID = as.numeric(stepID)+1)

  replay$turn_data <- turn_data
  return(replay)
}

