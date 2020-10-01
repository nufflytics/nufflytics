#' Coach information
#'
#' Returns information on coaches for a specified league/competition
#'
#' @param key API access key (required)
#' @param league League name (defaults to Cabalvision Official League)
#' @param competition Competition name (defaults to Free Ladder)
#' @param platform pc, ps4, or xb1 (optional)
#' @param limit Number of entries to return (defaults to 100)
#'
#' @return List containing two named elements:
#' \itemize{
#' \item{\code{coaches}: a data.frame of coach information - name, id, country, language}
#' \item{\code{meta}: a list containing information about the query}
#' }
#' @export
#'
#' @examples
#'
api_coaches <- function(key = NA, league = NA, competition = NA, platform = NA, limit = NA, ...) {
  api_call(
    method = "coaches",
    params = list(key=key, league=league, competition=competition, platform=platform, limit = limit),
    simplify = T,
    ...
  )
}

#' List matches for a league/competition
#'
#' Returns information on matches played in the specified league/competition. Starts from the most recent match within the specified window and works backwards.
#'
#' @param key API access key (required)
#' @param league League name (defaults to Cabalvision Official League)
#' @param competition Competition name (optional)
#' @param platform pc, ps4, or xb1 (optional)
#' @param limit Number of entries to return (defaults to 100)
#' @param id_only Return match summaries or just the match id (0|1, defaults to match summary)
#' @param start Start date (defaults to 20 days ago)
#' @param end End date (defaults to today)
#'
#' @return List containing four named elements:
#' \itemize{
#' \item{\code{debug}: a list containing the query parameters used}
#' \item{\code{matches}: a list containing basic information about each match - uuid, league/competition, coaches, teams and summarised statistics}
#' \item{\code{urls}: a list containing urls for various game assets}
#' \item{\code{meta}: a list containing information about the query}
#' }
#'
#' @export
#'
#' @examples
api_matches <- function(key = NA, league = NA, competition = NA, platform = NA, limit = NA, id_only = NA, start  = NA, end = NA, ...) {
  api_call(
    method = "matches",
    params = list(key = key, league = league, competition = competition, platform = platform, limit = limit, id_only = id_only, start = start, end = end),
    ...
  )
}

#' League information
#'
#' Returns information about a specified league
#'
#' @param key API access key (required)
#' @param league League name (defaults to Cabalvision Official League)
#' @param platform pc, ps4, or xb1 (optional)
#'
#' @return List containing two named elements:
#' \itemize{
#' \item{\code{league}: a list of league information - date_last_match, logo, treasury, team_count}
#' \item{\code{meta}: a list containing information about the query}
#' }
#'
#' @export
#'
#' @examples
api_league <- function(key = NA, league = NA, platform = NA, ...) {
  api_call(
    method = "league",
    params = list(key = key, league = league, platform = platform),
    simplify = F,
    ...
  )
}


#' Teams information
#'
#' Returns information for teams in a league/competition
#'
#' @param key API access key (required)
#' @param league League name (defaults to Cabalvision Official League)
#' @param competition Competition name (optional)
#' @param platform pc, ps4, or xb1 (optional)
#' @param limit Number of entries to return (defaults to 100)
#'
#' @return List containing three named elements:
#' \itemize{
#' \item{\code{teams}: a data.frame of team information - team, id, coach, logo, race_id, race, description (motto), dateLastMatch}
#' \item{\code{meta}: a list containing information about the query}
#' \item{\code{urls}: a list of urls for various game assets}
#' }
#'
#' @export
#'
#' @examples
api_teams <- function(key = NA, league = NA, competition = NA, platform = NA, limit = NA, ...) {
  api_call(
    method = "teams",
    params = list(key = key, league = league, competition = competition, platform = platform, limit = limit),
    simplify = T,
    ...
  )
}

#' Competition Ladder
#'
#' @param key API access key (required)
#' @param league League name (defaults to Cabalvision Official League)
#' @param competition Competition name (defaults to Free Ladder)
#' @param ladder_size Size of ladder to return (defaults to 100)
#'
#' @return List containing three named elements:
#' \itemize{
#' \item{\code{ranking}: a data.frame of the ladder ranking - team.rank, team.name, team.id, team.tv, team.score, team.race, team.logo, coach.name, coach.id, coach.lang, coach.country}
#' \item{\code{ladder}: a list of information about the ladder - league/competition names, ladder id}
#' \item{\code{meta}: a list containing information about the query}
#' }
#' @export
#'
#' @examples
api_ladder <- function(key = NA, league = NA, competition = NA, ladder_size = NA, ...) {
  api_call(
    method = "ladder",
    params = list(key = key, league = league, competition = competition, ladder_size = ladder_size),
    simplify = T,
    ...
  )
}



#' Detailed match information
#'
#' Returns detailed match information from a BB2 match
#'
#' @param key API access key (required)
#' @param match_id UUID for the match (required)
#' @param platform pc, ps4, or xb1 (optional)
#'
#' @return List containing six named elements:
#' \itemize{
#' \item{\code{uuid}: the match uuid}
#' \item{\code{match}: a list of information about the match containing, some top level information, a list of coach information from the match, a list of team information from the match (including individual player statistics)}
#' \item{\code{coaches}: a list of information about the coaches}
#' \item{\code{teams}: a list of information about the teams}
#' \item{\code{urls}: a list of urls for various game assets}
#' \item{\code{meta}: a list of information about the query}
#'
#' @export
#'
#' @examples
#'
api_match <- function(key = NA, match_id = NA, platform = NA, ...) {
  api_call(
    method = "match",
    params = list(key = key, match_id = match_id, platform = platform),
    ...
  )
}


#' Hall of Fame stats for a league/competition
#'
#' @param key API access key (required)
#' @param league League name (defaults to Cabalvision Official League)
#' @param competition Competition name (defaults to all competitions in league)
#' @param platform pc, ps4, or xb1 (optional)
#' @param limit Number of statistics to return (defaults to 100)
#' @param exact 0 or 1 - Exact matching for league name
#'
#' @return
#' @export
#'
#' @examples
api_halloffame <- function(key = NA, league = NA, competition = NA, platform = NA, limit = NA, exact = 1, ...) {
  api_call(
    method = "halloffame",
    params = list(key = key, league = league, competition = competition, platform = platform, limit = limit, exact = exact),
    ...
  )
}

#' Contests
#'
#' Information on contests for a league/competition (includes scheduled as well as played games)
#'
#' @param key API access key (required)
#' @param league League name (defaults to Cabalvision Official League)
#' @param competition Competition name (defaults to all competitions in league)
#' @param platform pc, ps4, or xb1 (optional)
#' @param limit Number of statistics to return (defaults to 100)
#' @param status scheduled, in_progress, or played (defaults to scheduled)
#' @param round Round of competition
#' @param exact 0 or 1 - Exact matching for league name
#'
#' @return List containing four named elements
#' \item{\code{upcoming_matches}: list of match information - match_id, competition, round, opponents, winner (if played)}
#' \item{\code{urls}: a list of urls for various game assets}
#' \item{\code{context}: Information about the league/competition requested}
#' \item{\code{meta}: a list of information about the query}
#' @export
#'
#' @examples
api_contests <- function(key = NA, league = NA, competition = NA, status = NA, limit = NA, round = NA, platform = NA, exact = 1, ...) {
  api_call(
    method = "contests",
    params = list(key = key, league = league, competition = competition, status = status, limit = limit, round = round, platform = platform, exact = exact),
    ...
  )
}


#' Team information
#'
#' @param key API access key (required)
#' @param name Team name (ignored if id is provided)
#' @param id Team id
#' @param platform pc, ps4, or xb1 (optional)
#'
#' @return List containing five named elements:
#' \item{\code{team}: a list of information about the team - name, race, rerolls, apo, nbplayers}
#' \item{\code{coach}: a list of information about the team's coach}
#' \item{\code{roster}: a list of information about the players on the team - id, name, value, stats, xp, skills, casualties_state}
#' \item{\code{urls}: a list of urls for various game assets}
#' \item{\code{meta}: a list of information about the query}
#'
#' @export
#'
#' @examples
api_team <- function(key = NA, name = NA, id = NA, platform = NA, ...) {
  api_call(
    method = "team",
    params = list(key = key, name = name, id = id, platform = platform),
    ...
  )
}

#' Generic API call method
#'
#' To be customised for each available method
#'
#' @param method The name of the method to call
#' @param params The query parameters to call the method with
#' @param simplify Should the resulting data be simplified? \code{fromJSON} parameter \code{simplifyDataFrame}.
#'
#' @return The data from the specified API call as a list
#'
#' @examples
api_call <- function(method, params, simplify = F, debug = F) {
  params[is.na(params)] <- NULL

  if(is.null(params$key)) stop("API access key is required")

  ret = httr::GET(
    "http://web1.cyanide-studio.com",
    path=c("ws","bb2",method,""),
    query = params
  )
  httr::warn_for_status(ret)

  type = httr::headers(ret)$`content-type`

  ret_list <- ret %>% httr::content(as = "text") %>% jsonlite::fromJSON(simplifyDataFrame = simplify)

  if (debug) ret_list$response <- ret

  if (grepl("application/json", type)) {
    ret_list
  } else {
    stop(paste(stringr::str_to_title(method), "API not returning json data"))
  }
}
