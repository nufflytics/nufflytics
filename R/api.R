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
api_coaches <- function(key = NULL, league = NULL, competition = NULL, platform = NULL, limit = NULL) {

  if(is.null(key)) stop("API access key is required")

  ret = httr::GET(
    "http://web.cyanide-studio.com",
    path=c("ws","bb2","coaches",""),
    query = list(key=key, league=league, competition=competition, platform=platform, limit=limit)
    )
  httr::warn_for_status(ret, "get coach list")

  type = headers(ret)$`content-type`

  if (grepl("application/json", type)) {
    ret %>% httr::content(as="text") %>% jsonlite::fromJSON()
  } else {
    stop("Coaches API not returning json data")
  }
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
api_matches <- function(key = NULL, league = NULL, competition = NULL, platform = NULL, limit = NULL, start  = NULL, end = NULL) {

  if(is.null(key)) stop("API access key is required")

  ret = httr::GET(
    "http://web.cyanide-studio.com",
    path=c("ws","bb2","matches",""),
    query = list(key=key, league=league, competition=competition, platform=platform, limit=limit, start=start, end=end)
  )
  httr::warn_for_status(ret, "get match list")

  type = headers(ret)$`content-type`

  if (grepl("application/json", type)) {
    ret %>% httr::content(as="text") %>% jsonlite::fromJSON(simplifyDataFrame = F)
  } else {
    stop("Matches API not returning json data")
  }
}


#' Team information
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
api_teams <- function(key = NULL, league = NULL, competition = NULL, platform = NULL, limit = NULL) {

  if(is.null(key)) stop("API access key is required")

  ret = httr::GET(
    "http://web.cyanide-studio.com",
    path=c("ws","bb2","teams",""),
    query = list(key=key, league=league, competition=competition, platform=platform, limit=limit)
  )
  httr::warn_for_status(ret, "get team list")

  type = headers(ret)$`content-type`

  if (grepl("application/json", type)) {
    ret %>% httr::content(as="text") %>% jsonlite::fromJSON()
  } else {
    stop("Matches API not returning json data")
  }
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
api_ladder <- function(key = NULL, league = NULL, competition = NULL, ladder_size = NULL) {
  if(is.null(key)) stop("API access key is required")

  ret = httr::GET(
    "http://web.cyanide-studio.com",
    path=c("ws","bb2","ladder",""),
    query = list(key=key, league=league, competition=competition, ladder_size=ladder_size)
  )
  httr::warn_for_status(ret, "get competition ladder")

  type = headers(ret)$`content-type`

  if (grepl("application/json", type)) {
    ret %>% httr::content(as="text") %>% jsonlite::fromJSON()
  } else {
    stop("Ladder API not returning json data")
  }
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
api_match <- function(key = NULL, match_id = NULL, platform = NULL) {
  if(is.null(key)) stop("API access key is required")

  ret = httr::GET(
    "http://web.cyanide-studio.com",
    path=c("ws","bb2","match",""),
    query = list(key=key, match_id=match_id, platform=platform)
  )
  httr::warn_for_status(ret, "get match details")

  type = headers(ret)$`content-type`

  if (grepl("application/json", type)) {
    ret %>% httr::content(as="text") %>% jsonlite::fromJSON(simplifyDataFrame = F)
  } else {
    stop("Match API not returning json data")
  }
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
api_halloffame <- function(key = NULL, league = NULL, competition = NULL, platform = NULL, limit = NULL, exact = 1) {
  if(is.null(key)) stop("API access key is required")

  ret = httr::GET(
    "http://web.cyanide-studio.com",
    path=c("ws","bb2","halloffame",""),
    query = list(key=key, league=league, competition=competition, platform=platform, limit=limit, exact=exact)
  )
  httr::warn_for_status(ret, "get hall of fame details")

  type = headers(ret)$`content-type`

  if (grepl("application/json", type)) {
    ret %>% httr::content(as="text") %>% jsonlite::fromJSON(simplifyDataFrame = F)
  } else {
    stop("Hall of Fame API not returning json data")
  }
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
api_contests <- function(key = NULL, league = NULL, competition = NULL, status = NULL, limit = NULL, round = NULL, platform = NULL, exact = 1) {
  if(is.null(key)) stop("API access key is required")

  ret = httr::GET(
    "http://web.cyanide-studio.com",
    path=c("ws","bb2","contests",""),
    query = list(key=key, league=league, competition=competition, platform=platform, limit=limit, status=status, round=round, exact=exact)
  )
  httr::warn_for_status(ret, "get contest details")

  type = headers(ret)$`content-type`

  if (grepl("application/json", type)) {
    ret %>% httr::content(as="text") %>% jsonlite::fromJSON(simplifyDataFrame = F)
  } else {
    stop("Contests API not returning json data")
  }
}


#' Title
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
api_team <- function(key = NULL, name = NULL, id = NULL, platform = NULL) {
  if(is.null(key)) stop("API access key is required")

  ret = httr::GET(
    "http://web.cyanide-studio.com",
    path=c("ws","bb2","team",""),
    query = list(key=key, name=name, id=id, platform=platform)
  )
  httr::warn_for_status(ret, "get team details")

  type = headers(ret)$`content-type`

  if (grepl("application/json", type)) {
    ret %>% httr::content(as="text") %>% jsonlite::fromJSON(simplifyDataFrame = F)
  } else {
    stop("Team API not returning json data")
  }
}
