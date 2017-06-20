#' Format team data
#'
#' Team data used to construct a team for simulations. Can be a data frame row, named vector or list. Will be converted to list prior to processing. Must have data for \code{TV_displayed}, \code{TV_extra}, \code{wins}, \code{draws}, and \code{losses}. Other data allowed and will be passed through to the returned team object.
#'
#' @param team_data
#'
#' @return List of team information ready for input to game simulation with \code{\link{process_game}}
#' @export
make_team <- function(team_data) {
  team <- as.list(team_data)
  team$TV <- team$TV_displayed + team$TV_extra
  total_matches <- sum(team$wins, team$draws ,team$losses)

  #Work out current W/D/L probablilty from team record
  if (total_matches == 0) { probs = list(Win = 1/3, Tie = 1/3, Loss = 1/3) }
  else {
    probs <- purrr::map(
      list(Win = team$wins, Tie = team$draws, Loss = team$losses),
      ~magrittr::divide_by(., total_matches)
    )
  }
  team$probs <- probs

  #Initialise points scored at 0
  team$points <- 0

  structure(team, class = c("team", "list"))
}

#' Update win probabilities
#'
#' Takes a team object and updates it's internal win probabilities to accurately match the W/D/L record. Should be run once the results of a game simulation have been included in the team's record if required.
#'
#' @param team A team object from \code{\link{make_team}}
#'
#' @return Returns the passed team object with win probabilities updated for additional games played
#' @export
update_probs <- function(team) {
  total_matches <- sum(team$wins, team$draws, team$losses)
  team$probs <- purrr::map(
    list(Win = team$wins, Tie = team$draws, Loss = team$losses),
    ~magrittr::divide_by(., total_matches)
  )

  team
}

#' Update TV
#'
#' Takes a team object and changes it's TV to something new. Takes the TV change from CCL data for the relevant race within +- 20 TV of the team's pre-game TV. To avoid single occurance outliers having a large impact on the results, sample five options and take the median
#'
#' @param team A team object from \code{\link{make_team}}
#'
#' @return Returns the passed team object with an updated TV
#' @export
update_tv <- function(team) {
  ccl_distribution <- ccl_tv_change %>%
    dplyr::filter(race == team$race, tv >= team$TV - 20, tv <= team$TV + 20) %>%
    magrittr::extract2("tv_change")

  if (length(ccl_distribution) > 0) {
    tv_change <- sample(ccl_distribution, 5, replace = TRUE) %>% median
  }
  else { #don't have CCL information to go by. If high TV, on average bring it down. If low, bring it up
    if(team$TV >= 1500)
      tv_change <- rnorm(1, mean = -10, sd = 10) %>% round %>% magrittr::multiply_by(10)
    else {
      tv_change <- rnorm(1, mean = 10, sd = 10) %>% round %>% magrittr::multiply_by(10)
    }
  }
  team$TV <- team$TV + tv_change

  team
}

#' Calculate game probabilities
#'
#' Combine probabilites derived from the home and away team's record, as well as a TV based win probability from CCL data, to determine probability of outcomes for a specific game. Relative importance of the factors is adjusted with the \code{weights} parameter. Function adds a little bit of randomness to the final calculated properties to avoid a deterministic outcome.
#'
#' @param home_team \code{team} object for the home team
#' @param away_team \code{team} object for the away team
#' @param weights Vector of weights for probabilities. Length three, order is CCL (TV difference), Home team, Away team
#'
#' @return List of probabilities for home team game outcomes
#' @export
calc_game_probs <- function(home_team, away_team, weights) {
  game_tv_diff = home_team$TV - away_team$TV

  if (game_tv_diff < -500) game_tv_diff <- -500
  if (game_tv_diff > 500) game_tv_diff <- 500

  ccl_probs <- ccl_data %>%
    dplyr::filter(tv_diff >= game_tv_diff-20, tv_diff <= game_tv_diff+20) %>%
    dplyr::group_by(result) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(pct = n/sum(n)) %>%
    magrittr::extract2("pct") %>%
    as.list %>%
    purrr::set_names(c("Loss","Tie","Win"))

  win_prob  <- weighted.mean(c(ccl_probs$Win,  home_team$probs$Win,  away_team$probs$Loss), weights)
  tie_prob  <- weighted.mean(c(ccl_probs$Tie,  home_team$probs$Tie,  away_team$probs$Tie),  weights)
  loss_prob <- weighted.mean(c(ccl_probs$Loss, home_team$probs$Loss, away_team$probs$Win),  weights)

  probs = list(Win = win_prob, Tie = tie_prob, Loss = loss_prob)

  #Tweak game_probs with a little bit of randomness
  probs <- map_dbl(probs, ~rnorm(1,.,0.02))

  probs
}

#' Simulate game
#'
#' Simulates game outcome between two teams and updates team's record and points tally.
#'
#' @param team_list List of team objects with home team, then away team
#' @param weights Vector of weights for probabilities. Length three, order is CCL (TV difference), Home team, Away team
#'
#' @return Return the team list with updated record and points tally.
#' @export
process_game <- function(team_list, weights) {
  home_team <- team_list[[1]]
  away_team <- team_list[[2]]

  game_probs <- calc_game_probs(home_team, away_team, weights)


  home_team_result <- sample(names(game_probs), size = 1, prob = game_probs)

  if (home_team_result == "Win") {
    home_team$wins <- home_team$wins + 1
    away_team$losses <- away_team$losses + 1

    home_team$points <- home_team$points + 3
  }
  if (home_team_result == "Tie") {
    home_team$draws <- home_team$draws + 1
    away_team$draws <- away_team$draws + 1

    home_team$points <- home_team$points + 1
    away_team$points <- away_team$points + 1
  }
  if (home_team_result == "Loss") {
    home_team$losses <- home_team$losses + 1
    away_team$wins <- away_team$wins + 1

    away_team$points <- away_team$points + 3
  }

  home_team <- update_probs(home_team)
  away_team <- update_probs(away_team)

  home_team <- update_tv(home_team)
  away_team <- update_tv(away_team)

  r = list(home_team, away_team)
  names(r) <- c(home_team$team_name, away_team$team_name)

  r
}

#' Simulate Round
#'
#' Simulates the results of a round of matches for a league.
#'
#' @param rnd Round number to simulate
#' @param teams List of all teams in the division
#' @param schedule Full schedule for division. Data frame with headers \code{round}, \code{home_team},\code{away_team}
#' @param weights Relative weights for probabilities provided to the \code{\link{calc_game_probs}} function
#'
#' @return Returns the team list with all games for the round simulated
#' @export
process_round <- function(rnd, teams, schedule, weights) {
  #Simulate games for the round
  games <- dplyr::filter(schedule, round == rnd)
  results <- purrr::map2(games$home_team, games$away_team, ~ process_game(list(teams[[.x]],teams[[.y]]), weights))

  #Update teams with simulated results
  for (game_num in 1:nrow(games)) {
    home_team_name <- games[[game_num,"home_team"]]
    away_team_name <- games[[game_num,"away_team"]]

    teams[[home_team_name]] <- results[[game_num]][[1]]
    teams[[away_team_name]] <- results[[game_num]][[2]]
  }

  teams
}

#' Simulate season
#'
#' Simulates an entire season from a list of teams and their schedule
#'
#' @param teams Data frame of with team_name, race, TV_displayed, TV_extra, wins, losses, draws.
#' @param schedule Data frame containing the league schedule. Must contain \code{round}, \code{home_team}, and\code{away_team}
#' @param weights List containing a vector per round of three weights for combining probabilities from CCL data and a team's own record. Provided to the \code{\link{calc_game_probs}} function. If a list with a single vector is provided, it expands it up to the length of the season, \code{i.e.} it is a shortcut for providing constant weights for all rounds.
#' @param simulation_run Number of this simulation run. Used to print logging info to console so monitor simulation progress when performed in bulk
#' @param num_sims Total number of simulations to run. Defaults to 10 for a quick run to check that everything is working. Recommend changing it to 1000 or more for real predictions
#'
#' @return Data frame containing the predicted results of the season.
#' @describeIn Simulate Simulates a single run through of a season
process_season <- function(teams, schedule, weights = list(c(60, 20, 20)), simulation_run = 0) {
  cat(paste0("\fprocessing run ",simulation_run,"...\n"))
  num_rounds = dplyr::n_distinct(schedule$round)

  #Convert team spreadsheet into team object list
  teams <- teams %>%
    purrrlyr::by_row(make_team, .to = "team") %>%
    magrittr::extract2("team") %>%
    set_names(teams$team_name)

  #expand weights up to number or rounds if not specified
  #Allows per-round weights to be provided for fresher teams if desired
  weights = rep(weights, length.out = num_rounds)

  full_results <- list()
  for (round in 1:num_rounds) {
    teams <- process_round(round, teams, schedule, weights[[round]])
    full_results[[round]] <- teams
  }
  names(full_results) <- 1:num_rounds

  #extract just the cumulative points for each team per round
  results <- purrr::map(names(teams), ~ purrr::map(full_results, c(.,"points")))
  names(results) <- names(teams)
  results <- results %>%
    purrr::map_df(as_data_frame, .id = "team") %>%
    tidyr::gather(round, score, -team)

  results
}

#' @describeIn Simulate Runs multiple simulations and returns the cumulative points totals for the teams across the season
simulate_league <- function(teams, schedule, weights = list(c(60, 20, 20)), num_sims = 10) {
  d <- map_df(1:num_sims, ~process_season(teams, schedule, weights, simulation_run = .x), .id = "sample")
  structure(d, class = c("sim_results", "data.frame"))
}


plot.sim_results <- function(sim_data, title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, ...) {

}
