% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{api_contests}
\alias{api_contests}
\title{Contests}
\usage{
api_contests(key = NA, league = NA, competition = NA, status = NA,
  limit = NA, round = NA, platform = NA, exact = 1, ...)
}
\arguments{
\item{key}{API access key (required)}

\item{league}{League name (defaults to Cabalvision Official League)}

\item{competition}{Competition name (defaults to all competitions in league)}

\item{status}{scheduled, in_progress, or played (defaults to scheduled)}

\item{limit}{Number of statistics to return (defaults to 100)}

\item{round}{Round of competition}

\item{platform}{pc, ps4, or xb1 (optional)}

\item{exact}{0 or 1 - Exact matching for league name}
}
\value{
List containing four named elements
\item{\code{upcoming_matches}: list of match information - match_id, competition, round, opponents, winner (if played)}
\item{\code{urls}: a list of urls for various game assets}
\item{\code{context}: Information about the league/competition requested}
\item{\code{meta}: a list of information about the query}
}
\description{
Information on contests for a league/competition (includes scheduled as well as played games)
}
