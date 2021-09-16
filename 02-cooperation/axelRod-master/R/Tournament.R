.onetime <- function(players, payoff, nreps) {
  plays <- t(combn(1:length(players), 2))

  res <- data.frame(rep = rep(1:nreps, each = 2 * nrow(plays)),
                    round = 1, player = NA, opponent = NA, play = NA,
                    outcome = NA, score = NA)

  for (i in 1:nreps) {
    for (j in 1:nrow(plays)) {
      players[[plays[j, 1]]]$reset()
      players[[plays[j, 2]]]$reset()

      p1 <- players[[plays[j, 1]]]$play(opponent = players[[plays[j, 2]]]$name)
      p2 <- players[[plays[j, 2]]]$play(opponent = players[[plays[j, 1]]]$name)

      if (p1 == p2) {
        if (p1 == "C") {
          sc1 <- sc2 <- payoff[1, 1]
        } else {
          sc1 <- sc2 <- payoff[2, 2]
        }
      } else {
        if (p1 == "C") {
          sc1 <- payoff[1, 2]
          sc2 <- payoff[2, 1]
        } else {
          sc1 <- payoff[2, 1]
          sc2 <- payoff[1, 2]
        }
      }

      idx <- which(is.na(res$player))[1]
      res[idx, ]$player <- players[[plays[j, 1]]]$name
      res[idx, ]$opponent <- players[[plays[j, 2]]]$name
      res[idx, ]$play <- p1
      res[idx, ]$outcome <- ifelse(p1 == p2, "draw", ifelse(sc1 > sc2, "win", "loss"))
      res[idx, ]$score <- sc1

      idx <- which(is.na(res$player))[1]
      res[idx, ]$player <- players[[plays[j, 2]]]$name
      res[idx, ]$opponent <- players[[plays[j, 1]]]$name
      res[idx, ]$play <- p2
      res[idx, ]$outcome <- ifelse(p1 == p2, "draw", ifelse(sc2 > sc1, "win", "loss"))
      res[idx, ]$score <- sc2
    }
  }

  res
}

.repeated <- function(players, payoff, nreps, nrounds) {
  plays <- t(combn(1:length(players), 2))

  res <- data.frame(rep = rep(1:nreps, each = 2 * nrow(plays)),
                    round = 1, player = NA, opponent = NA, play = NA,
                    outcome = NA, score = NA)

  pb <- progress::progress_bar$new(total = nrow(res) * nrounds / 2)

  for (i in 1:nreps) {
    for (j in 1:nrow(plays)) {
      players[[plays[j, 1]]]$reset()
      players[[plays[j, 2]]]$reset()

      for (k in 1:nrounds) {
        p1 <- players[[plays[j, 1]]]$play(opponent = players[[plays[j, 2]]]$name)
        p2 <- players[[plays[j, 2]]]$play(opponent = players[[plays[j, 1]]]$name)

        if (p1 == p2) {
          if (p1 == "C") {
            sc1 <- sc2 <- payoff[1, 1]
          } else {
            sc1 <- sc2 <- payoff[2, 2]
          }
        } else {
          if (p1 == "C") {
            sc1 <- payoff[1, 2]
            sc2 <- payoff[2, 1]
          } else {
            sc1 <- payoff[2, 1]
            sc2 <- payoff[1, 2]
          }
        }

        players[[plays[j, 1]]]$update(p1, players[[plays[j, 2]]]$name, p2, sc1, sc2)
        players[[plays[j, 2]]]$update(p2, players[[plays[j, 1]]]$name, p1, sc2, sc1)
        pb$tick()
      }

      idx <- which(is.na(res$player))[1]
      res[idx, ]$player <- players[[plays[j, 1]]]$name
      res[idx, ]$opponent <- players[[plays[j, 2]]]$name
      res[idx, ]$score <- sum(players[[plays[j, 1]]]$memory$score)

      idx <- which(is.na(res$player))[1]
      res[idx, ]$player <- players[[plays[j, 2]]]$name
      res[idx, ]$opponent <- players[[plays[j, 1]]]$name
      res[idx, ]$score <- sum(players[[plays[j, 2]]]$memory$score)
    }
  }

  res
}

.random <- function(players, payoff, nreps, nrounds) {

}

#' @title Tournament Class
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#'
#' @field type The tournament type ("onetime" or "repeated").
#'
#' @field players A list of the different \code{\link{Player}} object competing
#'  in the tournament.
#'
#' @field payoff A 2x2 payoff matrix of the tournament.
#'
#' @field nreps The number of replications of the tournament.
#'
#' @field nrounds The number of rounds in each replication of the tournament.
#'
#' @field res A data frame of the results of the tournament.
#'
#' @section Methods:
#'  \describe{
#'    \item{\code{new(type, players, nreps, nrounds, payoff)}}{Create an object
#'    of clase \code{Tournament}.
#'      \describe{
#'        \item{\code{type}}{A character string indicating the type of tournament
#'          to play: "onetime" for a single encounter tournament; "repeated" for
#'          a repeated encounter tournament (the default).}
#'        \item{\code{players}}{A named list of strategies to compete in the
#'          tournament.}
#'        \item{\code{nreps}}{A numerical value indicating the number of
#'          tournament replications to perform (default: 100).}
#'        \item{\code{nrounds}}{For repeated encounter tournaments only, a
#'          numerical value indicating the number of rounds of the tournament
#'          (default: 100).}
#'        \item{\code{payoff}}{A 2x2 matrix indicating the payoff received by
#'          the players as a function of their decision and of the decision of
#'          their opponent (default: \code{matrix(c(3, 5, 0, 1), nrow = 2)}).
#'          Here the first value indicates the payoff if both players cooperate;
#'          the second value is the payoff if the player defects and its opponent
#'          cooperates; the third value is the payoff if the player cooperates
#'          and its opponent defects; the fourth value is the payoff if both
#'          players defect.}
#'      }
#'    }
#'    \item{\code{play()}}{This method is used to run the tournament.}
#'  }
#'
#' @return \code{\link{R6Class}} object with methods for running an Axelrod-like
#'  tournament.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # TODO
#'
#' @export
Tournament <- R6::R6Class(
  classname = "Tournament",

  private = list(),

  public = list(
    type = NA,
    players = NA,
    payoff = NA,
    nreps = NA,
    nrounds = NA,
    res = NA,

    initialize = function(type, players, nreps = 100, nrounds = 100,
                          payoff = matrix(c(3, 5, 0, 1), nrow = 2)) {
      if (missing(type))
        self$type <- "onetime"
      else
        self$type <- type

      if (!(self$type %in% c("onetime", "repeated", "random")))
        stop(paste0("type must be one of the following: ", paste(c("onetime", "repeated", "random"), collapse = ", "), "."))

      if (missing(players))
        stop("Players must be provided.")

      if (is.null(names(players)) | !all(names(players) != ""))
        stop("All players must be named.")

      self$nreps <- nreps
      self$nrounds <- ifelse(self$type == "onetime", 1, nrounds)
      self$payoff <- payoff

      self$players <- c()
      for (i in 1:length(players)) {
        self$players <- c(self$players, Player$new(names(players)[i], players[[i]], nrounds))
      }
    },

    play = function() {
      if (self$type == "onetime")
        self$res <- .onetime(players = self$players, payoff = self$payoff, nreps = self$nreps)
      else if (self$type == "repeated")
        self$res <- .repeated(players = self$players, payoff = self$payoff, nreps = self$nreps, nrounds = self$nrounds)
      else if (self$type == "random")
        self$res <- .random(players = self$players, payoff = self$payoff, nreps = self$nreps, nrounds = self$nrounds)
    }
  )
)


















