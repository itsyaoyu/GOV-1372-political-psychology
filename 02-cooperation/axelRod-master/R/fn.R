.getStrategy <- function(path) {
  tmp <- roxygen2:::parse_file(path, .GlobalEnv)
  desc <- gsub("\n  ", "", tmp[[1]]$tags[[2]]$raw)
  desc <- gsub("   ", " ", desc)
  desc <- gsub("(\\d\\.)", "\n\\1", desc)
  list(name = tmp[[1]]$call[[2]],
       description = desc,
       fn = eval(parse(path)))
}

#' @title Strategies Provided with the \code{axelRod} Package
#'
#' @description This function returns a list of strategies that can be used to
#'  run an Axelrod-like game theory tournament.
#'
#' @return A list of lists. Each sublist will have three elements:
#'  \describe{
#'    \item{name}{A character string giving the name of the strategy.}
#'    \item{description}{A character string describing the strategy.}
#'    \item{fn}{The function implementing the strategy.}
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # TODO
#'
#' @export
#'
defaultStrategies <- function() {
  strat_path <- "./axelRod-master/inst/Strategies"
  strat_files <- list.files(strat_path, full.names = TRUE)
  lapply(strat_files, .getStrategy)
}
