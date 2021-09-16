#' @title Random
#'
#' @description Strategy rules:
#'    1. Cooperates or defects at random
#'
random <- function(opponent, memory) {
  sample(c("C", "D"), 1)
}
