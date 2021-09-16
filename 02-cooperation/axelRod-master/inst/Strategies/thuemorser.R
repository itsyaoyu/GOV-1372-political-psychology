#' @title Thue-Morse Sequencer
#'
#' @description Strategy rules:
#'    1. Cooperates or defects according to the Thue-Morse sequence (binary
#'      sequence obtained by starting with 0 and successively appending the
#'      Boolean complement of the sequence obtained thus far: 0, 01, 0110,
#'      01101001, 0110100110010110, etc.).
#'
thuemorser <- function(opponent, memory) {
  thuemorse <- function(n) {
    tm <- c(0)
    while (length(tm) < n) {
      tm <- c(tm, tm == 0)
    }
    tm[1:n]
  }

  idx <- which(memory$opponent == opponent)
  tm <- tail(thuemorse(length(idx)), 1)

  if (tm) {
    "C"
  } else {
    "D"
  }
}
