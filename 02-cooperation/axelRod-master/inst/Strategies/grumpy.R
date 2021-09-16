#' @title Grumpy
#'
#' @description Strategy rules:
#'    1. Cooperates at first.
#'    2. Each time the opponent defects, increases its level of grumpiness.
#'    3. Each time the opponent cooperates, decreases its level of grumpiness.
#'    4. If grumpiness level is above a threshold A, defects systematically.
#'    5. If grumpiness level returns below a threshold B, cooperates again.
#'    6. A > B
#'
grumpy <- function(opponent, memory, A = 0.5, B = 0.3, increment = 0.1, max_grump = 1) {
  idx <- which(memory$opponent == opponent)

  grump <- 0

  if (length(idx) > 1) {
    for (i in 1:length(memory$opponent_play[idx])) {
      grump <- grump + ifelse(memory$opponent_play[idx][i] == "D", increment, -increment)

      if (grump > max_grump)
        grump <- max_grump

      if (grump < 0)
        grump <- 0
    }
  }

  if (grump <= B) {
    "C"
  } else if (grump >= A) {
    "D"
  } else {
    last_play <- memory$play[tail(idx, 1)]

    if (length(last_play) == 0)
      last_play <- "C"

    if (last_play == "D") {
      "D"
    } else {
      "C"
    }
  }
}
