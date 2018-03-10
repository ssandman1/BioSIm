## function to kill off some individuals

cull <- function(dr, individuals, relMatrix = NULL) {
  n <- nrow(individuals)
  dies <- runif(n) < dr
  dead <- subset(individuals, dies)
  survivors <- subset(individuals, !dies)
  popAdjustment <- -popAdjust(dead$sex, dead$warner)
  ## TODO:  update relMatrix
  lst(individuals = survivors,
      relMatrix = relMatrix,
      popAdjustment = popAdjustment)
}