## function to kill off some individuals

cull <- function(dr, individuals, relMatrix = NULL) {
  n <- nrow(individuals)
  dies <- runif(n) < dr
  dead <- subset(individuals, dies)
  survivors <- subset(individuals, !dies)
  popAdjustment <- -popAdjust(dead$sex, dead$warner)
  relMatrix <- cutForDeaths(relMatrix, individuals$id, dies)
  list(individuals = survivors,
      relMatrix = relMatrix,
      popAdjustment = popAdjustment)
}

## cut down relationship matrix to account for culling;
## use also after an attack
## ids = individual$id
## dies = logical vector that is TRUE if individual should be removed
cutForDeaths <- function(relMatrix, ids, dies) {
  survivorIds <- ids[!dies]
  relMatrix[survivorIds, survivorIds]
}