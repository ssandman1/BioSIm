## functions to simulate predator attack

# library(dplyr)

## helper functions -----------

# calculate probability of death for each individual


# this version assumes:
#   * any individual who is warned will hide (even if he/she has warner >= 1)
#   * an active warner warns individuals who are not already hiding
#   * a warner will warn herself (i.e., hide) if there is no one else left to warn
#   * to be warned, an individual must bear relationship 0.25 or more to the warner
getDeathProb <- function(
                         individuals,
                         number_warned,
                         warner_death_prob,
                         nonwarner_death_prob,
                         relMatrix,
                         dominant) {
  id <- individuals$id
  warner <- individuals$warner
  deathProb <- rep(nonwarner_death_prob, times = length(id))
  warnerCutoff <- ifelse(dominant, 1, 2)
  warners <- id[warner >= warnerCutoff]
  hideable <- id
  # randomly shuffle the warners:
  warners <- sample(warners, replace = FALSE)
  while (length(warners) > 0) {
    warnerId <- warners[1]
    warners <- warners[-1]
    hideable <- hideable[hideable != warnerId]
    warnable <- hideable[relMatrix[warnerId, hideable] >= 0.25]
    warned <-
      head(warnable[order(relMatrix[warnerId, warnable], decreasing = TRUE)], 
           number_warned)
    if (length(warned) > 0) {
      deathProb[id %in% warned] <- 0
      deathProb[id == warnerId] <- warner_death_prob
    } else {
      deathProb[id == warnerId] <- 0
    }
    warners <- setdiff(warners, warned)
    hideable <- setdiff(hideable, warned)
  }
  deathProb
}

## in this version, warners only warn other warners
# getDeathProb <- function(
#                          individuals,
#                          number_warned,
#                          warner_death_prob,
#                          nonwarner_death_prob,
#                          relMatrix,
#                          dominant) {
#   id <- individuals$id
#   warner <- individuals$warner
#   deathProb <- rep(nonwarner_death_prob, times = length(id))
#   warnerCutoff <- ifelse(dominant, 1, 2)
#   warners <- id[warner >= warnerCutoff]
#   hideable <- id
#   # randomly shuffle the warners:
#   warners <- sample(warners, replace = FALSE)
#   while (length(warners) > 0) {
#     warnerId <- warners[1]
#     warners <- warners[-1]
#     warned <- head(warners, n = 10)
#     if (length(warned) > 0) {
#       deathProb[id %in% warned] <- 0
#       deathProb[id == warnerId] <- warner_death_prob
#     } else {
#       deathProb[id == warnerId] <- 0
#     }
#     warners <- setdiff(warners, warned)
#     hideable <- setdiff(hideable, warned)
#   }
#   deathProb
# }

## in this version, only warner = 2 warns, but
## all with warner = 1 or 2 can be warned
# getDeathProb <- function(
#                          individuals,
#                          number_warned,
#                          warner_death_prob,
#                          nonwarner_death_prob,
#                          relMatrix,
#                          dominant) {
#   id <- individuals$id
#   warner <- individuals$warner
#   deathProb <- rep(nonwarner_death_prob, times = length(id))
#   warnerCutoff <- 2
#   warners <- id[warner >= warnerCutoff]
#   hideable <- id[warner >= 1]
#   # randomly shuffle the warners:
#   warners <- sample(warners, replace = FALSE)
#   while (length(warners) > 0) {
#     warnerId <- warners[1]
#     warners <- warners[-1]
#     hideable <- hideable[hideable != warnerId]
#     warned <- head(hideable, n = 10)
#     if (length(warned) > 0) {
#       deathProb[id %in% warned] <- 0
#       deathProb[id == warnerId] <- warner_death_prob
#     } else {
#       deathProb[id == warnerId] <- 0
#     }
#     warners <- setdiff(warners, warned)
#     hideable <- setdiff(hideable, warned)
#   }
#   deathProb
# }


## determine who dies
determineAttackDeath <- function(deathProb) {
  randomUnif <- runif(length(deathProb))
  randomUnif < deathProb
}


## determine who dies
determineAttackDeath <- function(deathProb) {
  randomUnif <- runif(length(deathProb))
  randomUnif < deathProb
}

## attack ---------------

attack <- function(
                   individuals,
                   number_warned,
                   warner_death_prob,
                   nonwarner_death_prob,
                   relMatrix,
                   dominant) {
  deathProb <- getDeathProb(
    individuals,
    number_warned,
    warner_death_prob,
    nonwarner_death_prob,
    relMatrix,
    dominant
  )
  dies <- determineAttackDeath(deathProb)
  dead <- subset(individuals, dies)
  survivors <- subset(individuals, !dies)
  popAdjustment <- -popAdjust(dead$sex, dead$warner)
  relMatrix <- cutForDeaths(relMatrix, individuals$id, dies)
  lst(
    individuals = survivors,
    relMatrix = relMatrix,
    popAdjustment = popAdjustment
  )
}
