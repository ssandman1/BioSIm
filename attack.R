## functions to simulate predator attack

# library(dplyr)

## helper functions -----------

# calculate probability of death for each individual
# currently assumes:
#   * any individual who is warned will hide (even if he/she has warner >= 1)
#   * an active warner warns individuals who are not already hiding
#   * a warner will warn herself (i.e., hide) if there is no one else left to warn
#   * to be warned, an individual must bear relationship 0.25 or more to the warner
getDeathProb <- function(
                         id,
                         warner,
                         number_warned,
                         warner_death_prob,
                         nonwarner_death_prob,
                         relMatrix,
                         dominant) {
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
    warned <- data.frame(
      id = hideable,
      degree = relMatrix[warnerId, hideable]
    ) %>%
      filter(degree >= 0.25) %>% 
      arrange(desc(degree)) %>%
      head(n = number_warned)
    if (nrow(warned) > 0) {
      deathProb[id %in% warned$id] <- 0
      deathProb[id == warnerId] <- warner_death_prob
    } else {
      deathProb[id == warnerId] <- 0
    }
    warners <- setdiff(warners, warned)
  }
  deathProb
}

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
    individuals$id,
    individuals$warner,
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
