### MAIN SIMULATION FUNCTION
## holds all parameters identified in project outline
library(dplyr)
library(ggplot2)
source("initialization.R")
source("popAdjust.R")
source("reproduce.R")
source("DetermineSexFunction.R")
source("getChildGenesFunction.R")
source("deathRate.R")
source("cull.R")
source("attack.R")

mainSim<- function(dominant = FALSE,
                   average_litter_size = 5,
                   initial_males = 100,
                   initial_alt_males = 10,
                   initial_females = 100,
                   initial_alt_females = 10,
                   birth_rate_natural = .05,
                   death_rate_natural = .0,
                   prob_attack = .2,
                   number_warned = 10,
                   warner_death_prob = .4,
                   nonwarner_death_prob = .2,
                   hider_death_prob = 0,
                   sim_gens = 2,
                   capacity = 2000) {
  
  individuals <- individualInit(initial_males, 
                                initial_alt_males,
                                initial_females,
                                initial_alt_females)
  maxId <- max(as.numeric(individuals$id))
  population <- popInit(individuals, sim_gens)
  relMatrix <- relMatrixInit(individuals)

  # go through the generations
  for (i in 1:sim_gens) {
    
    ## reproduce:
    if (population$males[i] > 0 & population$females[i] > 0) {
      # compute number of couples
      targetChildren <- birth_rate_natural * nrow(individuals)
      number_of_couples <- ceiling(targetChildren / average_litter_size)
      # make the new generation:
      lst <- reproduce(average_litter_size, number_of_couples,
                       individuals, relMatrix = relMatrix, maxId)
      individuals <- lst$individuals
      relMatrix <- lst$relMatrix
      popAdjustment <- lst$popAdjustment
      maxId <- lst$maxId
      population[i + 1, ] <- colSums(rbind(population[i, ], popAdjustment))
    } else {
      population[i + 1, ] <- population[i, ]
    }
    
    ## cull
    #compute death rate
    dr <- deathRate(popSize = population[i + 1, 1],
                    capacity, 
                    death_rate_natural,
                    birth_rate_natural)
    lst <- cull(dr, individuals, relMatrix = relMatrix)
    individuals <- lst$individuals
    relMatrix <- lst$relMatrix
    popAdjustment <- lst$popAdjustment
    population[i + 1, ] <- colSums(rbind(population[i + 1, ], popAdjustment))
    
    
    ## will there be an attack?
    attackOccurs <- runif(1) < prob_attack
    # handle attack, if needed:
    if (attackOccurs) {
      lst <- attack(
        individuals,
        number_warned,
        warner_death_prob,
        nonwarner_death_prob,
        relMatrix,
        dominant)
      individuals <- lst$individuals
      relMatrix <- lst$relMatrix
      popAdjustment <- lst$popAdjustment
      population[i + 1, ] <- colSums(rbind(population[i + 1, ], popAdjustment))
    }
  }
  altProp <- with(
    population,
    (2*(males2 + females2) + males1 + females1)/(males + females))
  generation <- 0:sim_gens
  df <- data.frame(generation, altProp)
  p <-
    ggplot(df, aes(x = generation, y = altProp)) +
    geom_line()
  print(p)
  return(population)
}

## How fast is this?
# > system.time(pop <- mainSim(sim_gens = 800))
# user  system elapsed 
# 50.907   5.992  56.933 

## Once everyting is working as we like,
## we should do some code profiling and
## consider options for optimization.

## Also we should alter the setup to permit the user to specify
## how warners behave when the population is under attack.
## For example:
##   * warner = 1 has 50% chance to decide to warn
##   * warners warn only other warners (with preference for warner = 2)
##   * warners prioritize other warners but warn others if some limit is not
##     yet reached
##   * likelihoof of warning non-warners increases with percentage of warners
##     in the population
##   * etc.


