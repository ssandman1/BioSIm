### MAIN SIMULATION FUNCTION
## holds all parameters identified in project outline

library(tidyverse)
source("initialization.R")
source("reproduce.R")
source("DetermineSexFunction.R")
source("getChildGenesFunction.R")


mainSim<- function(dominant = FALSE,
                   average_litter_size = 5,
                   initial_males = 100,
                   initial_alt_males = 10,
                   initial_females = 100,
                   initial_alt_females = 10,
                   birth_rate_natural = .05,
                   death_rate_natural = .02,
                   prob_attack = .2,
                   number_warned = 10,
                   warner_death_prob = .7,
                   nonwarner_death_prob = .2,
                   hider_death_prob = 0,
                   sim_gens = 2,
                   capacity = 2000) {
  
  individuals <- individualInit(initial_males, 
                                initial_alt_males,
                                initial_females,
                                initial_alt_females)
  # popInit()
  # relMatrixInit()
  
  # go through the generations
  for (i in 1:sim_gens) {
    
    # compute number of couples
    targetChildren <- birth_rate_natural * nrow(individuals)
    number_of_couples <- ceiling(targetChildren / average_litter_size)
    lst <- reproduce(average_litter_size, number_of_couples,
                     individuals, relMatrix = NULL)
    individuals <- lst$individuals
    relMatrix <- lst$relMatrix
    #attack()
    #cull ()
  }
  # for now just give back individuals
  return(individuals)
}

## the following command:
## results <- mainSim(sim_gens = 100)
## is taking about 32 seconds to run on my machine
## hopefully it will get faster when we cull the population



