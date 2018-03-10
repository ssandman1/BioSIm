### MAIN SIMULATION FUNCTION
## holds all parameters identified in project outline

source("initialization.R")
source("popAdjust.R")
source("reproduce.R")
source("DetermineSexFunction.R")
source("getChildGenesFunction.R")
source("deathRate.R")
source("cull.R")



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
  population <- popInit(individuals, sim_gens)
  
  # TODO:  relMatrixInit()
  
  # go through the generations
  for (i in 1:sim_gens) {
    
    ## reproduce:
    # compute number of couples
    targetChildren <- birth_rate_natural * nrow(individuals)
    number_of_couples <- ceiling(targetChildren / average_litter_size)
    lst <- reproduce(average_litter_size, number_of_couples,
                     individuals, relMatrix = NULL)
    individuals <- lst$individuals
    relMatrix <- lst$relMatrix
    popAdjustment <- lst$popAdjustment
    population[i + 1, ] <- colSums(rbind(population[i, ], popAdjustment))
    
    ## cull
    #compute death rate
    dr <- deathRate(popSize = population[i + 1, 1],
                    capacity, 
                    death_rate_natural,
                    birth_rate_natural)
    lst <- cull(dr, individuals, relMatrix = NULL)
    individuals <- lst$individuals
    relMatrix <- lst$relMatrix
    popAdjustment <- lst$popAdjustment
    population[i + 1, ] <- colSums(rbind(population[i + 1, ], popAdjustment))
    
    # TODO:  attack()
  }
  return(population)
}

## how fast ist this?
# > system.time(pop <- mainSim(sim_gens = 500))
# user  system elapsed 
# 23.432   0.770  24.220 

## Not bad, but we must still work on attack and relMatrix



