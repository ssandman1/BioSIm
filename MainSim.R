####read about rpois
### MAIN SIMULATION FUNCTION
## holds all parameters identified in project outline
mainSim<- function(dominant = FALSE,
                   average_litter_size,
                   initial_males = 100,
                   initial_alt_males = 10,
                   inital_females = 100,
                   initial_atl_female = 10,
                   birth_rate_natural = .05,
                   death_rate_natural = .02,
                   prob_attack = .2,
                   number_warned = 10,
                   warner_death_prob = .7,
                   nonwarner_death_prob = .2,
                   hider_death_prob = 0,
                   sim_gens =2,
                   capacity = 2000) {
  pop.init()
  individual.init()

 
  for(i in 1:sim_gens){
    reproduce()
    attack()
    cull ()
    
    
  }
  return (population data frame)
  
  }



