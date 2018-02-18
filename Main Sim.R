####read about rpois
### MAIN SIMULATION FUNCTION
## holds all parameters identified in project outline
mainSim<- function(dominant = FALSE,
                   average_litter_size,
                   initial_males = 100,
                   initial_alt_males = 10,
                   inital_females = 100,
                   initial_atl_female = 10,
                   birth_rate_natural = .05) {
  pop.init()
  individual.init()

 
  for(i in 1:sim_gens){
    reproduce()
    attack()
    cull ()
    
    
  }
  return (population data frame)
  
  
  
}



