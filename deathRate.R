## function to compute death rate

deathRate <- function(popSize, capacity, 
                      death_rate_natural, birth_rate_natural) {
  (birth_rate_natural - death_rate_natural)/capacity * popSize + death_rate_natural
}