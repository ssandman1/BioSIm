geneChange<- function(x,y){
  pop <- simulate(sim_gens = 400, relationship_method = "graph",
                  initial_pop = 
                    list(m0 = y, m1 = 0, m2 = x, f0 = y, f1 = 0, f2 =x), 
                  average_litter_size = 5, birth_rate_natural = 0.05,
                  death_rate_natural = 0, prob_attack = 0.2, 
                  warner_death_prob = 0.4,nonwarner_death_prob = 0.2,
                  hider_death_prob = 0, graph= FALSE)
  totalpop<-pop[401,1]
  males1<-pop[401,4]
  males2<-pop[401,5]
  females1<-pop[401,8]
  females2<-pop[401,9]
  proportion<- (males1+(males2*2)+females1+(females2*2))/(totalpop*2)
  df<-data.frame(totalpop=pop[401,1],warnerProportion=proportion*100)
  df
}
x<-c(0,10,20,30,40,50,60,70,80,90,100)
y<-c(100,90,80,70,60,50,40,30,20,10,0)
lst(y,x)%>%
  pmap(geneChange)

## Problem:
## The output comes as a list and not one data frame

## Future Plan:
## after I fix the output problem, I am going to use the data (in my slides)
## to determine how to the percentage of warners affects the total population and 
## what starting percentage of warners leads to the highest warner proportion
## at the end of the simulation 
