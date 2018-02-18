# BioSim project : creating a simulation to test the benefits of alturistic behaviour in a population

#### Put into function : 

* if gene is recessive or dominant - `dominant = FALSE`
* initial numbers:
    * initial males - `initial_males = 100`
    * A males - `initial_alt_males = 10`
    * inital females - `inital_females = 100`
    * A females - `initial_atl_female = 10`
* birth rate - `birth_rate_natural = .05`
* death rate - `death_rate_natural = .02`
* attack frequency - `prob_attack = .2`
* number of individuals that are warned - `number_warned = 10`
* prob of getting killed when warner - `warner_death_prob = .7`
* prob of getting killed when non warner and not hidding - `nonwarner_death_prob = .2`
* prob of killed if hidding - `hider_death_prob = 0` 
* number of generations to simulate - `sim_gens =2` 
* carrying capacity - `capacity = 2000`

 
#### General Stucture of function:

* set up:
    * data structure to hold info about pop in each generation
    * Matrix relationship
        * determines the relationships between rabbitts (mother, father, offspring, sibling, etc)
    * Population : data frame about population
        * Each row gives info on population as a whole at the begining of the generation
        * collums : 
            * males, 
            * females, 
            * atl_males, 
            * alt_ female
    * Individual : data frame about individual rabbits
        * each row is a single rabbit
        * colums : 
            * id, 
            * sex ("F","M"), 
            * allele1 (0,1), 
            * allele2 (0,1)
            * mother
            * father
            * ? warner

* loop :
    * have non attack deaths
    * make rabbits reproduce
        * relationship matrix is used here 
    * determine if there is an attack and process it
 
#### Return: 

* size of pop( number of people in pop) at each generation, 
* number of alturistic individuals at each generation, 
* info to make graph (user has option to make graph or not)


Matrix  relMat[a,b]
relationship level between rabbut a and b
rows and collums are rabbit IDs
1 cell in matrix stands for 
rel(new,old) = .5*relationship(father of new,old)(.5mothernew,old)
equation doesnt work for oneself
have vector w all ids of previous rabbits 
when rabitts killed just wipe out rows and collum that they are
live<- relMat[live,live]

Mating Function :
