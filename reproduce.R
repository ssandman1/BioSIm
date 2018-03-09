#### REPRODUCE TEST FUNCTION
# library(tidyverse)

makelitter<-function(momId,dadId, 
                     actualsize,
                     individuals,
                     relMatrix = NULL) {
  n <- actualsize
  mom <- subset(individuals, id == momId)
  dad<-subset(individuals,id == dadId)
  
  lastRow <- nrow(individuals)
  lastId <- as.numeric(individuals[lastRow, "id"])
  ids <- (lastId + 1) : (lastId + n)
  
  sex <- determineSex(n)
  
  kidMom <- rep(momId, times = n)
  kidDad <- rep(dadId, times = n)
  
  dadGenes <- dad$warner
  momGenes <- mom$warner
  warner<- numeric(n)
  for(i in 1:n) {
    warner[i]<- getChildGenes(mg = momGenes, dg = dadGenes)
  }
 
  litter <- data.frame(id = as.character(ids), 
                       sex = sex, 
                       warner = warner,
                       mom = kidMom,
                       dad = kidDad,
                       stringsAsFactors = FALSE)
  individuals <- rbind(individuals, litter)
  
  # TODO:  update relMatrix

  results <- list(individuals = individuals,
                  relMatrix = relMatrix)
  results
}

reproduce <- function (average_litter_size, number_of_couples,
                       individuals, relMatrix = NULL) {
    
  allfemales<-subset(individuals,sex=="F")
  n <- nrow(allfemales)
  femaleMates <- allfemales[sample(1:n, size = number_of_couples, replace = FALSE), ] 
  momIDs<-femaleMates$id
  allmales<-subset(individuals,sex=="M") ### selecting all males
  maleMates <- allmales[sample(1:nrow(allmales),
                               size = number_of_couples, 
                               replace = TRUE), ]
  dadIDs<-maleMates$id
  
  for(i in 1:number_of_couples){
    actualsize <- 1 + rpois(1, average_litter_size - 1) 
    lst <- makelitter(momId = momIDs[i],
                      dadId = dadIDs[i],
                      actualsize = actualsize,
                      individuals = individuals,
                      relMatrix = relMatrix)
    individuals <- lst$individuals
    relMatrix <- lst$relMatrix
  }
  
  list(individuals = individuals,
       relMatrix = relMatrix)
}

