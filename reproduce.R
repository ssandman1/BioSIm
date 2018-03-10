#### REPRODUCE FUNCTION

makelitter<-function(momId,dadId, 
                     actualsize,
                     individuals,
                     relMatrix = NULL,
                     maxId) {
  n <- actualsize
  mom <- subset(individuals, id == momId)
  dad<-subset(individuals,id == dadId)
  
  ids <- (maxId + 1) : (maxId + n)
  
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
  popAdjustment <- popAdjust(sex = sex, warner = warner)
  
  # TODO:  update relMatrix

  results <- list(individuals = individuals,
                  relMatrix = relMatrix,
                  popAdjustment = popAdjustment)
  results
}

reproduce <- function (average_litter_size, number_of_couples,
                       individuals, relMatrix = NULL, maxId) {
    
  popAdjustment <- data.frame(populationSize = 0,
                          males = 0, males0 = 0, males1 = 0, males2 = 0,
                          females = 0, females0 = 0, females1 = 0, females2 = 0)
  
  allfemales <- subset(individuals, sex == "F")
  n <- nrow(allfemales)
  femaleMates <- allfemales[sample(1:n, size = number_of_couples, replace = FALSE), ] 
  momIDs <- femaleMates$id
  allmales <- subset(individuals,sex == "M")
  someMalesLeft <- nrow(allmales) > 0
  
  if (someMalesLeft) {
    maleMates <- allmales[sample(1:nrow(allmales),
                                 size = number_of_couples, 
                                 replace = TRUE), ]
    dadIDs <- maleMates$id
    
    for(i in 1:number_of_couples){
      actualsize <- 1 + rpois(1, average_litter_size - 1)
      lst <- makelitter(momId = momIDs[i],
                        dadId = dadIDs[i],
                        actualsize = actualsize,
                        individuals = individuals,
                        relMatrix = relMatrix,
                        maxId = maxId)
      individuals <- lst$individuals
      relMatrix <- lst$relMatrix
      popAdjustment <- colSums(rbind(popAdjustment,lst$popAdjustment))
      maxId <- maxId + actualsize
    }
    list(individuals = individuals,
         relMatrix = relMatrix,
         maxId = maxId,
         popAdjustment = popAdjustment)
  } else {
    list(individuals = individuals,
         relMatrix = relMatrix,
         maxId = maxId,
         popAdjustment = popAdjustment)
  }
}

