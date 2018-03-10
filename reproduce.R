#### REPRODUCE FUNCTION

extendWithLitter <- function(relMatrix, litter) {
  
  m <- nrow(relMatrix)
  n <- nrow(litter)
  
  # identify mother and father:
  mother <- litter$mom[1]
  father <- litter$dad[1]
  # extended will be the new relationship matrix:
  extended <- matrix(0, nrow = m + n, ncol = m + n)
  
  # fill in upper left with old matrix:
  extended[1:m, 1:m] <- relMatrix
  
  # relationship to oneself is 1, to one's siblings is 0.5
  # so matrix for litter should have 1's on diagonal
  # and 0.5's elsewhere:
  litterMat <- matrix(0.5, n, n) + diag(0.5, n, n)
  
  # fill in lower right with litter matrix:
  extended[(m + 1):(m + n), (m + 1):(m + n)] <- litterMat
  
  # determine relationship of the children with each previous
  # member of the population:
  relation <- 0.5 * (relMatrix[mother, ] + relMatrix[father, ])
  
  # construct matrix to become lower left, and fill in:
  lowerLeft <- matrix(rep(relation, n), nrow = n, byrow = TRUE)
  extended[(m + 1):(m + n), 1:m] <- lowerLeft
  
  # fill in the upper right:
  extended[1:m, (m + 1):(m + n)] <- t(lowerLeft)
  
  # add row and column names:
  rownames(extended) <- c(rownames(relMatrix), litter$id)
  colnames(extended) <- rownames(extended)
  
  # return updated relationship matrix:
  extended
}

makelitter <- function(momId,dadId, 
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
  relMatrix <- extendWithLitter(relMatrix, litter)

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

