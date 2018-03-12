#### REPRODUCE FUNCTION

# extendWithLitters <- function(relMatrix, momIds, dadIds) {
#   
#   m <- nrow(relMatrix)
#   n <- nrow(litter)
#   
#   # identify mother and father:
#   mother <- litter$mom[1]
#   father <- litter$dad[1]
#   # extended will be the new relationship matrix:
#   extended <- matrix(0, nrow = m + n, ncol = m + n)
#   
#   # fill in upper left with old matrix:
#   extended[1:m, 1:m] <- relMatrix
#   
#   # relationship to oneself is 1, to one's siblings is 0.5
#   # so matrix for litter should have 1's on diagonal
#   # and 0.5's elsewhere:
#   litterMat <- matrix(0.5, n, n) + diag(0.5, n, n)
#   
#   # fill in lower right with litter matrix:
#   extended[(m + 1):(m + n), (m + 1):(m + n)] <- litterMat
#   
#   # determine relationship of the children with each previous
#   # member of the population:
#   relation <- 0.5 * (relMatrix[mother, ] + relMatrix[father, ])
#   
#   # construct matrix to become lower left, and fill in:
#   lowerLeft <- matrix(rep(relation, n), nrow = n, byrow = TRUE)
#   extended[(m + 1):(m + n), 1:m] <- lowerLeft
#   
#   # fill in the upper right:
#   extended[1:m, (m + 1):(m + n)] <- t(lowerLeft)
#   
#   # add row and column names:
#   rownames(extended) <- c(rownames(relMatrix), litter$id)
#   colnames(extended) <- rownames(extended)
#   
#   # return updated relationship matrix:
#   extended
# }

makelitter <- function(mom, dad, actualsize, lastId) {
  
  n <- actualsize
  momId <- mom$id
  dadId <- dad$id
  ids <- (lastId + 1) : (lastId + n)
  sex <- determineSex(n)
  kidMom <- rep(momId, times = n)
  kidDad <- rep(dadId, times = n)
  dadGenes <- dad$warner
  momGenes <- mom$warner
  
  warner <- numeric(n)
  for (i in 1:n) {
    warner[i]<- getChildGenes(mg = momGenes, dg = dadGenes)
  }
 
  litter <- data.frame(id = as.character(ids), 
                       sex = sex, 
                       warner = warner,
                       mom = kidMom,
                       dad = kidDad,
                       stringsAsFactors = FALSE)
  litter
}

reproduce <- function (average_litter_size, number_of_couples,
                       individuals, relMatrix = NULL, maxId) {
  
  # To compute next row of the populaton data frame: 
  popAdjustment <- data.frame(populationSize = 0,
                          males = 0, males0 = 0, males1 = 0, males2 = 0,
                          females = 0, females0 = 0, females1 = 0, females2 = 0)
  allfemales <- subset(individuals, sex == "F")
  numberFemales <- nrow(allfemales)
  femaleMates <- allfemales[sample(
    1:numberFemales,
    size = number_of_couples,
    replace = FALSE), ] 
  momIDs <- femaleMates$id
  allmales <- subset(individuals,sex == "M")
  maleMates <- allmales[sample(
    1:nrow(allmales),
    size = number_of_couples, 
    replace = TRUE), ]
  dadIDs <- maleMates$id
  litterSizes <- 1 + rpois(number_of_couples, average_litter_size - 1)
  totalKids <- sum(litterSizes)
  newPopSize <- nrow(individuals) + sum(litterSizes)
  
  # prepare a new individuals data frame:
  extIndividuals <- data.frame(
    id = character(newPopSize),
    sex = character(newPopSize),
    warner = numeric(newPopSize),
    mom = character(newPopSize),
    dad = character(newPopSize),
    stringsAsFactors = FALSE
  )
  extIndividuals[1:nrow(individuals), ] <- individuals
  
  # prepare a new relationship matrix:
  newRelMatSize <- nrow(individuals) + totalKids
  extRelMat <- matrix(0, nrow = newRelMatSize, ncol = newRelMatSize)
  extRelMat[1:nrow(individuals), 1:nrow(individuals)] <- relMatrix
  rcNames <- c(
    rownames(relMatrix),
    as.character((maxId + 1):(maxId + totalKids))
    )
  rownames(extRelMat) <- rcNames
  colnames(extRelMat) <- rcNames
  
  # prepare for loop:
  m <- nrow(individuals)
  lastId <- maxId
  
  for(i in 1:number_of_couples){
    
    ## get the next litter:
    n <- litterSizes[i]
    litter <- makelitter(mom = femaleMates[i, ],
                      dad = maleMates[i, ],
                      actualsize = n,
                      lastId = lastId)
    
    ## enter into new individual data frame:
    extIndividuals[(m + 1):(m + n), ] <- litter
    
    ## enter into new relationship matrix
    litterMat <- matrix(0.5, n, n) + diag(0.5, n, n)
    # fill in lower right with litter matrix:
    extRelMat[(m + 1):(m + n), (m + 1):(m + n)] <- litterMat
    # determine relationship of the children with each previous
    # member of the population:
    momId <- femaleMates[i, ]$id
    dadId <- maleMates[i, ]$id
    relation <- 0.5 * (extRelMat[momId, 1:m] + extRelMat[dadId, 1:m])
    # construct matrix to become lower left, and fill in:
    lowerLeft <- matrix(rep(relation, n), nrow = n, byrow = TRUE)
    extRelMat[(m + 1):(m + n), 1:m] <- lowerLeft
    # fill in the upper right:
    extRelMat[1:m, (m + 1):(m + n)] <- t(lowerLeft)
    
    ## add to population adjustment:
    popAdjustment <- colSums(rbind(
      popAdjustment,
      popAdjust(litter$sex, litter$warner)
      ))
    
    # prepare for next iteration:
    m <- m + n
    lastId <- lastId + n
  }
  
  ## return results:
  list(individuals = extIndividuals,
       relMatrix = extRelMat,
       maxId = maxId,
       popAdjustment = popAdjustment)
  
}

