#### REPRODUCE FUNCTION

# Function to match males with females:
# in this version each female randomly selects
# one of the avialable males
makeMatches <- function(individuals, number_of_couples) {
  females <- subset(individuals, sex == "F")
  breedingFemales <- females[sample(
    1:nrow(females),
    size = number_of_couples,
    replace = FALSE), ]
  males <- subset(individuals,sex == "M")
  chosenMales <- males[sample(
    1:nrow(males),
    size = nrow(breedingFemales),
    replace = TRUE), ]
  list(females = breedingFemales, males = chosenMales)
}

# alternative mating function:
# female warner = 0 and female warner = 1 prefs can be set
# female warner = 1 picks a mate randomly
# makeMatches <- function(individuals, number_of_couples) {
#   breedingFemales <-
#     individuals %>% 
#     filter(sex == "F") %>% 
#     sample_n(number_of_couples, replace = FALSE) %>% 
#     arrange(warner)
#   warnerf <- factor(breedingFemales$warner, levels = 0:2)
#   femaleTallies <- table(warnerf)
#   males <- individuals %>% 
#     filter(sex == "M") %>% 
#     arrange(warner)
#   warnerm <- factor(males$warner, levels = 0:2)
#   maleTallies <- table(warnerm)
#   prefs0 <- c(
#     rep(1, times = maleTallies["0"]),
#     rep(5, times = maleTallies["1"]),
#     rep(10, times = maleTallies["2"])
#   )
#   prefs2 <- c(
#     rep(10, times = maleTallies["0"]),
#     rep(1, times = maleTallies["1"]),
#     rep(1, times = maleTallies["2"])
#   )
#   chosenMales <- data.frame(
#     id = character(number_of_couples),
#     sex = character(number_of_couples),
#     warner = numeric(number_of_couples),
#     mom = character(number_of_couples),
#     dad = character(number_of_couples),
#     stringsAsFactors = FALSE
#   )
#   fems0 <- femaleTallies["0"]
#   if (fems0 > 0) {
#     chosenMales[1:fems0, ] <-
#       males %>% 
#       sample_n(fems0, weight = prefs0, replace = TRUE)
#   }
#   fems1 <- femaleTallies["1"]
#   if (fems1 > 0) {
#     chosenMales[(fems0 + 1) : (fems0 + fems1), ] <-
#       males %>% 
#       sample_n(fems1, replace = TRUE)
#   }
#   fems2 <- femaleTallies["2"]
#   if (fems2 > 0) {
#     chosenMales[(fems0 + fems1 + 1) : number_of_couples, ] <-
#       males %>% 
#       sample_n(fems2, weight = prefs2, replace = TRUE)
#   }
#   results <- list(females = breedingFemales, males = chosenMales)
#   #print(results)
#   results
# }


## function to make a single litter
makeLitter <- function(mom, dad, actualsize, lastId) {
  
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
  
  ## To compute next row of the populaton data frame: 
  popAdjustment <- data.frame(populationSize = 0,
                          males = 0, males0 = 0, males1 = 0, males2 = 0,
                          females = 0, females0 = 0, females1 = 0, females2 = 0)
  
  ## get the pairs:
  couples <- makeMatches(individuals, number_of_couples)
  femaleMates <- couples$females
  maleMates <- couples$males
  
  momIDs <- femaleMates$id
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
    litter <- makeLitter(mom = femaleMates[i, ],
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

