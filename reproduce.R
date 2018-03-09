#### REPRODUCE TEST FUNCTION
# library(tidyverse)
average_litter_size<- 5
number_of_couples <- 5

makelitter<-function(momId,dadId, 
                     actualsize) {
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
  for(i in 1:n){
    warner[i]<- getChildGenes(mg=momGenes,dg=dadGenes)
  }
  litter <- data.frame(id = as.character(ids), 
                       sex = sex, 
                       warner = warner,
                       mom = kidMom,
                       dad = kidDad,
                       stringsAsFactors = FALSE)
  individuals <<- rbind(individuals, litter)
}

reproduce <- function (average_litter_size, number_of_couples){
  ### pick number of females to breed
  ### try to find a male for each of the female 
  ### end up w two vectors one F and one M
  ### each vector has id number of female and male
  ### if not enough males then only reproduce till the males run
  ### out
  ### for loop to put each mated pair through makeLitter function
    
    allfemales<-subset(individuals,sex=="F") ## all females from d.f.
    n <- nrow(allfemales)
    femaleMates <- allfemales[sample(1:n, size = number_of_couples, replace = FALSE), ] 
    momIDs<-femaleMates$id
    allmales<-subset(individuals,sex=="M") ### selecting all males
    maleMates <- allmales[sample(1:nrow(allmales),
                                size = number_of_couples, 
                                replace = TRUE), ]
    ### ramdomly pick 1 male
    dadIDs<-maleMates$id ### getting id of the male that was picked
    
    for(i in 1:number_of_couples){
      actualsize<- rpois(1, average_litter_size) 
      makelitter(momId = momIDs[i],dadId = dadIDs[i],
                 actualsize = actualsize)
    }
}

