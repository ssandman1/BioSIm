#### REPRODUCE TEST FUNCTION
# library(tidyverse)
average_litter_size<- 5 
actualsize<- rpois(1,average_litter_size) 

allfemales<-subset(individuals,sex=="F") ## all females from d.f.
n<-nrow(allfemales)
df <- allfemales[sample(1:n, size = 1, replace = FALSE), ] 
##randomly picking 1 female from data frame to be mom 
momsID<-df$id


allmales<-subset(individuals,sex=="M") ### selecting all males
### from d.f.
n<-nrow(allmales)
maleMate <- allmales[sample(1:n, size = 1, replace = FALSE), ]
### ramdomly pick 1 male
dadsID<-maleMate$id ### getting id of the male that was picked





### Putting the information above into makelitter() (helper 
### function of reproduce() )
makelitter<-function(momId,dadId, 
                     actualsize) {
  
  n<-actualsize
  
  mom <- subset(individuals, id == momId)
  dad<-subset(individuals,id == dadId)
  
  lastRow <- nrow(individuals)
  lastId<-as.numeric(individuals[lastRow, "id"])
  ids<- (lastId +1 ) : (lastId +n)
  
  sex<- determineSex(n)
  
  kidMom<- rep(momId,times=n)
  kidDad<- rep(dadId,times=n)
  
  dadGenes<- dad$warner
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

makelitter(momId = momsID,
           dadId = dadsID,
           actualsize=actualsize) 

addToPop <- function(litter, currentPop) {
  rbind(currentPop, litter)
}
#### get function to work for multiple males and females 
## pick males w replacement, so 1 male can reproduce w multiple
## females
### make better names 
### write out what each line is doing in future

### OUTLINE FOR ACTUALL REPRODUCE FUNCTION
reproduce <- function (){
  ### pick number of females to breed
  ### try to find a male for each of the female 
  ### end up w two vectors one F and one M
  ### each vector has id number of female and male
  ### if not enough males then only reproduce till the males run
  ### out
  ### for loop to put each mated pair through makeLitter function
  for (i in 1:length(females,meanLitterSize)) {
    mom<-females[i]
    dad<-males[i]
    makeLitter(mom, dad, meansize = meanLitterSize)
  }
}

