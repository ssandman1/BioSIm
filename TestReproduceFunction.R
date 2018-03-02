#### REPRODUCE TEST FUNCTION
library(tidyverse)
average_litter_size<- 5 
actualsize<- sample(average_litter_size,1) 

allfemales<-subset(individuals,sex=="F") ## all females from d.f.
n<-nrow(allfemales)
df <- allfemales[sample(1:n, size = 1, replace = FALSE), ] 
##randomly picking 1 female from data frame to be mom 
momsID<-df[c("id")]%>% ### subsetting females id
unlist()
momID2<-as.character(c(momsID)) ### makes the female picked from 
### data frame a character vector

allmales<-subset(individuals,sex=="M") ### selecting all males
### from d.f.
n<-nrow(allmales)
ff <- allmales[sample(1:n, size = 1, replace = FALSE), ]
### ramdomly picking 1 male
dadsID<-ff[c("id")]%>% ### getting id of the male that was picked
unlist()
dadID2<-as.character(c(dadsID)) ### converting id to character
### vector

momID2
dadID2

### Putting the information above into makelitter() (helper 
### function of reproduce() )
makelitter<-function(momID=momID2,dadID=dadID2,
                     actualsize=actualsize){
  
  actualsize<-n
  
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
  warner<- numeric()
  for(i in 1:n){
    warner[i]<- getChildGenes(mg=momGenes,dg=dadGenes)
  }
  litter <- data.frame(id= as.character(ids), sex=sex, warner =
                              warner, mom=kidMom, dad=kidDad)
  individuals<- rbind(individuals,litter)
}
makelitter() ### keep getting error stating that object 
### dadId not found and I dont know why




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

