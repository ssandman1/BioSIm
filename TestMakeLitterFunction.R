#### MAKE LITTER TEST FUNCTION
##### test data frame
individuals <- data.frame(
  id=c("1","2","3","4","5","6","7","8","9","10"), 
  sex=c("M","F","M","F","F","M","F","M","M","F"),
  warner= c(2,1,0,2,2,1,1,0,0,1),
  mom=NA_character_,dad= NA_character_,
  stringsAsFactors = FALSE)
#### data bellow will be determined by reproduce ()
dadId<- "1"
momId<-"2"
n <- 4
### actually testing function
mom <- subset(individuals, id == momId)
dad<-subset(individuals,id== dadId)
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
litter <- data.frame(id= as.character(ids), sex=sex, warner =
                       warner, mom=kidMom, dad=kidDad)
individuals <- rbind(individuals,litter)


