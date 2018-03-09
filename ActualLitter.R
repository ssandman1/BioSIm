### make Litter Function (helper function of reproduce())

makelitter<-function(momID,dadID,actualsize){
  
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
individuals <- data.frame(id= as.character(ids), sex=sex, warner =
                       warner, mom=kidMom, dad=kidDad)
individuals<- rbind(individual,litter)
}

