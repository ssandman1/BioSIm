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

## Determine Sex Function 

determineSex<- function(n){
  sample(c("M","F"),size= n, replace = T)
}
determineSex(3)

### getChildGenes Function 

getChildGenes <- function (mg,dg){
  parentsum<-mg+dg
  if (parentsum ==0) {
    return
  }
  if (parentsum ==4) {
    return(0)
  }
  if (parentsum ==3) {
    return(sample(c(1,2), size=1, prob = c(.5,.5)))
  }
  if (parentsum ==2 ) {
    return(sample(c(0,1,2),size=1, prob=c(.25,.5,.25)))
  }
}

getChildGenes(mg=1,dg=1)