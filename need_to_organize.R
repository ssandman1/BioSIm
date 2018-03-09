




### so user determines birth rate and mean litter size
### for one litter 
makeLitter<- function (momId,DadId, meansize= ) {
  ###determine how many kids in the litter (random, but use mean
  #### size to help out)
  ### determine sex of each 
  ## register each in dividual
  ### update relationship matrix
  ## to do, write find litter size function 
  ##n<-findLitterSize()### write function later
  lastId<-as.numeric(individuals[nrow(individuals,"id")])
  ids<- (lastId +1 ) : (lastId +n)
  sex<- determineSex(n)
  mom<- rep(momId,times=n)
  dad<- rep(DadId,times=n)
  dadGenes<- idividuals[id == dadId]$warner
  momGenes <- idividuals[id = momId]$warner
  warner<- numeric()
  for(i in 1:n){
    warner[i]<- getChildGenes(momGenes,dadGenes)
  }
  
  litter <- data.frame(id= as.character(ids), sex=sex, warner =
                         warner, mom=mom, dad=dad)
  individuals <- rbind(individuals,litter)
}

### try to set up individuals from init pop, use everything except 
### relationships matrix and test to see if it works

### funtion to determine child genes
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
#### reproduce 
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

