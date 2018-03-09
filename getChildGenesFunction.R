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