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