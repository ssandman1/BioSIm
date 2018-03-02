###### Data Frame about Individual Rabitts

individual<- data.frame(id, sex, warner,mom,dad)


#### Function to Determine actual litter size
birthRate<-.05
birthRate<-function(a=initial_males,
                              b=initial_alt_males,
                              c=inital_females,
                              d=initial_atl_female){
  totalPop<-a+b+c+d
  litterSize<-totalPop*average_litter_size
  sample(litterSize, 1)
 }
birthRate(a=10,b=10,c=10,d=10)



