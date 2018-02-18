###### Data Frame about Individual Rabitts
### need to make function to determine genetic makeup (if warner 
### or not)
### to  give rabbits id - at end of actuall_litter_size give each 
###new rabbit an id tag (how to come up with infinite number of 
### unique ids?)

individual.init<- data.frame(id, sex= determineSex(), warner,
                             nrow= id)
### nrow = id is to make as new rows for all id numbers

#### Function to Determine actual litter size

actuall_litter_size<-function(a=initial_males,
                              b=initial_alt_males,
                              c=inital_females,
                              d=initial_atl_female){
  totalPop<-a+b+c+d
  litterSize<-totalPop*average_litter_size
  sample (litterSize, 1)
 }
actuall_litter_size()

}

#### Function To Determine Sex
determineSex<- function(n=actuall_litter_size()){
  results<- character()
  for( i in n) {
  results<-rbinom(n,1,prob=.5)
  }
  results 
  a<-character()
  for( u in 1:length(results)) {
    
    a<-ifelse(results == 1,"M","F") 
    }
  a
}
determineSex()

#### Function to determine if warner
##### need help
### is m11 to determine genetic makeup of original population
### are genes going to be assigned randomly or via parent genes
### (0,1,2)
m11<- function(n=actuall_litter_size()){
  genes<-numeric()
  for(size in 1:n){
  genes<-sample(size = 1, c(0,1,2),prob = c(.25,.50,.25))
  genes[size]
  }
  genes
}
m11()
determineALTChild(mom,dad, (0,1,2))
if(mom==0&dad==0)return(rep0,litter)
if(mom==2&dad==2) return 2
if(mom==0&dad== 2) return(1) 
if (mom== 1& dad ==1) return(m11())
relMatInit()
