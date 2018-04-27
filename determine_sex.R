## Determine Sex Function 

determineSex<- function(n){
  sample(c("M","F"),size= n, replace = T)
}
determineSex(3)