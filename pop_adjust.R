## function to adjust population after births or deaths

popAdjust <- function(sex, warner) {
  
  # really number of new individuals:
  populationSize <- length(sex)
  
  males <- sum(sex == "M")
  males0 <- sum(sex == "M" & warner == 0)
  males1 <- sum(sex == "M" & warner == 1)
  males2 <- males - males0 - males1
  females <- sum(sex == "F")
  females0 <- sum(sex == "F" & warner == 0)
  females1 <- sum(sex == "F" & warner == 1)
  females2 <- females - females0 - females1
  data.frame(populationSize, 
             males, males0, males1, males2,
             females, females0, females1, females2)
}