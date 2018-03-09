## TODO  define popInit and individualInit


individualInit <- function(initial_males = 100,
                           initial_alt_males = 10,
                           initial_females = 100,
                           initial_alt_females = 10) {
  
  initSize <- initial_males + initial_females
  id <- as.character(1:initSize)
  sex <- c(rep("F", times = initial_females),
           rep("M", times = initial_males))
  warner <- c(rep(2, times = initial_alt_females),
              rep(0, times = initial_females - initial_alt_females),
              rep(2, times = initial_alt_males),
              rep(0, times = initial_males - initial_alt_males))
  mom <- rep(NA_character_, times = initial_females)
  dad <- rep(NA_character_, times = initial_males)
  data.frame(id, sex, warner, mom, dad, stringsAsFactors = FALSE)
  
}
