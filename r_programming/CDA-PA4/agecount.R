agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  ## Read "homicides.txt" data file
  ## Extract ages of victims; ignore records where no age is
  ## given
  ## Return integer containing count of homicides for that age
  homicides <- readLines('homicides.txt')
  
  r <- regexpr('[0-9]+ [Yy]ears [Oo]ld',homicides)
  s0 <- regmatches(homicides,r)
  s1 <- sub(' [Yy]ears [Oo]ld','',s0)
  data <- as.numeric(s1)
  length(data[data==age])
}