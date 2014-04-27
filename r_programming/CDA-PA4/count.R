count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  ## Check that specific "cause" is allowed; else throw error
  ## Read "homicides.txt" data file
  ## Extract causes of death
  ## Return integer containing count of homicides for that cause
  homicides <- readLines('homicides.txt')
  
  if(cause == "asphyxiation") {
    length(grep("[cC]ause: [Aa]sphyxiation",homicides))
  }else if(cause == "blunt force"){
    length(grep("[cC]ause: [Bb]lunt [Ff]orce",homicides))
  }else if(cause == "other"){
    length(grep("[cC]ause: [Oo]ther",homicides))
  }else if(cause == "shooting"){
    length(grep("[cC]ause: [Ss]hooting",homicides))    
  }else if(cause == "stabbing"){
    length(grep("[cC]ause: [Ss]tabbing",homicides))     
  }else if(cause == "unknown"){
    length(grep("[cC]ause: [Uu]nknown",homicides))        
  }else stop("meow!");
}