# test for packages
pkgTest <- function(x) {
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found") 
  
  }
}

## Team Name Setup

# make some team url corrections by hand
transformName1 <- function(name) {

  pairs1.length <- length(pairs1)

  full.name <- pairs1[seq(1,pairs1.length,2)]
  url.name  <- pairs1[seq(2,pairs1.length,2)] 

  idx <- which(as.character(full.name) == name)
  if(length(idx) > 0) {
    return(as.character(url.name[idx]))
  } else {
    return("")
  }
}

# some team url corrections
transformName2 <- function(name) { 

  pairs2.length <- length(pairs2)

  full.name <- pairs2[seq(1,pairs2.length,2)]
  url.name  <- pairs2[seq(2,pairs2.length,2)]
  
  idx <- which(as.character(full.name) == name)
  if(length(idx) > 0) {
    return(as.character(url.name[idx]))
  } else {
    return("")
  }
}


