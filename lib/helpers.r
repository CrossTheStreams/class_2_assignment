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

tournament.champs <- function(ncaa.winner,ncaa.team1,ncaa.team2) {
  winner.names <- c()

  for (i in 1:length(ncaa.winner)) {
    winner <- ncaa.winner[i]
    if (winner == 1) {
      winner.names <- c(winner.names, ncaa.team1[i]) 
    }
    else {
      winner.names <- c(winner.names, ncaa.team2[i]) 
    }
  } 


  unique.winners <- unique(winner.names)
  ncaa.champ.teams <- data.frame("team"=unique.winners,"tournament.wins"=vector(length=length(unique.winners)))

  for (i in 1:length(unique.winners)) {
    ncaa.champ.teams[which(ncaa.champ.teams$team == unique.winners[i]),"tournament.wins"] <- length(which(winner.names == unique.winners[i]))
  }

  ncaa.champ.teams <- ncaa.champ.teams[order(ncaa.champ.teams$tournament.wins,decreasing=T),]
  return(list("ncaa.champ.teams"=ncaa.champ.teams))
}

