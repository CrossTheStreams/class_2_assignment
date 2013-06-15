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
  ncaa.champ.teams <- data.frame("teams"=unique.winners,"tournament.wins"=vector(length=length(unique.winners)))

  for (i in 1:length(unique.winners)) {
    ncaa.champ.teams[which(ncaa.champ.teams$team == unique.winners[i]),"tournament.wins"] <- length(which(winner.names == unique.winners[i]))
  }

  ncaa.champ.teams <- ncaa.champ.teams[order(ncaa.champ.teams$tournament.wins,decreasing=T),]
  return(list("ncaa.champ.teams"=ncaa.champ.teams))
}

# Helper to apply to predictions.

apply.model <- function(teams,column,ncaa.team1,ncaa.team2,ncaa.winner) {
  correct <- 0
  total <- length(ncaa.team1)
  for(i in 1:total) {

    # Getting weird string errors here sometimes
    # North Carolina A & T isn't even in this data...
    if(ncaa.team2[i] == "north-carolina-at" || ncaa.team1[i] ==  "north-carolina-at" ) {
      next
    }
   
    score1 <- teams[,column][teams$url.name == ncaa.team1[i]]
    score2 <- teams[,column][teams$url.name == ncaa.team2[i]]
    
    score1 <- as.numeric(as.character(score1))
    score2 <- as.numeric(as.character(score2))   
    
    symbol <- "X"
    
    if(ncaa.winner[i] == 1 & score1 > score2) {
      correct <- correct + 1
      symbol <- "*"
    }
    
    if(ncaa.winner[i] == 2 & score2 > score1) {
      correct <- correct + 1
      symbol <- "*"
    }
    
    print(paste(symbol,paste(paste(ncaa.team1[i],"vs."),ncaa.team2[i])))
    
  }
  # correct picks in the NCAA tournement based on the applied column
  collection <- list("correct"=correct,"total"=total,"success.ratio"=(correct/total))
  print(collection) 
  return(collection)
}

highlight.points <- function(champ.teams,column1,column2){
  # Top 25 in tournament
  for(i in 1:25) {      
    height <- teams[which(teams$url.name==champ.teams[i]),column1]
    percent <- teams[which(teams$url.name==champ.teams[i]),column2] 
    points(x=height,y=percent,bg="red",cex=2,col="yellow")
  }

  # Top 10 in tournament
  for(i in 1:10) {      
    lrmc <- teams[which(teams$url.name==champ.teams[i]),column1]
    percent <- teams[which(teams$url.name==champ.teams[i]),column2] 
    points(x=lrmc,y=percent,bg="red",cex=1.5,col="green")
  }
}
