# Let's get the party started!
process.lrmc <- function() {

  teams <- setup.teams()
  n <- nrow(teams)
  t <- matrix(0,n,n)
  ncaa.team1  <- c()
  ncaa.team2  <- c()
  ncaa.winner <- c()


  # loop through all teams on the teams list
  for(teams.idx in 1:n) {
    
    # read in team game results
    team1.name <- teams$url.name[teams.idx]
    url <- paste("http://www.sports-reference.com/cbb/schools/",team1.name, sep="")
    url <- paste(url, "/2013-schedule.html", sep="")
    tables <- readHTMLTable(url)
    
    outcomes <- tables[[length(tables)]]
    if(ncol(outcomes) != 13) {
      print("error")
      break
    }
    outcomes$Tm <- as.numeric(as.character(outcomes$Tm))
    outcomes$Opp <- as.numeric(as.character(outcomes$Opp))  

    bad.rows <- which(is.na(outcomes$Tm))

    if (length(bad.rows) > 0) {
      outcomes <- outcomes[-bad.rows,] 
    }

 
    # note that the paper is inconsistent with the indexing convention
    # if game is held at home court i
    # observe i vs. j (i=home, j=away, margin is x_ij = home.points - away.points)
    # increment N_i by 1
    # increment N_j by 1
    # probability that home is better than away r_x(g(i,j)) = pnorm(a*spread-b)
    # intuition: if home wins big, r > 50% and votes prefer to go from j to i
    # probability of transfer from i to j increased by t_ij += 1 - r_x
    # probability of transfer from j to i increased by t_ji += r_x
    # probability of transfer from i to i increased by t_ii += r_x
    # probability of transfer from j to j increased by t_jj += 1 - r_x
    # 
    # if game is held at neutral court, adjust spread by h
    # let team1 = i and team2 = j, adjust spread as if game were played at home by team1
    # observe i vs. j (i=team1, j=team2, margin is x_ij = team1.points - team2.points + h)
    # update as if team1 = home and team2 = away
    
    for(games.idx in 1:nrow(outcomes)) {
      
      if(outcomes$Conf[games.idx] == "") {
        next
      }
    
      team2 <- outcomes$Opponent[games.idx]
      team2.name <- transformName2(team2)

      if(team2.name == "") {
        team2.name <- gsub(" ", "-", tolower(as.character(team2)))
      }  
      
      team.index <- which(teams$url.name == team1.name)
      opponent.index <- which(teams$url.name == team2.name)

      if(outcomes$Type[games.idx] == "NCAA") {
        # if it's a tournamet game, record the outcome
        if(length(opponent.index) > 0) {
          if(team.index < opponent.index) {
            ncaa.team1 <- c(ncaa.team1, as.character(teams$url.name[teams.idx]))
            ncaa.team2 <- c(ncaa.team2, as.character(team2.name))
            if(outcomes$Tm[games.idx] > outcomes$Opp[games.idx]) {
              ncaa.winner <- c(ncaa.winner, 1)
           } else {
              ncaa.winner <- c(ncaa.winner, 2)
           }
          }
        }      
        # exclude the game from the rankings calculation
        next
      }
         
      if(length(opponent.index) > 0) {
        # don't double count...
        if(opponent.index < team.index) {
          # in this case the game in question was already recorded when the other team was team1, so skip it
          print(paste("skipping",paste(team1.name,paste("vs.",team2.name))))
          next
        }
        
        # compute margin of victory for the home team
        if(as.character(outcomes[games.idx,4]) == "") {
          team.home <- team1.name
          team.away <- team2.name
          i <- team.index
          j <- opponent.index
          if(outcomes$OT[games.idx] == "OT") {
            spread <- 0
          } else {
            spread <- outcomes$Tm[games.idx] - outcomes$Opp[games.idx]
          }
        } else if(as.character(outcomes[games.idx,4]) == "@") {
          team.home <- team2.name
          team.away <- team1.name
          i <- opponent.index
          j <- team.index
          if(outcomes$OT[games.idx] == "OT") {
            spread <- 0
          } else {
            spread <- outcomes$Opp[games.idx] - outcomes$Tm[games.idx]
          }
        } else {
          # neutral court, adjust spread as if team1 played at home
          team.home <- team1.name
          team.away <- team2.name
          i <- team.index
          j <- opponent.index
          if(outcomes$OT[games.idx] == "OT") {
            spread <- h
          } else {
            spread <- outcomes$Tm[games.idx] - outcomes$Opp[games.idx] + h
          }
        }
      
        if(is.na(spread)) {
          # resolves problem when a game is listed online before a score is available
          next
        }
        
        
        teams$ngames[i] <- teams$ngames[i] + 1
        teams$ngames[j] <- teams$ngames[j] + 1
 
        # update the probability matrix: t[i,j] = probability that i is a better team than j
        r <- pnorm(a*spread-b)
        t[i,j] <- t[i,j] + (1-r)
        t[j,i] <- t[j,i] + r
        t[i,i] <- t[i,i] + r
        t[j,j] <- t[j,j] + (1-r)
        
        print(paste(paste(paste(team.home,"vs."), team.away),paste(spread,r)))
    
      } 
    }
  }

  # Phew! How about those for loops everybody!

  # normalize lmrc outputs
  for(i in 1:n) {
    if(teams$ngames[i]>0) {
      t[i,] <- t[i,]/teams$ngames[i]
    }
  }

  #initialize ranking procedure
  p <- matrix(1/n, 1, n)

  for(i in 1:n) {
    p[i] <- n-i+1
  }

  p <- p/sum(p)

  # run ranking procedure
  for(i in 1:1000) {
    p.next <- p %*% t
    print(norm(p.next - p))
    p <- p.next
  }

  # add LRMC score to table and sort to get ranking
  teams$LRMC.score <- t(p)

  teams <- teams[order(teams$LRMC.score, decreasing=TRUE),]

  return(list("teams"=teams,"ncaa.team1"=ncaa.team1,"ncaa.team2"=ncaa.team2,"ncaa.winner"=ncaa.winner))

}
