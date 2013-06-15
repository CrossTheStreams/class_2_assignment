# http://www.sports-reference.com/cbb/
source("lib/helpers.r")

pkgTest("XML")
pkg("outliers")
# include XML package to read data from HTML tables
library(XML)
library(outliers)

# set some constants
tau <- 4.26
sig <- 11 
h   <- 4

# code to compute the probability that one team is better than another based on point spread, x.
# compute P(Z>0 | X=x) from eq. (12) pnorm(2*tau^2/(sig*sqrt((sig^2+2*tau^2)*(sig^2+4*tau^2)))*x - h/sig*sqrt((sig^2+4*tau^2)/(sig^2+2*tau^2))) 
a <- 2*tau^2/(sig*sqrt((sig^2+2*tau^2)*(sig^2+4*tau^2)))
b <- 2*tau^2*h/(sig*sqrt((sig^2+2*tau^2)*(sig^2+4*tau^2)))

x <- 20
pnorm(a*x-b)

# Functions we need to setup our data.
source("lib/setup_teams.r")

# LRMC Algorithm
source("lib/lrmc.r")

# Run the LRMC
collection <- process.lrmc()

teams <- collection$teams
n <- nrow(teams)
ncaa.team1  <- collection$ncaa.team1
ncaa.team2  <- collection$ncaa.team2
ncaa.winner <- collection$ncaa.winner

#To save, just in case...
#write.csv(teams,"lib/lrmc_teams.csv")
#teams <- read.csv("lib/lrmc_teams.csv")

teams.alpha <- teams[order(teams$School, decreasing=FALSE),]

# for each matchup in the NCAA Tournement, compute the number of
# times the LRMC model predicted the winner.
correct <- 0
total <- length(ncaa.team1)

for(i in 1:total) {  
  
  score1 <- teams$LRMC.score[teams$url.name == ncaa.team1[i]]
  score2 <- teams$LRMC.score[teams$url.name == ncaa.team2[i]]
  
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

# correct picks in the NCAA tournement based on LMRC
print("LRMC")
print(correct)
print(total)
print(correct/total)


##########
#########

# Using average playing height for predicting wins
# Inspired by:
# http://www.guardian.co.uk/sport/blog/2013/mar/18/ncaa-march-madness-brackets-team-height

# teams <- read.csv("lib/lrmc_teams.csv")

# Character to vector help map strings to numeric values
heights_arr <- c("5-0","5-1","5-2","5-3","5-4","5-5","5-6","5-7","5-8","5-9","5-10","5-11","6-0","6-1","6-2","6-3","6-4","6-5","6-6","6-7","6-8","6-9","6-10","6-11","7-0","7-1","7-2","7-3","7-4","7-5","7-6","7-7","7-8","7-9","7-10","7-11","8-0")

teams["mean.playing.height"] <- NA

# Use total play time 

process.heights <- function () {

  for(teams.idx in 1:n) {
   
    team.name <- teams$url.name[teams.idx]
    url <- paste("http://www.sports-reference.com/cbb/schools/",team.name,"/2013.html",sep="")
    tables <- readHTMLTable(url)
    roster <- as.character(tables$roster[order(tables$roster$Player),"Player"])
    height_strs <- as.character(tables$roster[order(tables$roster$Player),"Ht"])

    heights <- c() 

    min.per.game <- as.numeric(as.vector(tables$advanced[order(tables$advanced$Player),"MP"]))
    total.min.played <- sum(min.per.game)
    
    for(str_i in 1:length(height_strs)) { 
      # inches to centimeters
      height_centimeters <- (59 + which(heights_arr == height_strs[str_i])) * 2.54

      # Got weird error concerning a zero length numeric vector?
      if (length(height_centimeters) == 1) {
        print(paste(as.character(roster[str_i])," : ",as.character(height_centimeters), " centimeters tall."))
        heights[str_i] <- height_centimeters
      }
    }

    # We should only have integers here, but coerce in case. (errors earlier)
    heights <- heights[which(!is.na(heights))]
    min.per.game <- min.per.game[which(!is.na(heights))]

    heights <- as.numeric(heights)

    time.adjusted.heights <- heights * (min.per.game/total.min.played)
       
    mean.playing.height <- sum(time.adjusted.heights,na.rm=TRUE) 
    teams[which(teams$url.name == team.name),"mean.playing.height"] <- mean.playing.height  
    print(paste(team.name," has time adjusted team height : ",mean.playing.height))
  }
  return(list("teams.with.heights"=teams))
}

# Last time I looked at height, I look for it's prediction of a winning season.
# 

collection <- process.heights()
teams <- collection$teams.with.heights
write.csv(teams.with.heights,"lib/lrmc_teams_with_heights.csv")

# for each matchup in the NCAA Tournement, compute the number of
# times the widely used RPI model predicted the winner.

correct <- 0
total <- length(ncaa.team1)
for(i in 1:total) {
  
  score1 <- teams$mean.playing.height[teams$url.name == ncaa.team1[i]]
  score2 <- teams$mean.playing.height[teams$url.name == ncaa.team2[i]]
  
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


# correct picks in the NCAA tournement based on Height
print("Height")
print(correct)
print(total)
print(correct/total)

# There are impurities in the data that are making outliers.
alpha <- teams
teams <- teams[-which(teams$mean.playing.height == outlier(teams$mean.playing.height)),]
teams <- teams[-which(teams$win.percent == outlier(teams$win.percent)),]

teams$win.percent <- teams$W/(teams$W + teams$L)

# Is there any value in above mean height for a team?

# Get our combined mean height among all teams.
all.mean.height <- mean(teams$mean.playing.height)
# Normalized variance of mean playing heights
teams$norm.height.diffs <- ((teams$mean.playing.height/all.mean.height) - 1)

# Get logical values
teams["above.mean.height"] <- (teams$mean.height > combined.mean.height)
teams["winning.season"] <- (as.numeric(teams$W) > as.numeric(teams$L))


# Linear Regressions of height data and LRMC
# Let's compare height with the LRMC as a predictor of winning percentage
height.fit <- lm(formula = teams.win.percent ~ teams.mean.playing.height, data = data.frame(teams$mean.playing.height, teams$win.percent)) 

lrmc.fit <- lm(formula = teams.win.percent ~ teams.LRMC.score, data = data.frame(teams$LRMC.score, teams$win.percent)) 

par(col="black",cex=1,lw=1)
plot(teams$mean.playing.height,teams$win.percent)
plot(teams$LRMC.score,teams$win.percent)
par(col="blue",cex=2,lwd=1.5)
abline(height.fit)
abline(lrmc.fit)

ncaa.champ.teams <- tournament.champs(ncaa.winner,ncaa.team1,ncaa.team2)$ncaa.champ.teams

# Top 25 in tournament
for(i in 1:25) {      
  height <- teams[which(teams$url.name==ncaa.champ.teams$teams[i]),"mean.playing.height"]
  percent <- teams[which(teams$url.name==ncaa.champ.teams$teams[i]),"win.percent"] 
  points(x=height,y=percent,bg="red",cex=2,col="green")
}

# Top 25 in tournament
for(i in 1:25) {      
  lrmc <- teams[which(teams$url.name==ncaa.champ.teams$teams[i]),"LRMC.score"]
  percent <- teams[which(teams$url.name==ncaa.champ.teams$teams[i]),"win.percent"] 
  points(x=lrmc,y=percent,bg="red",cex=1.5,col="green")
}

# Top 25 in tournament
for(i in 1:25) {      
  lrmc <- teams[which(teams$url.name==ncaa.champ.teams$teams[i]),"LRMC.score"]
  percent <- teams[which(teams$url.name==ncaa.champ.teams$teams[i]),"win.percent"] 
  points(x=lrmc,y=percent,bg="red",cex=1.5,col="green")
}
 
champ.team.rows <- teams[which(teams$url.name %in% ncaa.champ.teams$teams),]
ncaa.champ.teams$win.percent <- c()

for (i in which(!is.na(champ.team.rows$url.name))) {
  ncaa.champ.teams[which(ncaa.champ.teams$teams == champ.team.rows[i,"url.name"]),"win.percent"] <- teams[i,"win.percent"]
}

hist(ncaa.champ.teams$win.percent[1:25])

plot(sapply(X=ncaa.champ.teams$tournament.wins,FUN=function(x){x - rnorm(1)/10}),ncaa.champ.teams$win.percent)

true_positives <- length(teams[which(teams$above.mean.height == T & ason == T),1])
true_negatives <- length(teams[which(teams$above.mean.height == F & teams$winning.season == F),1])
false_positives <- length(teams[which(teams$above.mean.height == T & teams$winning.season == F),1])
false_negatives <- length(teams[which(teams$above.mean.height == F & teams$winning.season == T),1])

# Accuracy
accuracy <- (true_positives+true_negatives)/(nrow(teams))
print(accuracy)
# Precision
precision <-(true_positives)/(true_positives + false_positives) 
print(precision)
# Recall
recall <- (true_positives)/(true_positives + false_negatives) 
print(recall)

# Construct a contingency table.
contin.table <- as.table(rbind(c(true_positives,true_negatives),c(false_positives,false_negatives)))

Xsq <- chisq.test(contin.table)

# We have a high p value in our test.
print(Xsq)

# We are close to expected results.
print(contin.table)
print(Xsq$expected)

# Looking into this further, I could iterate past seasons to test this further.

# As an aside, while looking at plots, I found these interesting:

  # Average heights of teams follow a fairly normal distribution (due to the Central Limit Theorem?):
  qqnorm(teams$mean.playing.height)
  qqnorm(rnorm(1000))

  # There seems to be correlation between mean height and rank.
  plot(teams$SOS.Rank,teams$mean.height)
  par(col='red')
  abline(mean(teams$mean.height)+1,-1/mean(as.numeric(teams$SOS.Rank)))
  par(col='black')
  qqplot(as.numeric(teams$SOS.Rank),teams$min.height)


# Plotting wins against heights

  # Introduce noise
  plot(sapply(teams$max.height,function(x){return(x+(rnorm(1))/2)}),teams$W)

  plot(teams$min.height,teams$W)

  # Introduce noise
  plot(sapply(teams$min.height,function(x){return(x+(rnorm(1))/2)}),teams$W)

  plot(sapply(as.numeric(teams$SOS.Rank),function(x){return(x+(rnorm(1)/4))}),sapply(teams$max.height,function(x){return(x+(rnorm(1))/2)}))

  write.csv(teams,file="teams_with_heights.csv")

  asdf <- read.csv("teams_with_heights.csv")

  teams.by.mean.height <- teams[order(teams$mean.height, decreasing=TRUE),]

