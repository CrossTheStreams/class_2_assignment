# Andrew Hautau
# Methods for Data Analysis
# Weeks 8-10 Assignment


# http://www.sports-reference.com/cbb/
source("lib/helpers.r")
# include XML package to read data from HTML tables
# include outliers package
pkgTest("XML")
pkgTest("outliers")

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
collection <- process.lrmc(year="2012")

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

lrmc.picks <- apply.model(teams,"LRMC.score",ncaa.team1,ncaa.team2,ncaa.winner)


win.percent.picks <- apply.model(teams,"win.percent",ncaa.team1,ncaa.team2,ncaa.winner)

##########
#########

# My attempt at getting predictive value out of height data.
# Inspired by:
# http://www.guardian.co.uk/sport/blog/2013/mar/18/ncaa-march-madness-brackets-team-height

# teams <- read.csv("lib/lrmc_teams.csv")

source("lib/heights.r")

teams <- process.heights(teams,"2013")$teams.with.heights
write.csv(teams,"lib/lrmc_teams_with_heights.csv")
teams <- read.csv("lib/lrmc_teams_with_heights.csv")

# Off the cuff, height alone is no better than a coin flip.
height.picks <- apply.model(teams,"time.adjusted.height",ncaa.team1,ncaa.team2,ncaa.winner)


# There are impurities in the data that are making outliers.
teams <- teams[-which(teams$time.adjusted.height == outlier(teams$time.adjusted.height)),]
teams <- teams[-which(teams$win.percent == outlier(teams$win.percent)),]


# Get our combined mean height among all teams.
all.mean.height <- mean(teams$time.adjusted.height)
# Normalized variance of mean playing heights
teams$heights.scaled <- scale(teams$time.adjusted.height)
# Championship teams in order of tournament victories
ncaa.champ.teams <- tournament.champs(ncaa.winner,ncaa.team1,ncaa.team2)$ncaa.champ.teams

# Linear Regressions of height data and LRMC
# Let's compare height with the LRMC as a predictor of winning percentage
# Yellow hightlights signify a top 25 team and green top 10

height.fit <- lm(formula = teams.win.percent ~ teams.time.adjusted.height, data = data.frame(teams$time.adjusted.height, teams$win.percent)) 
par(col="black",cex=1,lw=1)
plot(teams$time.adjusted.height,teams$win.percent)
par(col="blue",cex=2,lwd=1.5)
abline(height.fit)
highlight.points(as.character(ncaa.champ.teams$teams),"time.adjusted.height","win.percent")

# We see a very modest correlation with height here with win percentage.
summary(height.fit)
# However, win percentage is a bad predictor of the tournament!
win.percent.2013.picks <- apply.model(teams,"win.percent",ncaa.team1,ncaa.team2,ncaa.winner)


# Let's attempt at weighting this in with the LRMC and see what happens.

teams$height.scaled <- scale(teams$time.adjusted.height)
teams$LRMC.scaled <- scale(teams$LRMC.score)

teams$lrmc.height <- weight.models(.2,teams$height.scaled,teams$LRMC.scaled)
# Applying a bit of the height didn't break anything! Hooray!
# However, it also didn't help a whole lot.
lrmc.height.2013.picks <- apply.model(teams,"lrmc.height",ncaa.team1,ncaa.team2,ncaa.winner)
# Applying a litte more is about the same..
teams$lrmc.height <- weight.models(.4,teams$height.scaled,teams$LRMC.scaled)
lrmc.height.2013.picks <- apply.model(teams,"lrmc.height",ncaa.team1,ncaa.team2,ncaa.winner)
# But this is when we get over-zealous 
teams$lrmc.height <- weight.models(.7,teams$height.scaled,teams$LRMC.scaled)
lrmc.height.2013.picks <- apply.model(teams,"lrmc.height",ncaa.team1,ncaa.team2,ncaa.winner)


# Time-adjusted height may have minor correlation with wins, but 
# it is NOT have beneficial predictive value for mixing it in with the LRMC model.

# Test again for 2012

collection <- process.lrmc("2012")
teams.2012 <- collection$teams

# Set these values for 2012
n <- nrow(teams)
ncaa.team1  <- collection$ncaa.team1
ncaa.team2  <- collection$ncaa.team2
ncaa.winner <- collection$ncaa.winner

# Next would be to test this with 2012
# Unfortunately, I have an odd string error occuring in process.heights() that I've yet to solve

#teams.2012$height.scaled <- scale(teams.2012$time.adjusted.height)
#teams.2012$LRMC.scaled <- scale(teams.2012$LRMC.score)

#teams.2012$lrm2.height <- weight.models(.2,teams.2012$height.scaled,teams.2012$LRMC.scaled)
## Applying a bit of the height didn't break anything! Hooray!
## However, it also didn't help a whole lot.
#lrmc.height.2012.picks <- apply.model(teams.2012,"lrmc.height",ncaa.team1,ncaa.team2,ncaa.winner)
## Applying a litte more is about the same..
#teams.2012$lrmc.height <- weight.models(.4,teams.2012$height.scaled,teams.2012$LRMC.scaled)
#lrmc.height.2022.picks <- apply.model(teams.2012,"lrmc.height",ncaa.team1,ncaa.team2,ncaa.winner)
## But this is when we get over-zealous 
#teams.2012$lrmc.height <- weight.models(.7,teams.2012$height.scaled,teams.2012$LRMC.scaled)
#lrmc.height.2013.picks <- apply.model(teams.2012,"lrmc.height",ncaa.team1,ncaa.team2,ncaa.winner)

