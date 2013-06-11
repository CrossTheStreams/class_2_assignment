pairs1 <- read.csv("lib/team_names/pairs_1.csv")[,2] 
pairs2 <- read.csv("lib/team_names/pairs_2.csv")[,2] 

setup.teams <- function() {
  # Set up teams list: include top 200 by RPI
  # Get top 68 teams
  url <- "http://www.cbssports.com/collegebasketball/rankings/rpi/index1"
  tables <- readHTMLTable(url)
  # skip first row for formatting reasons
  teams <- tables[[4]][-1,2:7]
  names(teams) <- sapply(tables[[4]][-1][1,], as.character)

  # Get remaining teams through 347
  url <- "http://www.cbssports.com/collegebasketball/rankings/rpi/index2"
  tables <- readHTMLTable(url)
  # skip first row for formatting reasons
  teams2 <- tables[[4]][-1,2:7]
  names(teams2) <- sapply(tables[[4]][-1][1,], as.character)

  teams <- rbind(teams, teams2)
  teams <- teams[-which(teams$W == "W"),]
  teams$W <- as.numeric(teams$W)
  teams$L <- as.numeric(teams$L)

  # regularize team names
  for(i in 1:length(teams$School)) {
    name <- teams$School[i]
    url.name <- gsub("&", "", gsub(" ", "-", tolower(as.character(name))))
    print(url.name)
    teams$url.name[i] <- url.name
    url <- paste("http://www.sports-reference.com/cbb/schools/",url.name, sep="")
  }
  n <- nrow(teams)
  for(i in 1:n) {
    name <- teams$School[i]
    url.name <- transformName1(name)
    print(url.name)
    if(url.name != "") {
      teams$url.name[i] <- url.name
    }
  }
  teams$ngames <- array(0, n)
  return(teams)
}

