# Team height adjusted by time on court

process.heights <- function (teams,year) {

  # Character to vector help map strings to numeric values
  heights_arr <- c("5-0","5-1","5-2","5-3","5-4","5-5","5-6","5-7","5-8","5-9","5-10","5-11","6-0","6-1","6-2","6-3","6-4","6-5","6-6","6-7","6-8","6-9","6-10","6-11","7-0","7-1","7-2","7-3","7-4","7-5","7-6","7-7","7-8","7-9","7-10","7-11","8-0")

  teams["time.adjusted.height"] <- NA
  
  for(teams.idx in 1:n) {
   
    team.name <- teams$url.name[teams.idx]
    url <- paste("http://www.sports-reference.com/cbb/schools/",team.name,"/",year,".html",sep="")
    tables <- readHTMLTable(url)
    if (!is.vector(tables$roster$Player)) {
      next 
    }
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
       
    time.adjusted.height <- sum(time.adjusted.heights,na.rm=TRUE) 
    teams[which(teams$url.name == team.name),"time.adjusted.height"] <- time.adjusted.height  
    print(paste(team.name," has time adjusted team height : ",time.adjusted.height))
  }
  return(list("teams.with.heights"=teams))
}


