library(jsonlite)
library(dplyr)
## 2nd Scraper, run after storSchedules.R

## Load schedule list from Rda
load("scheduleList.Rda")

## Strip all star game
minimizedList <- as.list(0)
minimizedList[[1]] <- scheduleList[[2]]
minimizedList[[2]] <- scheduleList[[3]]
minimizedList[[3]] <- scheduleList[[4]]
minimizedList[[4]] <- scheduleList[[5]]
minimizedList[[5]] <- scheduleList[[6]]
minimizedList[[6]] <- scheduleList[[7]]
minimizedList[[7]] <- scheduleList[[8]]
minimizedList[[8]] <- scheduleList[[9]]
rm(scheduleList)

## Initialize data frame
games <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(games) <- c("tournamentName", "tournamentID", "bracketID",
                     "matchID", "gameName", "gameID", "gameRealm", "gameCode")


## Cycle through all of the schedules
for (i in 1:8) {
  
  ## Cycle through each tourname for each schedule 
  for (k in 1:length(minimizedList[[i]][["highlanderTournaments"]][["id"]])) {
    tournamentName <- minimizedList[[i]][["highlanderTournaments"]][k,"title"]
    tournamentID <- minimizedList[[i]][["highlanderTournaments"]][[k, "id"]]      
    
    ## Subset brackets
    bracketsList <- as.list(minimizedList[[i]][["highlanderTournaments"]][["brackets"]])
    
    ## Loop through each bracket to extract game information
    for (l in 1:length(bracketsList)) {
      
      ## Store bracketID
      bracketID <- names(bracketsList[l])
      
      ## extract matches
      matchesList <- as.list(bracketsList[[l]][["matches"]])
      
      ## Loop through each match
      for (m in 1:length(matchesList)) {
        
        ## Store matchID
        matchID <- names(matchesList[m])
        
        ## Extract games
        gamesList <- as.list(matchesList[[m]][["games"]])
        
        ## Loop through each game in the match  
        for (n in 1:length(gamesList)) {
          
          ## Extract subgame list
          subgameList <- as.list(gamesList[[n]])
          
          # Initialize game data frame
          gamedf <- data.frame(matrix(ncol = 8, nrow = 0))
          colnames(gamedf) <- c("tournamentName", "tournamentID", "bracketID",
                                "matchID", "gameName", "gameID", "gameRealm", "gameCode")
          
          ## loop through each subgame
          for (o in 1:length(subgameList[["id"]])) {
            
            ## Test to see if game actually exists
            if (is.na(subgameList[["id"]][o])) {
              
            } else {
              
              ## Load gamedf
              gamedf[1, 1] <- tournamentName
              gamedf[1, 2] <- tournamentID
              gamedf[1, 3] <- bracketID
              gamedf[1, 4] <- matchID
              gamedf[1, 5] <- subgameList[["generatedName"]][o]
              gamedf[1, 6] <- subgameList[["id"]][o]
              
              ## Add NAs if games were not played        
              if (is.null(subgameList[["gameRealm"]][o])) {
                gamedf[1, 7] <- "NA"    
                gamedf[1, 8] <- "NA"
              } else {
                gamedf[1, 7] <- subgameList[["gameRealm"]][o]    
                gamedf[1, 8] <- subgameList[["gameId"]][o]
              }
              
            }
            
          }
          
          ## bind the new games into the larger dataframe
          games <- rbind(games, gamedf)  
          
          
        }
        
        
      }
      
      
    }
    
  }
  
}

## Save game list
save(games, file="scheduleGames.Rda")
