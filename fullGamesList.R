library(jsonlite)
library(dplyr)
## 3rd Scraper, run after storeGamesList.R


## Load worlds schedule
load("scheduleGames.Rda")

## Intialize df for gameID/gameHash/team/player information
gameHashDF <- data.frame(gameid=character(), gameHash=character(),
                         team1ID=numeric(), team1Slug=character(),
                         team1Name=character(), team1Acro=character(),
                         team2ID=numeric(), team2Slug=character(),
                         team2Name=character(), team2Acro=character(),
                         team1player1ID=numeric(),
                         team1player2ID=numeric(),
                         team1player3ID=numeric(),
                         team1player4ID=numeric(),
                         team1player5ID=numeric(),
                         team1player6ID=numeric(),
                         team1player1Starter=character(),
                         team1player2Starter=character(),
                         team1player3Starter=character(),
                         team1player4Starter=character(),
                         team1player5Starter=character(),
                         team1player6Starter=character(),
                         team2player1ID=numeric(),
                         team2player2ID=numeric(),
                         team2player3ID=numeric(),
                         team2player4ID=numeric(),
                         team2player5ID=numeric(),
                         team2player6ID=numeric(),
                         team2player1Starter=character(),
                         team2player2Starter=character(),
                         team2player3Starter=character(),
                         team2player4Starter=character(),
                         team2player5Starter=character(),
                         team2player6Starter=character()) 

## Loop through each match
for (i in 1:nrow(games)) {
    
    ## Intialize tempdf for gameID/gameHash/team/player information
    tempMatchDF <- data.frame(gameid=character(), gameHash=character(),
                              stringsAsFactors = FALSE)
    
    ## Temporarily store each match
    tempJSON <- fromJSON(paste0("http://api.lolesports.com/api/v2/highlanderMatchDetails?tournamentId=", games[i, "tournamentID"], "&matchId=", games[i, "matchID"]))
    
    ## Bind the gameID/gameHash onto df
    tempMatchDF <- rbind(tempMatchDF, tempJSON[["gameIdMappings"]])
    
    ## Determine number of games in match
    numGames <- nrow(tempJSON[["gameIdMappings"]])
    
    ## Begin loading team/player information
    tempMatchDF[1:numGames, "team1ID"] <- tempJSON[["teams"]][["id"]][[1]]    
    tempMatchDF[1:numGames, "team1Slug"] <- tempJSON[["teams"]]["slug"][[1]][1]
    tempMatchDF[1:numGames, "team1Name"] <- tempJSON[["teams"]][["name"]][[1]]
    tempMatchDF[1:numGames, "team1Acro"] <- tempJSON[["teams"]][["acronym"]][[1]]
    tempMatchDF[1:numGames, "team2ID"] <- tempJSON[["teams"]][["id"]][[2]]
    tempMatchDF[1:numGames, "team2Slug"] <- tempJSON[["teams"]]["slug"][[1]][2]
    tempMatchDF[1:numGames, "team2Name"] <- tempJSON[["teams"]][["name"]][[2]]
    tempMatchDF[1:numGames, "team2Acro"] <- tempJSON[["teams"]][["acronym"]][[2]]
    tempMatchDF[1:numGames, "team1player1ID"] <- tempJSON[["teams"]][["players"]][[1]][1]
    tempMatchDF[1:numGames, "team1player2ID"] <- tempJSON[["teams"]][["players"]][[1]][2]
    tempMatchDF[1:numGames, "team1player3ID"] <- tempJSON[["teams"]][["players"]][[1]][3]
    tempMatchDF[1:numGames, "team1player4ID"] <- tempJSON[["teams"]][["players"]][[1]][4]
    tempMatchDF[1:numGames, "team1player5ID"] <- tempJSON[["teams"]][["players"]][[1]][5]
    
    ## Test for 6th player
    if (length(tempJSON[["teams"]][["players"]][[1]]) == 6) {
      tempMatchDF[1:numGames, "team1player6ID"] <-
        tempJSON[["teams"]][["players"]][[1]][6]
    } else {
      tempMatchDF[1:numGames, "team1player6ID"] <- NA
    }
    
    ## Determine which players are starters
    for (k in 1:6) {
    
      if (!is.na(tempMatchDF[1, paste0("team1player",k,"ID")]) &&
           tempMatchDF[1, paste0("team1player",k,"ID")] %in% tempJSON[["teams"]][["starters"]][[1]]) {
        tempMatchDF[1:numGames, paste0("team1player",k,"Starter")] <- "YES"
      } else {
        tempMatchDF[1:numGames, paste0("team1player",k,"Starter")] <- "NO"
      }
      
    }
    
    ## Continue laoding team/player information
    tempMatchDF[1:numGames, "team2player1ID"] <- tempJSON[["teams"]][["players"]][[2]][1]
    tempMatchDF[1:numGames, "team2player2ID"] <- tempJSON[["teams"]][["players"]][[2]][2]
    tempMatchDF[1:numGames, "team2player3ID"] <- tempJSON[["teams"]][["players"]][[2]][3]
    tempMatchDF[1:numGames, "team2player4ID"] <- tempJSON[["teams"]][["players"]][[2]][4]
    tempMatchDF[1:numGames, "team2player5ID"] <- tempJSON[["teams"]][["players"]][[2]][5]
    
    ## Test for 6th player
    if (length(tempJSON[["teams"]][["players"]][[2]]) == 6) {
      tempMatchDF[1:numGames, "team2player6ID"] <-
        tempJSON[["teams"]][["players"]][[2]][6]
    } else {
      tempMatchDF[1:numGames, "team2player6ID"] <- NA
    }
    
    ## Determine which players are starters
    for (k in 1:6) {
      
      if (!is.na(tempMatchDF[1, paste0("team2player",k,"ID")]) &&
          tempMatchDF[1, paste0("team2player",k,"ID")] %in% tempJSON[["teams"]][["starters"]][[2]]) {
        tempMatchDF[1:numGames, paste0("team2player",k,"Starter")] <- "YES"
      } else {
        tempMatchDF[1:numGames, paste0("team2player",k,"Starter")] <- "NO"
      }
      
    }
    
    
}

## Filter for games with Realm
gamesPlayed <- filter(games, gameRealm != "NA")

## Filter for unique tournamentID/matchID
matchList <- distinct(select(gamesPlayed, tournamentID, matchID))




## Remove some variables
rm(i, tempJSON, matchList)

## Join gameHashDF and gamesPlayed by gameID
gamesPlayed <- left_join(gamesPlayed, gameHashDF,
                         by = c("gameID" = "gameID"))

## Remove some variables
rm(gameHashDF, games)

## Initialize list to hold games
gamesListNames <- gamesPlayed$gameHash
fullGameList <- vector("list", length(gamesListNames))
names(fullGameList) <- gamesListNames
rm(gamesListNames)

## Loop through game to download the game stats
for (i in 1:nrow(gamesPlayed)) {
    
    fullGameList[[i]] <- fromJSON(paste0("https://acs.leagueoflegends.com/v1/stats/game/",
                                           gamesPlayed[i, "gameRealm"], "/", gamesPlayed[i, "gameCode"], "?gameHash=", 
                                           gamesPlayed[i, "gameHash"]))
    print(i)
    
    
}

## Save raw game files
save(worldsGameList, file="Worlds2015Games.Rda")