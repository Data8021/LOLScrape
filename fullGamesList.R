library(jsonlite)
library(dplyr)
## 3rd Scraper, run after storeGamesList.R


## Load worlds schedule
load("scheduleGames.Rda")

## Filter for games played
gamesPlayed <- filter(games, gameRealm != "NA")

## Filter for unique tournamentID/matchID
matchList <- distinct(select(gamesPlayed, tournamentID, matchID))

## Intialize df for gameID/gameHash
gameHashDF <- data.frame(id=character(), gameHash=character()) 

## Loop through each match
for (i in 1:nrow(matchList)) {
    
    ## Temporarily store each match
    tempJSON <- fromJSON(paste0("http://api.lolesports.com/api/v2/highlanderMatchDetails?tournamentId=", matchList[i, 1], "&matchId=", matchList[i, 2]))
    
    ## Bind the gameID/gameHash onto df
    gameHashDF <- rbind(gameHashDF, tempJSON[["gameIdMappings"]])
}

## Remove some variables
rm(i, tempJSON, matchList)

## Join gameHashDF and gamesPlayed by gameID
colnames(gameHashDF)[1] <- "gameID"
gamesPlayed <- left_join(gamesPlayed, gameHashDF,
                         by = c("gameID" = "gameID"))

## Remove some variables
rm(gameHashDF, games)

## Initialize list to hold games
worldsGamesListNames <- gamesPlayed$gameHash
worldsGameList <- vector("list", length(worldsGamesListNames))
names(worldsGameList) <- worldsGamesListNames
rm(worldsGamesListNames)

## Loop through game to download the game stats
for (i in 1:nrow(gamesPlayed)) {
    
    worldsGameList[[i]] <- fromJSON(paste0("https://acs.leagueoflegends.com/v1/stats/game/",
                                           gamesPlayed[i, "gameRealm"], "/", gamesPlayed[i, "gameCode"], "?gameHash=", 
                                           gamesPlayed[i, "gameHash"]))
    
    
}
## Save raw game files
save(worldsGameList, file="Worlds2015Games.Rda")