library(jsonlite)
library(dplyr)
## 4th Scraper, run after fullGamesList.R

## Load files
save(gamesPlayed, file="gamesPlayed.Rda")
save(playerDF, file="playerDF.Rda")

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