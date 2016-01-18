library(jsonlite)
library(dplyr)
## 4th Scraper, run after fullGamesList.R and esportapediaMHScrape.R

## Load files
load("gamesPlayed.Rda")
load("esportapediaMHLinks.Rda")

## Downselect to variables needed to pull game files
mhLinks <- select(matchHistoryLinks, gameRealm, gameCode, gameHash)
gmPlay <- select(gamesPlayed, gameRealm, gameCode, gameHash)

## Integrate into single df/remove stray attributes
consMH <- rbind(mhLinks, gmPlay)
attributes(consMH$gameRealm) <- NULL
attributes(consMH$gameCode) <- NULL
attributes(consMH$gameHash) <- NULL

## Remove duplicates
consMH <- distinct(consMH)

## Initialize list to hold games
gamesListNames <- consMH$gameHash
fullGameList <- vector("list", length(gamesListNames))
names(fullGameList) <- gamesListNames
rm(gamesListNames)

## Loop through game to download the game stats
for (i in 1:nrow(consMH)) {
    
    fullGameList[[i]] <- fromJSON(paste0("https://acs.leagueoflegends.com/v1/stats/game/",
                                           consMH[i, "gameRealm"], "/", consMH[i, "gameCode"], "?gameHash=", 
                                           consMH[i, "gameHash"]))
    print(i)
}

## Save raw game files
save(fullGameList, file="fullGameList.Rda")
