library(jsonlite)
library(dplyr)
## 3rd Scraper, run after storeGamesList.R


## Load worlds schedule
load("scheduleGames.Rda")

## Filter for games with Realm
gamesPlayed <- filter(games, gameRealm != "NA")

## Filter for unique tournamentID/matchID
matchList <- distinct(select(gamesPlayed, tournamentID, matchID))

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
for (i in 1:nrow(matchList)) {
    
    ## Intialize tempdf for gameID/gameHash/team/player information
    tempHashDF <- data.frame(gameid=character(), gameHash=character(),
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

    ## Temporarily store each match
    tempJSON <- fromJSON(paste0("http://api.lolesports.com/api/v2/highlanderMatchDetails?tournamentId=", matchList[i, 1], "&matchId=", matchList[i, 2]))
    
    ## Bind the gameID/gameHash onto df
    tempHashDF <- rbind(tempHashDF, tempJSON[["gameIdMappings"]])
    
    ## Determine number of games in match
    numGames <- nrow(tempJSON[["gameIdMappings"]])
    
    tempHashDF[1:numGames,"team1ID"] <- tempJSON[["teams"]][["id"]][1]    
    tempHashDF[1:numGames,"team1Slug"] <- as.character(
                      tempJSON[["teams"]]["slug"][[1]][1])
}

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