library(jsonlite)
library(dplyr)
## 3rd Scraper, run after storeGamesList.R


## Load worlds schedule
load("scheduleGames.Rda")

## Filter for games with Realm
gamesPlayed <- filter(games, gameRealm != "NA")

## Get distinct tournament/match list
matchList <- distinct(select(gamesPlayed, tournamentID, matchID))


## Intialize df for gameID/gameHash/team information
gameHashDF <- data.frame(gameid=character(),
                         gameHash=character(),
                         team1ID=numeric(),
                         team1Slug=character(),
                         team1Name=character(),
                         team1Acro=character(),
                         team2ID=numeric(),
                         team2Slug=character(),
                         team2Name=character(),
                         team2Acro=character(),
                         stringsAsFactors = FALSE)

playerDF <- data.frame(teamID=numeric(),
                       playerID=numeric(),
                       playerSlug=character(),
                       playerName=character(),
                       playerFirstName=character(),
                       playerLastName=character(),
                       playerComboName=character(),
                       playerRole=character(),
                       stringsAsFactors = FALSE)

## Loop through each match
for (i in 1:nrow(matchList)) {
    
    ## Intialize tempdfs
    tempMatchDF <- data.frame(gameid=character(),
                              gameHash=character(),
                              stringsAsFactors = FALSE)
    
    tempPlayerDF <- data.frame(teamID=numeric(),
                               playerID=numeric(),
                               playerSlug=character(),
                               playerName=character(),
                               playerFirstName=character(),
                               playerLastName=character(),
                               playerComboName=character(),
                               playerRole=character(),
                               stringsAsFactors = FALSE)
    
    ## Temporarily store each match
    tempJSON <- fromJSON(paste0("http://api.lolesports.com/api/v2/highlanderMatchDetails?tournamentId=", matchList[i, "tournamentID"], "&matchId=", matchList[i, "matchID"]))
    
    ## Bind the gameID/gameHash onto df
    tempMatchDF <- rbind(tempMatchDF, tempJSON[["gameIdMappings"]])
    
    ## Test if any games played, if not, go to next iteration
    if (length(tempJSON[["gameIdMappings"]])==0){
      print(i)
      next
    }
    
    ## Determine number of games in match
    numGames <- nrow(tempJSON[["gameIdMappings"]])
    
    ## Load team information
    team1ID <- tempJSON[["teams"]][["id"]][[1]]
    tempMatchDF[1:numGames, "team1ID"] <- team1ID     
    tempMatchDF[1:numGames, "team1Slug"] <- tempJSON[["teams"]]["slug"][[1]][1]
    tempMatchDF[1:numGames, "team1Name"] <- tempJSON[["teams"]][["name"]][[1]]
    team1Acro <- tempJSON[["teams"]][["acronym"]][[1]]
    tempMatchDF[1:numGames, "team1Acro"] <- team1Acro
    team2ID <- tempJSON[["teams"]][["id"]][[2]]
    tempMatchDF[1:numGames, "team2ID"] <- team2ID
    tempMatchDF[1:numGames, "team2Slug"] <- tempJSON[["teams"]]["slug"][[1]][2]
    tempMatchDF[1:numGames, "team2Name"] <- tempJSON[["teams"]][["name"]][[2]]
    team2Acro <- tempJSON[["teams"]][["acronym"]][[2]]
    tempMatchDF[1:numGames, "team2Acro"] <- team2Acro
    
    ## Store player list for each team
    team1Players <- tempJSON[["teams"]][["players"]][[1]]
    team2Players <- tempJSON[["teams"]][["players"]][[2]]
    
    ## Cycle through each player loading information
    for (l in 1:length(tempJSON[["players"]][["id"]])) {
      
      ## Load basic player info
      playerID <- tempJSON[["players"]][["id"]][[l]]
      tempPlayerDF[l, "playerID"] <- playerID
      tempPlayerDF[l, "playerSlug"] <- tempJSON[["players"]][["slug"]][[l]]
      playerName <- tempJSON[["players"]][["name"]][[l]]
      tempPlayerDF[l, "playerName"] <- playerName
      tempPlayerDF[l, "playerFirstName"] <- tempJSON[["players"]][["firstName"]][[l]]
      tempPlayerDF[l, "playerLastName"] <- tempJSON[["players"]][["lastName"]][[l]]
      tempPlayerDF[l, "playerRole"] <- tempJSON[["players"]][["roleSlug"]][[l]]

      ## Check for team assignment then load
      if (playerID %in% team1Players) {
        tempPlayerDF[l, "teamID"] <- team1ID
        tempPlayerDF[l, "playerComboName"] <- paste(team1Acro, playerName)
      } else {
        tempPlayerDF[l, "teamID"] <- team2ID
        tempPlayerDF[l, "playerComboName"] <- paste(team2Acro, playerName)
      }      
      
    }
    
    ## Bind temp DFs on full DFs
    gameHashDF <- rbind(gameHashDF, tempMatchDF)
    playerDF <- rbind(playerDF, tempPlayerDF)
    
    print(i)
    
    
}

## Join gameHashDF and games by gameID
gamesPlayed <- left_join(gamesPlayed, gameHashDF,
                         by = c("gameID" = "id"))

## Strip duplicate player information
playerDF <- distinct(playerDF)


## Save files
save(gamesPlayed, file="gamesPlayed.Rda")
save(playerDF, file="playerDF.Rda")