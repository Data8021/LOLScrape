library(jsonlite)
library(dplyr)
library(lubridate)

dups <- data.frame(x, dup="YES", stringsAsFactors = FALSE)
row.names(dups) <- NULL
whichDups <- left_join(matchHistoryLinks, dups, by = c("gameHash" = "x")) %>%
  filter(dup == "YES") %>%
  arrange(gameHash)

gameLength <- data.frame(gameHash = character(),
                         length = numeric(),
                         stringsAsFactors = FALSE)

## Loop through game to download the game stats
for (i in 1:nrow(consMH)) {
  
  tempJSON <- fromJSON(paste0("https://acs.leagueoflegends.com/v1/stats/game/",
                                       consMH[i, "gameRealm"], "/", consMH[i, "gameCode"], "?gameHash=", 
                                       consMH[i, "gameHash"]))
  
  gameLength[i, 1] <- consMH[i, "gameHash"]
  gameLength[i, 2] <- tempJSON$gameDuration
  print(i)
  
  
}

td <- seconds_to_period(2224)
sprintf('%02d:%02d:%02d', td@hour, minute(td), second(td))


