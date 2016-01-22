library(jsonlite)
## START HERE

## Identify leagues to scrape schedules of
leagues <- c(1, 2, 3, 6, 7, 8, 9, 12, 14)
                                    
## Initialize list to hold schedules
scheduleList <- vector("list", length(leagues))
names(scheduleList) <- leagues

## Loop through schedules
for (i in 1:length(leagues)) {
    
    scheduleList[[i]] <- fromJSON(paste0("http://api.lolesports.com/api/v1/scheduleItems?leagueId=", leagues[i]))

}

## Save raw schedule files
save(scheduleList, file="scheduleList.Rda")
