library(XML)
library(rvest)
library(dplyr)

## Read in list of tournament match pages and links
tournamentMH <- read.csv("tournamentMatch.csv", stringsAsFactors = FALSE)

## Initialize df for tournament data
tournamentData <- data.frame(matrix(ncol=34, nrow=0))
colnames(tournamentData) <- c("tournamentName", "date", "blueTeam", "redTeam",
                                "winner", "blueBan1", "blueBan2", "blueBan3",
                                "redBan1", "redBan2", "redBan3", "bluePick1",
                                "bluePick2", "bluePick3", "bluePick4", "bluePick5",
                                "redPick1", "redPick2", "redPick3", "redPick4", "redPick5",
                                "length", "blueGold", "blueKills", "blueTowers",
                                "blueDragons", "blueBarons", "redGold", "redKills",
                                "redTowers", "redDragons", "redBarons", "sbLink",
                                "mhLink")

## Loop through links in tournamentMH
for (i in 1:nrow(tournamentMH)) {
        
    ## Store tournament name and link/url
    tournName <- tournamentMH[i, 1]
    u <- tournamentMH[i, 2]
    
    ## Read in the webpage
    tournSession <- read_html(u)
    
    ## render whole table to get size
    fullTable <- tournSession %>%
      html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[4]") %>%
        html_table()
    fullTable <- fullTable[[1]]
    
    ## create temporary df for game data
    tempDF <- data.frame(matrix(ncol=34, nrow=0))
    colnames(tempDF) <- c("tournamentName", "date", "blueTeam", "redTeam",
                                  "winner", "blueBan1", "blueBan2", "blueBan3",
                                  "redBan1", "redBan2", "redBan3", "bluePick1",
                                  "bluePick2", "bluePick3", "bluePick4", "bluePick5",
                                  "redPick1", "redPick2", "redPick3", "redPick4", "redPick5",
                                  "length", "blueGold", "blueKills", "blueTowers",
                                  "blueDragons", "blueBarons", "redGold", "redKills",
                                  "redTowers", "redDragons", "redBarons", "sbLink",
                                  "mhLink")
    tempDF[,2] <- as.Date(tempDF[,2])
    
    ## Loop through each element of table
    for (l in 1:nrow(fullTable)) {
        
        tempDF[l, 1] <- tournName
                
        tempDF[l, 2] <- tournSession %>%
          html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[4]/tr[2]/td[1]") %>%
          html_text() %>%
          gsub('[\n]', '', .) %>%
          as.Date()
        
        blueTeam <- tournSession %>%
          html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[4]/tr[2]/td[3]/a") %>%
          html_attr("title")
        tempDF[l, 3] <- blueTeam
        
        redTeam <- tournSession %>%
          html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[4]/tr[2]/td[4]/a") %>%
          html_attr("title")
        tempDF[l, 4] <- redTeam
        
        ## Determine winner's color
        winner <- tournSession %>%
            html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[4]/tr[2]") %>%
            html_attr("style")
        
            ## Determine winner
            if (winner == "background-color:#9bd6ff") {
                tempDF[l, 5] <- blueTeam 
            } else {
                tempDF[l, 5] <- redTeam
            }
        
            ## Determine if any bans
        blueBans1 <- matchSession %>%
          html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[4]/tr[2]/td[5]/a[1]") %>%
          html_attr("title")
        
    }
    

}