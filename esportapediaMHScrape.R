library(rvest)
library(dplyr)
library(BDWPkg)

## Load string splitting function


## Read in list of tournament match pages and links
tournamentMH <- read.csv("tournamentMatch.csv", stringsAsFactors = FALSE)

## Remove tournaments with mostly missing information
tournamentMH <- tournamentMH[-12,]

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
    
    ## Determine which table # is correct
    for (j in 1:10) {
      
      dateTest <- tournSession %>%
        html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",j,"]/tr[1]/th[1]")) %>%
        html_text()
      
      if ((length(dateTest) != 0) && (dateTest == " Date ")) {
        tableNum <- j
      }
    }
    
    ## render whole table to get size
    fullTable <- tournSession %>%
      html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]")) %>%
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
    tempDF[, 2] <- as.Date(tempDF[, 2])
    tempDF[, 22] <- as.numeric(tempDF[, 22])
    tempDF[, 24] <- as.numeric(tempDF[, 24])
    tempDF[, 25] <- as.numeric(tempDF[, 25])
    tempDF[, 26] <- as.numeric(tempDF[, 26])
    tempDF[, 27] <- as.numeric(tempDF[, 27])
    tempDF[, 29] <- as.numeric(tempDF[, 29])
    tempDF[, 30] <- as.numeric(tempDF[, 30])
    tempDF[, 31] <- as.numeric(tempDF[, 31])
    tempDF[, 32] <- as.numeric(tempDF[, 32])
    
    ## Loop through each element of table
    for (l in 1:nrow(fullTable)) {
        
        ## Tournament name
        tempDF[l, 1] <- tournName
        
        ## Game date        
        tempDF[l, 2] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[1]")) %>%
          html_text() %>%
          gsub('[\n]', '', .) %>%
          as.Date()
        
        ## Team names
        blueTeam <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[3]/a")) %>%
          html_attr("title")
        tempDF[l, 3] <- blueTeam
        
        redTeam <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[4]/a")) %>%
          html_attr("title")
        tempDF[l, 4] <- redTeam
        
        ## Determine winner's color
        winner <- tournSession %>%
            html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]")) %>%
            html_attr("style")
        
        ## Determine winner
        if (winner == "background-color:#9bd6ff") {
          tempDF[l, 5] <- blueTeam 
        } else {
          if (winner == "background-color:#ffcccc") {
            tempDF[l, 5] <- redTeam
          }
        }
        
        ## Determine if blue banned
        banTest <- tournSession %>%
                 html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[5]/a[1]")) %>%
                 html_attr("title")

        if (length(banTest)!=0L) {
          
          tempDF[l, 6] <- banTest
        
        }
        
        banTest <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[5]/a[2]")) %>%
          html_attr("title")
        
        if (length(banTest)!=0L) {
          
          tempDF[l, 7] <- banTest
          
        }
        
        banTest <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[5]/a[3]")) %>%
          html_attr("title")
        
        if (length(banTest)!=0L) {
          
          tempDF[l, 8] <- banTest
          
        }
        
        ## Determine if red banned
        banTest <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[6]/a[1]")) %>%
          html_attr("title")
        
        if (length(banTest)!=0L) {
          
          tempDF[l, 9] <- banTest  
          
        }
        
        banTest <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[6]/a[2]")) %>%
          html_attr("title")
        
        if (length(banTest)!=0L) {
          
          tempDF[l, 10] <- banTest  
          
        }
        
        banTest <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[6]/a[3]")) %>%
          html_attr("title")
        
        if (length(banTest)!=0L) {
          
          tempDF[l, 11] <- banTest  
          
        }
        
        ## Blue picks
        tempDF[l, 12] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[7]/a[1]")) %>%
          html_attr("title")
        
        tempDF[l, 13] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[7]/a[2]")) %>%
          html_attr("title")  
        
        tempDF[l, 14] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[7]/a[3]")) %>%
          html_attr("title")  
        
        tempDF[l, 15] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[7]/a[4]")) %>%
          html_attr("title")  
        
        tempDF[l, 16] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[7]/a[5]")) %>%
          html_attr("title")  
        
        ## Red picks
        tempDF[l, 17] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[8]/a[1]")) %>%
          html_attr("title")  
        
        tempDF[l, 18] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[8]/a[2]")) %>%
          html_attr("title")  
        
        tempDF[l, 19] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[8]/a[3]")) %>%
          html_attr("title")  
        
        tempDF[l, 20] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[8]/a[4]")) %>%
          html_attr("title")  
        
        tempDF[l, 21] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[8]/a[5]")) %>%
          html_attr("title")  
        
        ## Game length
        temp <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[9]")) %>%
          html_text() %>%
          gsub('[\n]', '', .)
          
          ## Convert ot seconds and assign, if exists
          if (temp != "") {
              tempDF[l, 22] <- toSeconds(temp)
          }
        
        ## Blue gold/kills/towers/dragons/barons
        tempDF[l, 23] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[10]")) %>%
          html_text() %>%
          gsub('[\n]', '', .)
        
        tempDF[l, 24] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[11]")) %>%
          html_text() %>%
          gsub('[\n]', '', .) %>%
          as.numeric()
        
        tempDF[l, 25] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[12]")) %>%
          html_text() %>%
          gsub('[\n]', '', .) %>%
          as.numeric()
        
        tempDF[l, 26] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[13]")) %>%
          html_text() %>%
          gsub('[\n]', '', .) %>%
          as.numeric()
        
        tempDF[l, 27] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[14]")) %>%
          html_text() %>%
          gsub('[\n]', '', .) %>%
          as.numeric()

        ## Red gold/kills/towers/dragons/barons
        tempDF[l, 28] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[15]")) %>%
          html_text() %>%
          gsub('[\n]', '', .)
        
        tempDF[l, 29] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[16]")) %>%
          html_text() %>%
          gsub('[\n]', '', .) %>%
          as.numeric()
        
        tempDF[l, 30] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[17]")) %>%
          html_text() %>%
          gsub('[\n]', '', .) %>%
          as.numeric()
        
        tempDF[l, 31] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[18]")) %>%
          html_text() %>%
          gsub('[\n]', '', .) %>%
          as.numeric()
        
        tempDF[l, 32] <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[19]")) %>%
          html_text() %>%
          gsub('[\n]', '', .) %>%
          as.numeric()
        
        ## Determine if scoreboard link exists
        linkTest <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[20]/a")) %>%
          html_attr("href")
        
        if (length(linkTest)!=0L) {
          
          ## Scoreboard link
          tempDF[l, 33] <- tournSession %>%
            html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[20]/a")) %>%
            html_attr("href")  
        
        }
        
        ## Determine if match history link exists
        linkTest <- tournSession %>%
          html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[21]/a")) %>%
          html_attr("href")
        
        if (length(linkTest)!=0L) {
          
          ## Scoreboard link
          tempDF[l, 34] <- tournSession %>%
            html_nodes(xpath=paste0("/html/body/div[3]/div[3]/div[4]/table[",tableNum,"]/tr[",l+1,"]/td[21]/a")) %>%
            html_attr("href")  
          
        }
        
        
        
    }
    
    ## bind the new games into the tournament DF
    tournamentData <- rbind(tournamentData, tempDF)  
    
}

## Save output
save(tournamentData, file="lolesportapediaMH.Rda")

## Strip out missing match history links
matchHistoryLinks <- tournamentData[!is.na(tournamentData$mhLink),]

## Strip non-match history links
matchHistoryLinks <- matchHistoryLinks[grep("matchhistory.na.leagueoflegends.com", matchHistoryLinks$mhLink), ]

## Decompose match history link into key elements
matchHistoryLinks <- mutate(matchHistoryLinks,
      gameRealm=sapply(matchHistoryLinks$mhLink,
            function(x) (strsplit(x, split="/", fixed=TRUE)[[1]][6])))

matchHistoryLinks <- mutate(matchHistoryLinks,
      gameCode=sapply(matchHistoryLinks$mhLink,
            function(x) (strsplit(strsplit(x, split="/", fixed=TRUE)[[1]][7], split="\\?")[[1]][1])))

matchHistoryLinks <- mutate(matchHistoryLinks,
      gameHash=sapply(matchHistoryLinks$mhLink,
                      function(x) {
                          x <- strsplit(x, split="/", fixed=TRUE)[[1]][7]
                          x <- strsplit(x, split="\\?")[[1]][2]
                          x <- gsub("^.*?=","",x)
                          x <- gsub("\\&.*","",x)    
                      }))

## Save output
save(matchHistoryLinks, file="esportapediaMHLinks.Rda")
