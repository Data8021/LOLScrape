library(XML)
library(rvest)

u <- "http://lol.esportspedia.com/wiki/2015_EU_Challenger_Series/Summer_Playoffs/Match_History"

matchSession <- read_html(u)
save(matchSession, file="matchSession.Rda")

fullTable <- matchSession %>%
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[4]")

winner <- matchSession %>%
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[4]/tr[2]") %>%
  html_attr("style")

mhDate <- matchSession %>%
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[4]/tr[2]/td[1]") %>%
  html_text() %>%
  gsub('[\n]', '', .)

blueTeam <- matchSession %>%
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[4]/tr[2]/td[3]/a") %>%
  html_attr("title")

redTeam <- matchSession %>%
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[4]/tr[2]/td[4]/a") %>%
  html_attr("title")

blueBans1 <- matchSession %>%
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[4]/tr[2]/td[5]/a[1]") %>%
  html_attr("title")