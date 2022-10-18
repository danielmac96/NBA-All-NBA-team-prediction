library(rvest)
library(dplyr)
Season <- seq(0,22)
Data <- data.frame()

#ALL NBA TEAMS
AllNBATeams <- read_html("https://www.basketball-reference.com/awards/all_league.html")
AllNBATeams <- AllNBATeams %>%
  html_nodes("#awards_all_league") %>%
  html_table() 
AllNBATeams <- AllNBATeams[[1]]
names(AllNBATeams) <- c("Season", "Lg","Team","Voting","C","PF","SF","SG","PG")
AllNBATeams <- AllNBATeams %>% select(-Voting)

AllNBATeams <- AllNBATeams %>% 
  mutate(C = substr(C,1,nchar(C)-2),
         PF = substr(PF,1,nchar(PF)-2),
         SF = substr(SF,1,nchar(SF)-2),
         SG = substr(SG,1,nchar(SG)-2),
         PG = substr(PG,1,nchar(PG)-2),
         Season = as.numeric(substr(Season, nchar(Season)-1,nchar(Season))))
AllNBATeams <- as.data.frame(lapply(AllNBATeams, iconv,from='UTF-8',to = 'ASCII//TRANSLIT'))
AllNBATeams <- AllNBATeams %>% mutate_all(as.character)

#ALL STAR SELECTIONS
ASData <- data.frame()
for (i in Season){
  if(nchar(i) > 1){
    AS <- read_html(paste0("https://basketball.realgm.com/nba/allstar/game/rosters/20",i))
  } else {
    AS <- read_html(paste0("https://basketball.realgm.com/nba/allstar/game/rosters/200",i))
  }
  AS1 <- AS %>%
    html_nodes(".compact") %>%
    html_table() 
  AS2 <- AS %>%
    html_nodes(".compact~ .compact") %>%
    html_table() 
  AS <- rbind(AS1[[1]], AS2[[1]])
  AS <- AS %>% mutate(Season = i)
  ASData <- rbind(AS, ASData)
  print(ifelse(nchar(i) > 1,
               paste0("20",i, " Season All Stars Added"),
               paste0("200",i, " Season All Stars Added")))
}

#NBA PLAYER STATS PER SEASON
for (i in Season){
  if(nchar(i) > 1){
    NBA <- read_html(paste0("https://www.basketball-reference.com/leagues/NBA_20",i,"_per_game.html"))
  } else {
    NBA <- read_html(paste0("https://www.basketball-reference.com/leagues/NBA_200",i,"_per_game.html"))
  }
  NBA <- NBA %>%
    html_nodes("#per_game_stats") %>%
    html_table() 
  NBA <- NBA[[1]]
  NBA <- NBA %>% distinct(Player, .keep_all = TRUE) %>% mutate(Season = i) %>% filter(Rk != "Rk")
  NBA$Player <- str_replace(NBA$Player, '\\*', '')
  NBA$Player <- iconv(NBA$Player, from='UTF-8',to = 'ASCII//TRANSLIT')
  AS <- ASData %>% filter(Season == i)
  AllNBATeams2 <- AllNBATeams %>% filter(Season == i)
  First <- AllNBATeams2 %>% filter(Team=="1st")
  Second <- AllNBATeams2 %>% filter(Team=="2nd")
  Third <- AllNBATeams2 %>% filter(Team=="3rd")
  NBA <- NBA %>% mutate(FirstTm = ifelse(Player %in% First,1,0),
                        SecondTm = ifelse(Player %in% Second,1,0),
                        ThirdTm = ifelse(Player %in% Third,1,0),
                        AnyTm = ifelse(FirstTm+SecondTm+ThirdTm > 0,1,0),
                        AllStar = ifelse(Player %in% AS$Player,1,0))
  Data <- rbind(Data, NBA)
  print(ifelse(nchar(i) > 1,
               paste0("20",i, " Season Player Stats Added"),
               paste0("200",i, " Season Player Stats Added")))
}
Data_Clean <- Data %>%
  select(-c(Rk, Player, Tm)) %>%
  mutate(AnyTm = as.factor(AnyTm),AllStar = as.factor(AllStar),Position = as.factor(ifelse(substr(Pos,1,1)=="C","C",substr(Pos,1,2)))) %>%
  select(-Pos) %>%
  mutate_if(is.character, as.numeric) %>%
  replace(is.na(.),0) %>%
  rename_all(funs(str_replace_all(., "%", "Percentage"))) %>%
  rename_all(funs(str_replace_all(., "3", "Three"))) %>%
  rename_all(funs(str_replace_all(., "2", "Two")))

Data_Clean <- cbind(Data[,c(2,3,5)], Data_Clean)

Data_Clean <- Data_Clean %>% select(-Pos) %>%
  mutate(Position = case_when(Position == "SF"~"F",
                              Position == "PF"~"F",
                              Position == "SG"~"G",
                              Position == "PG"~"G",
                              TRUE ~ "C")) 
write.csv(Data_Clean, "C:\\Users\\danie\\Documents\\NBA All-NBA team prediction\\Data_Clean.csv",row.names = FALSE)
