setwd("~/NBA All-NBA team prediction")
### Library ###
library(rvest)
library(dplyr)
library(ggplot2)
library(stringr)
### Data Collection and Cleaning ###
Season <- seq(10,21)
Data <- data.frame()

AllNBATeams <- read_html("https://www.basketball-reference.com/awards/all_league.html")
AllNBATeams <- AllNBATeams %>%
  html_nodes("#awards_all_league") %>%
  html_table() 
AllNBATeams <- AllNBATeams[[1]]
names(AllNBATeams) <- c("Season", "Lg","Team","C","PF","SF","SG","PG")

AllNBATeams <- AllNBATeams %>% 
  mutate(C = substr(C,1,nchar(C)-2),
         PF = substr(PF,1,nchar(PF)-2),
         SF = substr(SF,1,nchar(SF)-2),
         SG = substr(SG,1,nchar(SG)-2),
         PG = substr(PG,1,nchar(PG)-2),
         Season = substr(Season, nchar(Season)-1,nchar(Season)))

for (i in Season){
  NBA <- read_html(paste0("https://www.basketball-reference.com/leagues/NBA_20",i,"_per_game.html"))
  NBA <- NBA %>%
    html_nodes("#per_game_stats") %>%
    html_table() 
  NBA <- NBA[[1]]
  NBA <- NBA %>% distinct(Player, .keep_all = TRUE) %>% mutate(Season = i) %>% filter(Rk != "Rk")
  NBA$Player <- str_replace(NBA$Player, '\\*', '')
  NBA <- NBA %>% mutate(FirstTm = ifelse(Player %in% (AllNBATeams %>% filter(Season == i, Team == "1st")),1,0),
                        SecondTm = ifelse(Player %in% (AllNBATeams %>% filter(Season == i, Team == "2nd")),1,0),
                        ThirdTm = ifelse(Player %in% (AllNBATeams %>% filter(Season == i, Team == "3rd")),1,0),
                        AnyTm = ifelse(FirstTm+SecondTm+ThirdTm > 0,1,0))
  Data <- rbind(Data, NBA)
  print(paste0("Year 20",i," added"))
}
Data_Clean <- Data %>%
  select(-c(Rk,Player, Pos, Tm)) %>% 
  mutate(AnyTm = as.factor(AnyTm)) %>%
  mutate_if(is.character, as.numeric) %>%
  replace(is.na(.),0) %>%
  rename_all(funs(str_replace_all(., "%", "Percentage"))) %>%
  rename_all(funs(str_replace_all(., "3", "Three"))) %>%
  rename_all(funs(str_replace_all(., "2", "Two")))

Data_Clean <- cbind(Data[,c(2,3,5)], Data_Clean)

summary(Data_Clean)
#View(Data_Clean)
Data_Clean %>% filter(AnyTm == 1) %>% group_by(Season) %>% summarize(yes = sum(AnyTm==1), m = mean(GS), low = min(GS), high=max(GS))

### Data exploration ###

AllNBA <- Data_Clean %>% 
  filter(GS > 40) %>%
  select(-c(Player, Pos, Tm, Season, FirstTm, SecondTm, ThirdTm))

AllNBA_Summary <- AllNBA %>% 
  group_by(AnyTm) %>% 
  summarize(Tot = n(),
            Proportion = n()/nrow(AllNBA),
            PPG = mean(PTS),
            FG = mean(FGPercentage),
            RPG = mean(TRB),
            APG = mean(AST))
AllNBA_Summary

ShootingPlot <- ggplot(AllNBA, aes(x= FGPercentage, y = PTS, color = AnyTm)) + geom_point()
ShootingPlot

#Normalize based on season to get sense of rank within leage at the time to increase accuracy at the sacrifice of readability?


#PCA analysis may not use

AllNBA_predictors <- AllNBA %>% select(-c(AnyTm))
AllNBA_pca <- prcomp(AllNBA_predictors, center = TRUE, scale. = TRUE)

summary(AllNBA_pca)
print(AllNBA_pca)
prop_var <- tibble(sdev = AllNBA_pca$sdev)

prop_var <- prop_var %>%
  mutate(pca_comp = 1:n(),pcVar = sdev^2, propVar_ex = pcVar/sum(pcVar), pca_comp = pca_comp)

#ggplot(data= prop_var, aes(y=prop_var$pcVar, x=1:26)) + geom_line()


#ggplot(prop_var, aes(pca_comp, propVar_ex, group =1)) + geom_line() + geom_point()

#Random forest to find important variables

set.seed(1)

train_rows <- sample(nrow(AllNBA), 0.7*nrow(AllNBA))

rf_train <- AllNBA[train_rows,]
rf_test <- AllNBA[-train_rows,]

library(randomForest)

rf <- randomForest(AnyTm ~., data=rf_train, importance = TRUE)

print(rf)

varImpPlot(rf)

rf_imp <- as.data.frame(importance(rf))
rf_imp <- rf_imp[order(rf_imp$MeanDecreaseGini, decreasing = TRUE),]
row.names(rf_imp)

ggplot(rf_imp, aes(x=1:nrow(rf_imp)))+ 
  geom_line(aes(y=MeanDecreaseGini),col = 'Blue') + 
  geom_point(aes(y=MeanDecreaseGini),col = 'Blue') + 
  geom_line(aes(y=MeanDecreaseAccuracy),col = 'Red') +
  geom_point(aes(y=MeanDecreaseAccuracy),col = 'Red') +
  scale_x_continuous(breaks = 1:nrow(rf_imp), labels = row.names(rf_imp)) +
  theme(axis.text.x = element_text(angle = 90))

predict(rf, rf_test)

rf_pred <- cbind(rf_test, prediction_class = predict(rf, rf_test, type= "class"), prediciton_prob = predict(rf, rf_test, type= "prob"))

table(Truth = rf_pred$AnyTm,Prediction = rf_pred$prediction_class)

(423+33)/492 #acc
423/(10+423) #recall
423/(26+423) #precision

library(pROC)
rf.roc<-roc(as.numeric(rf_pred$AnyTm),as.numeric(rf_pred$prediction_class))
plot(rf.roc)
auc(rf.roc)








