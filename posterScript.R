#I ran into some potential issues with random effects this allows for the entire script to run on the same seed
set.seed(1163)

library(knitr)
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE) 
options(scipen=999,digits = 15)

# load packages
library(tidyverse)
library(randomForest)
library(tidymodels)
library(caret)
library(readxl)
library(gt)
library(htmltools)
library(rvest)

#read in downloaded data from kaggle
conferences<-read.csv("MDataFiles_Stage1/Conferences.csv")
Conf_Tourney_Games<-read.csv("MDataFiles_Stage1/MConferenceTourneyGames.csv")
MasseyOrdinals <- read.csv("MDataFiles_Stage1/MMasseyOrdinals.csv")
MNCAATourney_Compact_Results <- read.csv("MDataFiles_Stage1/MNCAATourneyCompactResults.csv")
MNCAATourney_Seed_Round_Slots <- read.csv("MDataFiles_Stage1/MNCAATourneySeedRoundSlots.csv")
MNCAATourney_Seeds <- read.csv("MDataFiles_Stage1/MNCAATourneySeeds.csv")
MNCAATourney_Slots <- read.csv("MDataFiles_Stage1/MNCAATourneySlots.csv")
Regular_Season_Compact_Results <- read.csv("MDataFiles_Stage1/MRegularSeasonCompactResults.csv")
Regular_Season_Detailed_Results <- read.csv("MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")
Seasons <- read.csv("MDataFiles_Stage1/MSeasons.csv")
Team_Conferences <- read.csv("MDataFiles_Stage1/MTeamConferences.csv")
Teams <- read.csv("MDataFiles_Stage1/MTeams.csv")
Names <- read.csv("MDataFiles_Stage1/MTeamSpellings.csv")

#makes 2 instances of each game in a data set, data is only one instance/game and 
#organized by winning team vs losing team

#will make training model easier and better

make_Binary_WL <- function(df, length) {
  temp_data <- bind_cols(
    df[c(1:2, 7:length)],
    "TeamID" = df$LTeamID,
    "OppTeamID" = df$WTeamID,
    "Score" = df$LScore,
    "OppScore" = df$WScore,
  )
  
  
  df <- df %>%
    mutate(
      TeamID = WTeamID,
      OppTeamID = LTeamID,
      Score = WScore,
      OppScore = LScore
    ) %>%
    select(-c(WTeamID, LTeamID, WScore, LScore))
  
  df <- df %>%
    bind_rows(temp_data) %>%
    mutate(teamWin = if_else(Score > OppScore, 1, 0))
  return(df)
}

MNCAATourney_Compact_Results <-
  make_Binary_WL(MNCAATourney_Compact_Results, 8)
Regular_Season_Compact_Results <-
  make_Binary_WL(Regular_Season_Compact_Results, 8)
Regular_Season_Detailed_Results <-
  make_Binary_WL(Regular_Season_Detailed_Results, 34)

#calculate ft percentages
ftPer_df <- Regular_Season_Detailed_Results %>%
  group_by(Season, TeamID) %>%
  summarise(fta = if_else(teamWin == 1, WFTA, LFTA),
            ftm = if_else(teamWin == 1, WFTM, LFTM)) %>%
  ungroup() %>%
  group_by(Season, TeamID) %>%
  summarise(
    ftm = sum(ftm),
    fta = sum(fta),
    ftper = ftm / fta,
    games = n()
  ) %>%
  ungroup()

#isolate final day AP rankings
AP_rankings <- filter(MasseyOrdinals, SystemName == "AP") %>%
  #keeping only the final rankings
  group_by(Season) %>%
  filter(RankingDayNum == max(RankingDayNum)) %>%
  ungroup()

#scrape data from torvik.com
getTorvikYearData <- function(year) {
  require(XML)
  require(rvest)
  require(stringr)
  require(dplyr)
  theUrl <-
    paste0(
      "https://barttorvik.com/trank.php?year=",
      as.character(year),
      "&sort=&hteam=&t2value=&conlimit=All&state=All&begin=",
      as.character(year-1),
      "1101&end=",
      as.character(year),
      "0501&top=0&revquad=0&quad=5&venue=All&type=R&mingames=0#"
    )
  page <- read_html(theUrl)
  tables <- page %>% html_nodes("table") %>% html_table()
  data <- as.data.frame(tables[1])
  
  colnames(data) <-
    c(
      "Rank",
      "Team",
      "Conf",
      "Game",
      "Record",
      "AdjO",
      "AdjD",
      "Barthag",
      "EffO",
      "EffD",
      "TopO",
      "TopD",
      "OrbP",
      "DrbP",
      "FtRO",
      "FtRD",
      "2pO",
      "2pD",
      "3pO",
      "3pD",
      "3PR",
      "3PRD",
      "ADJT",
      "WAB"
    )
  
  data <-
    data %>% filter(nchar(as.character(Rank)) > 0) # Remove empty rank rows.
  data$Year = year
  data<-data[2:nrow(data),]
  
  return(data)
}
torvikData <- tibble()

for (year in 2008:2022)
{
  torvikYear <- getTorvikYearData(year)
  torvikData <- rbind(torvikData, torvikYear)
}
rm(torvikYear)



#clean data
torvikDataTourney <- torvikData %>%
  mutate(Seed = str_extract(Team,"\\d+"),
         Team = str_extract(Team,"\\D+"),
         Team = str_to_lower(Team),
         Team = str_squish(Team))%>%
  filter(is.na(Seed) == F)%>%
  left_join(Names, by = c("Team" = "TeamNameSpelling"))%>%
  mutate(
    TeamID = case_when(
      Team == "louisiana lafayette" ~ 1418,
      Team == "texas a&m corpus chris" ~ 1394,
      Team == "mississippi valley st." ~ 1290,
      Team == "arkansas pine bluff" ~ 1115,
      Team == "cal st. bakersfield" ~ 1167,
      T ~ as.double(TeamID)
    )
  )


#join with kaggle sets to get predictor df
predictor_Torvik <- MNCAATourney_Compact_Results %>%
  mutate(round = case_when(between(DayNum,134,135) ~ 0,
                           between(DayNum,136,137) ~ 1,
                           between(DayNum,138,140) ~ 2,
                           between(DayNum,143,144) ~ 3,#2021 had 3 diff days, betweenrounds 2 and 4
                           between(DayNum,145,148) ~ 4,
                           DayNum == 152 ~ 5,
                           DayNum == 154 ~ 6)
  ) %>%
  filter(Season >= 2008) %>% #might change 03
  #add team KP data
  #must be done twice for each instance of a game
  left_join(torvikDataTourney, by = c("TeamID" = "TeamID", "Season" = "Year")) %>%
  left_join(AP_rankings, by = c("TeamID" = "TeamID", "Season" = "Season")) %>%
  left_join(ftPer_df,  by = c("TeamID" = "TeamID", "Season" = "Season")) %>%
  left_join(torvikDataTourney,
            by = c("OppTeamID" = "TeamID", "Season" = "Year")) %>%
  left_join(AP_rankings, by = c("OppTeamID" = "TeamID", "Season" = "Season")) %>%
  left_join(ftPer_df,  by = c("OppTeamID" = "TeamID", "Season" = "Season")) %>%
  #.x comes before .y so .y is relabeled as opponent, eliminates viewing them by winning or losing team
  rename_with(.fn = ~ gsub(".x", "", .x, fixed = T),
              .cols = ends_with(".x")) %>%
  rename_with(.fn = ~ gsub(".y", "_opp", .x, fixed = T),
              .cols = ends_with(".y")) %>%
  #remove lower id and so each game has 1 instance, with higher id first
  #random choice keeps the teamWin var about split 50/50 btwn 1 and 0
  filter(TeamID<OppTeamID) %>% 
  select(-ends_with("_R"),-ends_with("_R_opp")) %>%
  mutate(OrdinalRank=if_else(is.na(OrdinalRank),as.integer(100),OrdinalRank),
         OrdinalRank_opp=if_else(is.na(OrdinalRank_opp),as.integer(100),OrdinalRank_opp),
         isRanked = if_else(OrdinalRank!=100,1,0),
         isRanked_opp = if_else(OrdinalRank_opp!=100,1,0),
         is1v16 = if_else((Seed == 1 & Seed_opp ==16)|(Seed == 16 & Seed_opp == 1),1,0)
  )%>%
  select(
    -c(DayNum,WLoc,NumOT,Score,OppScore,Team,Conf,Record,RankingDayNum,SystemName,Team_opp,Conf_opp,
       Record_opp,RankingDayNum_opp,SystemName_opp)
  ) %>%
  map_at(1:62,as.numeric)%>%
  as_tibble()
summary(predictor_Torvik)
#ranks mess up whole numbers
#torvik data scraping added their ranks to the end of the value, while not an issue for decimals as its only adding
#a few hundredths, some values where whole integers, and when ranked 20 w/ a value of 8, where scraped as 820.
#the solution below isn't perfect, but kept the values in the correct range and close to what they should be.
predictor_Torvik<-predictor_Torvik%>%
  #Bart addded 3pr after I essentially finished training my models, hence it early removal
  select(-c(`3PR`,`3PRD`))%>%
  mutate_at(c(11:23,36:48),~case_when(between(.x,100,999)~.x/10,
                                      between(.x,1000,9999)~.x/100,
                                      .x>=10000~.x/1000,
                                      T~.x
  )
  )%>%
  #wab has a different range than the other vars
  mutate_at(c("WAB","WAB_opp"),~case_when(.x>100~.x/100,
                                          between(.x,30,99)~.x/10,
                                          between(.x,-99,-30)~.x/10,
                                          between(.x,-999,-100)~.x/100,
                                          between(.x,-9999,-1000)~.x/1000,
                                          between(.x,-99999,-10000)~.x/10000,
                                          T~.x
  )
  )%>%
  #adjem similar to kenpom
  mutate(AdjEMTorv = AdjO - AdjD,
         AdjEMTorv_opp = AdjO_opp - AdjD_opp)
#confirm the values aree distributed as expected
summary(predictor_Torvik)
sum(predictor_Torvik$teamWin)/nrow(predictor_Torvik)

#take differences to decrease covariance and better capture differences between 2 teams
diff_Torvik <- predictor_Torvik %>%
  mutate(
    rank_diff = Rank - Rank_opp,
    game_diff = Game - Game_opp,
    AdjO_diff = AdjO - AdjO_opp,
    AdjD_diff = AdjD - AdjD_opp,
    Barthag_diff = Barthag - Barthag_opp,
    EffO_diff = EffO - EffO_opp,
    EffD_diff = EffD - EffD_opp,
    ToPO_diff = TopO-TopO_opp,
    ToPD_diff = TopD - TopD_opp,
    OrbP_diff = OrbP - OrbP_opp,
    DrbP_diff = DrbP - DrbP_opp,
    Seed_diff = (Seed - Seed_opp),
    AP_rank_diff = OrdinalRank - OrdinalRank_opp,
    ftper_diff = ftper- ftper_opp,
    ftrO_diff = FtRO - FtRO_opp,
    ftrD_diff = FtRD -FtRD_opp,
    `p2O_diff` = `2pO`- `2pO_opp`,
    `p2D_diff` = `2pD` - `2pD_opp`,
    `p3O_diff` = `3pO` - `3pO_opp`,
    `p3D_diff` = `3pD` - `3pD_opp`,
    AdjT_diff = ADJT - ADJT_opp,
    WAB_diff = WAB - WAB_opp,
    AdjEMTorv_diff = AdjEMTorv - AdjEMTorv_opp,
    TO_differential = TopD - TopO,
    TO_differential_opp = TopD_opp - TopO_opp,
    TO_differential_diff = TO_differential-TO_differential_opp
  ) %>%
  select(c(Season,TeamID,OppTeamID,teamWin,round, ends_with("_diff"),is1v16))
normalPreds.diffTorvik<-diff_Torvik%>% map_at(6:29, ~(scale(.) %>% as.vector))%>%as_tibble()

#function to return MSE and RMSE for my models
errorCalc <- function(data) {
  
  n=1
  brier = 0
  while (n<=nrow(data)) {
    diff = data$preds[n] - data$teamWin[n]
    diff2 = diff^2
    brier = brier+diff2
    n=n+1
  }
  mse = brier/(n-1)
  rmse = sqrt(mse)
  
  df<-bind_cols(mse,rmse)%>%rename(MSE = ...1, RMSE=...2)
  return(df)
}

#all seasons in df in a list
predSeasonsTorvik = unique(predictor_Torvik$Season)

#cross validation leave 1 season out
cv_results.logDiffTorvik<-map_dfr(predSeasonsTorvik,function(x){
  test<-normalPreds.diffTorvik %>%
    filter(Season == x) %>%
    select(-Season)
  
  train<-normalPreds.diffTorvik %>%
    filter(Season != x) %>%
    select(-Season)

  logit <- glm(teamWin ~ . - round - p2O_diff - game_diff - EffO_diff - EffD_diff - p2D_diff -
                 p3O_diff - p3D_diff - AdjEMTorv_diff - DrbP_diff - ftrD_diff - 
                 AdjT_diff - ftper_diff - is1v16 - AP_rank_diff - rank_diff - ToPO_diff - 
                 ToPD_diff,
               data = train[3:29],
               family = binomial(link = "logit"))
  preds<-predict(logit,test,type='response')
  print(summary(logit))
  
  
  cv_data <- bind_cols(test, preds) %>% mutate(season = x)%>%rename("preds" = ...30)
  return((cv_data))
  #thanks ben again
  
})
cv_results.logDiffTorvik%>%select(Seed_diff,preds,teamWin)%>%arrange(desc(Seed_diff))

cv_results.logDiffTorvik%>% filter(TeamID== 1420) %>%#umbc
  select(Seed_diff,preds,teamWin)%>%arrange(desc(preds))
#look at 1v16
cv_results.logDiffTorvik%>%filter(is1v16==1)%>%arrange(preds)%>%select(preds,teamWin,TeamID,OppTeamID)

confusionMatrix(as.factor(cv_results.logDiffTorvik$teamWin), as.factor(if_else(
  cv_results.logDiffTorvik$preds > .5,1,0)))
#get mse rmse
errorTorvik<-errorCalc(cv_results.logDiffTorvik)

#random forest instead of logit
importances.diffTorv <- tibble()
cv_results.diffTorv<-map_dfr(predSeasonsTorvik,function(x){
  test<-diff_Torvik %>%
    filter(Season == x) %>%
    select(-Season)
  
  train<-diff_Torvik %>%
    filter(Season != x) %>%
    select(-Season)
  
  set.seed(2014)
  
  mirkwood <-
    randomForest::randomForest(teamWin ~ .-round - p2O_diff - game_diff - EffO_diff - EffD_diff - p2D_diff -
                                 p3O_diff - p3D_diff - is1v16 - ftper_diff,
                               ntree = 1500,
                               data = train[3:29],
                               importance = T
    )
  
  preds <- predict(mirkwood, test)
  currentIMP <- as_tibble(mirkwood$importance,rownames = "Type") %>% mutate(season = x)
  importances.diffTorv <- bind_rows(importances.diffTorv,currentIMP)
  
  
  cv_data <- bind_cols(test, preds) %>% mutate(season = x)%>%rename("preds" = ...30)
  return(list(cv_data,importances.diffTorv))
  #thanks ben again
  
})
importances.diffTorv <-
  cv_results.diffTorv %>% filter(is.na(IncNodePurity) == F) %>% select(Type, season,`%IncMSE`,IncNodePurity) %>%
  group_by(Type) %>% summarise(avgIMP = mean(IncNodePurity),
                               avgPer = mean(`%IncMSE`)) %>% arrange(desc(avgIMP))
cv_results.diffTorv <-
  cv_results.diffTorv %>% filter(is.na(IncNodePurity) == T) %>% select(-c(Type,`%IncMSE`,IncNodePurity
  )) %>% 
  tibble()  %>%
  na.omit()


cv_results.diffTorv%>%select(Seed_diff,preds,teamWin)%>%arrange(desc(preds))

cv_results.diffTorv%>% filter(TeamID== 1420) %>%#umbc
  select(Seed_diff,preds,teamWin)%>%arrange(desc(preds))
#look at 1v16
cv_results.diffTorv%>%filter(is1v16==1)%>%arrange(preds)%>%select(preds,teamWin,TeamID,OppTeamID)

confusionMatrix(as.factor(cv_results.diffTorv$teamWin), as.factor(if_else(
  cv_results.diffTorv$preds > .5,1,0)))
#getmse rmse
error.diffTorv<-errorCalc(cv_results.diffTorv)


autoplot(roc_curve(cv_results.logDiffTorvik, as.factor(teamWin), preds) )
autoplot(roc_curve(cv_results.diffTorv, as.factor(teamWin), preds) )

preds.difflogTorv <-bind_cols(cv_results.logDiffTorvik$preds,cv_results.logDiffTorvik$teamWin) %>%
  rename("pred"=...1 , "actual"=...2)

plot6<-preds.difflogTorv %>%
  mutate(bin_pred_prob = round(pred/.1)*.1)%>%
  group_by(bin_pred_prob)%>%
  summarise(n_games = n(),
            n_wins = length(which(actual==1)),
            bin_act_prob = n_wins/n_games,
  )%>%
  ungroup()

ann_text <- data.frame(
  x = c(.25, 0.75), y = c(0.75, 0.25),
  lab = c("More times\nthan expected", "Fewer times\nthan expected")
)

cal_plot6<-plot6 %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_act_prob, size = n_wins)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_act_prob), method = "loess", color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1),) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    size = "Number of games",
    x = "Estimated Win Probability",
    y = "Observed Win Probability",
    title = "Torvik Logistic Regression Model Calibration Plot"
  ) +
  geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 2) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 90),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )
cal_plot6

#confusion matrix if just predict by taking higher seed
NIMatrix<-confusionMatrix(as.factor(predictor_Torvik$teamWin),
                          as.factor(if_else(predictor_Torvik$Seed<predictor_Torvik$Seed_opp,1,0)))
NIMatrix


#get this years data TORVIK
mmarch2023_Torv <- getTorvikYearData(2023)

torvikDataTourney2023 <- mmarch2023_Torv %>%
  select(-c(`3PR`,`3PRD`))%>%
  mutate(Seed = str_extract(Team,"\\d+"),
         Team = str_extract(Team,"\\D+"),
         Team = str_to_lower(Team),
         Team = str_squish(Team))%>%
  filter(is.na(Seed) == F)%>%
  left_join(Names, by = c("Team" = "TeamNameSpelling"))%>%
  mutate(
    TeamID = case_when(
      Team == "louisiana lafayette" ~ 1418,
      Team == "texas a&m corpus chris" ~ 1394,
      Team == "mississippi valley st." ~ 1290,
      Team == "arkansas pine bluff" ~ 1115,
      Team == "cal st. bakersfield" ~ 1167,
      Team == "southeast missouri st." ~ 1369,
      T ~ as.double(TeamID)
    ) 
  ) %>%
  mutate_at(c(1,4,6:25),~as.numeric(.x))%>%
  mutate_at(c(9:21),~case_when(between(.x,100,999)~.x/10,
                               between(.x,1000,9999)~.x/100,
                               .x>=10000~.x/1000,
                               T~.x
  )
  )%>%
  mutate(WAB = case_when(WAB>100~WAB/100,
                         between(WAB,30,99)~WAB/10,
                         between(WAB,-99,-30)~WAB/10,
                         between(WAB,-999,-100)~WAB/100,
                         between(WAB,-9999,-1000)~WAB/1000,
                         between(WAB,-99999,-10000)~WAB/10000,
                         T~WAB
  )
  ) 



dancing2023 <- unique(torvikDataTourney2023$TeamID) #2023 tourney teams
#make game observations for each possible matchup
# 1 indexed
t1<-1 
#68 is number of teams
#2278 total
game1_2023 <- c()
game2_2023 <- c()
while (t1<68) {
  t2 <- t1+1
  while (t2<=68) {
    game1_2023 <- append(game1_2023,dancing2023[t1])
    game2_2023 <- append(game2_2023,dancing2023[t2])
    t2 <- t2 +1
  }
  t1 <- t1 + 1
}

# flip every other team and opp to make each team have similar count of games as team and opp
matchups2023 <- bind_cols(teamID = game1_2023, teamID_opp = game2_2023) %>%
  mutate(rowNum = row_number(),
         NEW_teamId = if_else(rowNum %% 2 == 0,teamID_opp,teamID),
         NEW_teamID_opp = if_else(rowNum %% 2 == 0,teamID,teamID_opp),
         teamID = NEW_teamId,
         teamID_opp = NEW_teamID_opp) %>%  ##this makes it so each team has 33 or 34 observations as team
  select(-starts_with("NEW")) %>%
  left_join(torvikDataTourney2023, by = c("teamID" = "TeamID")) %>%
  left_join(torvikDataTourney2023, by = c("teamID_opp" = "TeamID"))%>%
  rename_with(.fn = ~ gsub(".x", "", .x, fixed = T),
              .cols = ends_with(".x")) %>%
  rename_with(.fn = ~ gsub(".y", "_opp", .x, fixed = T),
              .cols = ends_with(".y")) 

diffmatchups2023 <- matchups2023 %>%
  mutate(
    rank_diff = Rank - Rank_opp,
    game_diff = Game - Game_opp,
    AdjO_diff = AdjO - AdjO_opp,
    AdjD_diff = AdjD - AdjD_opp,
    Barthag_diff = Barthag - Barthag_opp,
    EffO_diff = EffO - EffO_opp,
    EffD_diff = EffD - EffD_opp,
    ToPO_diff = TopO-TopO_opp,
    ToPD_diff = TopD - TopD_opp,
    OrbP_diff = OrbP - OrbP_opp,
    DrbP_diff = DrbP - DrbP_opp,
    Seed_diff = (Seed - Seed_opp),
    ftrO_diff = FtRO - FtRO_opp,
    ftrD_diff = FtRD -FtRD_opp,
    `p2O_diff` = `2pO`- `2pO_opp`,
    `p2D_diff` = `2pD` - `2pD_opp`,
    `p3O_diff` = `3pO` - `3pO_opp`,
    `p3D_diff` = `3pD` - `3pD_opp`,
    AdjT_diff = ADJT - ADJT_opp,
    WAB_diff = WAB - WAB_opp,
    TO_differential = TopD - TopO,
    TO_differential_opp = TopD_opp - TopO_opp,
    TO_differential_diff = TO_differential-TO_differential_opp
  ) %>%
  select(c(teamID,teamID_opp,AdjO_diff, AdjD_diff, 
           Barthag_diff, ToPO_diff, ToPD_diff, OrbP_diff, Seed_diff, ftrO_diff, WAB_diff,TO_differential_diff,
           AdjT_diff))%>%
  left_join(Teams,by = c("teamID" = "TeamID")) %>%
  left_join(Teams, by = c("teamID_opp" = "TeamID")) %>%
  rename_with(.fn = ~ gsub(".x", "", .x, fixed = T),
              .cols = ends_with(".x")) %>%
  rename_with(.fn = ~ gsub(".y", "_opp", .x, fixed = T),
              .cols = ends_with(".y")) %>%
  select(-c(starts_with("FirstD1"),starts_with("LastD1")))




TORV_2023logit <- glm(teamWin ~ AdjO_diff + AdjD_diff + 
                        Barthag_diff+ OrbP_diff+ Seed_diff+ ftrO_diff+ WAB_diff+TO_differential_diff,
                      data = normalPreds.diffTorvik[4:29],
                      family = binomial(link = "logit"))

normalDiffMatchups2023 <-scale(diffmatchups2023[3:12], 
                               center = colMeans(select(diff_Torvik,c(8:10,13:15,17,20,27,29) )), 
                               scale = map(select(diff_Torvik,c(8:10,13:15,17,20,27,29) ), sd)) %>% as_tibble()

#in respect to seed diff being + coeff, oral rob v houston only increase by ~1%, i believe it is capturing historical
#upsets, I could find a way to skirt this (dummy var each diff in seed or each seed match up), but with 
#the tournament "starting " tonight" i am going ahead with this. Can improve another day.
TORV_2023preds<-predict(TORV_2023logit, normalDiffMatchups2023, type='response')
diffmatchups2023Preds <- bind_cols(diffmatchups2023,TORV_2023preds)
print(summary(TORV_2023logit))


#i would like to add in tempo
#i beleive tempo is a factor that should be considered, it brings context to other stats
#also, it could extra noise, and be more accurate due to the noisiness of MM
TORV_2023logit_adjT <- glm(teamWin ~ AdjO_diff + AdjD_diff + 
                             Barthag_diff+ OrbP_diff+ Seed_diff+ ftrO_diff+ WAB_diff+TO_differential_diff+ AdjT_diff,
                           data = normalPreds.diffTorvik[4:29],
                           family = binomial(link = "logit"))

normalDiffMatchups2023_adjT <-scale(diffmatchups2023[3:13], 
                                    center = colMeans(select(diff_Torvik,c(8:10,13:15,17,20,26:27,29) )), 
                                    scale = map(select(diff_Torvik,c(8:10,13:15,17,20,26:27,29) ), sd)) %>% as_tibble()


TORV_2023preds_adjT<-predict(TORV_2023logit, normalDiffMatchups2023_adjT, type='response')
diffmatchups2023Preds_adjT <- bind_cols(diffmatchups2023,TORV_2023preds_adjT)
print(summary(TORV_2023logit_adjT))




#Hierarchical model
All2023_Slots<-MNCAATourney_Slots %>%
  filter(Season=='2023' & !str_detect(StrongSeed,"R"))%>%
  pivot_longer(cols = c(StrongSeed,WeakSeed)) %>%
  select(-name) %>%
  #this was the only way i could imagine to get the output i got. I wanted each seed as a row, with all its slots
  #after it
  mutate(R32_Slot = case_when(
    str_ends(Slot,'1') | str_ends(Slot,'8') ~ paste0("R2",str_extract(Slot,"[W-Z]"),1),
    str_ends(Slot,'2') | str_ends(Slot,'7') ~ paste0("R2",str_extract(Slot,"[W-Z]"),2),
    str_ends(Slot,'3') | str_ends(Slot,'6') ~ paste0("R2",str_extract(Slot,"[W-Z]"),3),
    str_ends(Slot,'4') | str_ends(Slot,'5') ~ paste0("R2",str_extract(Slot,"[W-Z]"),4),
  ),
  Sweet_Slot = case_when(
    str_ends(R32_Slot,'1') | str_ends(R32_Slot,'4') ~ paste0("R3",str_extract(R32_Slot,"[W-Z]"),1),
    str_ends(R32_Slot,'2') | str_ends(R32_Slot,'3') ~ paste0("R3",str_extract(R32_Slot,"[W-Z]"),2),
  ),
  Elite_slot = if_else(str_ends(Sweet_Slot,'1') | str_ends(Sweet_Slot,'2'),
                       paste0("R4",str_extract(Sweet_Slot,"[W-Z]"),1),'NA'),
  FF_slot = if_else(str_detect(Elite_slot,"[W-X]"),"R5WX","R5YZ"),
  Champ_slop = "RCH"
  ) %>%
  rename('R64_Slot' = Slot,
         "Seed" = value) %>%
  #play in seeds need different rules
  mutate(playin_slot = if_else(str_detect(Seed,"[a-b]"),R64_Slot,'NA'),
         R64_Slot = if_else(str_detect(Seed,"[a-b]"),
                            if_else(str_detect(Seed,"11"),paste0("R1",str_extract(Seed,"[Y-Z]"),"6"),
                                    paste0("R1",str_extract(Seed,"[W-X]"),"1")
                            ), R64_Slot
         ),
         R32_Slot = if_else(str_detect(Seed,"[a-b]"),
                            if_else(str_detect(Seed,"11"),paste0("R2",str_extract(Seed,"[Y-Z]"),"3"),
                                    paste0("R2",str_extract(Seed,"[W-X]"),"1")
                            ), R32_Slot
         ),
         Sweet_Slot = if_else(str_detect(Seed,"[a-b]"),
                              if_else(str_detect(Seed,"11"),paste0("R3",str_extract(Seed,"[Y-Z]"),"2"),
                                      paste0("R3",str_extract(Seed,"[W-X]"),"1")
                              ), Sweet_Slot
         ),#elite ff and ch same for all
  ) %>%
  select(Season,Seed,playin_slot,R64_Slot,R32_Slot,Sweet_Slot,Elite_slot,FF_slot,Champ_slop) 

#ALL possible games + preds
torv2023PredsBracket <- diffmatchups2023Preds %>%
  mutate(Season = 2023) %>%
  left_join(MNCAATourney_Seeds, 
            by = c("Season"="Season", "teamID" = "TeamID")) %>%
  left_join(MNCAATourney_Seeds, 
            by = c("Season"="Season", "teamID_opp" = "TeamID")) %>%
  rename_with(.fn = ~ gsub(".x", "", .x, fixed = T),
              .cols = ends_with(".x")) %>%
  rename_with(.fn = ~ gsub(".y", "_opp", .x, fixed = T),
              .cols = ends_with(".y")) %>%
  select(-AdjT_diff)%>%
  rename('teamPred' = ...16) %>%
  mutate(teamPredOpp = 1-teamPred) 

torv2023PredsBracket_adjT <- diffmatchups2023Preds_adjT%>%
  mutate(Season = 2023) %>%
  left_join(MNCAATourney_Seeds, 
            by = c("Season"="Season", "teamID" = "TeamID")) %>%
  left_join(MNCAATourney_Seeds, 
            by = c("Season"="Season", "teamID_opp" = "TeamID")) %>%
  rename_with(.fn = ~ gsub(".x", "", .x, fixed = T),
              .cols = ends_with(".x")) %>%
  rename_with(.fn = ~ gsub(".y", "_opp", .x, fixed = T),
              .cols = ends_with(".y")) %>%
  rename('teamPred' = ...16) %>%
  mutate(teamPredOpp = 1-teamPred) 

#wrote as csv for shiny app
write_csv(torv2023PredsBracket, "torvShinyData.csv")
write_csv(torv2023PredsBracket_adjT, "adjtShinyData.csv")

#function to get probability of advancing to each round
getPredsByRound<-function(df){
  #2 of each prediction, this 
  doublePreds <- 
    df %>%
    bind_rows(as_tibble(data.frame(matrix(nrow=2278,ncol=length(df)))))%>%
    select(teamID,Seed,TeamName,teamPred,teamID_opp,Seed_opp,TeamName_opp,teamPredOpp,)
  #swaps team and opp, 2 observations so its easier to search thru to find game outcomes
  doublePreds$teamID <- append(doublePreds$teamID, doublePreds$teamID_opp) %>% na.omit()
  doublePreds$Seed <- append(doublePreds$Seed, doublePreds$Seed_opp) %>% na.omit()
  doublePreds$TeamName <- append(doublePreds$TeamName, doublePreds$TeamName_opp) %>% na.omit()
  doublePreds$teamPred <- append(doublePreds$teamPred, doublePreds$teamPredOpp) %>% na.omit()
  
  doublePreds$teamID_opp <- append(doublePreds$teamID_opp, doublePreds$teamID[1:2278]) %>% na.omit()
  doublePreds$Seed_opp <- append(doublePreds$Seed_opp, doublePreds$Seed[1:2278]) %>% na.omit()
  doublePreds$TeamName_opp <- append(doublePreds$TeamName_opp, doublePreds$TeamName[1:2278]) %>% na.omit()
  doublePreds$teamPredOpp <- append(doublePreds$teamPredOpp, doublePreds$teamPred[1:2278]) %>% na.omit()
  
  doublePreds<-doublePreds %>%
    mutate(
      division = str_extract(Seed,"[W-Z]"),
      playinSeed = str_extract(Seed,"[a-b]"),
      seedNum = as.numeric(str_extract(Seed,"\\d+")),
      division_opp = str_extract(Seed_opp,"[W-Z]"),
      playinSeed_opp = str_extract(Seed_opp,"[a-b]"),
      seedNum_opp = as.numeric(str_extract(Seed_opp,"\\d+")),
      #rounds each game takes place in, most important part, this was the only way i could get it to work
      round = case_when(
        (!is.na(playinSeed) & !is.na(playinSeed_opp)) & division == division_opp & seedNum == seedNum_opp~ 0,#playin
        division_opp == division &
          seedNum + seedNum_opp == 17 ~1,#allr1
        division_opp == division & (seedNum == 1 |seedNum == 16) & (seedNum_opp == 8 | seedNum_opp == 9) ~2, #1+16
        division_opp == division & (seedNum ==8 |seedNum == 9) & (seedNum_opp == 1 | seedNum_opp == 16) ~2, #8+9
        division_opp == division & (seedNum == 5 |seedNum == 12) & (seedNum_opp == 4 | seedNum_opp == 13) ~2,# 5,12
        division_opp == division & (seedNum ==4 |seedNum == 13) & (seedNum_opp == 5 | seedNum_opp == 12) ~2, #4,13
        division_opp == division & (seedNum == 6 |seedNum == 11) & (seedNum_opp == 3 | seedNum_opp == 14) ~2, #6,11
        division_opp == division & (seedNum ==3 |seedNum == 14) & (seedNum_opp == 6 | seedNum_opp == 11) ~2, #3,14
        division_opp == division & (seedNum == 7 |seedNum == 10) & (seedNum_opp == 2 | seedNum_opp == 15) ~2, #1+16
        division_opp == division & (seedNum ==2 |seedNum == 15) & (seedNum_opp == 7 | seedNum_opp == 10) ~2, #8+9
        division_opp == division & (seedNum %in% c(1,16,8,9)) & seedNum_opp%in% c(5,12,4,13) ~3,
        division_opp == division & (seedNum %in% c(5,12,4,13)) & seedNum_opp%in% c(1,16,8,9) ~3,
        division_opp == division & (seedNum %in% c(6,11,3,14)) & seedNum_opp%in% c(7,10,2,15) ~3,
        division_opp == division & seedNum %in% c(7,10,2,15) & seedNum_opp%in% c(6,11,3,14) ~3,
        division_opp == division & seedNum %in% c(1,16,8,9,5,12,4,13) & seedNum_opp%in%c(6,11,3,14,7,10,2,15) ~4,
        division_opp == division & seedNum %in% c(6,11,3,14,7,10,2,15) & seedNum_opp%in%c(1,16,8,9,5,12,4,13) ~4,
        (division == "W" & division_opp == "X") | (division == "X" & division_opp == "W") ~ 5,
        (division == "Y" & division_opp == "Z") | (division == "Z" & division_opp == "Y") ~ 5,
        (division == "W" | division == "X") & (division_opp == 'Y' | division_opp== "Z") ~ 6,
        (division == "Y" | division == "Z") & (division_opp == 'W' | division_opp== "X") ~ 6
      )
    )
  
  r1Preds <- doublePreds %>%
    group_by(teamID,round)%>%
    summarise(r1Prob = max(if_else(round == 0,teamPred,1)))%>%
    filter(round ==0) %>%
    select(-round)%>%
    ungroup()
  
  joinPreds<-function(dfAll,dfRoundPreds){
    df<-left_join(dfAll,dfRoundPreds, by = c("teamID" = "teamID"))%>%
      left_join(dfRoundPreds, by = c("teamID_opp" = "teamID")) %>%
      rename_with(.fn = ~ gsub(".x", "", .x, fixed = T),
                  .cols = ends_with(".x")) %>%
      rename_with(.fn = ~ gsub(".y", "_opp", .x, fixed = T),
                  .cols = ends_with(".y")) 
    
    return(df)
  }
  
  allPreds<- joinPreds(doublePreds,r1Preds)  %>%
    mutate(r1Prob= if_else(is.na(r1Prob),1,r1Prob))%>%
    mutate(r1Prob_opp= if_else(is.na(r1Prob_opp),1,r1Prob_opp))
  
  r2Preds <- allPreds %>%
    group_by(teamID,round) %>%
    mutate(r2Prob = if_else(round == 1, teamPred*r1Prob,NA)) %>%
    filter(round == 1)%>%
    summarise(r2Prob,r1Prob_opp,
              r2ConProb = r2Prob*r1Prob_opp)%>%
    ungroup()%>%
    group_by(teamID) %>%
    summarise(r2Prob = sum(r2ConProb))%>%
    ungroup()
  
  allPreds<- joinPreds(allPreds,r2Preds)
  
  r3Preds <- allPreds %>%
    group_by(teamID,round) %>%
    mutate(r3Prob = if_else(round == 2, teamPred*r2Prob,NA)) %>%
    filter(round == 2)%>%
    summarise(r3Prob,r2Prob_opp,
              r3ConProb = r3Prob*r2Prob_opp)%>%
    ungroup()%>%
    group_by(teamID) %>%
    summarise(r3Prob = sum(r3ConProb))%>%
    ungroup()
  
  allPreds<- joinPreds(allPreds,r3Preds)
  
  r4Preds <- allPreds %>%
    group_by(teamID,round) %>%
    mutate(r4Prob = if_else(round == 3, teamPred*r3Prob,NA)) %>%
    filter(round == 3)%>%
    summarise(r4Prob,r3Prob_opp,
              r4ConProb = r4Prob*r3Prob_opp)%>%
    ungroup()%>%
    group_by(teamID) %>%
    summarise(r4Prob = sum(r4ConProb))%>%
    ungroup()
  
  allPreds<- joinPreds(allPreds,r4Preds)
  
  r5Preds <- allPreds %>%
    group_by(teamID,round) %>%
    mutate(r5Prob = if_else(round == 4, teamPred*r4Prob,NA)) %>%
    filter(round == 4)%>%
    summarise(r5Prob,r4Prob_opp,
              r5ConProb = r5Prob*r4Prob_opp)%>%
    ungroup()%>%
    group_by(teamID) %>%
    summarise(r5Prob = sum(r5ConProb))%>%
    ungroup()
  
  allPreds<- joinPreds(allPreds,r5Preds)
  
  r6Preds <- allPreds %>%
    group_by(teamID,round) %>%
    mutate(r6Prob = if_else(round == 5, teamPred*r5Prob,NA)) %>%
    filter(round == 5)%>%
    summarise(r6Prob,r5Prob_opp,
              r6ConProb = r6Prob*r5Prob_opp)%>%
    ungroup()%>%
    group_by(teamID) %>%
    summarise(r6Prob = sum(r6ConProb))%>%
    ungroup()
  
  allPreds<- joinPreds(allPreds,r6Preds)
  
  ChampPreds <- allPreds %>%
    group_by(teamID,round) %>%
    mutate(champProb = if_else(round == 6, teamPred*r6Prob,NA)) %>%
    filter(round == 6)%>%
    summarise(champProb,r6Prob_opp,
              champConProb = champProb*r6Prob_opp)%>%
    ungroup()%>%
    group_by(teamID) %>%
    summarise(champProb = sum(champConProb))%>%
    ungroup()
  
  allPreds<- joinPreds(allPreds,ChampPreds)
  
  futuresPredictions <- allPreds %>%
    group_by(teamID,'region' = division, TeamName) %>%
    summarise(
      r1Prob = max(r1Prob),
      r2Prob = max(r2Prob),
      r3Prob = max(r3Prob),
      r4Prob = max(r4Prob),
      r5Prob = max(r5Prob),
      r6Prob = max(r6Prob),
      champProb = max(champProb)
    )%>%  
    ungroup()
  return(futuresPredictions)
}
#write csv for shiny app
futurePredsTorv <- getPredsByRound(torv2023PredsBracket)
write.csv(futurePredsTorv,"fPredsTorv.csv")
futurePredsAdjT <- getPredsByRound(torv2023PredsBracket_adjT)
write_csv(futurePredsAdjT,"fPredsAdjt.csv")
#made csv with all 68 game outcomes, twice to make sure it would join, 134 obs total
outcomes <- readxl::read_xlsx("mmOutcomes.xlsx") 
#same deal, just fixed to deal w/ different data
get2023error <- function(df,round = NULL, modelName = ""){
  data<-df %>%
    left_join(outcomes, by = c("teamID" = 'team','teamID_opp' = 'team_opp' ))%>%
    filter(!is.na(actual))
  if (!is.null(round)) {
    data<-filter(data,rd == round)
  }
  
  
  
  n=1
  brier = 0
  while (n<=nrow(data)) {
    diff = data$teamPred[n] - data$actual[n]
    diff2 = diff^2
    brier = brier+diff2
    n=n+1
  }
  mse = brier/(n-1)
  rmse = sqrt(mse)
  
  df<-bind_cols(mse,rmse)%>%rename(MSE = ...1, RMSE=...2)%>%mutate(model = modelName)
  return(df)
  
}

torvError <- get2023error(torv2023PredsBracket,NULL,"torv")
adjTerror <- get2023error(torv2023PredsBracket_adjT,NULL,"adjT")
torvError
adjTerror


#get logos
hoopRteams <- hoopR::espn_mbb_teams() %>%
  #get names same
  mutate(names = case_when(
    short_name == "Texas Southern" ~ "TX Southern",
    short_name == "Fair Dickinson" ~ "F Dickinson",
    short_name == "Kennesaw St" ~ "Kennesaw",
    short_name == "UCSB" ~ "UC Santa Barbara",
    short_name == "Penn State" ~ "Penn St",
    short_name == "Miami" ~ "Miami FL",
    short_name == "Kent State" ~ "Kent",
    short_name == "Utah State" ~ "Utah St",
    short_name == "Iowa State" ~ "Iowa St",
    T~short_name
  )
  )

futuresTable <-function(df,modelName,scaleColor = "#85C1E9", regionSort = F ){
  if(regionSort == F){
    df %>%
      select(-c(teamID))%>%
      arrange(desc(champProb))%>%
      #changing to names w/ more sense
      mutate(TeamName = case_when(
        TeamName == "Connecticut" ~ "UConn",
        TeamName == "St Mary's CA" ~ "Saint Mary's",
        TeamName == "FL Atlantic" ~ 'FAU',
        TeamName == "Col Charleston" ~ "Charleston",
        TeamName == "Pittsburgh" ~ "Pitt",
        TeamName == "TAM C. Christi" ~ "Texas A&M-CC",
        T~TeamName
      )
      )%>%
      left_join(
        hoopRteams,
        by = c("TeamName" = 'names')
      )%>%
      select(logo,TeamName,ends_with("prob"))%>%
      gt()%>%
      tab_header(
        title = "2023 March Madness Logistic Regression Results",
        subtitle = paste0(modelName," Model")
      )%>%
      data_color(columns = ends_with("prob"), 
                 method = "numeric",
                 palette  = c("#FDFEFE",scaleColor),
                 domain = c(0,1)
      )%>%
      fmt_percent(columns = ends_with('prob')) %>%
      tab_spanner(columns = ends_with('prob'),
                  label = 'Probability of Advancing to Each Round') %>%
      cols_label(
        TeamName = '',
        logo = "",
        r1Prob = ("RD of 64"),
        r2Prob = ('RD of 32'),
        r3Prob = ("Sweet 16"),
        r4Prob = ('Elite 8'),
        r5Prob = ('Final 4'),
        r6Prob = ('Champ Game'),
        champProb = ('National Champion')
      )%>%
      text_transform(
        locations = cells_body(columns =logo),
        fn = function(x){web_image(url=x)}
      )%>%
      cols_width(
        ends_with('prob') ~ px(85),
        TeamName ~ px(85)
      )%>%
      tab_source_note(
        html('Model built with data from: ',
             as.character(a(href = 'barttorvik.com',"barttorvik.com")), " and ",
             as.character(a(href = 'https://www.kaggle.com/competitions/march-machine-learning-mania-2023/overview'
                            ,"Kaggle"))
        )
      )
  }
  else{
    df %>%
      select(-c(teamID))%>%
      #sort by region & FF prob instead of champ
      arrange(region,desc(r5Prob))%>%
      #changing to names w/ more sense
      mutate(TeamName = case_when(
        TeamName == "Connecticut" ~ "UConn",
        TeamName == "St Mary's CA" ~ "Saint Mary's",
        TeamName == "FL Atlantic" ~ 'FAU',
        TeamName == "Col Charleston" ~ "Charleston",
        TeamName == "Pittsburgh" ~ "Pitt",
        TeamName == "TAM C. Christi" ~ "Texas A&M-CC",
        T~TeamName
      )
      )%>%
      left_join(
        select(hoopRteams,c(names,logo)),
        by = c("TeamName" = 'names')
      )%>%
      select(region,logo,TeamName,ends_with("prob"))%>%
      gt()%>%
      tab_header(
        title = "2023 March Madness Logistic Regression Results",
        subtitle = paste0(modelName," Model")
      )%>%
      data_color(columns = ends_with("prob"), 
                 method = "numeric",
                 palette  = c("#FDFEFE",scaleColor),
                 domain = c(0,1)
      )%>%
      fmt_percent(columns = ends_with('prob')) %>%
      tab_spanner(columns = ends_with('prob'),
                  label = 'Probability of Advancing to Each Round') %>%
      cols_label(
        TeamName = '',
        logo = "",
        r1Prob = ("RD of 64"),
        r2Prob = ('RD of 32'),
        r3Prob = ("Sweet 16"),
        r4Prob = ('Elite 8'),
        r5Prob = ('Final 4'),
        r6Prob = ('Champ Game'),
        champProb = ('National Champion')
      )%>%
      text_transform(
        locations = cells_body(columns =logo),
        fn = function(x){web_image(url=x)}
      )%>%
      cols_width(
        ends_with('prob') ~ px(85),
        TeamName ~ px(85)
      )%>%
      tab_source_note(
        html('Model built with data from: ',
             as.character(a(href = 'barttorvik.com',"barttorvik.com")), " and ",
             as.character(a(href = 'https://www.kaggle.com/competitions/march-machine-learning-mania-2023/overview'
                            ,"Kaggle"))
        )
      )%>%
      tab_row_group(
        label = "East Region",
        rows = region == "W"
      )%>%
      tab_row_group(
        label = "South Region",
        rows = region == "X"
      )%>%
      tab_row_group(
        label = "MidWest Region",
        rows = region == "Y"
      )%>%
      tab_row_group(
        label = "West Region",
        rows = region == "Z"
      ) %>%
      cols_hide(region)
    
  }
}
#gt update lol
library(webshot2)

chartTorv <- futuresTable(futurePredsTorv,modelName = "Torvik", scaleColor = "#F08F28") 
gtsave(chartTorv,"chartTorv.png")

chartAdjt <- futuresTable(futurePredsAdjT,modelName = "Torvik + AdjT", scaleColor = "#F08F28")
gtsave(chartAdjt,"chartAdjt.png")

regionTorv<-futuresTable(futurePredsTorv,modelName = "Torvik", scaleColor = "#F08F28",regionSort=T) 
gtsave(regionTorv,"regionTorv.png")

regionAdjt<-futuresTable(futurePredsAdjT,modelName = "Torvik + AdjT", scaleColor = "#F08F28",regionSort=T)
gtsave(regionAdjt,"regionAdjt.png")








