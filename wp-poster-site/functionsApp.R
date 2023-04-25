library(shiny)
library(tidyverse)
library(randomForest)
library(tidymodels)
library(caret)
library(readxl)
library(gt)
library(htmltools)
library(rvest)

torv2023PredsBracket_Shiny <- torv2023PredsBracket%>%
  mutate(
    TeamName = case_when(
      TeamName == "Connecticut" ~ "UConn",
      TeamName == "St Mary's CA" ~ "Saint Mary's",
      TeamName == "FL Atlantic" ~ 'FAU',
      TeamName == "Col Charleston" ~ "Charleston",
      TeamName == "Pittsburgh" ~ "Pitt",
      TeamName == "TAM C. Christi" ~ "Texas A&M-CC",
      T ~ TeamName
    ))

torv2023PredsBracket_adjT_Shiny <- torv2023PredsBracket_adjT%>%
  mutate(
    TeamName = case_when(
      TeamName == "Connecticut" ~ "UConn",
      TeamName == "St Mary's CA" ~ "Saint Mary's",
      TeamName == "FL Atlantic" ~ 'FAU',
      TeamName == "Col Charleston" ~ "Charleston",
      TeamName == "Pittsburgh" ~ "Pitt",
      TeamName == "TAM C. Christi" ~ "Texas A&M-CC",
      T ~ TeamName
    ))
get_gameWPTorv <- function(team1,team2) {
  df<-torv2023PredsBracket_Shiny %>%
    filter((team1 == TeamName & team2 == TeamName_opp)|
             (team2 == TeamName & team1 == TeamName_opp)
    ) %>%
    mutate(model = 'Torvik') %>%
    # mutate(
    #   decimalOdds = 1/teamPred,
    #   odds = if_else(decimalOdds >=2, (decimalOdds-1)*100, -100/(decimalOdds-1)),
    #   decimalOdds_opp = 1/teamPredOpp,
    #   odds_opp = if_else(decimalOdds_opp >=2, (decimalOdds_opp-1)*100, -100/(decimalOdds_opp-1)),
    #   )%>%
    select(Model = model,TeamName,teamPred,TeamName_opp,teamPredOpp)
  
  return(df)
}

get_gameWPTorv_adjT <- function(team1,team2) {
  df<-torv2023PredsBracket_adjT_Shiny %>%
    filter((team1 == TeamName & team2 == TeamName_opp)|
             (team2 == TeamName & team1 == TeamName_opp)
    ) %>%
    mutate(model = 'TorvikAdjT') %>%
    select(Model = model,TeamName,teamPred,TeamName_opp,teamPredOpp)
  
  return(df)
}
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