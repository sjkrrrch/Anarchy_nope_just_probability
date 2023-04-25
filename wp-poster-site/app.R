
library(shiny)
library(tidyverse)
library(randomForest)
library(tidymodels)
library(caret)
library(readxl)
library(gt)
library(htmltools)
library(rvest)
library(hoopR)

torv2023PredsBracket<-read_csv("torvShinyData.csv")
torv2023PredsBracket_adjT<-read_csv("adjtShinyData.csv")
fPredsTorv <- read_csv("fPredsTorv.csv")%>%select(-...1)
fPredsAdjt <- read_csv("fPredsAdjt.csv")

torv2023PredsBracket_Shiny <- torv2023PredsBracket%>%
  mutate_at(13:14,~
     case_when(
      .x == "Connecticut" ~ "UConn",
      .x == "St Mary's CA" ~ "Saint Mary's",
      .x == "FL Atlantic" ~ 'FAU',
      .x == "Col Charleston" ~ "Charleston",
      .x == "Pittsburgh" ~ "Pitt",
      .x == "TAM C. Christi" ~ "Texas A&M-CC",
      T ~ .x
    ))

torv2023PredsBracket_adjT_Shiny <- torv2023PredsBracket_adjT%>%
  mutate_at( 14:15,~
    case_when(
      .x == "Connecticut" ~ "UConn",
      .x == "St Mary's CA" ~ "Saint Mary's",
      .x == "FL Atlantic" ~ 'FAU',
      .x == "Col Charleston" ~ "Charleston",
      .x == "Pittsburgh" ~ "Pitt",
      .x == "TAM C. Christi" ~ "Texas A&M-CC",
      T ~ .x
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
    mutate(model = 'Torvik + AdjT') %>%
    select(Model = model,TeamName,teamPred,TeamName_opp,teamPredOpp)
  
  return(df)
}
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
        r3Prob = ("SWT 16"),
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
        r3Prob = ("SWT 16"),
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

ui <- fluidPage(
  h1("Championship and Win Probabilities for Every Theoretical Tournament Matchup
             in the NCAA Tournament from 2 Comparable Logistic Regressions",
             align = "center"),
  p("Beyond the model in my poster, I also used the exact same process to build another model,
    the only difference between the two being the inclusion of Adjusted Tempo (AdjT) as a predictor, 
    which I had originally removed to lower the MSE of the cross-validation training model.
    I did this for two reasons: The first being I feel tempo is a defining characteristic of how a team
    plays, and the second being I was curious if including a predictor that the logistic regression 
    considered highly insignificant - with a P-Value of .70 - would add more noise to the model 
    and potentially make it more accurate due to the unpredictability of March Madness.
    I believe the reason the model with AdjT included performed mariginally better with respect to MSE was because many of this
    years upsets came in matchups betweeen two teams with a large difference in average tempo, take
    Alabama vs SDSU for example. However, I don’t think that adding AdjT made a better model due to a
    plethora of bizarre results; Houston having a roughly 14% chance of losing to 16 seeded
    Northern Kentucky is just one of many examples.",
    align = "center"),
  h6("Please use the drop down to view different matchups, as well as toggle between overall and 
  regionally sorted championship and Final Four probabilities",
     align="center"),
  fluidRow(
  column(2,selectInput(
    inputId = 'input1',
    label = "",
    choices = sort(unique(torv2023PredsBracket_Shiny$TeamName)),
    selected = "UConn"
  ), offset = 4),
  column(2,selectInput(
    inputId = 'input2',
    label = "",
    choices = sort(unique(torv2023PredsBracket_Shiny$TeamName_opp)),
    selected = "San Diego St"
  ))
  ),
  gt_output("wpTable"),
  checkboxInput("byRegion", label = "Sort by Region?"),
  fluidRow(
    column(6,gt_output("torvF")),
    column(6,gt_output("adjtF"))
  )
)

server <- function(input, output) {
  output$wpTable <- render_gt({
    bind_rows(
      get_gameWPTorv(team1 = input$input1, team2 = input$input2),
      get_gameWPTorv_adjT(team1 = input$input1, team2 = input$input2)
    )%>%
      left_join(select(hoopRteams, c(names, logo)),
                by = c("TeamName" = 'names')) %>%
      left_join(select(hoopRteams, c(names, logo)),
                by = c("TeamName_opp" = 'names')) %>%
      select(Model,
             logo.x,
             TeamName,
             teamPred,
             logo.y,
             TeamName_opp,
             teamPredOpp) %>%
      gt() %>%
      data_color(columns = contains("pred"),
                 method = "numeric",
                 palette  = c("#FDFEFE", "#F08F28"),
                 domain = c(0, 1)
      ) %>%
      text_transform(
        locations = cells_body(columns = contains('logo')),
        fn = function(x) {
          web_image(url = x)
        }
      ) %>%
      fmt_percent(columns = contains("pred"))%>%
      cols_label(
        logo.x = "",
        teamPred = "Win Probability",
        logo.y = "",
        teamPredOpp = "Win Probability"
      ) %>%
      cols_hide(starts_with("TeamName"))
    
  })
  
  output$torvF <- render_gt({
    futuresTable(fPredsTorv,modelName = "Torvik", scaleColor = "#F08F28",
                 regionSort = input$byRegion) 
  })
  
  output$adjtF <- render_gt({
    futuresTable(fPredsAdjt,modelName = "Torvik + AdjT", scaleColor = "#F08F28",
                 regionSort = input$byRegion) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
