# References --------------------------------------------------------------
#Reference: https://www.pinnacle.com/en/betting-articles/soccer/how-to-calculate-poisson-distribution
#League Tables: http://www.football.co.uk/league-tables/premier-league/#WlUFZkQV0jYtcPgy.97

# Load libraries ----------------------------------------------------------
library(dplyr)
library(nnet) #For which.is.max function

# Read data for previous seasons, current status of present season --------
season_previous <- read.csv(file.choose())
fixtures <- read.csv(file.choose())

#Calcualte mean home goals and away goals per game ----------------------
mean_season_previous_home_goals_scored <- season_previous %>%
  select(home_games, home_goals_for) %>%
  summarise(sum(home_goals_for)/length(home_games))

mean_season_previous_away_goals_scored <- season_previous %>%
  select(away_games, away_goals_for) %>%
  summarise(sum(away_goals_for)/length(away_games))

mean_season_previous_home_goals_conceded <- mean_season_previous_away_goals_scored

mean_season_previous_away_goals_conceded <- mean_season_previous_home_goals_scored

# Start of main code to calculate match results ------------------------------------

for (i in 1:nrow(fixtures)){
  home_team <- fixtures[i,1]
  away_team <- fixtures[i,2]
  
# Calculate attack and defence stengths -----------------
  attack_strength_home_team <- season_previous %>%
    select(team, home_games, home_goals_for) %>%
    filter(team %in% home_team)%>%
    summarise((home_goals_for/home_games)/mean_season_previous_home_goals_scored)
  #print(attack_strength_home_team)
    
  defence_strength_away_team <- season_previous %>%
    select(team, away_games, away_goals_against) %>%
    filter(team %in% away_team)%>%
    summarise((away_goals_against/away_games)/mean_season_previous_away_goals_conceded)
  #print(defence_strength_away_team)
  
  predicted_home_goals <- attack_strength_home_team * defence_strength_away_team * mean_season_previous_home_goals_scored
  #print(predicted_home_goals)
  
  attack_strength_away_team <- season_previous %>%
    select(team, away_games, away_goals_for) %>%
    filter(team %in% away_team)%>%
    summarise((away_goals_for/away_games)/mean_season_previous_away_goals_scored)
  #print(attack_strength_away_team)
  
  defence_strength_home_team <- season_previous %>%
    select(team, home_games, home_goals_against) %>%
    filter(team %in% home_team)%>%
    summarise((home_goals_against/home_games)/mean_season_previous_home_goals_conceded)
  #print(attack_strength_home_team)
  
  predicted_away_goals <- attack_strength_away_team * defence_strength_home_team * mean_season_previous_away_goals_scored
  #print(predicted_away_goals)
  
# Function to produce poisson predicted match result ----------------------
  
    home_list <- dpois(0:10, predicted_home_goals$`sum(home_goals_for)/length(home_games)`)*100
    away_list <- dpois(0:10, predicted_away_goals$`sum(away_goals_for)/length(away_games)`)*100
    
    #print(home_list)
    #print(away_list)
    
    max_home <- which.is.max(home_list)-1
    max_away <- which.is.max(away_list)-1
   
    print(sprintf("%s scored %d and %s scored %d", home_team, max_home, away_team, max_away))
    flush.console()
    Sys.sleep(0.2)
}  


















