# Exercise 7: using dplyr on external data

# Load the dplyr library
library(dplyr)

# Use the `read.csv()` function to read in the included data set. Remember to
# save it as a variable.
nba_teams <- read.csv("nba_teams_2016.csv", stringsAsFactors = FALSE)

# View the data frame you loaded, and get some basic information about the 
# number of rows/columns. 
# Note the "X" preceding some of the column titles as well as the "*" following
# the names of teams that made it to the playoffs that year.
View(nba_teams)
nrow(nba_teams)
ncol(nba_teams)
row.names(nba_teams)
colnames(nba_teams)
# Add a column that gives the turnovers to steals ratio (TOV / STL) for each team
nba_teams <- mutate(nba_teams, TO_to_STL_ratio = TOV / STL)

# Sort the teams from lowest turnover/steal ratio to highest
# Which team has the lowest turnover/steal ratio?
lowest_ratio <- nba_teams %>% arrange(TO_to_STL_ratio) %>% 
  filter(TO_to_STL_ratio == max(TO_to_STL_ratio)) %>% 
  select(Team)
lowest_ratio
# Using the pipe operator, create a new column of assists per game (AST / G) 
# AND sort the data.frame by this new column in descending order.
nba_teams <- nba_teams %>% 
  mutate(ast_per_game = AST / G) %>% 
  arrange(-ast_per_game)

# Create a data frame called `good_offense` of teams that scored more than 
# 8700 points (PTS) in the season
good_offense <- nba_teams %>% filter(PTS >= 8700)

# Create a data frame called `good_defense` of teams that had more than 
# 470 blocks (BLK)
good_defense <- nba_teams %>% filter(BLK >= 470)

# Create a data frame called `offense_stats` that only shows offensive 
# rebounds (ORB), field-goal % (FG.), and assists (AST) along with the team name.
offense_stats <- nba_teams %>%  select(Team, ORB, FG, AST)

# Create a data frame called `defense_stats` that only shows defensive 
# rebounds (DRB), steals (STL), and blocks (BLK) along with the team name.
defense_stats <- nba_teams %>%  select(Team, DRB, STL, BLK)

# Create a function called `better_shooters` that takes in two teams and returns
# a data frame of the team with the better field-goal percentage. Include the 
# team name, field-goal percentage, and total points in your resulting data frame
better_shooters <- function(team1, team2) {
  better_team <- nba_teams %>%  filter(Team == c(team1, team2)) %>% 
    filter(FG. == max(FG.)) %>% 
    select(Team, FG., PTS)
  better_team
}

# Call the function on two teams to compare them (remember the `*` if needed)
winner <- better_shooters("Clevaland Cavaliers*", "Atlanta Hawks*")
