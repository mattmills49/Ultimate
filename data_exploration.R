#' # Data Exploration
#' 
#' I just found the http://www.ultianalytics.com/ which contains a ton of data
#' on the AUDL (American Ultimate Disc League). This analysis will just be to
#' explore the data sets corresponding to each team and what I could possibly
#' do with it. 
#' 
#+
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(purrr)
options(dplyr.width = Inf)

custom_theme <- theme_bw() +
  theme(axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

#' First I need to make sure that all the files have the same names (I told you
#' I literally no nothing about this data). 

dir_string <- "Datasets/Raw Team Scripts"
files <- dir(dir_string)
col_names <- list()
for(file in files){
  team_df <- suppressWarnings(read_csv(str_c(dir_string, file, sep = "/")))
  team_names <- names(team_df)
  col_names <- c(col_names, list(team_names))
}

test_names <- col_names[[1]]
name_lgl <- map_lgl(col_names, ~ all.equal(.x, test_names))

#' good news, the files all have the same names!
#' 
#' How many are all missing?

readin_function <- function(filename) {
  suppressWarnings(str_c(dir_string, filename, sep = "/") %>%
  read_csv %>% 
  magrittr::set_names(str_replace_all(names(.), " ", "_")) %>%
  mutate(team = filename) %>%
  mutate_at(vars(Begin_Area:Toward_Our_Goal_Distance), as.numeric))
}

season_df <- map(files, readin_function) %>%
  bind_rows

nas <- map_int(season_df, ~ sum(is.na(.x)))
bad_cols <- names(nas[nas == nrow(season_df)])

season_df <- season_df[, !(names(season_df) %in% bad_cols)]

#' Now let's go through the columns and figure out what they are
#'
#' * Tournament
#+ fig.height = 10, fig.width = 8

season_df %>%
  filter(!is.na(Tournamemnt)) %>%
  mutate(team = str_replace_all(team, "2016-stats.csv", "")) %>%
  count(Tournamemnt, team) %>%
  ggplot(aes(x = Tournamemnt, y = n, fill = team)) +
  geom_bar(stat = "identity") +
  custom_theme +
  coord_flip() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 6, byrow = TRUE)) +
  ggtitle("Tournament Value Counts by Team")

#' * Point_Elapsed_Seconds
#+

season_df %>%
  select(team, Opponent, Point_Elapsed_Seconds, `Our_Score_-_End_of_Point`, `Their_Score_-_End_of_Point`) %>%
  distinct %>%
  filter(Point_Elapsed_Seconds < 10 * 60) %>%
  ggplot(aes(Point_Elapsed_Seconds)) +
  geom_histogram(boundary = 0, binwidth = 15) +
  xlab("Duration of Point\n (seconds)") +
  ggtitle("Distribution of the Duration of Points") +
  custom_theme

#' * Line
#' This is wether the team is on defense or offense
#' 
#' * Our Score End of Point / Their Score End of Point
#' The score for both teams after completion of the point
#' 
#' * Event Type
#' If the Action is for the offense or defense team. 
#' 
#' * Action
#' The different "play types" in frisbee

season_df %>%
  count(Action) %>%
  arrange(Action) %>%
  knitr::kable()

#' * Passer / Receiver / Defender 
#' The players involved in the play
#' 
#' * Hang Time
#' I assume this is the time of the disc?
#' 
#' * Player_0 - Player_17
#' The first 7 columns are the players on the field, I have no idea why there 
#' are other players, perhaps because of injury?

season_df %>%
  select(starts_with("Player")) %>%
  gather(Position, Player) %>%
  mutate(Position = factor(Position, levels = c("Player_0", "Player_1", "Player_2", "Player_3", "Player_4", "Player_5", "Player_6", "Player_7", "Player_8", "Player_9", "Player_10", "Player_11", "Player_12", "Player_13", "Player_14", "Player_15", "Player_16", "Player_17"))) %>%
  group_by(Position) %>%
  summarise(na_perc = mean(is.na(Player))) %>%
  ggplot(aes(x = Position, y = na_perc)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("% of Missing Values") +
  ggtitle("Frequency of When Each Position is Missing") +
  custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%"))

#' * Begin_X, Begin_Y, End_X, End_Y, Absolute_Distance, Lateral_Distance, Toward_our_goal
#' These are only recorded for the Montral Royal and in only 4 of their games. 
#'
#' ### Clean Data
#' 
#' The list of things to do:
#' 1. Clean names
#' 2. Create Play ID
#' 3. Get the real team names
#' 4. Merge offense and defense
#' 
#' #### Clean Names
#+

season_df <- rename(season_df, 
                    Game_Time = `Date/Time`,
                    Point_Duration = Point_Elapsed_Seconds,
                    Team_Score = `Our_Score_-_End_of_Point`,
                    Opponent_Score = `Their_Score_-_End_of_Point`,
                    Hang_Time = `Hang_Time_(secs)`,
                    Action_Time = `Elapsed_Time_(secs)`,
                    Team = team)

#' #### Create Play ID
#+
play_nums <- season_df %>%
  mutate(play_order = 1:n()) %>%
  group_by(Game_Time, Team, Opponent, Point_Duration, Line, Action, Team_Score, Opponent_Score) %>%
  summarize(contains_cess = 1 * any(Event_Type == "Cessation"), min_play = min(play_order)) %>%
  group_by(Game_Time, Team, Opponent, Point_Duration, Line, Team_Score, Opponent_Score, contains_cess) %>%
  summarize(min_play = min(min_play)) %>%
  arrange(Game_Time, Team, Opponent, min_play) %>% 
  group_by(Game_Time, Team, Opponent) %>%
  mutate(Play_Num = 1:n()) %>%
  select(-min_play) %>%
  ungroup
  
season_plays <- season_df %>%
  group_by(Game_Time, Team, Opponent, Point_Duration, Line, Team_Score, Opponent_Score) %>%
  mutate(contains_cess = 1 * (Event_Type == "Cessation")) %>%
  fill(contains_cess) %>%
  left_join(play_nums, by = c("Game_Time", "Opponent", "Point_Duration", "Line", "Team_Score", "Opponent_Score", "Team", "contains_cess")) %>%
  group_by(Game_Time, Team, Opponent, Play_Num) %>%
  mutate(Action_Num = 1:n()) %>%
  arrange(Game_Time, Play_Num) %>%
  ungroup

#' #### Team Names
#+

teams <- unique(season_plays$Team) %>%
  str_replace_all("2016-stats.csv", "")
opps <- unique(season_plays$Opponent)

team_df <- expand.grid(Team = teams, Opponent = opps, stringsAsFactors = F) %>%
  mutate(bad_opps = str_replace_all(Opponent, " ", ""),
         match = 1 * (Team == bad_opps),
         Team = str_c(Team, "2016-stats.csv")) %>%
  filter(match == 1) %>%
  select(-bad_opps, -match) %>%
  rename(Team_Name = Opponent)

season_plays <- left_join(season_plays, team_df, by = c("Team"))

#' #### Merge Offense and Defense
#' 
#' Each row is from one team's perspective, they can either be on offense or 
#' defense. So I'm going to take every Offense point and the relevant info and
#' then merge the D line from that same point. 
#' To merge the data I'll need to switch the Team and Opponent variables around
#' so that can be matched up with the other team's lines. 
#' To make sure I do it right I probably want to keep the score, times, and 
#' point durations to compare. 
#+
offense_info <- season_plays %>%
  filter(Line == "O") %>%
  select(Game_Time:Player_6, Play_Num:Team_Name)

defense_info <- season_plays %>%
  filter(Line == "D") %>%
  select(Game_Time, Tournamemnt)
