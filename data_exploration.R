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

nas <- map_int(test, ~ sum(is.na(.x)))
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
  geom_histogram(boundary = 0, binwidth = 15)

filter(season_df, !is.na(Player_17)) %>% count(team)
filter(season_df, team == "AtlantaHustle2016-stats.csv", Opponent == "Dallas Roughnecks", Point_Elapsed_Seconds == 291) %>% View
