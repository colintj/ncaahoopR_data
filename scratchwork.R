library(tidyverse)
library(rvest)


## get scheds by teams

sched_path <- paste0(here::here(), "/2021-22/schedules/")
scheds <- paste0(sched_path, list.files(sched_path))

teams <- str_remove(str_remove_all(str_extract(scheds, "schedules/.+_schedule.csv"), "schedule|.csv"), "s/")
teams <- str_remove(teams, "_$")
teams <- str_replace_all(teams, "_", " ")

scheds_df <- 
  tibble(team = teams, scheds) %>%
  mutate(scheds = map(scheds, read_csv)) %>%
  unnest(scheds)


## get tourney teams

dict <- ncaahoopR::dict 
dict[dict$ESPN == "TCU", ]$sref_name <- "TCU"
dict[dict$ESPN == "UAB", ]$sref_name <- "UAB"

bref_html <- read_html("https://www.sports-reference.com/cbb/seasons/2022-school-stats.html")
bref_teams <- html_elements(bref_html, css = ".left") %>% html_text()
bref_teams <- bref_teams[grepl("NCAA", bref_teams)]
bref_teams <- str_remove(bref_teams, "\\sNCAA")

trny_tms <- 
  tibble(team = bref_teams) %>%
  left_join(dict, by = c("team" = "sref_name")) %>% 
  select(sref = team, ESPN)

## def get csv

get_csv <- function(team, id){
  
  path <- paste0(here::here(), "/2021-22/box_scores/", team, "/")
  files <- list.files(path)
  ids <- as.integer(str_remove(files, ".csv"))
  
  if (any(id == ids)){
    box <- read_csv(paste0(here::here(), "/2021-22/box_scores/", team, "/", id, ".csv"))
    box <- box %>% select(-date, -opponent, -location)
    
    box <- janitor::clean_names(box)
    
    box$poss <- box$fga + 0.44*box$fta + box$to - box$oreb
    team_poss <- box[which(box$player == "TEAM"), ]$poss
    box$tsa <- box$fga + 0.44*box$fta 
    box$ts_pct <- box$pts/(2*box$tsa)
    box$usg <- (box$fga + 0.44*box$fta + box$to + box$ast)/team_poss/box$min*(sum(box$min, na.rm = T)/5)
    box$stock_rate <- (box$blk + box$stl)/team_poss/box$min*(sum(box$min, na.rm = T)/5)
    box
  } else {
    tibble(error = 1)
  }
}

## get box scores

boxes_df <- 
  scheds_df %>%
  filter(!is.na(team_score)) %>%
  filter(!is.na(game_id)) %>%
  filter(team %in% trny_tms$ESPN) %>%
  mutate(boxes = map2(team, game_id, get_csv))

## unnest into player level data

plyr_df <- boxes_df %>% unnest(boxes, names_repair = janitor::make_clean_names)

plyr_df <- 
  plyr_df %>%
  group_by(team) %>%
  complete(date, player) %>%
  ungroup()

plyr_final_df <- 
  plyr_df %>%
  select(team, date, player, min, usg, stock_rate) %>%
  arrange(team, player, date) %>%
  mutate(across(.cols = 4:6, .fns = function(x) ifelse(is.na(x), 0, x)))

saveRDS(plyr_final_df, "2022-tourney-player-min-usg-stock-data-by-game.rds")

## scratchwork


is.double(as.Date("2022-01-01"))

plyr_df %>% filter(team == "Iowa")

plyr_df %>% 
  filter(team == "Iowa") %>%  
  mutate(across(.cols = where(is.double), .fns = function(x) ifelse(is.na(x), 0, x))) %>%
  filter(player == "K. Murray") %>%
  ggplot(aes(x = date, y = usg))+
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 15))


plyr_df %>% 
  filter(team == "Baylor") %>% 
  mutate(across(.cols = where(is.double), .fns = function(x) ifelse(is.na(x), 0, x))) %>%
  filter(player == "J. Tchamwa Tchatchoua") %>%
  ggplot(aes(x = date, y = usg))+
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 15))

plyr_df %>% 
  filter(team == "Michigan") %>% 
  mutate(across(.cols = where(is.double), .fns = function(x) ifelse(is.na(x), 0, x))) %>%
  filter(player == "D. Jones") %>%
  ggplot(aes(x = date, y = usg))+
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 7))

##




