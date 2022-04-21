library(tidyverse)
library(mgcv)


df <- readRDS("2022-tourney-player-min-usg-stock-data-by-game.rds")

dts <- 
  tibble(dt = seq(min(df$date), max(df$date), by = "day")) %>%
  mutate(day_no = row_number())

df <- 
  df %>% 
  inner_join(dts, by = c("date" = "dt"))

players <-
  df %>%
  group_by(team, player) %>%
  nest() %>%
  ungroup() %>%
  mutate(max_min = map_dbl(data, function(x) max(x$min))) %>%
  filter(max_min >= 20) %>%
  select(team, player, max_min, data)

get_min_mod <- function(plyr, tm, dat){
  
  print(paste0(plyr, " ", tm))
  
  # mod <- gam(min ~ s(day_no, k = 5), method = "REML", family = poisson(link = "log"), data = dat)
  mod <- gam(min ~ s(day_no, k = 7), method = "REML", data = dat)
  mod
  
}

get_usg_mod <- function(plyr, tm, dat){
  
  print(paste0(plyr, " ", tm))
  
  # mod <- gam(min ~ s(day_no, k = 5), method = "REML", family = poisson(link = "log"), data = dat)
  mod <- gam(usg ~ s(day_no, k = 7), method = "REML", data = dat)
  mod
  
}


get_preds <- function(mod){
  dat <- tibble(day_no = min(dts$day_no):max(dts$day_no))
  # dat$pred <- as.vector(predict(mod, dat, type = "response"))
  dat$pred <- as.vector(predict(mod, dat, type = "response"))
  dat
}


players <- 
  players %>%
  mutate(min_mod = pmap(list(player, team, data), get_min_mod)) %>%
  mutate(usg_mod = pmap(list(player, team, data), get_usg_mod)) %>%
  mutate(min_preds = map(min_mod, get_preds)) %>%
  mutate(usg_preds = map(usg_mod, get_preds))


players %>%
  mutate(current_exp_min = map_dbl(min_preds, function(x) ifelse(last(x$pred) < 0, 0, last(x$pred))),
         median_usg = map_dbl(usg_preds, function(x) median(x$pred)),
         current_exp_usg = map_dbl(usg_preds, function(x) ifelse(last(x$pred) < 0, 0, last(x$pred))),
         diff = current_exp_usg - median_usg) %>%
  arrange(desc(diff)) %>%
  slice(1:20) %>%
  group_by(team) %>%
  summarise(diff = sum(diff)) %>% arrange(desc(diff)) %>% print(n = "all")
         

players %>%
  mutate(current_exp_min = map_dbl(min_preds, function(x) ifelse(last(x$pred) < 0, 0, last(x$pred))),
         median_usg = map_dbl(usg_preds, function(x) median(x$pred)),
         current_exp_usg = map_dbl(usg_preds, function(x) ifelse(last(x$pred) < 0, 0, last(x$pred))),
         diff = current_exp_usg - median_usg) %>%
  select(team, player, current_exp_min, current_exp_usg) %>%
  filter(current_exp_min > 30) %>%
  arrange(desc(current_exp_usg)) %>% slice(1:20)

###

tm <- "Houston"
plyr <- "T. Moore"
data <- players[which(players$team == tm & players$player == plyr), ]$data[[1]]
usg_preds <- players[which(players$team == tm & players$player == plyr), ]$usg_preds[[1]]

ggplot()+
  geom_line(data = usg_preds, aes(x = day_no, y = pred))+
  geom_point(data = data, aes(x = day_no, y = usg))


players %>%
  filter(team == "LSU") %>%
  select(player) %>%
  distinct()

## H. Dickinson
## K. Murray
## J. Tchamwa Tchatchoua


tm <- "Baylor"
plyr <- "J. Tchamwa Tchatchoua"
dat <- players[which(players$team == tm & players$player == plyr), ]$data[[1]]
preds <- players[which(players$team == tm & players$player == plyr), ]$min_preds[[1]]

ggplot()+
  geom_line(data = preds, aes(x = day_no, y = pred))+
  geom_point(data = dat, aes(x = day_no, y = min))




