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
         median_min_pred = map_dbl(min_preds, function(x) median(x$pred)),
         min_diff = (current_exp_min - median_min_pred)/median_min_pred,
         median_usg = map_dbl(data, function(x) mean(x$usg))*100,
         median_stocks = map_dbl(data, function(x) median(x$stock_rate))*100,
         missing_usg = median_usg*min_diff,
         missing_stocks = median_stocks*min_diff) %>%
  arrange(missing_usg) %>%
  select(team, player, current_exp_min, median_min_pred, median_usg, median_stocks, missing_usg, missing_stocks) %>%
  slice(1:20)


players %>%
  mutate(current_exp_min = map_dbl(min_preds, function(x) ifelse(last(x$pred) < 0, 0, last(x$pred))),
         median_min_pred = map_dbl(min_preds, function(x) median(x$pred)),
         min_diff = (current_exp_min - median_min_pred)/median_min_pred,
         median_usg = map_dbl(data, function(x) mean(x$usg))*100,
         median_stocks = map_dbl(data, function(x) median(x$stock_rate))*100,
         missing_usg = median_usg*min_diff,
         missing_stocks = median_stocks*min_diff) %>%
  arrange(missing_usg) %>%
  select(team, player, current_exp_min, median_min_pred, median_usg, median_stocks, missing_usg, missing_stocks) %>%
  slice(1:20)


###

players %>% select(team) %>% distinct()
  
tm <- "Michigan St"
plyr <- "T. Walker"
data <- players[which(players$team == tm & players$player == plyr), ]$data[[1]]
usg_preds <- players[which(players$team == tm & players$player == plyr), ]$usg_preds[[1]]

ggplot()+
  geom_line(data = usg_preds, aes(x = day_no, y = pred))+
  geom_point(data = data, aes(x = day_no, y = usg))



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




