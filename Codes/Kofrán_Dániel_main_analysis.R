library(tidyverse)
library(modelsummary)
library(purrr)
library(kableExtra)
library(plm)
library(cowplot)
library(gridExtra)
library(haven)
library(stargazer)
library(car)
library(huxtable)
library(estimatr)
library(lmtest)
library(fixest)
library(hrbrthemes)

# set data dir, data used
setwd("C:/Rajk - újgép/Szakdolgozat Közgáz Elemző")

# load theme and functions
source("Békés/ch00-tech-prep/theme_kd.R")
source("Békés/ch00-tech-prep/da_helper_functions.R")
a <- getwd()
output = paste0(a,"/results")

# Loading and preparing data ----------------------------------------------

data <- read.csv("C:/Rajk - újgép/Szakdolgozat Közgáz Elemző/final.csv")
data <- data %>% mutate(manager_id = men_id,
                        near_relegation = ifelse(league == "GER",ifelse(prev_pos >=14,1,0),ifelse(prev_pos >=16,1,0)))
# describe data
data %>%
  select(season, team, gameno, points) %>%
  summary()

# create manager change variable
data <- data %>%
  arrange(team, season, date) %>%
  group_by(team, season) %>%
  mutate(managchange = as.numeric(manager_id != dplyr::lag(manager_id))) %>%
  mutate(countmanagchange = sum(managchange, na.rm = TRUE)) %>%
  ungroup()

table(data$managchange, useNA = "always")

# some teams have multiple management changes in the season
data %>%
  group_by(countmanagchange) %>%
  summarise(n_distinct(team, season))

# define intervention as management change
# at least 12 games before (since season started or previous management changed)
# at least 12 games after (till season ends or next management change)
data_aux <- data %>%
  arrange(team, season, date) %>%
  group_by(team, season) %>%
  mutate(max_gameno = max(gameno)) %>%
  filter(managchange == 1) %>%
  mutate(gamesbefore = ifelse(is.na(dplyr::lag(gameno)), gameno - 1, gameno - dplyr::lag(gameno)),
         gamesafter = ifelse(is.na(dplyr::lead(gameno)), max_gameno - gameno, dplyr::lead(gameno) - gameno)) %>%
  mutate(intervention = ifelse(gamesbefore<12 | gamesafter<12, 0, managchange)) %>%
  select(team, season, date, intervention) %>%
  ungroup()

data_balanced <- left_join(data, data_aux) %>%
  mutate(intervention = replace_na(intervention, 0)) %>%
  group_by(team, season) %>%
  mutate(countinterv = sum(intervention, na.rm = TRUE)) %>%
  mutate(intervention_time = min(ifelse(intervention == 1, gameno, NA), na.rm = TRUE)) %>%
  mutate(t_event = ifelse(is.infinite(intervention_time), NA, gameno - intervention_time)) %>%
  mutate(t_event = ifelse(t_event>=0 & t_event<=38, t_event+1, t_event)) %>% # Intervention event study time (to -1 before, from +1 after)
  ungroup() %>%
  filter((countinterv==1 & t_event>=-12 & t_event<=12) | countmanagchange==0)

data_balanced %>%
  group_by(countinterv) %>%
  summarise(n_distinct(team, season))

# figure: average number interventions by game number
p3<- ggplot(data = data_balanced, aes(x = gameno, y = intervention)) +
  geom_col(fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(y = "Menedzserváltások száma", x = "Meccsnap") +
  scale_y_continuous(expand=c(0.0,0.0), breaks = seq(1, 15, by = 1), limits = c(0, 15)) +
  scale_x_continuous(expand=c(0.01,0.01), breaks = seq(0, 38, 4), limits = c(0, 38)) +
  background_grid(major = "none", minor = "none")+
  theme_bg()
p3
save_fig("manchange_by_matchday_kd_theme", output, "large")


p3_es_fel <- ggplot(data = data_balanced, aes(x = season, y = intervention)) +
  geom_col(fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(y = "Menedzserváltások száma", x = "Szezon") +
  scale_y_continuous(expand=c(0.0,0.0), breaks = seq(1, 16, by = 1), limits = c(0, 16)) +
  scale_x_continuous(expand=c(0.01,0.01), breaks = seq(7, 19, 2), limits = c(7, 19)) +
  background_grid(major = "none", minor = "none")+
  theme_bg()
p3_es_fel
save_fig("manchange_by_season_kd_theme", output, "large")

# figure: average points by event time
p4<-getPointsGraph(data_balanced, colors = color, num=12)
p4
save_fig("abra_all_without_psudo_kd_theme", output, "large")

# for each game, define avg diff of points 12-7 before
# dip: avg diff of points 6-1 before minus 12-7 before
data_balanced <- data_balanced %>%
  arrange(team, season, date) %>%
  group_by(team, season) %>%
  mutate(points_b_7_12 = dplyr::lag(points, 12) + dplyr::lag(points, 11) + dplyr::lag(points, 10) +
           dplyr::lag(points, 9) + dplyr::lag(points, 8) + dplyr::lag(points, 7),
         points_b_1_6 = dplyr::lag(points, 6) + dplyr::lag(points, 5) + dplyr::lag(points, 4) +
           dplyr::lag(points, 3) + dplyr::lag(points, 2) + dplyr::lag(points, 1)) %>%
  mutate(dip = points_b_1_6/6 - points_b_7_12/6,
         points_b_1= dplyr::lag(points)) %>%
  ungroup()

# summary stats of dip when intervention
data_balanced %>%
  filter(intervention == 1) %>%
  select(points_b_1_6, points_b_7_12, dip, points_b_1) %>%
  summary()

# set ranges to define control group
points_b_7_12min = 6.5
points_b_7_12max = 7.8
dipmin = -2.5
dipmax = -0
points_b_1min = 0
points_b_1max = 1

# define pseudo-intervention
data_balanced <- data_balanced %>%
  mutate(pseudo = as.numeric(countmanagchange==0 & dip>=dipmin & dip<=dipmax
                   & points_b_7_12>=points_b_7_12min & points_b_7_12<=points_b_7_12max
                   & points_b_1>=points_b_1min & points_b_1<=points_b_1max
                   & gameno < (38-12) # games with 12 left in the season
                   ))

table(data_balanced$pseudo, useNA = "always")
data_balanced %>%
  filter(pseudo == 1) %>%
  select(points_b_7_12, dip, points_b_1) %>%
  summary()

# if more such games in a teamXseason, choose one randomly

data_balanced <- chooseRandomPseudo(data_balanced)

table(data_balanced$pseudo, useNA = "always")
data_balanced %>%
  filter(pseudo == 1) %>%
  select(points_b_7_12, dip, points_b_1) %>%
  summary()

data_balanced <- data_balanced %>%
  group_by(team, season) %>%
  mutate(countpseudo = sum(pseudo, na.rm = TRUE)) %>%
  mutate(pseudo_time = min(ifelse(pseudo == 1, gameno, NA), na.rm = TRUE)) %>%
  mutate(t_pseudo = ifelse(is.infinite(pseudo_time), NA, gameno - pseudo_time)) %>%
  mutate(t_pseudo = ifelse(t_pseudo>=0 & t_pseudo<=38, t_pseudo+1, t_pseudo)) %>%
  mutate(t_event = ifelse(is.na(t_event), t_pseudo, t_event)) %>%
  ungroup() %>%
  filter(t_event>=-12 & t_event<=12)

table(data_balanced$intervention, data_balanced$pseudo)
data_balanced %>%
  group_by(countinterv, countpseudo) %>%
  summarise(n_distinct(team, season))

data_balanced_wizard <- data_balanced %>%
  filter(intervention == 1) 
hist(data_balanced_wizard$points_b_1)

# FIGURE with intervention and pseudo-intervention averages
p5<-getPointsGraphWithPseudo(data_balanced, colors = color,num=12)
p5
save_fig("abra_all_kd_theme", output, "large")

# plot
p6 <- data_balanced_wizard %>%
  ggplot( aes(x=points_b_1)) +
  geom_histogram( binwidth=1, fill="#1D5C63FF", color="#e9ecef", alpha=0.9) +
  theme_ipsum() +
  xlab("menedzserváltás előtti meccsen szerzett pontok eloszlása") +
  ylab("Gyakoriság") +
  theme(
    plot.title = element_text(size=15)
  )
p6
save_fig("results_last_match_kd_theme", output, "large")

# REGRESSION with 6-game averages
data_balanced_agg_plusvars <- data_balanced %>%
  mutate(teamseason = factor(paste0(team, season))) %>%
  group_by(teamseason, t6_event = droplevels(cut(t_event, c(-13,-6,0, 1,7,13), right = FALSE))) %>%
  summarise(treated = mean(countinterv), 
            points6avg = round(mean(points), 6), 
            season_year = mean(season) + 2000,
            manager_quality = mean(mean_points_for_manager),
            near_relegation_for_percent_of_matches = mean(near_relegation)*100,
            near_relegation_for_min_1_match = ifelse(mean(near_relegation)>0,1,0),
            team = team,
            league = league) %>%
  arrange(teamseason, t6_event) %>%
  distinct() %>% 
  group_by(teamseason) %>%
  mutate(Dp6avg = points6avg - dplyr::lag(points6avg)) %>%
  ungroup()  

data_balanced_agg_plusvars <- cbind(data_balanced_agg_plusvars,
                           sapply(levels(data_balanced_agg_plusvars$t6_event),
                                  function(x) as.integer(x == data_balanced_agg_plusvars$t6_event)))

colnames(data_balanced_agg_plusvars)[12:15] <- c("before_7_12", "before_1_6", "after_1_6", "after_7_12")

data_balanced_agg_plusvars <- data_balanced_agg_plusvars %>% mutate(after_1_6_treat = after_1_6 * treated,
                                                                    after_7_12_treat = after_7_12 * treated)


data_balanced_agg_plusvars %>%
  select(Dp6avg, after_1_6, after_7_12) %>%
  summary()

# FD REGRESSIONS for thesis table 1
fd_treatment <- lm(Dp6avg ~ after_1_6 + after_7_12, 
                   data = filter(data_balanced_agg_plusvars, treated == 1))
fd_control <- lm(Dp6avg ~ after_1_6 + after_7_12, 
                 data = filter(data_balanced_agg_plusvars, treated == 0))
fd <- lm(Dp6avg ~ after_1_6 + after_7_12 + treated + I(treated*after_1_6) + I(treated*after_7_12),
                data = data_balanced_agg_plusvars)

summary(fd_treatment)
summary(fd_control)
summary(fd)

stargazer_r(list(fd_treatment, fd_control, fd), se = 'traditional', 
            float=T, digits=3, 
            column.labels   = c("1. modell", "2. modell", "3. modell"),
            model.numbers          = FALSE,
            covariate.labels = c("1-6 utána", "7-12 utána", "kezelt",
                                 "kezelt, 1-6 utána ", "kezelt, 7-12 utána ", "konstans"),
            dep.var.caption  = "<em>Függő változó</em>",
            dep.var.labels   = "A 6 meccses átlagpontok elsőfokú differenciája",
            type = "html",
            out="C:/Rajk - újgép/Szakdolgozat Közgáz Elemző/final_regression_outputs/thesis_table_1.html")

# FD REGRESSIONS for thesis table 2

fd <- lm(Dp6avg ~ after_1_6 + after_7_12 + treated + after_1_6_treat + after_7_12_treat,
         data = data_balanced_agg_plusvars)

fd_clustered_sd<- lm_robust(Dp6avg ~ after_1_6 + after_7_12 + treated + after_1_6_treat + after_7_12_treat,
                                    data = data_balanced_agg_plusvars,
                                    se_type = "stata", 
                                    clusters = team)

fd_clustered_sd_with_y <- lm_robust(Dp6avg ~ after_1_6 + after_7_12 + treated + after_1_6_treat + after_7_12_treat 
                                    + season_year,
                             data = data_balanced_agg_plusvars,
                             se_type = "stata", 
                             clusters = team)

fd_clustered_sd_with_y_c <- lm_robust(Dp6avg ~ after_1_6 + after_7_12 + treated + after_1_6_treat + after_7_12_treat 
                                      + season_year + manager_quality,
                                    data = data_balanced_agg_plusvars,
                                    se_type = "stata", 
                                    clusters = team)

tab<-huxreg('1. modell' = fd, 
             '2. modell' = fd_clustered_sd,
             '3. modell' = fd_clustered_sd_with_y, 
             '4. modell' = fd_clustered_sd_with_y_c,
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.10),
             statistics = c(`Megfigyelések száma` = "nobs", `R^2` = "r.squared"), 
             #omit_coefs = c(paste("year", levels(data_balanced$year), sep= ""), paste("c", levels(data_balanced$c), sep= "")),
             coefs = c("Rövidtávú kezelési hatás" = "after_1_6_treat",
                       "Hosszútávú kezelési hatás" = "after_7_12_treat")
)

# produce table 2
thesis_table_2 <- tab %>%
  insert_row(c("Klaszterezett standard hibák", "Nem", "Igen", "Igen", "Igen"), after = 5) %>%
  insert_row(c("Szezon dummy-k", "Nem", "Nem", "Igen", "Igen"), after = 6) %>%
  insert_row(c("Menedzseri minőség kontrollal", "Nem", "Nem", "Nem", "Igen"), after = 7)


quick_html(thesis_table_2, file = "C:/Rajk - újgép/Szakdolgozat Közgáz Elemző/final_regression_outputs/thesis_table_2.html")
