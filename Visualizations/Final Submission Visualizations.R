# random libraries
library(tidyverse) 
library(rvest)
library(robotstxt)
library(dplyr)
library(vtable)
library(gtsummary)
library(gmodels)
library(sjPlot)
library(xtable)
library(psych)
library(car)
library(olsrr)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(Dict)
library(stargazer)
library(xgboost)
library(fastDummies)
library(GPArotation)
library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(DT)
library(glue)
library(xgboostExplainer)
library(pROC)
library(SHAPforxgboost)
library(data.table)
library(caret)
library(shapviz)
library(DALEX)
library(readxl)
library(ggdark) # Load ggdark
library(RCurl) # Load RCurl
library(grid) # Load grid
library(jpeg) # Load jpeg
library(gganimate) # Load gganimate
library(transformr)
library(ggridges) # Load ggridges
library(png)
library(ggimage)
source('~/Desktop/MSBA-SA/The Book Project v_3/Source Files/pbp_functions.r')

# avoid scientific notation
options(scipen = 9999)

load('~/Desktop/MSBA-SA/The Book Project v_3/Final Submission/Model Building Scripts and Code/Data Files/nfl_pbp_xgboost_data_v1_dummy_cols.rda')

nfl_pbp_data_v1 <- nfl_pbp_xgboost_data_v1_dummy_cols

nfl_pbp_data_v1_viz_holder <- nfl_pbp_data_v1

# undo the dummy columns for play_type
nfl_pbp_data_v1_viz_holder$play_type <- 'NA'
nfl_pbp_data_v1_viz_holder$play_type[nfl_pbp_data_v1_viz_holder$play_type_pass == 1] <- 'pass'
nfl_pbp_data_v1_viz_holder$play_type[nfl_pbp_data_v1_viz_holder$play_type_run == 1] <- 'run'
nfl_pbp_data_v1_viz_holder$play_type[nfl_pbp_data_v1_viz_holder$play_type_punt == 1] <- 'punt'
nfl_pbp_data_v1_viz_holder$play_type[nfl_pbp_data_v1_viz_holder$play_type_field_goal == 1] <- 'field_goal'

# undo the dummy columns for score_diff_cat_score_diff_
colnames_score_diff <- colnames(nfl_pbp_data_v1_viz_holder)[grepl('score_diff_cat_score_diff_', colnames(nfl_pbp_data_v1_viz_holder))]
nfl_pbp_data_v1_viz_holder$score_diff_cat <- 'NA'
for(colname_i in colnames_score_diff){
  nfl_pbp_data_v1_viz_holder$score_diff_cat[nfl_pbp_data_v1_viz_holder[, colname_i] == 1] <- colname_i %>% str_remove_all('score_diff_cat_score_diff_')
  # nfl_pbp_data_v1_viz_holder[, colname_i][nfl_pbp_data_v1_viz_holder[, colname_i] == 1] <- as.numeric(strsplit(colname_i, '_')[[1]][length(strsplit(colname_i, '_')[[1]])])
}

# undo the dummy columns for cat_game_time_
colnames_game_time <- colnames(nfl_pbp_data_v1_viz_holder)[grepl('cat_game_time_', colnames(nfl_pbp_data_v1_viz_holder))]
nfl_pbp_data_v1_viz_holder$game_time_cat <- 'NA'
for(colname_i in colnames_game_time){
  nfl_pbp_data_v1_viz_holder$game_time_cat[nfl_pbp_data_v1_viz_holder[, colname_i] == 1] <- colname_i %>% str_remove_all('cat_game_time_')
}

nfl_pbp_data_v1_viz_holder$game_time_cat <- factor(nfl_pbp_data_v1_viz_holder$game_time_cat, levels = c('first_qtr', 'second_qtr_15_4_mins', 'second_qtr_4_2_mins', 'second_qtr_2_0_mins', 'third_qtr', 'fourth_qtr_15_7_mins', 'fourth_qtr_7_4_mins', 'fourth_qtr_4_2_mins', 'fourth_qtr_2_0_mins'))


# violin plot
nfl_pbp_data_v1_viz_holder$pos_neg <- ifelse(nfl_pbp_data_v1_viz_holder$wpa >= 0.0, 'positive', 'negative')

viz_holder <- ggplot() +
  geom_violin(data = nfl_pbp_data_v1_viz_holder %>% filter(pos_neg == 'positive'), mapping = aes(x = wpa, y = play_type, color = pos_neg, fill = pos_neg)) +
  geom_violin(data = nfl_pbp_data_v1_viz_holder %>% filter(pos_neg == 'negative'), mapping = aes(x = wpa, y = play_type, color = pos_neg, fill = pos_neg)) +
  scale_color_manual(values = c("positive" = "blue", "negative" = "red")) +
  scale_fill_manual(values = c("positive" = "blue", "negative" = "red")) +
  # facet_wrap(.~game_time_cat, labeller = labeller(qtr =c("1" = "1st Quarter", "2" = "2nd Quarter", "3" = "3rd Quarter", "4" = "4th Quarter"))) +
  # facet_wrap(.~factor(game_time_cat, levels = c('first_qtr', 'second_qtr_15_4_mins', 'second_qtr_4_2_mins', 'second_qtr_2_0_mins', 'third_qtr', 'fourth_qtr_15_7_mins', 'fourth_qtr_7_4_mins', 'fourth_qtr_4_2_mins', 'fourth_qtr_2_0_mins')), labeller = labeller(game_time_cat = c('first_qtr' = '1st Quarter', 'second_qtr_15_4_mins' = '2nd Quarter 15 to 4 minutes left', 'second_qtr_4_2_mins' = '2nd Quarter 4 to 2 minutes left',  'second_qtr_2_0_mins' = '2nd Quarter 2 to 0 minutes left',  'third_qtr' = '3rd Quarter',  'fourth_qtr_15_7_mins' = '4th Quarter 15 to 7 minutes left',  'fourth_qtr_7_4_mins' = '4th Quarter 7 to 4 minutes left', 'fourth_qtr_4_2_mins' = '4th Quarter 4 to 2 minutes left' ,  'fourth_qtr_2_0_mins' = '4th Quarter 2 to 0 minutes left'))) + # 'first_qtr', 'second_qtr_15_4_mins', 'second_qtr_4_2_mins', 'second_qtr_2_0_mins', 'third_qtr', 'fourth_qtr_15_7_mins', 'fourth_qtr_7_4_mins', 'fourth_qtr_4_2_mins', 'fourth_qtr_2_0_mins'
  facet_wrap(.~factor(game_time_cat, levels = c('first_qtr', 'second_qtr_15_4_mins', 'second_qtr_4_2_mins', 'second_qtr_2_0_mins', 'third_qtr', 'fourth_qtr_15_7_mins', 'fourth_qtr_7_4_mins', 'fourth_qtr_4_2_mins', 'fourth_qtr_2_0_mins'))) +
  dark_theme_bw() +
  theme(panel.grid.major = element_blank(), # Turn of the background grid
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  ylab('Play Type') +
  xlab('Win Probability Added')

print(viz_holder)

ggsave('~/Desktop/MSBA-SA/The Book Project v_3/Final Submission/Visualizations/wpa_violin_by_play_type_game_time_cat.png', width = 10, height = 10, units = 'in', dpi = 300)


# # now do the same thing but for wpa by play_type
# viz_holder <- ggplot() +
#   geom_violin(data = nfl_pbp_data_v1_viz_holder %>% filter(pos_neg == 'positive'), mapping = aes(y = wpa, x = play_type, color = pos_neg, fill = pos_neg)) +
#   geom_violin(data = nfl_pbp_data_v1_viz_holder %>% filter(pos_neg == 'negative'), mapping = aes(y = wpa, x = play_type, color = pos_neg, fill = pos_neg)) +
#   scale_color_manual(values = c("positive" = "blue", "negative" = "red")) +
#   scale_fill_manual(values = c("positive" = "blue", "negative" = "red")) +
#   facet_wrap(.~play_type) +
#   dark_theme_bw() +
#   theme(panel.grid.major = element_blank(), # Turn of the background grid
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         legend.position = "none") +
#   xlab('Play Type') +
#   ylab('Win Probability Added')
# 
# print(viz_holder)
# 
# ggsave('~/Desktop/MSBA-SA/The Book Project v_3/Final Submission/Visualizations/wpa_violin_by_play_type.png', width = 10, height = 10, units = 'in', dpi = 300)
# 


viz_holder <- nfl_pbp_data_v1_viz_holder %>% 
  ggplot() + 
  geom_point(mapping = aes(x = offense_run_elo_pre_game_pos, y = run_epa_cumulative_lagged_1_week)) +
  geom_smooth(mapping = aes(x = offense_run_elo_pre_game_pos, y = run_epa_cumulative_lagged_1_week), method = 'lm') +
  dark_theme_bw() + 
  labs(x = 'Offense Run Elo Pre Game', y = 'Run EPA Cumulative Lagged 1 Week')

print(viz_holder)

ggsave('~/Desktop/MSBA-SA/The Book Project v_3/Final Submission/Visualizations/offense_run_elo_pre_game_pos_vs_run_epa_cumulative_lagged_1_week.png', width = 10, height = 10, units = 'in', dpi = 300)


# # show data table
# load('~/Desktop/MSBA-SA/The Book Project v_3/Final Model v0/Game Prediction/Data Files/specific_game_pred_2023_13_atl_vs_no.rda')
# # colnames_focus <- colnames(specific_game_pred_2023_13_atl_vs_no)[colnames(specific_game_pred_2023_13_atl_vs_no) %>% str_to_lower() %>% str_detect('elo|epa|lagged')]
# # # get the indicides of the colnmaes focus
# # colnames_focus_indicies <- colnames(specific_game_pred_2023_13_atl_vs_no) %>% str_to_lower() %>% str_detect('elo|epa|lagged') %>% which()
# # specific_game_pred_2023_13_atl_vs_no[1,colnames_focus] %>% datatable(options = list(autoWidth = TRUE,
# #                                                                           columnDefs = list(list(className = 'wrap', targets = "_all")),
# #                                                                           pageLength = 5),
# #                                                        callback = JS("table.column('colnames_focus_indicies').nodes().to$().css({wordWrap: 'break-word', whiteSpace: 'normal'});"))
# # 
# 
# # pivot the data set to show the measures for possession team (falcons) vs. not possesion team (saints)
# specific_game_pred_2023_13_atl_vs_no_filt <- specific_game_pred_2023_13_atl_vs_no[1,] %>% select(all_of(colnames_focus))
# specific_game_pred_2023_13_atl_vs_no_filt_atl_pos <- specific_game_pred_2023_13_atl_vs_no_filt %>% select(contains('pos'))
# specific_game_pred_2023_13_atl_vs_no_filt_atl_epa <- specific_game_pred_2023_13_atl_vs_no_filt %>% select(contains('epa'))
# specific_game_pred_2023_13_atl_vs_no_filt_atl_lagged <- specific_game_pred_2023_13_atl_vs_no_filt %>% select(contains('lagged'))
# specific_game_pred_2023_13_atl_vs_no_filt_atl <- cbind(specific_game_pred_2023_13_atl_vs_no_filt_atl_pos, specific_game_pred_2023_13_atl_vs_no_filt_atl_epa, specific_game_pred_2023_13_atl_vs_no_filt_atl_lagged)



# look for ideas for other plots
# https://opensourcefootball.com/posts/2020-09-28-nflfastr-ep-wp-and-cp-models/

# undo the yardline_100
nfl_pbp_data_v1_viz_holder$yardline_100_cat <- 
  ifelse(nfl_pbp_data_v1_viz_holder$yardline_100 <= 5,'yardline_1_5',
         ifelse(nfl_pbp_data_v1_viz_holder$yardline_100 <= 15, 'yardline_6_15',
                ifelse(nfl_pbp_data_v1_viz_holder$yardline_100 <= 25, 'yardline_16_25',
                       ifelse(nfl_pbp_data_v1_viz_holder$yardline_100 <= 38, 'yardline_26_38',
                              ifelse(nfl_pbp_data_v1_viz_holder$yardline_100 <= 45, 'yardline_39_45',
                                     ifelse(nfl_pbp_data_v1_viz_holder$yardline_100 <= 60, 'yardline_46_60', 'yardline_61_99'))))))



# make a distribution for wpa_pred by the the conitnous and categorical columns
# nfl_pbp_data_v1_viz_holder
nfl_pbp_data_v1_viz_holder_filt <- nfl_pbp_data_v1_viz_holder %>% 
  filter(down == 4)
nfl_pbp_data_v1_viz_holder_wpa_continous_yardline_100 <- nfl_pbp_data_v1_viz_holder_filt %>% 
  group_by(yardline_100, yardline_100_cat) %>% 
  summarize(mean_wpa = mean(wpa, na.rm = TRUE))

nfl_pbp_data_v1_viz_holder_wpa_categorical_yardline_100 <- nfl_pbp_data_v1_viz_holder_filt %>% 
  group_by(yardline_100_cat) %>% 
  summarize(mean_wpa = mean(wpa, na.rm = TRUE))

# now merge yardline_100 back on this
nfl_pbp_data_v1_viz_holder_wpa_categorical_yardline_100 <- merge(nfl_pbp_data_v1_viz_holder_wpa_categorical_yardline_100, nfl_pbp_data_v1_viz_holder_wpa_continous_yardline_100 %>% select(yardline_100, yardline_100_cat), by = 'yardline_100_cat', all.x = TRUE)

plot_holder <- ggplot() +
  geom_line(data = nfl_pbp_data_v1_viz_holder_wpa_continous_yardline_100, mapping = aes(x = yardline_100, y = mean_wpa)) +
  geom_col(data = nfl_pbp_data_v1_viz_holder_wpa_categorical_yardline_100, mapping = aes(x = yardline_100, y = mean_wpa),fill = 'white', width = 1, alpha = 0.5) +
  dark_theme_bw() +
  theme(panel.grid.major = element_blank(), # Turn of the background grid
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x = 'Yardline / Yardline Categorical', y = 'Mean WPA', title = 'Visualizing the WPA by yardline & yardline bins ', subtitle = 'Line : Yardline | Bars : Yardline Categorical')

ggsave(plot_holder, filename = '~/Desktop/MSBA-SA/The Book Project v_3/Final Submission/Visualizations/wpa_by_yardline_and_yardline_cat.png', width = 10, height = 10, units = 'in', dpi = 300)

# get some historical play type for the given situation
nfl_pbp_data_v1_viz_holder_filt <- nfl_pbp_data_v1_viz_holder %>% 
  filter(down == 4, ydstogo == 3, yardline_100_cat == 'yardline_46_60', game_time_cat == 'fourth_qtr_7_4_mins')

nfl_pbp_data_v1_viz_holder_filt_count <- nfl_pbp_data_v1_viz_holder_filt %>% 
  summarize(Pass = sum(play_type_pass),
            Run = sum(play_type_run),
            Punt = sum(play_type_punt),
            `Field Goal` = sum(play_type_field_goal))

nfl_pbp_data_v1_viz_holder_filt_count <- nfl_pbp_data_v1_viz_holder_filt_count %>% 
  pivot_longer(cols = c(`Field Goal`, 'Punt', 'Pass', 'Run'), names_to = 'play_type', values_to = 'count')

plot_holder <- ggplot() +
  geom_col(data = nfl_pbp_data_v1_viz_holder_filt_count, mapping = aes(x = factor(play_type, levels = c('Field Goal', 'Punt', 'Pass', 'Run')), y = count), fill = 'white', width = 0.8) +
  geom_text(data = nfl_pbp_data_v1_viz_holder_filt_count, mapping = aes(x = factor(play_type, levels = c('Field Goal', 'Punt', 'Pass', 'Run')), y = count, label = as.character(count)), vjust = -0.5) +
  dark_theme_bw() +
  # scale_y_continuous(limits = c(0, 2 * max(nfl_pbp_data_v1_viz_holder_filt_count$count))) +
  theme(panel.grid.major = element_blank(), # Turn of the background grid
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x = 'Play Type', y = 'Count', title = 'Visualizing the Play Type by Situation', subtitle = 'Down : 4 | Yards To Go : 3 | Yardline : 46 to 60 | Game Time : 7 to 4 minutes left')

print(plot_holder)

ggsave(plot_holder, filename = '~/Desktop/MSBA-SA/The Book Project v_3/Final Submission/Visualizations/play_type_by_situation.png', width = 10, height = 6, units = 'in', dpi = 300)


# get how many times teams go for it inside their own 40
load('~/Desktop/MSBA-SA/The Book Project v_3/Final Submission/Model Building Scripts and Code/Data Files/nfl_pbp_data_v1_filt.rda')


nfl_pbp_data_v1_filt_v2 <- nfl_pbp_data_v1_filt %>% 
  filter(down == 4, yardline_100 > 60)

nfl_pbp_data_v1_filt_v2 <- nfl_pbp_data_v1_filt_v2 %>% 
  filter(abs(score_differential) <= 16)

table(nfl_pbp_data_v1_filt_v2$play_type)

# get it in %
table(nfl_pbp_data_v1_filt_v2$play_type) / nrow(nfl_pbp_data_v1_filt_v2)

nfl_pbp_data_v1_filt_v2 <- nfl_pbp_data_v1_filt_v2 %>% 
  filter(abs(score_differential) <= 16, game_seconds_remaining >= 120)

table(nfl_pbp_data_v1_filt_v2$play_type)

# get it in %
table(nfl_pbp_data_v1_filt_v2$play_type) / nrow(nfl_pbp_data_v1_filt_v2)
