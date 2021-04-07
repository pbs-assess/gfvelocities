setwd(here::here())

library(TMB)
library(tidyverse)
library(patchwork)
library(gfranges)

model <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-11-trend-with-do-family-family-1-500.rds")

temp_slopes <- chopstick_slopes(model,
  x_variable = "temp_trend_scaled",
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
)

p_temp_worm <- plot_chopstick_slopes(temp_slopes,
  type = "temp",
  legend_position = c(.3, .95),
  add_grey_bars = T
) 