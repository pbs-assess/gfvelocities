

if (!require(patchwork)) install.packages("patchwork")
if (!require(ggpubr)) install.packages("ggpubr")

library(TMB)
library(tidyverse)
library(gfranges)
library(patchwork)
library(ggpubr)

setwd(here::here("analysis", "VOCC"))

model <- readRDS("data/trend-all-95-all-newclim-06-25-trend-with-do-1-500.rds") # full dataset

# stats <- readRDS(paste0("data/life-history-behav.rds"))

# stats_old <- readRDS(paste0("data/life-history-behav-old-growth.rds"))
# plot(stats_old$age_mat~stats$age_mat)

model2 <- add_colours(model$coefs, species_data = stats)

temp_slopes <- chopstick_slopes(model,
  x_variable = "temp_trend_scaled",
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
)
do_slopes <- chopstick_slopes(model,
  x_variable = "DO_trend_scaled",
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO"
)

temp_slopes1 <- left_join(temp_slopes, stats)
do_slopes1 <- left_join(do_slopes, stats)

long_slopes <- rbind(temp_slopes1, do_slopes1) %>% mutate(slope_type = paste(chopstick, type)) %>% ungroup()

long_slopes <- long_slopes %>% mutate(
  type = factor(type, levels = c("temp", "DO")),
  species_age = paste(species, age),
  age = factor(age, levels = c("immature", "mature"),  labels = c("Immature", "Mature")),
  Rockfish = factor(rockfish, levels = c("rockfish", "other fishes"),  labels = c("Rockfish", "Other fishes")),
  slope_type = factor(slope_type, levels = c("high temp", "low temp", "high DO", "low DO")),
  Diet = factor(Diet, levels = c("Zooplankton", "Generalist", "Polychaetes", "Crustaceans", "Fish")),
  Zone = factor(BenthoPelagicPelagicDemersal, levels = c("Demersal", "Benthopelagic", "Pelagic")),
  Latitude = factor(NorthMiddleSouth, levels = c("North", "Middle", "South")),
  Schooling = as.factor(Schooling),
  Trophic = factor(if_else(Diet == "Zooplankton", "Lower", "Higher"), levels = c("Lower", "Higher")),
  Specialist = factor(if_else(Diet == "Generalist", "Generalist", "Specialist"), levels = c("Generalist", "Specialist")),
  depth_iqr_scaled = scale(log(depth_iqr), center = T),
  depth_mean_scaled = scale((depth), center = T),
  log_age_scaled = scale(log(age_mean + 1), center = T),
  max_mass_scaled = scale(log(weight_99th + 1), center = T),
  # age_mat is the 95 quantile of ages for immature females
  growth_rate_scaled = scale(log((length_50_mat_f / age_mat)+1), center = T)
  
  )

# collapse Middle and South together because only 3 "southern" species
long_slopes$Latitude[long_slopes$Latitude == "Middle"] <- "South"

# collapse Pelagic into Benthopelagic because only 3 "pelagic" species
long_slopes$Zone[long_slopes$Zone == "Pelagic"] <- "Benthopelagic"

long_slopes <- long_slopes %>% mutate(
  Zone = factor(Zone, levels = c("Demersal", "Benthopelagic")),
  Latitude = factor(Latitude, levels = c("North", "South"))
)

long_slopes <- long_slopes %>% group_by(slope_type) %>% mutate(slope_trim = collapse_outliers(slope, c(0.005, 0.995))) %>% ungroup()
# long_slopes <- long_slopes %>% group_by(slope_type) %>% mutate(slope_trim = collapse_outliers(slope, c(0.025, 0.975))) %>% ungroup()
# long_slopes <- long_slopes %>% rename(slope_raw = slope, slope = slope_trim)

temp_slopes <- long_slopes %>% filter(type == "temp")
do_slopes <- long_slopes %>% filter(type == "DO") %>% mutate (chopstick = factor(chopstick, levels = c("low", "high")))

#### Which slopes have non-zero effects? ####
temp_slopes %>% lmerTest::lmer(slope ~ 0 + chopstick + (1|species) + (1|species_age), data = .) %>% summary() # *
do_slopes %>% lmerTest::lmer(slope ~ 0 + chopstick + (1|species) + (1|species_age), data = .) %>% summary() # *

# high temp and low DO have sig non-zero effects (in both cases negative) 
best_slopes <- long_slopes %>% filter(slope_type == "high temp" | slope_type == "low DO") 


#### paired test of slopes by age ####
# no sig differences by age

matched_temp <- long_slopes %>% filter(species != "Pacific Halibut") %>% filter(species != "Curlfin Sole") %>% filter(species != "Shortbelly Rockfish") %>% filter(species != "Shortraker Rockfish") %>% filter(species != "Spotted Ratfish") %>% filter(parent_taxonomic_unit != "rajidae(skates)") %>% filter(type == "temp")
compare_means(slope~age, matched_temp, method = "wilcox.test", paired = T, id = "species")

matched_do <- long_slopes %>% filter(species != "Pacific Halibut") %>% filter(species != "Curlfin Sole") %>% filter(species != "Shortbelly Rockfish") %>% filter(species != "Shortraker Rockfish") %>% filter(species != "Spotted Ratfish") %>% filter(parent_taxonomic_unit != "rajidae(skates)") %>% filter(type == "DO")
compare_means(slope~age, matched_do, method = "wilcox.test", paired = T, id = "species")

matched <- rbind(matched_temp, matched_do)
ggpaired(matched, x = "age", y = "slope",  color = "slope_type", id = "species", ylab = "Slopes from trend-based model", line.color = "gray", line.size = 0.4, facet.by = c("slope_type") ) + 
  stat_compare_means(
    aes(label = paste0("p = ", ..p.format..)),
    # aes(label = paste0(..p.signif..)), 
    method = "t.test", 
    paired = TRUE, 
    ref.group = NULL) + scale_colour_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan")) + 
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  facet_grid(cols = vars(chopstick), rows = vars(type), scales = "free") +
  # facet_grid(cols = vars(slope_type), scales = "free") +
  theme(legend.position = "none")

ggsave(here::here("ms", "figs", "supp-age-effect-by-slope.pdf"), width = 7, height = 4)


#### Is there a difference in strength of effect between high and low slopes? ####
# paired across species and ages - NO
compare_means(slope~chopstick, temp_slopes, method = "wilcox.test", paired = T, id= "species_age") # **
compare_means(slope~chopstick, do_slopes, method = "wilcox.test", paired = T, id= "species_age") # **

# ## split by age classes 
# ggpaired(long_slopes, x = "chopstick", y = "slope",  color = "slope_type", id = "species_age", ylab = "Slope", line.color = "gray", line.size = 0.4, facet.by = c("type", "age") ) + 
#   stat_compare_means(
#     aes(label = paste0("p = ", ..p.format..)),
#     # aes(label = paste0(..p.signif..)), 
#     # method = "t.test",
#     method = "wilcox.test",
#     paired = TRUE, 
#     ref.group = NULL) + scale_colour_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan")) + 
#   geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
#   facet_grid(rows = vars(type), cols = vars(age), scales = "free") + theme(legend.position = "none")


#### within more extreme slopes test test unpaired effects of ecology ####
# t test
#temp
compare_means(slope~Latitude, filter(temp_slopes, chopstick == "high"), method = "t.test") # * North - Middle
compare_means(slope~Rockfish, filter(temp_slopes, chopstick == "high"), method = "t.test")
compare_means(slope~Schooling, filter(temp_slopes, chopstick == "high"), method = "t.test")
compare_means(slope~Zone, filter(temp_slopes, chopstick == "high"), method = "t.test") # * Demersal - Benthopelagic
compare_means(slope~Diet, filter(temp_slopes, chopstick == "high"), method = "t.test") # * Zooplankton - Crustaceans
compare_means(slope~Trophic, filter(temp_slopes, chopstick == "high"), method = "t.test") # ns
compare_means(slope~Specialist, filter(temp_slopes, chopstick == "high"), method = "t.test") 

# DO
compare_means(slope~Latitude, filter(do_slopes, chopstick == "low"), method = "t.test") # ** 
compare_means(slope~Rockfish, filter(do_slopes, chopstick == "low"), method = "t.test")
compare_means(slope~Schooling, filter(do_slopes, chopstick == "low"), method = "t.test") # ***
compare_means(slope~Zone, filter(do_slopes, chopstick == "low"), method = "t.test") # ***
compare_means(slope~Diet, filter(do_slopes, chopstick == "low"), method = "t.test") # *
compare_means(slope~Trophic, filter(do_slopes, chopstick == "low"), method = "t.test") # **
compare_means(slope~Specialist, filter(do_slopes, chopstick == "low"), method = "t.test") 

# wilcox test
#temp
compare_means(slope~Latitude, filter(temp_slopes, chopstick == "high"), method = "wilcox.test")
compare_means(slope~Rockfish, filter(temp_slopes, chopstick == "high"), method = "wilcox.test")
compare_means(slope~Schooling, filter(temp_slopes, chopstick == "high"), method = "wilcox.test")
compare_means(slope~Zone, filter(temp_slopes, chopstick == "high"), method = "wilcox.test") 
compare_means(slope~Diet, filter(temp_slopes, chopstick == "high"), method = "wilcox.test") 
compare_means(slope~Trophic, filter(temp_slopes, chopstick == "high"), method = "wilcox.test") 
compare_means(slope~Specialist, filter(temp_slopes, chopstick == "high"), method = "wilcox.test") 

# DO
compare_means(slope~Latitude, filter(do_slopes, chopstick == "low"), method = "wilcox.test") # **
compare_means(slope~Rockfish, filter(do_slopes, chopstick == "low"), method = "wilcox.test")
compare_means(slope~Schooling, filter(do_slopes, chopstick == "low"), method = "wilcox.test") # ***
compare_means(slope~Zone, filter(do_slopes, chopstick == "low"), method = "wilcox.test") # ***
compare_means(slope~Diet, filter(do_slopes, chopstick == "low"), method = "wilcox.test") 
compare_means(slope~Trophic, filter(do_slopes, chopstick == "low"), method = "wilcox.test") # **
compare_means(slope~Specialist, filter(do_slopes, chopstick == "low"), method = "wilcox.test") 


#### TIME & GROWTH RATE EFFECTS ####
# for immature only
temp_slopes <- temp_slopes %>% mutate(age = factor(age, levels = c("Mature", "Immature")) )
do_slopes <- do_slopes %>% mutate(age = factor(age, levels = c("Mature", "Immature")) )

ddat1 <- temp_slopes %>% select(slope, slope_est, slope_se, slope_trim,
  age, depth_mean_scaled,  depth_iqr_scaled, Zone,
  log_age_scaled, growth_rate_scaled, max_mass_scaled,
  Latitude, Trophic, chopstick, species, species_age) 

ddat2 <- ddat1 #%>% Hmisc::na.delete() %>% filter(age == "Immature")

ddat1do <- do_slopes %>% select(slope, slope_est, slope_se, slope_trim,
  age, depth_iqr_scaled, depth_mean_scaled,
  log_age_scaled, growth_rate_scaled, max_mass_scaled,
  Zone, Latitude, Trophic, 
  chopstick, species, species_age) 

ddat2do <- ddat1do #%>% Hmisc::na.delete() %>% filter(age == "Immature")

###################
# should raw data points be trimmed ones?
# if yes, run this 
ddat2 <- ddat2 %>% mutate(slope_est = slope_trim)
ddat2do <- ddat2do %>% mutate(slope_est = slope_trim)
# # # # # if no, run this
# ddat2 <- ddat2 %>% mutate(slope_est = slope)
# ddat2do <- ddat2do %>% mutate(slope_est = slope)



#### TEMPERATURE WITH AGE PLOTS ####
#### for immature only ####
# MEAN DEPTH ####
tempslopemod <- lmerTest::lmer(slope_est ~ 1 +
  depth_mean_scaled + chopstick +
  depth_mean_scaled * chopstick +
  (1|species),  REML = T, data = filter(ddat2, age == "Immature"))
  # (1|species_age) + (1|species),  REML = T, data = ddat2)
summary(tempslopemod)

(p_depth_mean <- temp_interact_plot(
  tempslopemod,
  # data = ddat2, include_mat = T,  
  data = filter(ddat2, age == "Immature"), include_mat = F, 
  # trend = T, y_limits = c(-7,2), # 99% trim
  trend = T, y_limits = c(-5,2), # 95% trim
  pred = "depth_mean_scaled"
) + xlab("Mean depth") +
    # ylab("Biomass change ~ warming") +
    # ylab(expression(~italic("Y")~"~ temperature trend")) +
    ylab(expression(~italic("Y")~"~ warming trend")) + 
    # labs(tag = "D") + 
    scale_colour_manual(values = c("white", "white")) +
    theme(  
      plot.margin = margin(0.2, 0.2, 0, 0.3, "cm"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      legend.title = element_blank(),
      # legend.position = c(0.2,0.85))
      legend.position = "none")
)
  
###
# DEPTH RANGE #### 
tempslopemodr <- lmerTest::lmer(slope_est ~ 1 + 
    depth_iqr_scaled + chopstick +
    depth_iqr_scaled * chopstick +
    (1|species),  REML = T, data = filter(ddat2, age == "Immature"))
    # (1|species_age) + (1|species),  REML = T, data = ddat2)
summary(tempslopemodr)

(p_depth_iqr <- temp_interact_plot(
  tempslopemodr,
  # data = ddat2, include_mat = T,
  data = filter(ddat2, age == "Immature"), include_mat = F,
  # trend = T, y_limits = c(-7,2), # 99% trim
  trend = T, y_limits = c(-5,2), # 95% trim
  pred = "depth_iqr_scaled"
) + xlab("Depth range (IQR)") +
    # ylab("Biomass change ~ warming") +
    # ylab(expression(~italic("Y")~"~ temperature trend")) +
    ylab(expression(~italic("Y")~"~ warming trend")) + 
    # labs(tag = "D") + 
    scale_colour_manual(values = c("white", "white")) +
    theme(  
      plot.margin = margin(0.2, 0.2, 0, 0.3, "cm"),
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      legend.title = element_blank(),
      # legend.position = c(0.2,0.85))
      legend.position = "none")
)
#

# AGE within immatures by depth ####
tempslopemoda <- lmerTest::lmer(slope_est ~ 1 + 
    log_age_scaled + chopstick +
    log_age_scaled * chopstick +
    (1|species),  REML = T, data = filter(ddat2, age == "Immature"))
    # (1|species_age) + (1|species),  REML = T, data = ddat2)
summary(tempslopemoda)

(p_log_age <- temp_interact_plot(
      tempslopemoda,
      # data = ddat2, include_mat = T,  
      data = filter(ddat2, age == "Immature"), include_mat = F, 
  # trend = T, y_limits = c(-7,2), # 99% trim
  trend = T, y_limits = c(-5,2), # 95% trim
      pred = "log_age_scaled"
    ) + xlab("Mean age") +
        # ylab("Biomass change ~ warming") +
        # ylab(expression(~italic("Y")~"~ temperature trend")) +
        ylab(expression(~italic("Y")~"~ warming trend")) + 
        # labs(tag = "D") + 
        # scale_colour_manual(values = c("white", "white")) +
        theme(  
          plot.margin = margin(0.2, 0.2, 0, 0.3, "cm"),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.tag.position = c(0.225,0.925),
          legend.title.align=0,
          legend.title = element_blank(),
          # legend.position = c(0.2,0.85))
          legend.position = "none")
    )

###
# Immature growth rate with chopsticks ####

tempslopemodg <- lmerTest::lmer(slope_est ~ 1 + 
    growth_rate_scaled +
    chopstick +
    growth_rate_scaled * chopstick +
    (1|species),  REML = T, data = filter(ddat2, age == "Immature"))
# (1|species_age) + (1|species),  REML = T, data = ddat2)
summary(tempslopemodg)

(p_growth_rate <- temp_interact_plot(
  tempslopemodg,
  # data = ddat2, include_mat = T,  
  data = filter(ddat2, age == "Immature"), include_mat = F, 
  # trend = T, y_limits = c(-7,2), # 99% trim
  trend = T, y_limits = c(-5,2), # 95% trim
  pred = "growth_rate_scaled"
) + xlab("Immature growth rate") +
    # ylab("Biomass change ~ warming") +
    # ylab(expression(~italic("Y")~"~ temperature trend")) +
    # ylab(expression(~italic("Y")~"~ warming rate")) + 
    ylab(expression(~italic("Y")~"~ warming trend")) + 
    # labs(tag = "D") + 
    scale_colour_manual(values = c("white", "white")) +
    theme(  
      plot.margin = margin(0.2, 0.2, 0, 0.3, "cm"),
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      legend.title = element_blank(),
      # legend.position = c(0.2,0.85))
      legend.position = "none")
)

#### DO WITH AGE PLOTS ######
# MEAN DEPTH ####
doslopemod2 <- lmerTest::lmer(slope_est ~ 1 +
    # poly(depth_mean_scaled, 2) +
    chopstick + depth_mean_scaled +
    chopstick * depth_mean_scaled +
    # chopstick * poly(depth_mean_scaled, 2) +
    # (1|species_age) + (1|species), REML = T, data = ddat2do)
    (1|species), REML = T, data = filter(ddat2do, age == "Immature")) 
doslopemod2 %>% summary()

(pd_depth_mean <- do_interact_plot(
  doslopemod2,
  # data = ddat2do, 
  data = filter(ddat2do, age == "Immature"),
  # trend = T,  y_limits = c(-2,4), # 99% trim
  trend = T,  y_limits = c(-1.2,1.7),
  pred = "depth_mean_scaled"
) + xlab("Mean depth") +
    ylab(expression(~italic("Y")~"~ DO trend")) + 
    # labs(tag = "D") + 
    # scale_colour_manual(values = c("white", "white")) +
    theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0.3, "cm"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      # legend.title = element_blank(),
      # legend.position = c(0.3,0.8))
      legend.position = "none")
)
###
#### DEPTH RANGE ####
doslopemodr <- lmerTest::lmer(slope_est ~ 1 +
    depth_iqr_scaled +
    chopstick +
    depth_iqr_scaled * chopstick +
    # (1|species_age) + (1|species), REML = T, data = ddat2do)
    (1|species), REML = T, data = filter(ddat2do, age == "Immature")) 
doslopemodr %>% summary()

(pd_depth_iqr <- do_interact_plot(
  doslopemodr,
  # data = ddat2do, 
  data = filter(ddat2do, age == "Immature"),
  # trend = T,  y_limits = c(-2,4), # 99% trim
  trend = T,  y_limits = c(-1.2,1.7),
  pred = "depth_iqr_scaled"
) + xlab("Depth range (IQR)") +
    ylab(expression(~italic("Y")~"~ DO trend")) + 
    # labs(tag = "D") + 
    scale_colour_manual(values = c("white", "white")) +
    theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0.3, "cm"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      # legend.title = element_blank(),
      # legend.position = c(0.3,0.8))
      legend.position = "none")
)
###
#### AGE in immatures ####

doslopemod2a <- lmerTest::lmer(slope_est ~ 1 +
    log_age_scaled +
    chopstick +
    log_age_scaled * chopstick +
    # (1|species_age) + (1|species), REML = T, data = ddat2do)
    (1|species), REML = T, data = filter(ddat2do, age == "Immature")) 
doslopemod2a %>% summary()

(pd_log_age <- do_interact_plot(
  doslopemod2a,
  # data = ddat2do, 
  data = filter(ddat2do, age == "Immature"),
  # trend = T,  y_limits = c(-2,4), # 99% trim
  trend = T,  y_limits = c(-1.2,1.7),
  pred = "log_age_scaled"
) + xlab("Mean age") +
    ylab(expression(~italic("Y")~"~ DO trend")) + 
    # labs(tag = "D") + 
    # scale_colour_manual(values = c("white", "white")) +
    theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0.3, "cm"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      # legend.title = element_blank(),
      # legend.position = c(0.3,0.8))
      legend.position = "none")
)
###
#### Immature growth rate ####
doslopemod2g <- lmerTest::lmer(slope ~ 1 +
    growth_rate_scaled +
    chopstick +
    growth_rate_scaled * chopstick +
    # (1|species_age) + (1|species), REML = T, data = ddat2do)
    (1|species), REML = T, data = filter(ddat2do, age == "Immature")) 
doslopemod2g %>% summary()

(pd_growth_rate <- do_interact_plot(
  doslopemod2g,
  # data = ddat2do, 
  data = filter(ddat2do, age == "Immature"),
  # trend = T,  y_limits = c(-2,4), # 99% trim
  trend = T,  y_limits = c(-1.2,1.7), # 95% trim
  pred = "growth_rate_scaled"
) + xlab("Immature growth rate") +
    ylab(expression(~italic("Y")~"~ DO trend")) + 
    # labs(tag = "D") + 
    scale_colour_manual(values = c("white", "white")) +
    theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0.3, "cm"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      # legend.title = element_blank(),
      # legend.position = c(0.3,0.8))
      legend.position = "none")
  )

#### set margins and axes afterwards #####


(p_depth_mean <- p_depth_mean + 
    labs(tag = "a.")  + 
    # coord_cartesian(ylim = c(-5,3)) + # 95 trim
    coord_cartesian(ylim = c(-7,2.9)) + # 99 trim 
    theme( 
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.92,0.92),
      # legend.title = element_blank(),
      # legend.position = c(0.6,0.2))
      legend.position = "none"))

( p_depth_iqr <-  p_depth_iqr +
    labs(tag = "b.")  +
    coord_cartesian(ylim = c(-7,2.9)) + # 99 trim 
    theme(
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      plot.tag.position = c(0.92,0.92),
      legend.title.align=0,
      # legend.title = element_blank(),
      # legend.position = c(0.6,0.2))
      legend.position = "none"))


(p_log_age <- p_log_age + 
    # labs(tag = "b.")  + 
    # coord_cartesian(ylim = c(-5,3)) +
    labs(tag = "c.")  +
    coord_cartesian(ylim = c(-7,2.9)) + # 99 trim 
    theme( 
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.92,0.92),
            legend.title = element_blank(),
            legend.position = c(0.8,0.2)))
      # legend.position = "none"))
( p_growth_rate <-  p_growth_rate +
    labs(tag = "d.")  +
    coord_cartesian(ylim = c(-7,2.9)) + # 99 trim 
    theme(
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      plot.tag.position = c(0.92,0.92),
      legend.title.align=0,
      # legend.title = element_blank(),
      # legend.position = c(0.6,0.2))
      legend.position = "none"))

#### DO ####
(pd_depth_mean <- pd_depth_mean + 
    # labs(tag = "c.")  + 
    labs(tag = "e.")  + #99 trim
    coord_cartesian(ylim = c(-1.5,4)) + # 99 trim 
    theme( 
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.92,0.92),
      legend.title = element_blank(),
      legend.position = c(0.8,0.8)))
      # legend.position = "none"))

(pd_depth_iqr <-  pd_depth_iqr + 
    labs(tag = "f.")  + #99 trim
    coord_cartesian(ylim = c(-1.5,4)) + # 99 trim 
    theme( 
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      plot.tag.position = c(0.92,0.92),
      legend.title.align=0,
      # legend.title = element_blank(),
      # legend.position = c(0.6,0.2))
      legend.position = "none"))

(pd_log_age <- pd_log_age + 
    # labs(tag = "d.")  + 
    labs(tag = "g.")  + #99 trim
    coord_cartesian(ylim = c(-1.5,4)) + # 99 trim 
    theme( 
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      plot.tag.position = c(0.92,0.92),
      legend.position = "none"))

(pd_growth_rate <- pd_growth_rate +
    labs(tag = "h.")  + #99 trim
    coord_cartesian(ylim = c(-1.5,4)) + # 99 trim 
    theme(
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      plot.tag.position = c(0.92,0.92),
      legend.title.align=0,
      legend.position = "none"))


(vp_depth_mean <- vp_depth_mean + 
    # labs(tag = "e.")  + 
    labs(tag = "i.")  + #99 trim
    # coord_cartesian(ylim = c(-1.5,4)) + # 99 trim 
    theme( 
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.92,0.92),
      legend.position = "none"))
( vp_depth_iqr <-  vp_depth_iqr +
    labs(tag = "j.")  +
    theme(
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      plot.tag.position = c(0.92,0.92),
      legend.title.align=0,
      legend.position = "none"))
(vp_log_age <- vp_log_age + 
    # labs(tag = "f.")  + 
    labs(tag = "k.")  + #99 trim
    theme( 
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.92,0.92),
      legend.position = "none"))

(vp_growth_rate <- vp_growth_rate + 
    labs(tag = "l.")  + #99 trim
    theme( 
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.92,0.92),
      legend.position = "none"))

(vpd_depth_mean <- vpd_depth_mean + 
    # labs(tag = "g.")  + 
    labs(tag = "m.")  + #99 trim
      theme( 
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      plot.tag.position = c(0.92,0.92),
      legend.position = "none"))
( vpd_depth_iqr <-  vpd_depth_iqr +
    labs(tag = "n.")  +
    theme(
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      plot.tag.position = c(0.92,0.92),
      legend.title.align=0,
      legend.position = "none"))
(vpd_log_age <- vpd_log_age + 
    labs(tag = "o.")  + 
    theme( 
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      plot.tag.position = c(0.92,0.92),
      legend.position = "none"))
(vpd_growth_rate <- vpd_growth_rate + 
    labs(tag = "p.")  + 
    theme( 
      # plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      plot.tag.position = c(0.92,0.92),
      legend.position = "none"))

# SAVE FIGURES just DO ####
(pd_depth_mean + pd_depth_iqr +  pd_log_age + pd_growth_rate  +plot_layout(nrow = 1))

# ggsave(here::here("ms", "figs", "immature-do-model.pdf"), width = 10, height = 3.5)

# SAVE FIGURES for both####
# # ### trend slopes
# # (p_depth_iqr + p_growth_rate + pd_depth_mean + pd_growth_rate + plot_layout(ncol = 2))
# # 
# # # ggsave(here::here("ms", "figs", "immature-growth-rate-models-trimmed.pdf"), width = 9, height = 7) # was trimmed model, but raw dat 
# ggsave(here::here("ms", "figs", "immature-growth-rate-models.pdf"), width = 6, height = 6)
# # 
# 
# 
# #### velocity slopes
# (   #p_log_age + 
#     p_growth_rate + #pd_log_age + 
#     pd_growth_rate + plot_layout(ncol = 1))
# 
# # ggsave(here::here("ms", "figs", "immature-growth-rate-models-trimmed.pdf"), width = 9, height = 7) # was trimmed model, but raw dat 
# ggsave(here::here("ms", "figs", "immature-growth-rate-vel.pdf"), width = 6, height = 6)


(p_depth_mean + p_depth_iqr + p_log_age + p_growth_rate  + 
    pd_depth_mean + pd_depth_iqr + pd_log_age + pd_growth_rate  + 
    vp_depth_mean + vp_depth_iqr + vp_log_age + vp_growth_rate  + 
    vpd_depth_mean + vpd_depth_iqr + vpd_log_age + vpd_growth_rate  + 
    plot_layout(ncol = 4))
 
# ggsave(here::here("ms", "figs", "immature-models-w-age.pdf"), width = 9.5, height = 5.5)

ygrob <- grid::textGrob(("Slopes"),
  gp = grid::gpar(fontsize = 13), hjust = 0.5, rot = 90
)

layout <- "
      ABCDE
      AFGHI
      AJKLM
      ANOPQ
      "
wrap_plots(ygrob, p_depth_mean , p_depth_iqr , p_log_age , p_growth_rate  , 
    pd_depth_mean , pd_depth_iqr , pd_log_age , pd_growth_rate  , 
    vp_depth_mean , vp_depth_iqr , vp_log_age , vp_growth_rate  , 
    vpd_depth_mean , vpd_depth_iqr , vpd_log_age , vpd_growth_rate) + plot_layout(design = layout, widths = c(0.05, 1, 1, 1, 1))

ggsave(here::here("ms", "figs", "immature-growth-rate-models-all.png"), width = 9.5, height = 9) # was trimmed model, but raw dat


layout <- "
      ABE
      AFI
      AJM
      ANQ
      "
wrap_plots(ygrob, 
  p_depth_mean,  p_log_age,
  pd_depth_mean, pd_log_age,
  vp_depth_mean, vp_log_age,
  vpd_depth_mean, vpd_log_age) + plot_layout(design = layout, widths = c(0.05, 1, 1))

# ggsave(here::here("ms", "figs", "immature-growth-rate-models-best.png"), width = 9, height = 9) # was trimmed model, but raw dat

ggsave(here::here("ms", "figs", "depth-age-models-best-95.png"), width = 5.5, height = 9) 


# TWEAK FIG FOR AGE ####

young <- filter(temp_slopes, age_mean < 10) %>%  select(
  slope, slope_est, slope_se, slope_trim,
  age, depth_mean_scaled,  depth_iqr_scaled, Zone,
  log_age_scaled, #growth_rate_scaled, 
  max_mass_scaled,
  Latitude, Trophic, chopstick, species, species_age) 

slope_young <- lmerTest::lmer(slope_est ~ 1 +
    # growth_rate_scaled +
    log_age_scaled * chopstick +
    (1|species) +
    (1|species_age), REML = T, data = young)
slope_young %>% summary()


(p_log_age <- interactions::interact_plot(
  # tempslopemod2d, # trend
  slope_young, # vel
  pred = log_age_scaled,
  modx = chopstick,
  interval = TRUE, int.type = c("prediction"), line.thickness = 0.5,
  outcome.scale = "response", 
  #plot.points = TRUE,
  # partial.residuals = T,
  # point.alpha = 0.25,
  point.shape = F,
  # modx.values = c("high", "low"),
  vary.lty =TRUE) +
    # scale_colour_manual(values = c("white", "white")) +
    scale_colour_manual(values = c("red 3", "royalblue4")) +
    scale_fill_manual(values = c("red 3", "royalblue4")) +
    scale_shape_manual(values = c(21, 21)) +
    geom_point(data = filter(ddat2, chopstick == "high"),
      aes(log_age_scaled, slope_est,
      ), alpha = 1, size = 1.5, colour = "red 3", shape = 21,
      inherit.aes = F) +
    geom_point(data = filter(ddat2, chopstick == "high" & age == "Mature"),
      aes(log_age_scaled, slope_est,
      ), alpha = 1, size = 1.5, colour = "red 3", shape = 19,
      inherit.aes = F) +
    # geom_linerange(data = filter(ddat2, chopstick == "high"),
    #   aes(x = log_age_scaled,
    #     ymin = (slope_est - slope_se * 1.96),
    #     ymax = (slope_est + slope_se * 1.96),
    #   ), colour = "red 3", shape = 21, alpha = 1, fatten = 1, inherit.aes = F) +
    # geom_linerange(data = filter(ddat2, chopstick == "low"),
    #   aes(x = log_age_scaled,
    #     ymin = (slope_est - slope_se * 1.96),
    #     ymax = (slope_est + slope_se * 1.96)
    #   ), colour = "royalblue4", shape = 21, alpha = 1, fatten = 1, inherit.aes = F) +
    # back transform axis labels on scaled log growth rate
    scale_x_continuous(labels = function(x)
      paste0(round(exp(x * attributes(temp_slopes$log_age_scaled)[[3]] +
          attributes(temp_slopes$log_age_scaled)[[2]])- 1))) +
    # coord_cartesian(ylim = c(-10,5)) +
    coord_cartesian(xlim = c(-1.8,0))+
    xlab("Mean age ") +
    ylab(expression(~italic("Y")~"~ warming rate")) + 
    # ylab("Biomass change ~ warming rate") +
    # labs(tag = "D") +
    # labs(colour = "Maturity") +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0.2, 0, 0, "cm"),
      # axis.title.x=element_blank(),
      # axis.text.x=element_blank(),
      # axis.ticks.x=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      legend.title = element_blank(),
      # legend.position = c(0.8,0.15))
      legend.position = "none")
)


################
# ECOLOGICAL EFFECTS FOR BOTH MATURITY CLASSES ####
# hist(temp_slopes$slope)
### models for just highest temperatures ####
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(
  slope ~ 1 + Trophic + age + Zone + Latitude + (1|species), data = .) %>% 
  anova() #*
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(
  slope ~ 1 + Trophic + age + Zone + Latitude + (1|species), data = .) %>% 
  summary()
# matches both slopes result

#### DO need to redo but preliminary exploration suggests ####
# bigger adults = more negative
# faster growing juvi = more negative
# but collapsing outliers loses sig

# MODELS FOR TEMPERATURE #####
# change order to force dashed line in effect_plots to be for immature
temp_slopes <- temp_slopes %>% mutate(age = factor(age, levels = c("Mature", "Immature")) ) 

ddat <- temp_slopes %>% select(slope, slope_est, slope_se, slope_trim, 
  depth_iqr_scaled, depth_mean_scaled, 
  Zone, Latitude, Trophic, Specialist, Schooling, Rockfish, 
  age, max_mass_scaled,
  chopstick, species, species_age)

ddat1 <- filter(ddat, chopstick == "high")



# seems like each of these variables in combination with depth range shows some weak, non-sig patterns...
# some become sig when combined with eachother, but sample size too small to trust those results

# ggplot(ddat1) + geom_point(aes(Zone, depth_iqr_scaled))

# ddat <- ddat %>% mutate(Latitude = factor(Latitude, levels = c("South", "North")) )

# set for most extreme slopes
ddat <- ddat %>% mutate(chopstick = factor(chopstick, levels = c("high", "low")) )
ddat <- ddat %>% mutate(Latitude = factor(Latitude, levels = c("North", "South")) )
ddat <- ddat %>% mutate(Zone = factor(Zone, levels = c("Benthopelagic", "Demersal")) )
ddat <- ddat %>% mutate(Trophic = factor(Trophic, levels = c("Higher", "Lower")) )
ddat <- ddat %>% mutate(Schooling = factor(Schooling, levels = c("Schooling", "Solitary")) )

# set intercept for most common type
# best_slopes %>% filter(type == "temp") %>% filter (age == "Mature") %>% filter(Trophic == "Higher") %>% tally()
# best_slopes %>% filter(type == "temp") %>% filter (age == "Mature") %>% filter(Zone == "Demersal") %>% tally()
# best_slopes %>% filter(type == "temp") %>% filter (age == "Mature") %>% filter(Latitude == "South") %>% tally()

# ddat <- ddat %>% mutate(Zone = factor(Zone, levels = c("Demersal", "Benthopelagic")) )
# ddat <- ddat %>% mutate(Latitude = factor(Latitude, levels = c("South", "North")) )


#####
# MODELS FOR DO #####
# change order to force dashed line in effect_plots to be for immature
do_slopes2 <- do_slopes %>% ungroup() %>% 
  mutate(age = factor(age, levels = c("Mature", "Immature")) ) %>% 
  filter(chopstick == "low")

ddatdo <- do_slopes %>% 
  select(slope, slope_est, slope_se, slope_trim, 
    depth_iqr_scaled, depth_mean_scaled, 
    Zone, Latitude, Trophic, Specialist,
    Schooling, Rockfish, 
    age, max_mass_scaled,
    chopstick, species, species_age)


# set for most extreme slopes
ddatdo <- ddatdo %>% mutate(chopstick = factor(chopstick, levels = c( "low", "high")) )
ddatdo <- ddatdo %>% mutate(Latitude = factor(Latitude, levels = c("North", "South")) )
ddatdo <- ddatdo %>% mutate(Zone = factor(Zone, levels = c("Benthopelagic", "Demersal")) )
ddatdo <- ddatdo %>% mutate(Trophic = factor(Trophic, levels = c("Higher", "Lower")) )
ddatdo <- ddatdo %>% mutate(Schooling = factor(Schooling, levels = c("Schooling", "Solitary")) )
ddatdo <- ddatdo %>% mutate(age = factor(age, levels = c("Mature", "Immature")) ) 


ddatdo1 <- ddatdo %>% filter(chopstick == "low")

# set intercept for most common type
# best_slopes %>% filter(type == "temp") %>% filter (age == "Mature") %>% filter(Trophic == "Higher") %>% tally()
# best_slopes %>% filter(type == "temp") %>% filter (age == "Mature") %>% filter(Zone == "Demersal") %>% tally()
# best_slopes %>% filter(type == "temp") %>% filter (age == "Mature") %>% filter(Latitude == "South") %>% tally()

# ddatdo <- ddatdo %>% mutate(Zone = factor(Zone, levels = c("Demersal", "Benthopelagic")) )
# ddatdo <- ddatdo %>% mutate(Latitude = factor(Latitude, levels = c("South", "North")) )

#### PLOT INTERACTION EFFECTS ####
##### TEMP ###############
#### for high temp slopes #### 
# low temp slopes are currently removed entirely


# should raw data points be trimmed ones?
# # if yes, run this 
ddat <- ddat %>% mutate(slope_est = slope_trim)
ddatdo <- ddatdo %>% mutate(slope_est = slope_trim)
# # # if no, run this
# ddat <- ddat %>% mutate(slope_est = slope)
# ddatdo <- ddatdo %>% mutate(slope_est = slope)

#### LATITUDE ####
### trend ###
tempslopemod <- lmerTest::lmer(slope_est ~ 
    Latitude +
    depth_mean_scaled +
    # depth_iqr_scaled +
    Latitude * depth_mean_scaled  +
    chopstick +
    # age +
    (1|species) + (1|species_age), REML = T, 
  data = ddat) 
tempslopemod %>% summary()

(p_depth_lat <- interactions::interact_plot(tempslopemod,
  pred = depth_mean_scaled,
  modx = Latitude,
  interval = TRUE, int.type = c("prediction"), line.thickness = 0.5,
  outcome.scale = "response", 
  #partial.residuals = T,
  # plot.points = T, 
  point.alpha = 0.2, 
  point.shape = T,
  # legend.main = "age",
  modx.values = c("North", "South"),
  vary.lty =TRUE
) + 
    geom_point(data = filter(ddat, chopstick == "high" & age == "Mature" & Latitude == "North"),
      aes(depth_mean_scaled, slope_trim), alpha = 1, size = 1.5, shape = 19, colour = "royalblue4",
      inherit.aes = F) +
    geom_point(data = filter(ddat, chopstick == "high" & age == "Immature" & Latitude == "North"),
      aes(depth_mean_scaled, slope_trim), alpha = 1, size = 1.5, shape = 21, colour = "royalblue4",
      inherit.aes = F) +
    geom_linerange(data = filter(ddat, chopstick == "high" & Latitude == "North"),
      aes(x = depth_mean_scaled,
        ymin = (slope_trim - slope_se * 1.96),
        ymax = (slope_trim + slope_se * 1.96)), alpha = 1, colour = "royalblue4", fatten = 1, inherit.aes = F) +
    geom_point(data = filter(ddat, chopstick == "high" & age == "Mature" & Latitude == "South"),
      aes(depth_mean_scaled, slope_trim), alpha = 1, size = 1.5, shape = 19, colour = "deepskyblue3",
      inherit.aes = F) +
    geom_point(data = filter(ddat, chopstick == "high" & age == "Immature" & Latitude == "South"),
      aes(depth_mean_scaled, slope_trim), alpha = 1, size = 1.5, shape = 21, colour = "deepskyblue3",
      inherit.aes = F) +
    geom_linerange(data = filter(ddat, chopstick == "high" & Latitude == "South"), 
      aes(x = depth_mean_scaled,
        ymin = (slope_trim - slope_se * 1.96),
        ymax = (slope_trim + slope_se * 1.96)), alpha = 1, colour = "deepskyblue3", fatten = 1, inherit.aes = F) +
    
    # geom_point(data = filter(ddat, chopstick == "high" & age == "Mature"),
    #   aes(depth_mean_scaled, slope, colour = Latitude), alpha = 1, size = 1.5, shape = 19,
    #   inherit.aes = F) +
    # 
    # geom_point(data = filter(ddat, chopstick == "high" & age == "Immature"),
    #   aes(depth_mean_scaled, slope, colour = Latitude), alpha = 1, size = 1.5, shape = 21,
    #   inherit.aes = F) +
    # 
    # geom_linerange(data = filter(ddat, chopstick == "high"),
    #   aes(x = depth_mean_scaled,
    #     ymin = (slope_est - slope_se * 1.96),
    #     ymax = (slope_est + slope_se * 1.96),
    #     colour = Latitude, shape = Latitude), alpha = 1, fatten = 1, inherit.aes = F) +
    # geom_linerange(data = filter(ddat, chopstick == "low"),
    #   aes(x = depth_mean_scaled,
    #     ymin = (slope_est - slope_se * 1.96),
    #     ymax = (slope_est + slope_se * 1.96),
    #     colour = Latitude, shape = Latitude), alpha = 0.2, fatten = 1, inherit.aes = F) +
    scale_colour_manual(values = c("royalblue4", "deepskyblue3")) +
    # scale_colour_manual(values = c("white", "white")) +
    scale_fill_manual(values = c("royalblue4", "deepskyblue3")) +
    scale_shape_manual(values = c(19, 19)) +
    # back transform axis labels on scaled depth iqr
    scale_x_continuous(labels = function(x)
      paste0(round((x * attributes(temp_slopes$depth_mean_scaled)[[3]] +
          attributes(temp_slopes$depth_mean_scaled)[[2]])))) +
    coord_cartesian(ylim = c(-7,3)) +
    ylab(expression(~italic("Y")~"~ warming rate at high temp")) + 
    # ylab("Biotic trend ~ warming rate at high temp") +
    xlab("Depth range (IQR)") +
    ggtitle("Latitude") +
    # labs(tag = "D") +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0.1, 0.2, 0.3, "cm"),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      # legend.position = "none")
      # legend.position = c(0.8,0.15))
      legend.position = c(0.15,0.85))
)

###
#### FORAGING ZONE ####
tempslopemodz <- lmerTest::lmer(slope_est ~ 
    Zone +
    # depth_iqr_scaled +
    depth_mean_scaled +
    # Zone * depth_mean_scaled  +
    chopstick +
    # age +
    (1|species) + (1|species_age), REML = T, 
  data = ddat) 


tempslopemodz %>% summary()
#### trend ####
(p_depth_zone <- interactions::interact_plot(tempslopemodz,
  pred = depth_mean_scaled, modx = Zone,
  interval = TRUE, int.type = c("prediction"), line.thickness = 0.5,
  outcome.scale = "response", 
  # plot.points = TRUE,
  #partial.residuals = T,
  point.alpha = 0.005,
  point.shape = T,
  modx.values = c("Demersal", "Benthopelagic"),
  vary.lty =TRUE
) + geom_point(data = filter(ddat, chopstick == "high" & age == "Mature" & Zone == "Demersal"),
  aes(depth_mean_scaled, slope_trim), alpha = 1, size = 1.5, shape = 19, colour = "royalblue4",
  inherit.aes = F) +
    geom_point(data = filter(ddat, chopstick == "high" & age == "Immature" & Zone == "Demersal"),
      aes(depth_mean_scaled, slope_trim), alpha = 1, size = 1.5, shape = 21, colour = "royalblue4",
      inherit.aes = F) +
    geom_linerange(data = filter(ddat, chopstick == "high" & Zone == "Demersal"),
      aes(x = depth_mean_scaled,
        ymin = (slope_trim - slope_se * 1.96),
        ymax = (slope_trim + slope_se * 1.96)), alpha = 1, colour = "royalblue4", fatten = 1, inherit.aes = F) +
    geom_point(data = filter(ddat, chopstick == "high" & age == "Mature" & Zone == "Benthopelagic"),
      aes(depth_mean_scaled, slope_trim), alpha = 1, size = 1.5, shape = 19, colour = "deepskyblue3",
      inherit.aes = F) +
    geom_point(data = filter(ddat, chopstick == "high" & age == "Immature" & Zone == "Benthopelagic"),
      aes(depth_mean_scaled, slope_trim), alpha = 1, size = 1.5, shape = 21, colour = "deepskyblue3",
      inherit.aes = F) +
    geom_linerange(data = filter(ddat, chopstick == "high" & Zone == "Benthopelagic"), 
      aes(x = depth_mean_scaled,
        ymin = (slope_trim - slope_se * 1.96),
        ymax = (slope_trim + slope_se * 1.96)), alpha = 1, colour = "deepskyblue3", fatten = 1, inherit.aes = F) +
    # geom_linerange(data = filter(temp_slopes, chopstick == "low"),
    #   aes(x = depth_mean_scaled,
    #     ymin = (slope_est - slope_se * 1.96),
    #     ymax = (slope_est + slope_se * 1.96),
    #     colour = Zone,
    #     ), alpha = 0.2, fatten = 1, inherit.aes = F) +
    
    # scale_colour_manual(values = c("white", "white")) +
    scale_fill_manual(values = c("royalblue4", "deepskyblue3")) +
    scale_colour_manual(values = c("royalblue4", "deepskyblue3")) +
    scale_shape_manual(values = c(19, 19)) +
    # back transform axis labels on scaled depth iqr
    scale_x_continuous(labels = function(x)
      paste0(round((x * attributes(temp_slopes$depth_mean_scaled)[[3]] +
          attributes(temp_slopes$depth_mean_scaled)[[2]])))) +
    coord_cartesian(ylim = c(-7,3)) +
    ggtitle("Foraging zone") +
    # ylab("Peak Frequency") +
    xlab("Depth range (IQR)") +
    # labs(tag = "D") +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      legend.title = element_blank(),
      # legend.position = "none")
      legend.position = c(0.2,0.85))
)


###
#### TROPHIC LEVEL ####

tempslopemodt <- lmerTest::lmer(slope_est ~
    Trophic +
    depth_mean_scaled +
    Trophic * depth_mean_scaled +
    chopstick +
    # age +
    (1|species) + (1|species_age), REML = T,
  data = ddat)

tempslopemodt %>% summary()

(p_depth_troph <- interactions::interact_plot(tempslopemodt, 
  pred = depth_mean_scaled, modx = Trophic, 
  interval = TRUE, int.type = c("prediction"), line.thickness = 0.5, 
  outcome.scale = "response", 
  # plot.points = TRUE,
  #partial.residuals = T, 
  point.alpha = 0.01,
  point.shape = T, 
  # legend.main = "Trophic level",
  modx.values = c("Lower", "Higher"),
  vary.lty =TRUE
) + 
    geom_point(data = filter(ddat, chopstick == "high" & age == "Mature" & Trophic == "Lower"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 19, colour = "royalblue4",
      inherit.aes = F) +
    geom_point(data = filter(ddat, chopstick == "high" & age == "Immature" & Trophic == "Lower"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 21, colour = "royalblue4",
      inherit.aes = F) +
    geom_linerange(data = filter(ddat, chopstick == "high" & Trophic == "Lower"),
      aes(x = depth_mean_scaled,
        ymin = (slope_est - slope_se * 1.96),
        ymax = (slope_est + slope_se * 1.96)), alpha = 1, colour = "royalblue4", fatten = 1, inherit.aes = F) +
    geom_point(data = filter(ddat, chopstick == "high" & age == "Mature" & Trophic == "Higher"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 19, colour = "deepskyblue3",
      inherit.aes = F) +
    geom_point(data = filter(ddat, chopstick == "high" & age == "Immature" & Trophic == "Higher"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 21, colour = "deepskyblue3",
      inherit.aes = F) +
    geom_linerange(data = filter(ddat, chopstick == "high" & Trophic == "Higher"), 
      aes(x = depth_mean_scaled,
        ymin = (slope_est - slope_se * 1.96),
        ymax = (slope_est + slope_se * 1.96)), alpha = 1, colour = "deepskyblue3", fatten = 1, inherit.aes = F) +
    # geom_linerange(data = filter(temp_slopes, chopstick == "low"), 
    #   aes(x = depth_mean_scaled,
    #     ymin = (slope_est - slope_se * 1.96),
    #     ymax = (slope_est + slope_se * 1.96),
    #     colour = Trophic, shape = Trophic), alpha = 0.2, fatten = 1, inherit.aes = F) +
    
    scale_colour_manual(values = c("white", "white")) +
    # scale_colour_manual(values = c("royalblue4", "deepskyblue3")) +
    scale_fill_manual(values = c("royalblue4", "deepskyblue3")) +
    # scale_colour_manual(values = c("royalblue4", "red 4")) +
    # scale_fill_manual(values = c("royalblue4", "red 3")) +
    scale_shape_manual(values = c(19, 19)) +
    # back transform axis labels on scaled depth iqr
    scale_x_continuous(labels = function(x) 
      paste0(round((x * attributes(temp_slopes$depth_mean_scaled)[[3]] + 
          attributes(temp_slopes$depth_mean_scaled)[[2]])))) +
    coord_cartesian(ylim = c(-7,3)) +
    ggtitle("Trophic level") +
    # ylab("Peak Frequency") +
    xlab("Depth range (IQR)") +
    # labs(tag = "D") + 
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0.1, "cm"),
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      legend.title = element_blank(),
      # legend.position = "none")
      # legend.position = c(0.8,0.15))
      legend.position = c(0.15,0.85))
)


###
#### SCHOOLING ####

tempslopemods <- lmerTest::lmer(slope_est ~
    # Latitude +
    Schooling +
    depth_mean_scaled +
    # Schooling * depth_mean_scaled +
    chopstick +
    # age +
    (1|species) + (1|species_age), REML = T,
  data = ddat) 

tempslopemods %>% summary()

(p_depth_sch <- interactions::interact_plot(tempslopemods, 
  pred = depth_mean_scaled, modx = Schooling, 
  interval = TRUE, int.type = c("prediction"), line.thickness = 0.5, 
  outcome.scale = "response", 
  plot.points = TRUE,
  #partial.residuals = T, 
  point.alpha = 0.005,
  point.shape = T, 
  # colors = c("white", "white"),
  # legend.main = "Trophic level",
  modx.values = c("Solitary", "Schooling"),
  vary.lty =TRUE
) + geom_point(data = filter(ddat, chopstick == "high" & age == "Mature" & Schooling == "Solitary"),
  aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 19, colour = "royalblue4",
  inherit.aes = F) +
    geom_point(data = filter(ddat, chopstick == "high" & age == "Immature" & Schooling == "Solitary"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 21, colour = "royalblue4",
      inherit.aes = F) +
    geom_linerange(data = filter(ddat, chopstick == "high" & Schooling == "Solitary"),
      aes(x = depth_mean_scaled,
        ymin = (slope_est - slope_se * 1.96),
        ymax = (slope_est + slope_se * 1.96)), alpha = 1, colour = "royalblue4", fatten = 1, inherit.aes = F) +
    geom_point(data = filter(ddat, chopstick == "high" & age == "Mature" & Schooling == "Schooling"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 19, colour = "deepskyblue3",
      inherit.aes = F) +
    geom_point(data = filter(ddat, chopstick == "high" & age == "Immature" & Schooling == "Schooling"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 21, colour = "deepskyblue3",
      inherit.aes = F) +
    geom_linerange(data = filter(ddat, chopstick == "high" & Schooling == "Schooling"), 
      aes(x = depth_mean_scaled,
        ymin = (slope_est - slope_se * 1.96),
        ymax = (slope_est + slope_se * 1.96)), alpha = 1, colour = "deepskyblue3", fatten = 1, inherit.aes = F) +
    # geom_linerange(data = filter(temp_slopes, chopstick == "low"), 
    #   aes(x = depth_mean_scaled,
    #     ymin = (slope_est - slope_se * 1.96),
    #     ymax = (slope_est + slope_se * 1.96),
    #     colour = Schooling, shape = Schooling), alpha = 0.2, fatten = 1, inherit.aes = F) +
    scale_colour_manual(values = c("white", "white")) +
    scale_fill_manual(values = c("royalblue4", "deepskyblue3")) +
    # scale_colour_manual(values = c("royalblue4", "red 4")) +
    # scale_fill_manual(values = c("royalblue4", "red 3")) +
    ## alternate colours 
    # scale_colour_manual(values = c("darkorchid4", "maroon")) +
    # scale_fill_manual(values = c("white", "white")) +
    scale_shape_manual(values = c(19, 19)) +
    # back transform axis labels on scaled depth iqr
    scale_x_continuous(labels = function(x) 
      paste0(round((x * attributes(temp_slopes$depth_mean_scaled)[[3]] + 
          attributes(temp_slopes$depth_mean_scaled)[[2]])))) +
    # coord_cartesian(ylim = c(-7,3)) +
    ggtitle("Sociality") + 
    # ylab("Peak Frequency") +
    xlab("Depth range (IQR)") +
    # labs(tag = "D") + 
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      legend.title = element_blank(),
      # legend.position = "none")
      # legend.position = c(0.8,0.15))
      legend.position = c(0.15,0.85))
)

###
#### SPECIALIZATION ####
# tempslopemod <- lmerTest::lmer(slope_est ~
#     Specialist +
#     depth_mean_scaled +
#     Specialist * depth_mean_scaled +
#     chopstick +
#     # age +
#     (1|species) + (1|species_age), REML = T,
#   data = ddat)
# 
# tempslopemod %>% summary()
# 
# (p_depth_troph <- interactions::interact_plot(tempslopemod, 
#   pred = depth_mean_scaled, modx = Specialist, 
#   interval = TRUE, int.type = c("prediction"), line.thickness = 0.5, 
#   outcome.scale = "response", 
#   # plot.points = TRUE,
#   #partial.residuals = T, 
#   point.alpha = 0.01,
#   point.shape = T, 
#   # legend.main = "Specialist level",
#   modx.values = c("Lower", "Higher"),
#   vary.lty =TRUE
# ) + 
#     geom_point(data = filter(ddat, chopstick == "high" & age == "Mature" & Specialist == "Lower"),
#       aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 19, colour = "royalblue4",
#       inherit.aes = F) +
#     geom_point(data = filter(ddat, chopstick == "high" & age == "Immature" & Specialist == "Lower"),
#       aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 21, colour = "royalblue4",
#       inherit.aes = F) +
#     geom_linerange(data = filter(ddat, chopstick == "high" & Specialist == "Lower"),
#       aes(x = depth_mean_scaled,
#         ymin = (slope_est - slope_se * 1.96),
#         ymax = (slope_est + slope_se * 1.96)), alpha = 1, colour = "royalblue4", fatten = 1, inherit.aes = F) +
#     geom_point(data = filter(ddat, chopstick == "high" & age == "Mature" & Specialist == "Higher"),
#       aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 19, colour = "deepskyblue3",
#       inherit.aes = F) +
#     geom_point(data = filter(ddat, chopstick == "high" & age == "Immature" & Specialist == "Higher"),
#       aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 21, colour = "deepskyblue3",
#       inherit.aes = F) +
#     geom_linerange(data = filter(ddat, chopstick == "high" & Specialist == "Higher"), 
#       aes(x = depth_mean_scaled,
#         ymin = (slope_est - slope_se * 1.96),
#         ymax = (slope_est + slope_se * 1.96)), alpha = 1, colour = "deepskyblue3", fatten = 1, inherit.aes = F) +
#     # geom_linerange(data = filter(temp_slopes, chopstick == "low"), 
#     #   aes(x = depth_mean_scaled,
#     #     ymin = (slope_est - slope_se * 1.96),
#     #     ymax = (slope_est + slope_se * 1.96),
#     #     colour = Specialist, shape = Specialist), alpha = 0.2, fatten = 1, inherit.aes = F) +
#     
#     scale_colour_manual(values = c("white", "white")) +
#     # scale_colour_manual(values = c("royalblue4", "deepskyblue3")) +
#     scale_fill_manual(values = c("royalblue4", "deepskyblue3")) +
#     # scale_colour_manual(values = c("royalblue4", "red 4")) +
#     # scale_fill_manual(values = c("royalblue4", "red 3")) +
#     scale_shape_manual(values = c(19, 19)) +
#     # back transform axis labels on scaled depth iqr
#     scale_x_continuous(labels = function(x) 
#       paste0(round(exp(x * attributes(temp_slopes$depth_mean_scaled)[[3]] + 
#           attributes(temp_slopes$depth_mean_scaled)[[2]])))) +
#     coord_cartesian(ylim = c(-7,3)) +
#     ggtitle("Specialist level") +
#     # ylab("Peak Frequency") +
#     xlab("Depth range (IQR)") +
#     # labs(tag = "D") + 
#     gfplot::theme_pbs() + theme(
#       plot.margin = margin(0.2, 0.2, 0.2, 0.1, "cm"),
#       axis.title=element_blank(),
#       axis.text.y=element_blank(),
#       axis.ticks.y=element_blank(),
#       plot.tag.position = c(0.225,0.925),
#       legend.title.align=0,
#       legend.title = element_blank(),
#       # legend.position = "none")
#       # legend.position = c(0.8,0.15))
#       legend.position = c(0.15,0.85))
# )

ddat %>% mutate(pearson = residuals(tempslopemod,type="pearson"),
  # x = Schooling
  # x = Latitude
  # x = Trophic
  # x = Zone
  x = depth_mean_scaled
  ) 
# ggplot(aes(x ,y=pearson)) +
#   geom_point() +
#   theme_bw()

##### DO ################
#### for low DO slopes #### 
#### LATITUDE ####

doslopemod <- lmerTest::lmer(slope_est ~
    Latitude +
    depth_mean_scaled +
    Latitude * depth_mean_scaled  +
    chopstick +
    # age +
    (1|species) + (1|species_age), REML = T,
    data = ddatdo) 

doslopemod %>% summary()


(pd_depth_lat <- interactions::interact_plot(doslopemod,
  pred = depth_mean_scaled, modx = Latitude,
  interval = TRUE, int.type = c("prediction"), line.thickness = 0.5,
  outcome.scale = "response", 
  #partial.residuals = T,
  # plot.points = T,
  point.alpha = 0.2, 
  point.shape = T,
  # legend.main = "age",
  modx.values = c("North", "South"),
  vary.lty = TRUE
) + 
    geom_point(data = filter(ddatdo, chopstick == "low" & age == "Mature" & Latitude == "North"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 19, colour = "royalblue4",
      inherit.aes = F) +
    geom_point(data = filter(ddatdo, chopstick == "low" & age == "Immature" & Latitude == "North"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 21, colour = "royalblue4",
      inherit.aes = F) +
    geom_linerange(data = filter(ddatdo, chopstick == "low" & Latitude == "North"),
      aes(x = depth_mean_scaled,
        ymin = (slope_est - slope_se * 1.96),
        ymax = (slope_est + slope_se * 1.96)), alpha = 1, colour = "royalblue4", fatten = 1, inherit.aes = F) +
    geom_point(data = filter(ddatdo, chopstick == "low" & age == "Mature" & Latitude == "South"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 19, colour = "deepskyblue3",
      inherit.aes = F) +
    geom_point(data = filter(ddatdo, chopstick == "low" & age == "Immature" & Latitude == "South"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 21, colour = "deepskyblue3",
      inherit.aes = F) +
    geom_linerange(data = filter(ddatdo, chopstick == "low" & Latitude == "South"), 
      aes(x = depth_mean_scaled,
        ymin = (slope_est - slope_se * 1.96),
        ymax = (slope_est + slope_se * 1.96)), alpha = 1, colour = "deepskyblue3", fatten = 1, inherit.aes = F) +
    # geom_linerange(data = filter(ddatdo, chopstick == "low"), 
    #   aes(x = depth_mean_scaled,
    #     ymin = (slope_est - slope_se * 1.96),
    #     ymax = (slope_est + slope_se * 1.96),
    #     colour = Schooling), alpha = 0.2, fatten = 1, inherit.aes = F) +
    scale_colour_manual(values = c("white", "white")) +
    # scale_colour_manual(values = c("royalblue4", "deepskyblue3")) +
    scale_fill_manual(values = c("royalblue4", "deepskyblue3")) +
    # back transform axis labels on scaled depth iqr
    scale_x_continuous(labels = function(x) 
      paste0(round((x * attributes(do_slopes$depth_mean_scaled)[[3]] + 
          attributes(do_slopes$depth_mean_scaled)[[2]])))) +
    # coord_cartesian(ylim = c(-10,5)) +
    gfplot::theme_pbs() + 
    ylab(expression(~italic("Y")~"~ DO velocity at low DO")) +
    # ylab(expression(~italic("Y")~"~ DO trend at low DO")) + 
    # ylab("Biotic trend ~ DO trend at low DO") +
    xlab("Mean depth occupied") +
    # labs(tag = "D") + 
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0.3, "cm"),
      axis.title.x = element_blank(),
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      # legend.title = element_blank(),
      legend.position = "none")
  # legend.position = c(0.8,0.15))
  # legend.position = c(0.2,0.85))
)  
###

#### TROPHIC LEVEL ####

doslopemod <- lmerTest::lmer(slope_est ~
    Trophic +
    depth_mean_scaled +
    # Trophic * depth_mean_scaled +
    chopstick +
    # age +
    (1|species) + (1|species_age), REML = T,
  data = ddatdo) 

doslopemod %>% summary()

(pd_depth_troph <- interactions::interact_plot(doslopemod, 
  pred = depth_mean_scaled, modx = Trophic, 
  interval = TRUE, int.type = c("prediction"), line.thickness = 0.5, 
  outcome.scale = "response", 
  # plot.points = TRUE,
  #partial.residuals = T, 
  point.alpha = 0.01,
  point.shape = T, 
  # # legend.main = "Trophic level",
  # colors = c("royalblue4", "deepskyblue3"),
  modx.values = c("Lower", "Higher"),
  vary.lty =TRUE
) +    
    geom_point(data = filter(ddatdo, chopstick == "low" & age == "Mature" & Trophic == "Lower"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 19, colour = "royalblue4",
      inherit.aes = F) +
    geom_point(data = filter(ddatdo, chopstick == "low" & age == "Immature" & Trophic == "Lower"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 21, colour = "royalblue4",
      inherit.aes = F) +
    geom_linerange(data = filter(ddatdo, chopstick == "low" & Trophic == "Lower"),
      aes(x = depth_mean_scaled,
        ymin = (slope_est - slope_se * 1.96),
        ymax = (slope_est + slope_se * 1.96)), alpha = 1, colour = "royalblue4", fatten = 1, inherit.aes = F) +
    geom_point(data = filter(ddatdo, chopstick == "low" & age == "Mature" & Trophic == "Higher"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 19, colour = "deepskyblue3",
      inherit.aes = F) +
    geom_point(data = filter(ddatdo, chopstick == "low" & age == "Immature" & Trophic == "Higher"),
      aes(depth_mean_scaled, slope_est), alpha = 1, size = 1.5, shape = 21, colour = "deepskyblue3",
      inherit.aes = F) +
    geom_linerange(data = filter(ddatdo, chopstick == "low" & Trophic == "Higher"), 
      aes(x = depth_mean_scaled,
        ymin = (slope_est - slope_se * 1.96),
        ymax = (slope_est + slope_se * 1.96)), alpha = 1, colour = "deepskyblue3", fatten = 1, inherit.aes = F) +
    # geom_linerange(data = filter(ddatdo, chopstick == "low"), 
    #   aes(x = depth_mean_scaled,
    #     ymin = (slope_est - slope_se * 1.96),
    #     ymax = (slope_est + slope_se * 1.96),
    #     colour = Schooling), alpha = 0.2, fatten = 1, inherit.aes = F) +
    scale_colour_manual(values = c("royalblue4", "deepskyblue3")) +
    scale_fill_manual(values = c("royalblue4", "deepskyblue3")) +
    # back transform axis labels on scaled depth iqr
    scale_x_continuous(labels = function(x) 
      paste0(round((x * attributes(do_slopes$depth_mean_scaled)[[3]] + 
          attributes(do_slopes$depth_mean_scaled)[[2]])))) +
    # coord_cartesian(ylim = c(-10,5)) +
    gfplot::theme_pbs() + 
    # ylab("Peak Frequency") +
    xlab("Mean depth occupied") +
    # labs(tag = "D") + 
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      # legend.title = element_blank(),
      legend.position = "none")
  # legend.position = c(0.8,0.15))
  # legend.position = c(0.2,0.85))
)

###

#### FORAGING ZONE ####
doslopemod <- lmerTest::lmer(slope_est ~
    Zone +
    depth_mean_scaled +
    # Zone * depth_mean_scaled +
    chopstick +
    # age +
    (1|species) + (1|species_age), REML = T,
  data = ddatdo) 

# doslopemod <- lmerTest::lmer(slope ~
#     Zone +
#     depth_mean_scaled +    
#     Zone * depth_mean_scaled +
#     (1|species), REML = T,
#   data = ddatdo1)
# 
doslopemod %>% summary()


(pd_depth_zone <- interactions::interact_plot(doslopemod,
  pred = depth_mean_scaled, modx = Zone,
  interval = TRUE, int.type = c("prediction"), line.thickness = 0.5,
  outcome.scale = "response", 
  # plot.points = TRUE,
  #partial.residuals = T,
  point.alpha = 0.25,
  point.shape = T,
  modx.values = c("Demersal", "Benthopelagic"),
  vary.lty =TRUE
) + geom_point(data = filter(ddatdo, chopstick == "low" & age == "Mature"),
  aes(depth_mean_scaled, slope_est, colour = Zone), alpha = 1, size = 1.5, shape = 19,
  inherit.aes = F) +
    geom_point(data = filter(ddatdo, chopstick == "low" & age == "Immature"),
      aes(depth_mean_scaled, slope_est, colour = Zone), alpha = 1, size = 1.5, shape = 21,
      inherit.aes = F) +
    geom_linerange(data = filter(ddatdo, chopstick == "low"), 
      aes(x = depth_mean_scaled,
        ymin = (slope_est - slope_se * 1.96),
        ymax = (slope_est + slope_se * 1.96),
        colour = Zone), alpha = 1, fatten = 1, inherit.aes = F) +
    # geom_linerange(data = filter(ddatdo, chopstick == "low"), 
    #   aes(x = depth_mean_scaled,
    #     ymin = (slope_est - slope_se * 1.96),
    #     ymax = (slope_est + slope_se * 1.96),
    #     colour = Schooling), alpha = 0.2, fatten = 1, inherit.aes = F) +
    scale_colour_manual(values = c("royalblue4", "deepskyblue3")) +
    scale_fill_manual(values = c("royalblue4", "deepskyblue3")) +
    # scale_colour_manual(values = c("darkorchid4", "maroon")) +
    # scale_fill_manual(values = c("darkorchid4", "maroon")) +
    ## alternate colours 
    # scale_shape_manual(values = c(19, 19)) +
    # back transform axis labels on scaled depth iqr
    scale_x_continuous(labels = function(x) 
      paste0(round((x * attributes(do_slopes$depth_mean_scaled)[[3]] + 
          attributes(do_slopes$depth_mean_scaled)[[2]])))) +
    # coord_cartesian(ylim = c(-10,5)) +
    gfplot::theme_pbs() + 
    # ylab("Peak Frequency") +
    xlab("Mean depth occupied") +
    # labs(tag = "D") + 
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      # legend.title = element_blank(),
      legend.position = "none")
  # legend.position = c(0.8,0.15))
  # legend.position = c(0.2,0.85))
)

#### SCHOOLING ####

doslopemod <- lmerTest::lmer(slope_est ~
    Schooling +
    depth_mean_scaled +
    # Schooling * depth_mean_scaled +
    chopstick +
    # age +
    (1|species) + (1|species_age), REML = T,
  data = ddatdo) 

# doslopemod <- lmerTest::lmer(slope ~
#     Schooling +
#     depth_mean_scaled +
#     Schooling * depth_mean_scaled +
#     (1|species), REML = T,
#   data = ddatdo1)

doslopemod %>% summary()


(pd_depth_sch <- interactions::interact_plot(doslopemod, 
  pred = depth_mean_scaled, modx = Schooling, 
  interval = TRUE, int.type = c("prediction"), line.thickness = 0.5, 
  outcome.scale = "response", 
  # plot.points = TRUE,
  #partial.residuals = T, 
  point.alpha = 0.005,
  point.shape = T, 
  modx.values = c("Solitary", "Schooling"),
  vary.lty =TRUE
) + geom_point(data = filter(ddatdo, chopstick == "low" & age == "Mature"),
      aes(depth_mean_scaled, slope_est, colour = Schooling), alpha = 1, size = 1.5, shape = 19,
      inherit.aes = F) +
    geom_point(data = filter(ddatdo, chopstick == "low" & age == "Immature"),
      aes(depth_mean_scaled, slope_est, colour = Schooling), alpha = 1, size = 1.5, shape = 21,
      inherit.aes = F) +
    geom_linerange(data = filter(ddatdo, chopstick == "low"), 
      aes(x = depth_mean_scaled,
        ymin = (slope_est - slope_se * 1.96),
        ymax = (slope_est + slope_se * 1.96),
        colour = Schooling), alpha = 1, fatten = 1, inherit.aes = F) +
    # geom_linerange(data = filter(ddatdo, chopstick == "low"), 
    #   aes(x = depth_mean_scaled,
    #     ymin = (slope_est - slope_se * 1.96),
    #     ymax = (slope_est + slope_se * 1.96),
    #     colour = Schooling), alpha = 0.2, fatten = 1, inherit.aes = F) +
    scale_colour_manual(values = c("royalblue4", "deepskyblue3")) +
    scale_fill_manual(values = c("royalblue4", "deepskyblue3")) +
    # back transform axis labels on scaled depth iqr
    scale_x_continuous(labels = function(x) 
      paste0(round((x * attributes(do_slopes$depth_mean_scaled)[[3]] + 
          attributes(do_slopes$depth_mean_scaled)[[2]])))) +
    # coord_cartesian(ylim = c(-10,5)) +
    gfplot::theme_pbs() + 
    # ylab("Peak Frequency") +
    xlab("Depth range (IQR)") +
    # labs(tag = "D") + 
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0, "cm"),
      axis.title=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title.align=0,
      # legend.title = element_blank(),
      legend.position = "none")
      # legend.position = c(0.8,0.15))
      # legend.position = c(0.2,0.85))
)

### model checks ####

ddatdo %>% mutate(pearson = residuals(doslopemod,type="pearson"),
  # x = Schooling
  # x = Latitude
  # x = Trophic
  # x = Zone
  x = depth_mean_scaled
) %>% 
  ggplot(aes(x ,y=pearson)) +
  geom_point() +
  theme_bw()

#### SAVE FIGURE ####
# (p_depth_lat + p_depth_zone + p_depth_troph + plot_layout(ncol = 3))/grid::textGrob("Depth range (IQR)",
#   just = 0.3, gp = grid::gpar(fontsize = 11)) + plot_layout(nrow = 2, heights = c(1, 0.02))

# (p_depth_sch + p_depth_zone + plot_layout(ncol = 3))/grid::textGrob("Depth range (IQR)", 
#   just = 0.3, gp = grid::gpar(fontsize = 11)) + plot_layout(nrow = 2, heights = c(1, 0.02))

# (p_depth_lat + p_depth_troph + p_sch + 
#     plot_layout(ncol = 3, widths = c(1,1,0.5)))/grid::textGrob("Depth range (IQR)",
#     just = 1, gp = grid::gpar(fontsize = 11)) + plot_layout(nrow = 2, heights = c(1, 0.02))

# ggsave(here::here("ms", "figs", "ecology-slope-model-2.pdf"), width = 10, height = 3.5)
# 
# (p_depth_lat + p_depth_troph + p_depth_zone + p_depth_sch +
#     plot_layout(ncol = 4, widths = c(1,1,1,1)))/grid::textGrob("Depth range (IQR)",
#       just = 0.3, gp = grid::gpar(fontsize = 11)) + plot_layout(nrow = 2, heights = c(1, 0.02)) 
# 
# ggsave(here::here("ms", "figs", "ecology-slope-model-4-trimmed.pdf"), width = 12, height = 3.5)
# 
# 


((p_depth_lat + p_depth_troph + p_depth_zone + p_depth_sch +
    plot_layout(ncol = 4, widths = c(1,1,1,1)))
# /grid::textGrob("Depth range (IQR)",
#       just = 0.3, gp = grid::gpar(fontsize = 11))
  /
    (pd_depth_lat + pd_depth_troph + pd_depth_zone + pd_depth_sch +
          plot_layout(ncol = 4, widths = c(1,1,1,1)))/grid::textGrob("Mean depth occupied",
            just = 0.3, gp = grid::gpar(fontsize = 11)) + 
    # plot_layout(nrow = 4, heights = c(1, 0.02, 1, 0.02))) 
    plot_layout(nrow = 3, heights = c(1, 1, 0.02))) 

ggsave(here::here("ms", "figs", "ecology-slope-model-trimmed.pdf"), width = 12, height =7)

((p_depth_lat + p_depth_troph + p_depth_zone + p_depth_sch +
    plot_layout(ncol = 4, widths = c(1,1,1,1)))/grid::textGrob("",
      just = 0.3, gp = grid::gpar(fontsize = 1))/
    (pd_depth_lat + pd_depth_troph + pd_depth_zone + pd_depth_sch +
        plot_layout(ncol = 4, widths = c(1,1,1,1)))/grid::textGrob("Mean depth occupied",
          just = 0.3, gp = grid::gpar(fontsize = 11)) + plot_layout(nrow = 4, heights = c(1, 0.001, 1, 0.02))) 

ggsave(here::here("ms", "figs", "ecology-slope-model-vel.pdf"), width = 12, height =7)

# ggsave(here::here("ms", "figs", "ecology-slope-models-all2.pdf"), width = 12, height = 7)


##### effect code for factors or without interactions #####
(p_lat <- effect_plot(tempslopemod, pred = Latitude,
  interval = TRUE, int.type = c("prediction"),
  # cat.interval.geom = "linerange", 
  line.thickness = 0.5, point.alpha = 1, cat.pred.point.size = 2.5,
  # partial.residuals = T,  # plot.points = TRUE, # point.alpha = 0.2,
  outcome.scale = "response"
) + geom_hline(yintercept = 0, colour = "grey") +
    geom_violin(data = filter(temp_slopes, chopstick == "high"), aes(Latitude, slope, colour = Latitude, fill = Latitude), alpha = 0.2, inherit.aes = F) +
    
    scale_colour_manual(values = c("royalblue4", "red 4")) +
    scale_fill_manual(values = c("royalblue4", "red 3")) + 
    # scale_colour_manual(values = c("darkorchid4", "royalblue4")) +
    # scale_shape_manual(values = c(21, 19)) +
    coord_cartesian(ylim = c(-10,5) ) +
    ylab("Biomass change ~ warming rate") +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0, 0.2, 0, "cm"),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title = element_blank(),
      legend.position = "none"
    )
)

(p_zone <- effect_plot(tempslopemod, pred = Zone,
  interval = TRUE, int.type = c("prediction"),
  # cat.interval.geom = "linerange", 
  line.thickness = 0.5, point.alpha = 1, cat.pred.point.size = 2.5,
  # partial.residuals = T,  # plot.points = TRUE, # point.alpha = 0.2,
  outcome.scale = "response"
) + geom_hline(yintercept = 0, colour = "grey") +
    geom_violin(data = filter(temp_slopes, chopstick == "high"), 
      aes(Zone, slope, colour = Zone, fill = Zone), 
      alpha = 0.2, inherit.aes = F) +
    scale_colour_manual(values = c("royalblue4", "red 4")) +
    scale_fill_manual(values = c("royalblue4", "red 3")) +
    # scale_colour_manual(values = c("darkcyan", "orange")) +
    # scale_fill_manual(values = c("darkcyan", "orange")) + 
    # scale_colour_manual(values = c("darkorchid4", "royalblue4")) +
    coord_cartesian(ylim = c(-10,5) ) +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0, 0.2, 0, "cm"),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title = element_blank(),
      legend.position = "none"
    )
)

(p_troph <- effect_plot(tempslopemod, pred = Trophic,
  interval = TRUE, int.type = c("prediction"),
  # cat.interval.geom = "linerange", 
  line.thickness = 0.5, point.alpha = 1, cat.pred.point.size = 2.5,
  # partial.residuals = T,  # plot.points = TRUE, # point.alpha = 0.2,
  outcome.scale = "response"
) + geom_hline(yintercept = 0, colour = "grey") +
    geom_violin(data = filter(temp_slopes, chopstick == "high"), 
      aes(Trophic, slope, colour = Trophic, fill = Trophic), 
      alpha = 0.2, inherit.aes = F) +
    scale_colour_manual(values = c("royalblue4", "red 4")) +
    scale_fill_manual(values = c("royalblue4", "red 3")) +
    # scale_colour_manual(values = c("darkcyan", "orange")) +
    # scale_fill_manual(values = c("darkcyan", "orange")) + 
    # scale_colour_manual(values = c("darkorchid4", "royalblue4")) +
    
    coord_cartesian(ylim = c(-10,5) ) +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0, 0.2, 0, "cm"),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title = element_blank(),
      legend.position = "none"
    )
)

(p_chop <- effect_plot(tempslopemod, pred = chopstick,
  interval = TRUE, int.type = c("prediction"),
  line.thickness = 0.5, point.alpha = 1, cat.pred.point.size = 2.5,
  # cat.interval.geom = "linerange", 
  # partial.residuals = T,  # plot.points = TRUE, # point.alpha = 0.2,
  outcome.scale = "response"
) + geom_hline(yintercept = 0, colour = "grey") +
    geom_violin(data = temp_slopes, 
      aes(chopstick, slope, colour = chopstick, fill = chopstick), 
      alpha = 0.2, inherit.aes = F) +
    scale_colour_manual(values = c("red 4", "royalblue4")) +
    scale_fill_manual(values = c("red 3", "royalblue4")) +
    scale_x_discrete(limits = rev(levels(as.factor(temp_slopes$chopstick)))) +
    # scale_colour_manual(values = c("darkcyan", "orange")) +
    # scale_fill_manual(values = c("darkcyan", "orange")) + 
    # scale_colour_manual(values = c("darkorchid4", "royalblue4")) +
    xlab("Mean temperature") +
    coord_cartesian(ylim = c(-10,5) ) +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0, 0.2, 0, "cm"),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title = element_blank(),
      legend.position = "none"
    )
)



(p_age <- effect_plot(tempslopemod2, pred = age,
  interval = TRUE, int.type = c("prediction"),
  line.thickness = 0.5, point.alpha = 1, cat.pred.point.size = 2.5,
  # cat.interval.geom = "linerange", 
  # partial.residuals = T,  # plot.points = TRUE, # point.alpha = 0.2,
  outcome.scale = "response"
) + geom_hline(yintercept = 0, colour = "grey") +
    geom_violin(data = temp_slopes, 
      aes(age, slope, colour = age, fill = age), 
      alpha = 0.2, inherit.aes = F) +
    scale_colour_manual(values = c("royalblue4", "deepskyblue3")) +
    scale_fill_manual(values = c("cornflowerblue", "deepskyblue")) +
    scale_x_discrete(limits = rev(levels(as.factor(temp_slopes$age)))) +
    # scale_colour_manual(values = c("darkcyan", "orange")) +
    # scale_fill_manual(values = c("darkcyan", "orange")) + 
    # scale_colour_manual(values = c("darkorchid4", "royalblue4")) +
    xlab("Mean temperature") +
    coord_cartesian(ylim = c(-10,5) ) +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0.2, 0, 0.2, 0, "cm"),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.tag.position = c(0.225,0.925),
      legend.title = element_blank(),
      legend.position = "none"
    )
)
#
# (p_lat + p_zone + p_troph + p_chop + plot_layout(nrow = 1))
# 
# # ggsave(here::here("ms", "figs", "ecology-slope-model-violins.pdf"), width = 7, height = 3.5)
# 
# (p_depth_lat + p_lat +p_depth_zone +p_zone + p_depth_troph + p_troph + 
#     plot_layout(nrow = 1, widths = c(1, 0.2, 1, 0.2, 1, 0.2)))/grid::textGrob("Depth range (IQR)", 
#       just = 0.3, gp = grid::gpar(fontsize = 11)) + plot_layout(nrow = 2, heights = c(1, 0.02))
# 
# ggsave(here::here("ms", "figs", "ecology-slope-model-w-violins.pdf"), width = 12, height = 3.5)
# 
# (p_depth_lat + p_depth_sch + p_depth_troph + 
#     plot_layout(nrow = 1))/grid::textGrob("Depth range (IQR)", 
#       just = 0.3, gp = grid::gpar(fontsize = 11)) + plot_layout(nrow = 2, heights = c(1, 0.02))
# 
# ggsave(here::here("ms", "figs", "ecology-slope-models-IQR.pdf"), width = 10, height = 3.5)
# 
# (p_depth_age + p_age + p_growth_rate +  p_log_age + plot_layout(nrow = 1, widths = c(1,0.2,1,1)))
# 
# ggsave(here::here("ms", "figs", "age-slope-model-quad.pdf"), width = 9, height = 3.5)


#
#
#
#
#

##############################
#########################################
### MIXED MODELS for independent effects ####
# including age doesn't change anything ####
# temp_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick * age + (1|species), data = .) %>% summary() # *
# do_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick * age + (1|species), data = .) %>% summary() # *

ann_text <- data.frame(slope = c(-11.5,-3.7), 
  chopstick = c("High", "Low"), tag = c("*","*"), slope_type = c("high temp", "low DO"),
  type = factor(c("temp","DO"),levels = c("temp","DO")))

(p1 <- long_slopes %>% mutate(chopstick = factor(chopstick, levels = c("low", "high"), labels = c("Low", "High"))) %>%
    ggplot( aes(slope, chopstick, colour = slope_type, fill = slope_type )) + 
    ylab("Mean\nClimate") +
    scale_colour_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan") ) + 
    scale_fill_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan")) + 
    geom_violin( alpha = 0.1) + #scale = "width",
    geom_vline(xintercept = 0, colour = "grey") +
    # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
    geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
    scale_x_continuous(expand = c(0.1, 0)) +
    facet_grid(cols = vars(type), scales = "free") + 
    geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") +
    gfplot:::theme_pbs() + 
    theme(axis.title.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      strip.text.x = element_blank(),
      plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
      legend.position = "none"
      # legend.title = element_blank(),
      # legend.position = c(.6, .3)
    ) )
### MATURITY ####

### No sig diff between maturity classes although only immature show consitant non-zero effects ####
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + age + (1|species) , data = .) %>% summary() 
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 0 + age + (1|species) , data = .) %>% summary()
filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + age + (1|species), data = .) %>% summary()
filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 0 + age + (1|species), data = .) %>% summary()

ann_text <- data.frame(slope = c(-11.5,-3.7), 
  age = c("Immature", "Immature"), tag = c("-","*"),
  type = factor(c("temp","DO"),levels = c("temp","DO")))

p2 <- ggplot(best_slopes, aes(slope, age, colour = slope_type, fill = slope_type )) + 
  ylab("Maturity") +
  geom_text(data = ann_text, aes(slope, age , label = tag), size = 6, inherit.aes = F) + 
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin( alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  # ggtitle("Range limits") +
  gfplot:::theme_pbs() + 
  theme(axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) 


### LATITUDE ####

# use moscaic plots to check how unbalanced groups are
# library(ggmosaic)
plot(Latitude~Rockfish, data = filter(best_slopes, type == "temp")) 
plot(Latitude~Zone, data = filter(best_slopes, type == "temp")) 
plot(Latitude~Trophic, data = filter(best_slopes, type == "temp")) 

# temp_slopes %>% lmerTest::lmer(slope ~ 0 + Latitude + (1|species) + (1|species_age), data = .) %>% summary() # *
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Latitude + (1|species), data = .) %>% anova() # *


# do_slopes %>% lmerTest::lmer(slope ~ 0 + Latitude + (1|species) + (1|species_age), data = .) %>% summary() 
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + Latitude + (1|species), data = .) %>% anova()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + Latitude + (1|species), data = .) %>% summary()

ann_text <- data.frame(
  Latitude = c("North"), 
  slope_type = c("high temp"),
  slope = c(-11.5, -3.7), tag = c("*", " "), 
  type = factor(c("temp", "DO"), levels = c("temp","DO")))


best_slopes <- mutate(best_slopes, Latitude = factor(Latitude, levels = c("South", "North")))

p3 <- ggplot(best_slopes, aes(slope, Latitude, colour = slope_type, fill = slope_type )) + 
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin( alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") + 
  ylab("Latitude") +
  gfplot:::theme_pbs() + 
  theme(axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) 

### ROCKFISH ####
plot(Rockfish~age, data = filter(best_slopes, type == "temp")) 

### No sig diff between rockfish and other fishes but rockfish do show consitant non-zero effects more than other ####
# temp_slopes %>% lmerTest::lmer(slope ~ 0 + Rockfish + (1|species) + (1|species_age), data = .) %>% summary()
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Rockfish + (1|species), data = .) %>% anova() 
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 0 + Rockfish + (1|species), data = .) %>% summary() # *

# do_slopes %>% lmerTest::lmer(slope ~ 0 + Rockfish + (1|species) + (1|species_age), data = .) %>% summary()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + Rockfish + (1|species), data = .) %>% summary()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 0 + Rockfish + (1|species), data = .) %>% summary()

ann_text <- data.frame(
  Rockfish = c("Rockfish"), 
  slope_type = c("high temp"),
  slope = c(-11.5, -3.7), tag = c("-", " "), 
  type = factor(c("temp", "DO"), levels = c("temp","DO")))

p4 <- ggplot(best_slopes, aes(slope, Rockfish, colour = slope_type, fill = slope_type )) + 
  ylab("Rockfish") +
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin( alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") + 
  gfplot:::theme_pbs() + 
  theme(axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) 

### SOCIALITY ####
plot(Schooling~age, data = filter(best_slopes, type == "temp")) 
plot(Schooling~Latitude, data = filter(best_slopes, type == "temp")) 
plot(Schooling~Zone, data = filter(best_slopes, type == "temp")) 
plot(Schooling~Rockfish, data = filter(best_slopes, type == "temp")) 

### No sig diff between groups but schooling fishes do show consitant non-zero effects more than solitary ####
# temp_slopes %>% lmerTest::lmer(slope ~ 0 + Schooling + (1|species) + (1|species_age), data = .) %>% summary() # .
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Schooling + (1|species), data = .) %>% anova() 
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 0 + Schooling + (1|species), data = .) %>% summary() # *

# do_slopes %>% lmerTest::lmer(slope ~ 0 + Schooling + (1|species) + (1|species_age), data = .) %>% summary()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 0 + Schooling + age + (1|species), data = .) %>% summary()
ann_text <- data.frame(
  Schooling = c("Schooling"), 
  slope_type = c("high temp"),
  slope = c(-11.5, -3.7), tag = c("-", " "), 
  type = factor(c("temp", "DO"), levels = c("temp","DO")))

p5 <- ggplot(best_slopes, aes(slope,  Schooling, colour = slope_type, fill = slope_type )) + 
  ylab("Schooling") +
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin( alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") + 
  gfplot:::theme_pbs() + 
  theme(axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) 

### ZONE ####
plot(Zone~age, data = filter(best_slopes, type == "temp")) 
plot(Zone~Schooling, data = filter(best_slopes, type == "temp")) 
plot(Zone~Rockfish, data = filter(best_slopes, type == "temp")) 

temp_slopes %>% lmerTest::lmer(slope ~ 0 + Zone + (1|species) + (1|species_age), data = .) %>% summary() # **

filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Zone + (1|species), data = .) %>% anova() # **
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 0 + Zone + (1|species), data = .) %>% summary() # ***

# do_slopes %>% lmerTest::lmer(slope ~ 0 + Zone + (1|species) + (1|species_age), data = .) %>% summary()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + Zone + (1|species), data = .) %>% anova()
# filter(do_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Zone * Schooling + (1|species), data = .) %>% anova()
# filter(do_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Zone * Rockfish + (1|species), data = .) %>% anova()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 0 + Zone + (1|species), data = .) %>% summary()

ann_text <- data.frame(
  Zone = c("Benthopelagic"), 
  slope_type = c("high temp"),
  slope = c(-11.5, -3.7), tag = c("*", " "), 
  type = factor(c("temp", "DO"), levels = c("temp","DO")))

p6 <- ggplot(best_slopes, aes(slope,  Zone, colour = slope_type, fill = slope_type )) + 
  ylab("Zone") +
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") + 
  gfplot:::theme_pbs() + 
  theme(axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) 

### DIET ####
# not sig on its own, 
# temp_slopes %>% lmerTest::lmer(slope ~ 1 + Diet + (1|species) + (1|species_age), data = .) %>% anova() # .
# filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Diet + (1|species), data = .) %>% anova()

### but not all diets are represented in all zones, in north, or amoungst rockfish
plot(Diet~age, data = filter(best_slopes, type == "temp"))
plot(Diet~Rockfish, data = filter(best_slopes, type == "temp"))
plot(Diet~Latitude, data = filter(best_slopes, type == "temp"))
plot(Diet~Zone, data = filter(best_slopes, type == "temp")) 

# filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Diet * Zone + (1|species), data = .) %>% anova()
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope_trim ~ 1 + Diet * Rockfish + (1|species), data = .) %>% anova() # *
# filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope_trim ~ 1 + Diet * Latitude + (1|species), data = .) %>% anova()

# 
## split by age shows effects are only in immatures 
# filter(temp_slopes, age == "Immature") %>% #filter(Zone == "Benthopelagic") %>% 
#   # filter(chopstick == "high") %>%
#   lm(slope_trim ~ 1 + Diet + Zone, data = .) %>% anova() # ***
filter(temp_slopes, age == "Immature") %>% #filter(Zone == "Benthopelagic") %>% 
  # filter(chopstick == "high") %>%
  lm(slope_trim ~ 0 + Diet + Zone, data = .) %>% summary() # ***

filter(temp_slopes, age == "Mature") %>% #filter(Zone == "Benthopelagic") %>%
  #   # filter(chopstick == "high") %>%
  lm(slope ~ 1 + Diet , data = .) %>% anova()


# do_slopes %>% lmerTest::lmer(slope ~ 1 + Diet + (1|species) + (1|species_age), data = .) %>% anova()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + Diet + (1|species), data = .) %>% anova()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + Diet * Zone * age + (1|species), data = .) %>% anova()

ann_text <- data.frame(slope = c(-11.5, -11.5, -3.7),
  Diet = c("Zooplankton", "Fish", "Fish"), 
  Trophic = c("Lower", "Higher", "Higher"), 
  Specialist = c("Specialist", "Specialist", "Specialist"), 
  slope_type = c("high temp", "high temp", "low DO"), 
  tag = c(" ", " ", " "),
  type = factor(c("temp", "temp", "DO"), levels = c("temp","DO")))

(p7 <- ggplot(best_slopes, aes(slope, Diet, colour = slope_type, fill = slope_type)) + 
    ylab("Diet") +
    scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
    scale_fill_manual(values = c("Red 3", "darkcyan")) + 
    geom_violin(alpha = 0.1) + #scale = "width",
    geom_vline(xintercept = 0, colour = "grey") +
    # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
    geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
    scale_x_continuous(expand = c(0.1, 0)) +
    facet_grid(cols = vars(type), scales = "free") + 
    geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") + 
    gfplot:::theme_pbs() + 
    theme(#axis.title.x = element_blank(),
      # axis.text.x = element_blank(),
      strip.text.x = element_blank(),
      plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
      legend.position = "none"
    ) )

(p8 <- ggplot(best_slopes, aes(slope, Trophic, colour = slope_type, fill = slope_type)) + 
    ylab("Trophic-level") +
    scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
    scale_fill_manual(values = c("Red 3", "darkcyan")) + 
    geom_violin(alpha = 0.1) + #scale = "width",
    geom_vline(xintercept = 0, colour = "grey") +
    # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
    geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
    scale_x_continuous(expand = c(0.1, 0)) +
    facet_grid(cols = vars(type), scales = "free") + 
    geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") +
    gfplot:::theme_pbs() + 
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      strip.text.x = element_blank(),
      plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
      legend.position = "none"
    ) )

p9 <- ggplot(best_slopes, aes(slope, Specialist, colour = slope_type, fill = slope_type)) + 
  ylab("Diet") +
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") +
  gfplot:::theme_pbs() + 
  theme(#axis.title.x = element_blank(),
    # axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) 
p9b <- ggplot(best_slopes, aes(slope, Specialist, colour = slope_type, fill = slope_type)) + 
  ylab("Diet") +
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") +
  gfplot:::theme_pbs() + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) 
(p7b <- ggplot(filter(best_slopes, Diet != "Generalist"), aes(slope, Diet, colour = slope_type, fill = slope_type)) + 
    ylab("Specialist type") +
    scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
    scale_fill_manual(values = c("Red 3", "darkcyan")) + 
    geom_violin(alpha = 0.1) + #scale = "width",
    geom_vline(xintercept = 0, colour = "grey") +
    # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
    geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
    scale_x_continuous(expand = c(0.1, 0)) +
    facet_grid(cols = vars(type), scales = "free") + 
    geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") + 
    gfplot:::theme_pbs() + 
    theme(#axis.title.x = element_blank(),
      # axis.text.x = element_blank(),
      strip.text.x = element_blank(),
      plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
      legend.position = "none"
    ) )

# p1 + p2 + p3 + p4 + p5 + p6 + p7 + patchwork::plot_layout(ncol=1, heights = c(1, 1, 1, 1, 1, 1, 2.3)) 
# p1 + p2 + p3 + p4 + p5 + p6 + p8 + p9 + patchwork::plot_layout(ncol=1, ) 
p1 + p2 + p3 + p4 + p5 + p6 + p8 + p9b + p7b + patchwork::plot_layout(ncol=1, heights = c(1, 1, 1, 1, 1, 1, 1, 1, 2.3)) 

ggsave(here::here("ms", "figs", "behav-slope-diff-zero2.pdf"), width = 5, height = 10 )


#### explore zone and diet combinations ####
temp_slopes %>% filter(chopstick == "high") %>% 
  # filter(age == "Immature") %>%
  ggboxplot(x = "Diet", y = "slope",  color = "slope_type", id = "species", 
    ylab = "Slope", repel = T, facet.by = c("Zone")) + 
  scale_colour_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan")) + 
  geom_point() +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  # scale_y_continuous(trans = fourth_root_power) +
  facet_grid(rows = vars(Zone), cols = vars(age),
    scales = "free") +
  stat_compare_means(
    ref.group = "Zooplankton",
    aes(label = paste0("p = ", ..p.format..)),
    # aes(label = paste0(..p.signif..)), 
    # label.x = c(0.1,0.25,0.5,0.75,0.9), #sort(unique(temp_slopes$Diet))# method = "t.test",
    method = "wilcox.test"
  ) + theme(legend.position = "none")

# Specialists
temp_slopes %>% filter(chopstick == "high") %>% 
  # filter(age == "Immature") %>%
  ggboxplot(x = "Specialist", y = "slope",  color = "slope_type", id = "species", 
    ylab = "Slope", repel = T, facet.by = c("Zone")) + 
  scale_colour_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan")) + 
  geom_point() +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  # scale_y_continuous(trans = fourth_root_power) +
  facet_grid(rows = vars(Zone), cols = vars(age),
    scales = "free") +
  stat_compare_means(
    ref.group = "Specialist",
    aes(label = paste0("p = ", ..p.format..)),
    # aes(label = paste0(..p.signif..)), 
    # label.x = c(0.1,0.25,0.5,0.75,0.9), #sort(unique(temp_slopes$Diet))# method = "t.test",
    method = "wilcox.test"
  ) + theme(legend.position = "none")

# trophic level 
ggboxplot(filter(temp_slopes, chopstick == "high"), x = "Trophic", y = "slope",  color = "slope_type", id = "species", 
  ylab = "Slope", repel = T, facet.by = c("Zone")) + 
  scale_colour_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan")) + 
  geom_point() +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  facet_grid(rows = vars(Zone), #cols = vars(Diet), 
    scales = "free") + 
  scale_y_continuous(trans = fourth_root_power) +
  stat_compare_means(
    ref.group = "Low",
    aes(label = paste0("p = ", ..p.format..)),
    # aes(label = paste0(..p.signif..)), 
    # label.x = c(0.1,0.25,0.5,0.75,0.9), #sort(unique(temp_slopes$Diet))# method = "t.test",
    method = "wilcox.test"
  ) + theme(legend.position = "none")

#### try adding to above 
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + log_age_scaled + Trophic * Zone + Specialist * Zone + Latitude + max_mass_scaled + growth_rate_scaled * age + (1|species), data = .) %>% anova()

filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + log_age_scaled + Trophic * Zone + Specialist + Zone + Latitude + max_mass_scaled + growth_rate_scaled * age + (1|species), data = .) %>% anova()

filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + log_age_scaled + Trophic + Zone + Latitude + growth_rate_scaled * age + (1|species), data = .) %>% anova()

filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + log_age_scaled + Specialist + Zone + Latitude + growth_rate_scaled * age + (1|species), data = .) %>% anova()




#### preliminary conclusions #### 
# temperature has stronger -ve effects for higher trophic level, northern, Benthopelagic, and maybe immatures
# most -ve for immatures in benthopalagic zones
### COEFFICIENT MODELS ####

model2 <- model2 %>%
  group_by(group) %>%
  mutate(spp_count = length(unique(species))) %>%
  ungroup()
model2 <- model2 %>% mutate(group = forcats::fct_reorder(group, Estimate, .desc = F))
model2 <- model2 %>% mutate(rockfish = forcats::fct_reorder(rockfish, Estimate, .desc = F))
trendeffects <- model2 %>%
  filter(coefficient %in% c("temp_trend_scaled", "DO_trend_scaled")) %>%
  # transform(coefficient = factor(coefficient,
  mutate(coefficient = factor(coefficient,
    levels = c("temp_trend_scaled", "DO_trend_scaled"),
    labels = c("Temperature", "DO")
  ), Std..Error = `Std. Error`)

trendeffects <- trendeffects %>%
  mutate(coefficient = forcats::fct_reorder(coefficient, Estimate, .desc = F))
# trendeffects <- mutate(trendeffects, rockfish = firstup(rockfish) # didn't work
trendeffects$allspp <- "All species"

trendeffects <- 
  trendeffects %>% mutate(
    # type = factor(type, levels = c("temp", "DO")),
    species_age = paste(species, age),
    age = factor(age, levels = c("immature", "mature"),  labels = c("Immature", "Mature")),
    Rockfish = factor(rockfish, levels = c("rockfish", "other fishes"),  labels = c("Rockfish", "Other fishes")),
    # slope_type = factor(slope_type, levels = c("high temp", "low temp", "high DO", "low DO")),
    Diet = factor(Diet, levels = c("Zooplankton", "Generalist", "Polychaetes", "Crustaceans", "Fish")),
    Zone = factor(BenthoPelagicPelagicDemersal, levels = c("Demersal", "Benthopelagic", "Pelagic")),
    Latitude = factor(NorthMiddleSouth, levels = c("North", "Middle", "South")),
    Schooling = as.factor(Schooling),
    Trophic = factor(if_else(Diet == "Zooplankton", "Lower", "Higher"), levels = c("Lower", "Higher")),
    Specialist = factor(if_else(Diet == "Generalist", "Generalist", "Specialist"), levels = c("Generalist", "Specialist")),
    depth_iqr_scaled = scale(log(depth_iqr), center = T),
    log_age_scaled = scale(log(age_mean + 1), center = T),
    max_mass_scaled = scale(log(weight_99th + 1), center = T),
    # age_mat is the 95 quantile of ages for immature females
    growth_rate_scaled = scale(log((length_50_mat_f / age_mat)+1), center = T))

# collapse Middle and South together because only 3 "southern" species
trendeffects$Latitude[trendeffects$Latitude == "Middle"] <- "South"

# collapse Pelagic into Benthopelagic because only 3 "pelagic" species
trendeffects$Zone[trendeffects$Zone == "Pelagic"] <- "Benthopelagic"

trendeffects <- trendeffects %>% mutate(
  Zone = factor(Zone, levels = c("Demersal", "Benthopelagic")),
  Latitude = factor(Latitude, levels = c("North", "South"))
)

filter(trendeffects, coefficient == "Temperature") %>% lmerTest::lmer(Estimate ~ 1 + depth_iqr_scaled * Trophic + depth_iqr_scaled * age + depth_iqr_scaled * Zone + depth_iqr_scaled * Latitude + (1|species), data = .) %>% anova()

# add in age-based variables that limit the species included
filter(trendeffects, coefficient == "Temperature") %>% lmerTest::lmer(Estimate ~ 1 + depth_iqr_scaled * age + growth_rate_scaled * age + log_age_scaled + (1|species), data = .) %>% summary()


###################
#### OLD CODE ####
### basic boxplots for slopes ####
# p0 <- ggplot(long_slopes, aes(age, slope, colour =slope_type, fill = slope_type )) + 
#   # geom_violin( alpha = 0.1) + #scale = "width",
#   geom_hline(yintercept = 0, colour = "grey") +
#   geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
#   geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
#   # scale_y_continuous(trans = fourth_root_power) +
#   scale_colour_brewer(palette = "Spectral", direction = 1) + scale_fill_brewer(palette = "Spectral", direction = 1) +
#   # scale_colour_viridis_d(begin = .2, end = .8) + scale_fill_viridis_d(begin = .2, end = .8) +
#   facet_grid(rows = vars(type), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
#   ggtitle("Maturity") +
#   gfplot:::theme_pbs() + 
#   theme(plot.title = element_text(hjust = 0.5),
#     axis.title.x = element_blank(),
#     strip.text.y = element_blank(),
#     plot.margin = margin(0.1, 0, 0.1, 0, "cm"),
#     
#     # plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position = "none"
#   ) 
# 
# 
# p2 <- ggplot(long_slopes, aes(Schooling, slope, colour =slope_type, fill = slope_type )) + 
#   # geom_violin( alpha = 0.1) + #scale = "width",
#   geom_hline(yintercept = 0, colour = "grey") +
#   geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
#   geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
#   # scale_y_continuous(trans = fourth_root_power) +
#   scale_colour_brewer(palette = "Spectral", direction = 1) + scale_fill_brewer(palette = "Spectral", direction = 1) +
#   # scale_colour_viridis_d(begin = .2, end = .8) + scale_fill_viridis_d(begin = .2, end = .8) +
#   facet_grid(rows = vars(type), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
#   ggtitle("Sociality") +
#   gfplot:::theme_pbs() + 
#   theme(plot.title = element_text(hjust = 0.5),
#     axis.title.x = element_blank(),
#     strip.text.y = element_blank(),
#     plot.margin = margin(0.1, 0, 0.1, 0, "cm"),
#     
#     # plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position = "none"
#   ) 
# 
# p3 <- ggplot(long_slopes, aes(Zone, slope, colour = slope_type, fill = slope_type )) + 
#   # geom_violin( alpha = 0.1) + #scale = "width",
#   geom_hline(yintercept = 0, colour = "grey") +
#   geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
#   geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
#   # scale_y_continuous(trans = fourth_root_power) +
#   scale_colour_brewer(palette = "Spectral", direction = 1) + scale_fill_brewer(palette = "Spectral", direction = 1) +
#   # scale_colour_viridis_d(begin = .2, end = .8) + scale_fill_viridis_d(begin = .2, end = .8) +
#   facet_grid(rows = vars(type), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
#   ggtitle("Foraging zone") +
#   gfplot:::theme_pbs() + 
#   theme(plot.title = element_text(hjust = 0.5),
#     axis.title.x = element_blank(),
#     strip.text.y = element_blank(),
#     plot.margin = margin(0.1, 0, 0.1, 0, "cm"),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position = "none"
#   ) 
# 
# p4 <- ggplot(long_slopes, aes(Diet, slope, colour = slope_type, fill = slope_type )) + 
#   # geom_violin( alpha = 0.1) + #scale = "width",
#   geom_hline(yintercept = 0, colour = "grey") +
#   geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
#   geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
#   # scale_y_continuous(trans = fourth_root_power) +
#   scale_colour_brewer(palette = "Spectral", direction = 1) + scale_fill_brewer(palette = "Spectral", direction = 1) +
#   # scale_colour_viridis_d(begin = .2, end = .8) + scale_fill_viridis_d(begin = .2, end = .8) +
#   facet_grid(rows = vars(type), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) +
#   ggtitle("Diet") +
#   gfplot:::theme_pbs() + 
#   theme(plot.title = element_text(hjust = 0.5),
#     axis.title.x = element_blank(),
#     plot.margin = margin(0.1, 0.1, 0.1, 0, "cm"),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position = "none"
#   ) 
# p1 + p0 + p2 + p3 + p4 + patchwork::plot_layout(nrow = 1, widths = c(1, 0.66, 0.66, 0.9, 1.5))
# 
# ggsave(here::here("ms", "figs", "behav-slope-boxplots.pdf"), width = 13, height = 5)
# ggsave(here::here("ms", "figs", "behav-slope-boxplots-imm.pdf"), width = 12, height = 5)


###################