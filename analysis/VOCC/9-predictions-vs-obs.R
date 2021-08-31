# library(TMB)
library(tidyverse)
library(ggplot2)
library(sdmTMB)
# options(scipen = 999)

#### climate ####
m1 <- readRDS(here::here("analysis/tmb-sensor-explore/models/model-temp-all-years-800kn.rds"))
m2 <- readRDS(here::here("analysis/tmb-sensor-explore/models/model-temp-all-years-800kn-2.rds"))

(se1 <- as.list(m1$sd_report, "Std. Error"))
(se2 <- as.list(m2$sd_report, "Std. Error"))
# yes they appear identical!

m3 <- readRDS((here::here("analysis/tmb-sensor-explore/models/model-do-without-wcvi2016b-800kn-update.rds")))
m4 <- readRDS((here::here("analysis/tmb-sensor-explore/models/model-do-without-wcvi2016b-800kn-update-2.rds")))

(se3 <- as.list(m3$sd_report, "Std. Error"))
(se4 <- as.list(m4$sd_report, "Std. Error"))
# yes they appear identical!

predictions <- readRDS((here::here("analysis/tmb-sensor-explore/models/model-temp-all-years-800kn-predictions.rds")))
events_roms <- readRDS(here::here("analysis/tmb-sensor-explore/data/roms_temp_by_events2.rds")) %>% select(fishing_event_id, roms, year)

predictions <- left_join(predictions, events_roms) %>% filter(year >2007)


pred_do <- readRDS(file = here::here("analysis/tmb-sensor-explore/models/model-do-without-wcvi2016b-800kn-predictions.rds"))

# cor(pred_do$roms, pred_do$est, use = "pairwise.complete.obs")


tplot <- ggplot(predictions, aes(temperature_c, est, colour = year)) + 
  geom_point(alpha = 0.75, size = 0.75) +
  coord_fixed(xlim = c(2.5,14), ylim = c(2.5,14)) +
  # coord_cartesian(xlim = c(2,13), ylim = c(2,13)) +
  geom_abline(intercept = 0, slope = 1) +
  # labs(tag = 'a.') +
  scale_colour_viridis_c(name = "Year", option = "A", end = 0.75) +
  ylab("Predicted value") +
  xlab("Observed CTD value") +
  ggtitle("a.  Bottom Temperature (°C)") +
  ggsidekick::theme_sleek() + theme(
    axis.title.x = element_blank(), legend.position = c(0.15,0.75))

doplot <- ggplot(pred_do, aes(do_mlpl, exp(est), colour = year)) + 
  geom_point(alpha = 0.75, size = 0.75) +
  coord_fixed(expand = F, xlim = c(0.01,8.5), ylim = c(0.01,8.5)) +
  # coord_cartesian(xlim = c(2,13), ylim = c(2,13)) +
  geom_abline(intercept = 0, slope = 1) +
  # labs(tag = 'b.') +
  scale_colour_viridis_c(name = "Year", option = "A", end = 0.75) +
  # ylab("Predicted") +
  xlab("Observed CTD value") +
  ggtitle("b.  Bottom DO (ml per L)") +
  ggsidekick::theme_sleek() + theme(
    axis.title = element_blank(),
    legend.position = "none")

(tplot | doplot )/grid::textGrob("Observed CTD value", just = 0.5, gp = grid::gpar(fontsize = 10, col = "black", alpha = 0.75)) + patchwork::plot_layout(height = c(10, 0.02)) 

ggsave(here::here("ms/figs/supp-est-vs-obs-climate.jpg"), width = 6.5, height = 3.5)


(rplot <- ggplot(predictions, aes(roms, est, colour = year)) + 
  geom_point(alpha = 0.75, size = 1) +
  coord_fixed(xlim = c(2.5,14), ylim = c(2.5,14)) +
  # coord_cartesian(xlim = c(2,13), ylim = c(2,13)) +
  geom_abline(intercept = 0, slope = 1) +
  # labs(tag = 'a.') +
  scale_colour_viridis_c(name = "Year", option = "A", end = 0.75) +
  ylab("Mean May-August CTD-based Model Predictions") +
  xlab("Mean April-September ROMS Predictions") +
  # ggtitle("Contrast Models of Bottom Temperature (°C)") +
  ggsidekick::theme_sleek() + theme(
    # axis.title.x = element_blank(), 
    legend.position = c(0.15,0.8)))

ggsave(here::here("ms/figs/supp-est-vs-obs-roms.jpg"), width = 4.5, height = 4.5)


#### mature ####

species <- c(
  "Big Skate",
  "Longnose Skate",
  "Spotted Ratfish",
  "North Pacific Spiny Dogfish",
  # "Pacific Tomcod",
  "Walleye Pollock",
  "Pacific Cod",
  "Sablefish",
  "Lingcod", # already rerun
  # "Pacific Hake", # previously excluded
  "Rosethorn Rockfish", # previously excluded
  "Yellowmouth Rockfish",
  # "Harlequin Rockfish", # too small sample
  "Canary Rockfish", # already rerun
  # "Copper Rockfish", # too small sample
  "Darkblotched Rockfish",
  "Greenstriped Rockfish",
  "Pacific Ocean Perch", # schooling
  "Redbanded Rockfish", # already rerun
  "Sharpchin Rockfish",
  #"Shortbelly Rockfish", # too small sample
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  "Yellowtail Rockfish", # schooling
  # "Longspine Thornyhead", # too small sample
  "Shortspine Thornyhead",
  "Arrowtooth Flounder",
  "Rex Sole",
  "Petrale Sole",
  "English Sole", # already rerun
  "Dover Sole",
  "Southern Rock Sole",
  "Flathead Sole",
  "Curlfin Sole",
  # "Sand Sole",# too small sample
  "Slender Sole",# previously excluded
  "Pacific Sanddab",# previously excluded
  "Pacific Halibut",
  "Redstripe Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Widow Rockfish",
  "Quillback Rockfish",
  "Bocaccio", # rerun with fewer knots
  "Shortraker Rockfish",# small sample so run with fewer knots
  "Yelloweye Rockfish"
)



dat <- purrr::map_dfr(species, function(x){
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(x)))

# ~ July 2020 models
try({
  d <- readRDS(here::here(paste0("analysis/VOCC/data/", spp, "/check-mod-predictions-", spp, "-tv-depth-only-1n3n4n16-prior-FALSE.rds")))
})

## these intermediate ones may be for imm models? 
# try({
#   d <- readRDS(here::here(paste0("analysis/VOCC/data/", spp, "/check-mod-predictions-", spp, "-tv-depth-only-1n3n4n16.rds")))
# })

try({d <- readRDS(here::here(paste0("analysis/VOCC/data/", spp,
  "/check-mod-predictions-", spp, "-tv-depth-only-1n3n4n16-new.rds")))
})

try({
d <- filter(d, year > 2007)
d$species <- x

if (any(names(d) == "adult_density")) {
  d$density <- d$adult_density
}
})
try({d})

})

dat$species[dat$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"


dat <- dat %>% mutate(
  density2 = (density)*10000*1000,
  est_exp2 = exp(est)*10000*1000,
  density3 = ifelse(density2 == 0, 0.001, density2), 
  est_exp3 = ifelse(est_exp2 < 0.00001, 0.000001, est_exp2)
)

# raw densities are in kg/m2 so *10000*1000 to get g/ha
(g <- dat %>% 
    # filter(year %in% c(2008,2009,2018,2019)) %>%
    # ggplot( aes((density)*10000*1000, exp(est)*10000*1000))+
    ggplot(aes(density3, est_exp3, colour=year))+
    geom_segment(
      x = log10(0.06), 
      y = log10(0.06), 
      xend = log10(max(exp(dat$est))*10000*1000), 
      yend = log10(max(exp(dat$est))*10000*1000), 
      # x = log10(0.00000001), 
      # y = log10(0.00000001), 
      # xend = (1000), 
      # yend = (1000), 
      colour = "black", size = 0.1) +
    geom_jitter(width = 0.3, height = 0, 
      # alpha = 0.3, 
      size = 0.1, 
      shape = 20) + 
    # coord_cartesian(
    #   ylim=c(min(dat_imm$est_exp2), max(exp(dat_imm$est))*10000*1000), 
    #   xlim=c(0.01, max(dat_imm$density)*10000*1000)) +
    coord_fixed(
      expand = F,
      ylim=c(0.00001, max(exp(dat$density))*10000*1000),
      xlim=c(0.00001, max(exp(dat$density))*10000*1000)
    ) +
    scale_x_log10(
      # breaks = c(0.1, 1, 10, 1000),
      # labels = c(0.1, 1, 10, 1000)
      breaks = c(0.001, 1, 1000), 
      labels = c(0.001, 1, 1000)
    ) +
    scale_y_log10(
      breaks = c(0.001, 1, 1000), 
      labels = c(0.001, 1, 1000)
    ) +
    scale_colour_viridis_c(name = "Year", option = "A", end = 0.75) +
    ylab("Predicted biomass density (kg/ha)") +
    xlab("Observed biomass density (kg/ha)") +
    facet_wrap(~species, ncol = 8) +
    ggsidekick::theme_sleek())

ggsave(here::here("ms/figs/supp-est-vs-obs-jul-2020-mat2.jpg"), width= 14, height = 9.5)
 

#### new models ####


spp <- "pacific-cod"

spp <- "yelloweye-rockfish"

spp <- "quillback-rockfish" # converged
spp <- "widow-rockfish" # converged
# spp <- "pacific-ocean-perch" # converged
# spp <- "petrale-sole"  # converged


# m <- readRDS(here::here(paste0("analysis/VOCC/data/", spp, "/mod-imm-biomass-", spp, "-tv-depth-only-1n3n4n16.rds")))
# (se <- as.list(m$sd_report, "Std. Error"))
# m <- sdmTMB:::update_model(m, xy_cols = c("X", "Y"))


# m <- readRDS(here::here(paste0("analysis/VOCC/data/", spp, "/mod-imm-biomass-", spp, "-tv-depth-only-1n3n4n16-2021.rds")))
# class(m) <- "sdmTMB"
m <- readRDS(here::here(paste0("analysis/VOCC/data/", spp, "/mod-imm-biomass-", spp, "-tv-depth-only-1n3n4n16-2021-noRW.rds")))

m
(se <- as.list(m$sd_report, "Std. Error"))
max(m$gradients)

p <- predict(m)

saveRDS(p, here::here(paste0("analysis/VOCC/data/", spp, "/check-mod-predictions-", spp, "-tv-depth-only-1n3n4n16.rds")))

# # check for improvement?
# 
# d1 <- readRDS(here::here(paste0("analysis/VOCC/data/", spp, "/check-mod-predictions-", spp, "-tv-depth-only-1n3n4n16-old-imm.rds")))
# 
# d2 <- readRDS(here::here(paste0("analysis/VOCC/data/", spp, "/check-mod-predictions-", spp, "-tv-depth-only-1n3n4n16.rds")))
# 
# d1$est[1:10]
# d2$est[1:10]
# 
# (g1 <- d1 %>% filter(year > 2008) %>% mutate(
#   density2 = (density)*10000*1000,
#   est_exp2 = exp(est)*10000*1000,
#   density3 = ifelse(density2 == 0, 0.0001, density2), 
#   est_exp3 = ifelse(est_exp2 < 0.00000001, 0.00000001, est_exp2)
# ) %>%
#     # ggplot( aes((density)*10000*1000, exp(est)*10000*1000, colour=year))+
#     ggplot(aes(density3, est_exp3, colour=year))+
#     geom_point(alpha=0.1) + 
#     # coord_cartesian(
#     #   ylim=c(min(dat_imm$est_exp2), max(exp(dat_imm$est))*10000*1000), 
#     #   xlim=c(0.01, max(dat_imm$density)*10000*1000)) +
#     coord_fixed(
#       ylim=c(0.00000001, max(exp(dat_imm$est))*10000*1000),
#       xlim=c(0.00000001, max(exp(dat_imm$est))*10000*1000)
#     ) +
#     scale_x_log10(
#       # breaks = c(0.1, 1, 10, 1000),
#       # labels = c(0.1, 1, 10, 1000)
#       breaks = c(0.001, 1, 1000), 
#       labels = c(0.001, 1, 1000)
#     ) +
#     scale_y_log10(
#       breaks = c(0.001, 1, 1000), 
#       labels = c(0.001, 1, 1000)
#     ) +
#     # geom_abline(a=0.01,b=1) +
#     scale_colour_viridis_c(option = "A", end = 0.75) +
#     ylab("Predicted biomass density (g/ha)") +
#     xlab("Observed biomass density (g/ha)") +
#     # facet_wrap(~year) +
#     # ggtitle("fixed depth") +
#     ggtitle("time-varying depth") +
#     ggsidekick::theme_sleek())
# 
# g1 + g2 + patchwork::plot_layout(guides = "collect")
# 

#### immature ####
species_imm <- c(
  "Pacific Cod",
  "North Pacific Spiny Dogfish",
  "Walleye Pollock",
  "Sablefish",
  "Lingcod",
  "Rosethorn Rockfish",
  "Yellowmouth Rockfish",
  "Canary Rockfish",
  "Darkblotched Rockfish",
  "Greenstriped Rockfish",
  "Pacific Ocean Perch",
  "Redbanded Rockfish",
  "Sharpchin Rockfish",
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  "Yellowtail Rockfish",
  "Shortspine Thornyhead",
  "Arrowtooth Flounder",
  "Rex Sole",
  "Petrale Sole",
  "English Sole", # already rerun
  "Dover Sole",
  "Southern Rock Sole",
  "Flathead Sole",
  "Slender Sole",# previously excluded
  "Pacific Sanddab",
  "Redstripe Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Widow Rockfish",
  "Quillback Rockfish",
  "Yelloweye Rockfish"
)

dat_imm <- purrr::map_dfr(species_imm, function(x){
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(x)))
  
  ## these ones are for imm and total models rebuilt Nov 18, 2020 w 2-biomass-depth-only-model2.Rmd
  try({
    d <- readRDS(here::here(paste0("analysis/VOCC/data/", spp, "/check-mod-predictions-", spp, "-tv-depth-only-1n3n4n16.rds")))
    ## or new updated ones
    # d <- readRDS(here::here(paste0("analysis/VOCC/data/", spp, "/check-mod-predictions-", spp, "-tv-depth-only-1n3n4n16-imm.rds")))
    
  })

  try({
    d <- filter(d, year > 2007)
    d$species <- x
    
    if (any(names(d) == "adult_density")) {
      d$density <- d$adult_density
    }
  })
  try({d})
  
})

dat_imm$species[dat_imm$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

min_pos <- dat_imm %>% filter(density != 0) 
min(min_pos$density*10000*1000)
# min(exp(dat_imm$est)*10000*1000)
dat_imm <- dat_imm %>% mutate(
  density2 = (density)*10000*1000,
  est_exp2 = exp(est)*10000*1000,
  density3 = ifelse(density2 == 0, 0.001, density2), 
  est_exp3 = ifelse(est_exp2 < 0.00001, 0.00001, est_exp2)
)

# raw densities are in kg/m2 so *10000*1000 to get g/ha
(g <- dat_imm %>% 
    # filter(year %in% c(2008,2009,2018,2019)) %>%
    # ggplot( aes((density)*10000*1000, exp(est)*10000*1000))+
  ggplot(aes(density3, est_exp3, colour=year))+
    geom_segment(
      x = log10(0.06), 
      y = log10(0.06), 
      xend = log10(max(exp(dat$est))*10000*1000), 
      yend = log10(max(exp(dat$est))*10000*1000), 
      # x = log10(0.00000001), 
      # y = log10(0.00000001), 
      # xend = (1000), 
      # yend = (1000), 
      colour = "black", size = 0.1) +
    geom_jitter(width = 0.3, height = 0, 
      # alpha = 0.3, 
      size = 0.1, 
      shape = 20) + 
    # coord_cartesian(
    #   ylim=c(min(dat_imm$est_exp2), max(exp(dat_imm$est))*10000*1000), 
    #   xlim=c(0.01, max(dat_imm$density)*10000*1000)) +
    coord_fixed(expand = F,
      ylim=c(0.00001, max(exp(dat_imm$density))*10000*1000),
      xlim=c(0.00001, max(exp(dat_imm$density))*10000*1000)
      ) +
    scale_x_log10(
      # breaks = c(0.1, 1, 10, 1000),
      # labels = c(0.1, 1, 10, 1000)
      breaks = c(0.001, 1, 1000), 
      labels = c(0.001, 1, 1000)
      ) +
    scale_y_log10(
      breaks = c(0.001, 1, 1000), 
      labels = c(0.001, 1, 1000)
      ) +
    
    # geom_abline(a=0.01,b=1) +
    scale_colour_viridis_c(name = "Year", option = "A", end = 0.75) +
    ylab("Predicted biomass density (g/ha)") +
    xlab("Observed biomass density (g/ha)") +
    facet_wrap(~species, ncol = 8
      # , scales = "free_y"
      ) +
    ggsidekick::theme_sleek())

 
ggsave(here::here("ms/figs/supp-est-vs-obs-nov-2020-imm2.jpg"), width= 14, height = 8)


#### check gradients ####

grad_imm <- purrr::map_dfr(species_imm, function(x){
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(x)))
  
  try({m <- readRDS(here::here(paste0("analysis/VOCC/data/", spp, "/mod-imm-biomass-", spp, "-tv-depth-only-1n3n4n16.rds")))
  })
  try({
  max_grad <- max(m$gradients)
  .d <- list(spp, max_grad)
  names(.d) <- c("species", "gradient")
  })
  try({.d})
})

grad_imm  
