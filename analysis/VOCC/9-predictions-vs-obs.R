# library(TMB)
library(tidyverse)
library(ggplot2)
# options(scipen = 999)

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

# raw densities are in kg//m2 so *10000 to get kg/ha
(g <- dat %>% #filter(year %in% c(2008,2009,2018,2019)) %>%
    ggplot( aes((density)*10000, exp(est)*10000, colour=year
      ))+
    geom_point(alpha=0.1) + 
    scale_x_log10(breaks = c(0.001, 1, 1000), labels = c(0.001, 1, 1000))+
    scale_y_log10(breaks = c(0.001, 1, 1000), labels = c(0.001, 1, 1000))+
    coord_cartesian(ylim=c(0.00001, max(dat$est)*10000), xlim=c(0.00001, max(dat$density)*10000)) +
    geom_abline(a=0,b=1) +
    scale_colour_viridis_c(option = "A", end = 0.75) +
    ylab("Predicted biomass density (kg/ha)") +
    xlab("Observed biomass density (kg/ha)") +
    facet_wrap(~species, ncol = 8) +
    ggsidekick::theme_sleek())

ggsave(here::here("ms/figs/supp-est-vs-obs-july-2020.jpg"), width= 12.5, height = 8)
 

species_imm <- c(
  "North Pacific Spiny Dogfish",
  "Walleye Pollock",
  # "Pacific Cod", # missing... maybe because model rebuilt in july when others werent?
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

# raw densities are in kg/m2 so *10000*1000 to get g/ha
(g <- dat_imm %>% #filter(year %in% c(2008,2009,2018,2019)) %>%
    ggplot( aes((density)*10000*1000, exp(est)*10000*1000, colour=year
    ))+
    geom_point(alpha=0.1) + 
    coord_cartesian(ylim=c(0.00000001, max(dat_imm$est)*10000*1000), xlim=c(0.00000001, max(dat_imm$density)*10000*1000)) +
    scale_x_log10(breaks = c(0.001, 1, 1000), labels = c(0.001, 1, 1000))+
    scale_y_log10(breaks = c(0.001, 1, 1000), labels = c(0.001, 1, 1000))+
    geom_abline(a=0,b=1) +
    scale_colour_viridis_c(option = "A", end = 0.75) +
    ylab("Predicted biomass density (g/ha)") +
    xlab("Observed biomass density (g/ha)") +
    facet_wrap(~species, ncol = 8) +
    ggsidekick::theme_sleek())

ggsave(here::here("ms/figs/supp-est-vs-obs-nov-2020-imm.jpg"), width= 12.5, height = 7)


