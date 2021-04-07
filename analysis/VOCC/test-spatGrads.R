## Test spatial grad functions ##

d1 <- readRDS(("analysis/vocc/data/pacific-cod/predictions-pacific-cod-tv-depth-only-1n3n4n16-mature-biomass-prior-FALSE.rds")) %>% filter(year >= 2008 & ssid == 4)
d2 <- readRDS(paste0("analysis/vocc/data/predicted-DO-new.rds")) %>% select(X, Y, year, ssid, temp, do_est) %>% filter(year >= 2008 & ssid == 4)

# d1 <- readRDS(("analysis/vocc/data/pacific-cod/predictions-pacific-cod-tv-depth-only-1n3n4n16-mature-biomass-prior-FALSE.rds")) %>% filter(year >= 2008 & ssid == c(1,3))
# d2 <- readRDS(paste0("analysis/vocc/data/predicted-DO-new.rds")) %>% select(X, Y, year, ssid, temp, do_est) %>% filter(year >= 2008 & ssid == c(1,3))


d <- left_join(d1, d2, by = c("X", "Y", "year"))

d <- d %>%
  filter(do_est > 0.28) %>%
  filter(do_est < 7.06) # 0.005 and 0.995
# d <- d %>% filter(temp > 2.61) %>% filter(temp < 14.31) # full range
d <- d %>%
  filter(temp > 3.07) %>%
  filter(temp < 11.3) # 0.005 and 0.995


dv <- vocc_gradient_calc(d, "temp",
  scale_fac = 2,
  # all layers with max indice will be averaged for spatial gradient
  # if default (NULL) will use all years
  indices = c(1, 1, 1, 1, 2, 2),
  # divisor = 1, #default of 10 gives per decade
  quantile_cutoff = 0.05
)

dv <- dv %>% dplyr::mutate(X = x, Y = y, trend2 = trend*10)
quantile(dv$trend, 0.975)
quantile(dv$trend, 0.025)

# not working with VoCC yet
dv2 <- vocc_gradient_calc(d, "temp",
  scale_fac = 2,
  indices = c(1, 1, 1, 1, 2, 2),
  # divisor = 1, #default of 10 gives per decade
  use_VoCC = T,
  quantile_cutoff = 0.05
)

dv2 <- dv2 %>% dplyr::mutate(X = x, Y = y)