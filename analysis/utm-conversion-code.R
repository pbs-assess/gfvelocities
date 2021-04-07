library(readr)
library(tidyverse)
# install.packages("devtools")
devtools::install_github("pbs-assess/gfplot")
library(gfplot)

d <- read_csv("analysis/t18km_iteration_5.csv")


d.start <- d %>% select(-x.end, -y.end) %>% 
  mutate(X = x.start, Y = y.start, whichend = "start") %>%
  gfplot:::utm2ll(., utm_zone = 9) %>% 
  rename(x.utm = x.start, y.utm = y.start)

d.end <- d %>% select(-x.start, -y.start) %>% 
  mutate(X = x.end, Y = y.end, whichend = "end") %>% 
  gfplot:::utm2ll(., utm_zone = 9) %>% 
  rename(x.utm = x.end, y.utm = y.end)
  

d <- rbind(d.start, d.end)

write.csv(...)