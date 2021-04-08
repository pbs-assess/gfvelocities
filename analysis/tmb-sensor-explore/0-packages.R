# this code installs these packages only if not already installed
# if installed version is outdated you may need to update some of the these packages
if (!require(remotes))  install.packages("remotes")
if (!require(gfvelocities)) remotes::install_github("pbs-assess/gfvelocities")
if (!require(Matrix)) install.packages("Matrix", type = "source")
if (!require(TMB)) remotes::install_github("kaskr/adcomp/TMB")
if (!require(sdmTMB)) remotes::install_github("pbs-assess/sdmTMB") 
# models may need rerunning if built with older version of sdmTMB in order to use some sdmTMB and gfvelocity functions

# primary tools
if (!require(here)) install.packages("here")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(scales)) install.packages("scales")
if (!require(ggnewscale)) install.packages("ggnewscale")
if (!require(stringr)) install.packages("stringr")
if (!require(lubridate)) install.packages("lubridate")

# misc figure tools
if (!require(gfplot)) install.packages("gfplot")

# misc mapping tools
if (!require(sp)) install.packages("sp")
if (!require(sf)) install.packages("sf")
if (!require(raster)) install.packages("raster")
if (!require(rgdal)) install.packages("rgdal")

# only works on DFO network
# if (!require(gfdata)) install.packages("gfdata")

# for comparison with sdmTMB climate models
# if (!require(gmb)) install.packages("gmb")
