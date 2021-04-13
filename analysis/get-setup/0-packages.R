# this code installs these packages only if not already installed
# if installed version is outdated you may need to update some of the these packages

if (!require(remotes))  install.packages("remotes")
if (!require(gfvelocities)) remotes::install_github("pbs-assess/gfvelocities")

# only works on DFO network
if (!require(gfdata)) install.packages("gfdata")

# for comparison with sdmTMB climate models
if (!require(gmb)) install.packages("gmb")


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
if (!require(dotwhisker)) install.packages("dotwhisker")
if (!require(patchwork)) install.packages("patchwork")
if (!require(cowplot)) install.packages("cowplot")
if (!require(ggrepel)) install.packages("ggrepel")
if (!require(egg)) install.packages("egg")
if (!require(forcats)) install.packages("forcats")

# misc mapping tools
if (!require(ggmap)) install.packages("ggmap")
if (!require(grid)) install.packages("grid")
if (!require(sp)) install.packages("sp")
if (!require(sf)) install.packages("sf")
if (!require(raster)) install.packages("raster")
if (!require(rgdal)) install.packages("rgdal")


# weighted quantiles for species statistics
# if (!require(reldist)) install.packages("reldist")

# probably only used in tests of ecological and life history covariates
# if (!require(Hmisc)) install.packages("Hmisc")
# if (!require(ggpubr)) install.packages("ggpubr")
# if (!require(lmerTest)) install.packages("lmerTest")
# if (!require(interactions)) install.packages("interactions")
# if (!require(jtools)) install.packages("jtools")

# null simulations
# if (!require(RandomFields)) install.packages("RandomFields")

# for producing latex values and table code
# if (!require(readr)) install.packages("readr")
# if (!require(kableExtra)) install.packages("kableExtra")

# I think I've replaced this with a more customizable internal function
# if (!require( ggquiver)) install.packages(" ggquiver")
