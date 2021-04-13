## gfvelocities: spatiotemporal modelling of groundfish biomass density change in response to climate

This package contains the code used in the analysis described in this preprint: https://ecoevorxiv.org/b87ng/.

These functions were built for use with British Columbia synoptic trawl survey data and prediction grids from spatiotemporal models built with the package [sdmTMB](https://pbs-assess.github.io/sdmTMB/index.html).
This analysis also relies on many of the same data extraction, data tidying, model fitting, and plotting functions underlying **[A reproducible data synopsis report for over 100 species of British Columbia groundfish](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_041-eng.html)**. Most of these functions are contained in the packages [gfplot](https://github.com/pbs-assess/gfplot) and [gfdata](https://github.com/pbs-assess/gfdata). In some cases functions from these packages have been modified and the customized versions included in this package. 

Velocity calculation functions are designed and annotated in an effort for them to be usable (or easily modified for use) with other data types. 

The 'analysis' contains three subfolders. 
The 'get-setup' folder contains all primliminary organization and data extraction code. 
Parts of these scripts will only work on the DFO network.
The 'tmb-senser-explore' folder contains code to construct and select most appropriate sdmTMB models of bottom temperature and dissolved oxygen using CTD data. 
The 'VOCC' folder contains a work flow that builds biomass density models and completes the rest of velocity of climate change analyses.  Files in these folders are numbered in the order in which they need to be run. 

*This is a work in progress: arguments may change and not all functionality has been tested.*

