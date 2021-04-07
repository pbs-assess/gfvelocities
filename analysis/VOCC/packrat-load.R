path_to_my_locally_built_package <- "/Library/Frameworks/R.framework/Versions/3.5/Resources/library/my_package"

devtools::install(path_to_my_locally_built_package)

library(my_package)


packrat::set_opts(local.repos = c("~/Library/R/library"))
packrat::install_local("tidyverse")
packrat::install_local("broom")
packrat::install_local("Rcpp")
packrat::install_local("tidyselect")

library(tidyverse)

packrat::snapshot()

packrat::init(options = list(local.repos = c("packages")))
