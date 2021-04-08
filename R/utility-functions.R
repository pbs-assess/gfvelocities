#' Collapse outliers produced by velocity calculations
#' 
#' @param .x Vector to be collapsed
#' @param outliers Vector of quantile thresholds used in collapse
#'
#' @export
collapse_outliers <- function(.x, outliers) {
  .x_max <- quantile(.x, outliers[2], na.rm = T)
  .x_min <- quantile(.x, outliers[1], na.rm = T)
  .x[.x > .x_max] <- .x_max
  .x[.x < .x_min] <- .x_min
  .x
}

#' Strip qualifiers from variable names
#' @export
shortener <- function(string) {
  out <- gsub("\\(", "", string) #coef_names[1:length()]
  out <- gsub("\\, center = F)", "", out)
  out <- gsub("_scaled", "", out)
  out <- gsub("scale", "", out)
  out <- gsub("\\)", "", out)
  out <- gsub("squashed_", "", out)
  out <- gsub("mean_", "", out)
  out <- gsub("immature", "", out)
  out <- gsub("mature ", "", out)
  #out <- gsub("_", "", out)
  out
}

#' Add capitals to start of strings
#' @export
firstup <- function(x){
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


