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
