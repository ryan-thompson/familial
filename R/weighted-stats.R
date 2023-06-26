#==================================================================================================#
# Weighted statistics
#==================================================================================================#

#' @title Weighted statistics
#'
#' @author Ryan Thompson <ryan.thompson1@unsw.edu.au>
#'
#' @description Assorted weighted statistics unavailable in base R.
#'
#' @param x a numeric vector of data
#' @param w a numeric vector of weights
#'
#' @return A length-one numeric vector.
#'
#' @name weighted
#'

#' @rdname weighted
#' @export

weighted.median <- \(x, w) {
  interpolate <- length(unique(w)) == 1
  matrixStats::weightedMedian(x, w, interpolate = interpolate)
}

#' @rdname weighted
#' @export

weighted.mad <- \(x, w) {
  mu <- weighted.median(x, w)
  weighted.median(abs(x - mu), w)
}
