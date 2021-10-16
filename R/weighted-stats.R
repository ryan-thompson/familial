#==================================================================================================#
# Weighted statistics
#==================================================================================================#

#' @title Weighted statistics
#'
#' @author Ryan Thompson <ryan.thompson@monash.edu>
#'
#' @description Assorted weighted statistics unavailable in base R
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
  w <- w / sum(w)
  id <- order(x)
  x <- x[id]
  w <- w[id]
  cs <- cumsum(w)
  keep <- c(which(cs == 0.5), which.min(cs <= 0.5))
  sum(x[keep] * w[keep]) / sum(w[keep])
}

#' @rdname weighted
#' @export

weighted.mad <- \(x, w) {
  mu <- weighted.median(x, w)
  weighted.median(abs(x - mu), w)
}
