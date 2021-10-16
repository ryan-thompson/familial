#==================================================================================================#
# Bootstrap function
#==================================================================================================#

#' @title Bayesian bootstrap
#'
#' @author Ryan Thompson <ryan.thompson@monash.edu>
#'
#' @description Performs a Bayesian bootstrap for a statistic defined via a suitable function.
#'
#' @param x a numeric vector to be passed as the first argument to \code{fun}
#' @param fun the function to bootstrap; must accept data \code{x} and weights \code{w} (in that
#' order), and return a data frame
#' @param nboot the number of bootstraps to perform
#' @param cluster an optional cluster for running bootstraps in parallel; must be set up using
#' \code{parallel::makeCluster}
#' @param ... any other arguments for \code{fun}
#'
#' @return An object of class \code{bayes.boot}; a data frame with the following columns:
#' \item{boot.id}{the bootstrap iteration index}
#' \item{...}{any columns returned by \code{fun}}
#'
#' @example R/examples/example-bayes-boot.R
#'
#' @export

bayes.boot <- \(x, fun, nboot = 1000, cluster = NULL, ...) {

  # Preliminaries
  n <- length(x)

  # Function to perform a single bootstrap
  boot.fun <- \(boot.id) {
    w <- rudirichlet(1, n)
    fit <- fun(x = x, w = w, ...)
    data.frame(boot.id, fit)
  }

  # Sequential or parallel bootstrap
  if (is.null(cluster)) {
    result <- lapply(1:nboot, boot.fun)
  } else {
    parallel::clusterCall(cluster, \() library(familial))
    result <- parallel::parLapply(cluster, 1:nboot, boot.fun)
  }
  result <- do.call(rbind, result)

  # Return result
  class(result) <- c('bayes.boot', 'data.frame')
  return(result)

}
