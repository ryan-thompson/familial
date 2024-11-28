#==================================================================================================#
# Dirichlet distribution
#==================================================================================================#

#' @title Uniform Dirichlet distribution
#'
#' @author Ryan Thompson <ryan.thompson-1@uts.edu.au>
#'
#' @description Random number generation for the uniform Dirichlet distribution (having all
#' concentration parameters set to one).
#'
#' @param n the number of observations
#' @param d the number of dimensions
#'
#' @return A matrix; each row is a random draw and each column is a dimension.
#'
#' @export

rudirichlet <- \(n, d) {
  x <- stats::rexp(n * d)
  x <- matrix(x, n, d)
  x <- x / rowSums(x)
  return(x)
}
