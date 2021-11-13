#==================================================================================================#
# Family fitting function
#==================================================================================================#

#' @title Fit family
#'
#' @author Ryan Thompson <ryan.thompson@monash.edu>
#'
#' @description Fits a family of centers.
#'
#' @param x a numeric vector of data
#' @param w a numeric vector of weights
#' @param family the location family; currently only allows 'huber' for Huber family
#' @param spread.fun a function used for the spread of \code{x}; must accept data \code{x} and
#' weights \code{w} (in that order), and return a numeric
#' @param eps a numerical tolerance parameter
#'
#' @return An object of class \code{fit.family}; a data frame with the following columns:
#' \item{mu.hat}{the fitted values}
#' \item{lambda}{the thresholding parameter}
#'
#' @example R/examples/example-fit-family.R
#'
#' @export

fit.family <- \(x, w = rep(1, length(x)), family = 'huber', spread.fun = weighted.mad,
                eps = .Machine$double.eps) {

  # Check arguments are valid
  family <- match.arg(family)
  if (anyNA(x)) stop('x must not contain NA values')
  if (any(w < 0)) stop('w must not contain negative values')
  if (all(w == 0)) stop('w must contain at least one positive value')
  if (length(x) != length(w)) stop('x and w must have the same length')

  # Calculate mean and median
  mean. <- stats::weighted.mean(x, w)
  median. <- weighted.median(x, w)

  # Compute family
  if (family == 'huber') {
    result <- huber.family(x, w, median., eps)
    spread.x <- spread.fun(x, w)
    if (is.nan(spread.x) | spread.x == 0) spread.x <- 1 # Handles constant x
    result$lambda <- result$lambda / spread.x
  }

  # Return result
  class(result) <- c('fit.family', 'data.frame')
  attributes(result)$family <- family
  attributes(result)$mean <- mean.
  attributes(result)$median <- median.
  return(result)

}

# Huber family
huber.family <- \(x, w, med, eps) {
  n <- length(x)
  w <- w / sum(w)
  s <- sign(x - med)
  lambda <- mu <- numeric(n)
  mu[1] <- sum(x * w)
  lambda[1] <- max(abs(x - mu[1]))
  for (m in 2:n) {
    d <- lambda[m - 1] - pmin(s * (x - mu[m - 1]), lambda[m - 1])
    A <- d ^ 2 < eps
    if (sum(A) == n) {m <- m - 1; break}
    eta <- - sum(w[A] * s[A]) / sum(w[!A])
    gamma <- min(d[!A] / (1 - s[!A] * eta))
    mu[m] <- mu[m - 1] + gamma * eta
    lambda[m] <- lambda[m - 1] - gamma
  }
  data.frame(mu.hat = mu[1:m], lambda = lambda[1:m])
}

#==================================================================================================#
# Plot function for fit.family object
#==================================================================================================#

#' @title Plot function for \code{fit.family} object
#'
#' @author Ryan Thompson <ryan.thompson@monash.edu>
#'
#' @description Plot a fitted family.
#'
#' @param x an object of class \code{fit.family}
#' @param y an object of class \code{fit.family}
#' @param ... any other arguments
#'
#' @return A plot of the fitted family.
#'
#' @method plot fit.family
#'
#' @export
#'
#' @importFrom graphics "plot"

plot.fit.family <- \(x, y = NULL, ...) {

  # Interpolate x and y at same lambda sequence
  if (!is.null(y)) {
    # if (attributes(x)$family != attributes(y)$family) {
    #   stop('x and y must belong to the same family')
    # }
    family <- attributes(x)$family
    mean.diff <- attributes(x)$mean - attributes(y)$mean
    median.diff <- attributes(x)$median - attributes(y)$median
    lambda <- union(x$lambda, y$lambda)
    x <- stats::approx(x$lambda, x$mu.hat, lambda, yleft = x$mu.hat[which.min(x$lambda)],
                       yright = x$mu.hat[which.max(x$lambda)],
                       method = ifelse(length(x$mu.hat) > 1, 'linear', 'constant'))
    y <- stats::approx(y$lambda, y$mu.hat, lambda, yleft = y$mu.hat[which.min(y$lambda)],
                       yright = y$mu.hat[which.max(y$lambda)],
                       method = ifelse(length(y$mu.hat) > 1, 'linear', 'constant'))
    x <- data.frame(mu.hat = x$y - y$y, lambda = lambda)
    attributes(x)$family <- family
    attributes(x)$mean <- mean.diff
    attributes(x)$median <- median.diff
  }

  # Plot family
  family.name <- tools::toTitleCase(attributes(x)$family)
  x$center <- family.name
  x.mean <- data.frame(mu.hat = attributes(x)$mean, lambda = c(0, Inf), center = 'Mean')
  x.median <- data.frame(mu.hat = attributes(x)$median, lambda = c(0, Inf), center = 'Median')
  x <- rbind(x, x.mean, x.median)
  x$center <- factor(x$center, c(family.name, 'Mean', 'Median'))
  ggplot2::ggplot(x, ggplot2::aes_string('lambda', 'mu.hat', linetype = 'center')) +
    ggplot2::geom_line() +
    ggplot2::xlab(expression(lambda)) +
    ggplot2::ylab(ifelse(is.null(y), expression(hat(mu)(lambda)),
                         expression(hat(mu)[X](lambda)-hat(mu)[Y](lambda)))) +
    ggplot2::labs(linetype = 'Center')

}
