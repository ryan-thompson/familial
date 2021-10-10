#==================================================================================================#
# Family fitting function
#==================================================================================================#

#' @title Fit family
#'
#' @author Ryan Thompson <ryan.thompson@monash.edu>
#'
#' @description Fits the Huber or trimmed mean location families.
#'
#' @param x a numeric vector of data
#' @param w a numeric vector of weights
#' @param family the location family; currently allows 'huber' for Huber family (default) or
#' 'trimmed' for trimmed mean family
#' @param scale.fun a function used to estimate the scale of \code{x} for the Huber family; ensures
#' that the tuning parameter is comparable across variables with different scales
#' @param eps a numerical tolerance parameter
#'
#' @return An object of class \code{fit.family}; a data frame with the following columns:
#' \item{mu.hat}{the fitted values}
#' \item{lambda}{the indexing parameter}
#'
#' @example R/examples/example-fit-family.R
#'
#' @export

fit.family <- \(x, w = rep(1, length(x)), family = c('huber', 'trimmed'), scale.fun = weighted.mad,
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
    scale.x <- scale.fun(x, w)
    if (is.nan(scale.x) | scale.x == 0) scale.x <- 1 # Handles constant x
    result$lambda <- result$lambda / scale.x
  } else if (family == 'trimmed') {
    result <- trimmed.family(x, w, eps)
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
  w <- w / sum(w) * n
  x <- x - med
  s <- sign(x)
  lambda <- mu <- numeric(n)
  mu[1] <- mean(x * w)
  lambda[1] <- max(abs(x - mu[1]))
  for (m in 2:n) {
    r <- x - mu[m - 1]
    d <- lambda[m - 1] - s * r
    L <- which(d ^ 2 < eps)
    if (length(L) == n) {
      m <- m - 1
      break
    }
    eta <- 1 + s * sum(s[L] * w[L]) / sum(w[- L])
    gamma <- min(d[- L] / eta[- L])
    x[L] <- x[L] - s[L] * gamma * eta[L]
    mu[m] <- mean(x * w)
    lambda[m] <- lambda[m - 1] - gamma
  }
  data.frame(mu.hat = mu[1:m] + med, lambda = lambda[1:m])
}

# Trimmed mean family
trimmed.family <- \(x, w, eps) {
  n <- length(x)
  id <- order(x)
  w <- w / sum(w)
  w.o <- w[id]
  x.w.o <- x[id] * w.o
  cs.w <- cumsum(w.o)
  lambda <- mu <- numeric(n)
  lambda[1] <- 0.5
  A <- which(cs.w == lambda[1])
  B <- which.min(cs.w <= lambda[1])
  active <- c(A, B)
  mu[1] <- sum(x.w.o[active]) / sum(w.o[active])
  if (length(A) == 0) A <- c(A, n + 1)
  for (m in 2:n) {
    gamma <- min(c(abs(lambda[m - 1] - cs.w[- A]), abs(1 - lambda[m - 1] - cs.w[- A])))
    lambda[m] <- lambda[m - 1] - gamma
    A <- which((cs.w + eps >= lambda[m]) + (cs.w - eps <= 1 - lambda[m]) == 2)
    if (length(A) == n) {
      m <- m - 1
      break
    }
    max.A <- max(A)
    if ((1 - lambda[m] - cs.w[max.A]) ^ 2 <= eps) B <- max.A + 1
    active <- c(A, B)
    mu[m] <- sum(x.w.o[active]) / sum(w.o[active])
  }
  data.frame(mu.hat = mu[m:1], lambda = lambda[m:1])
}

# Compare weighted version against:
# tm <- function(x, w, lambda) {
#   n <- length(x)
#   id <- order(x)
#   w <- w / sum(w)
#   w.o <- w[id]
#   x.o <- x[id]
#   cumsum_w <- cumsum_x <- wsumlambda <- 0
#   for (i in 1:n) {
#     cumsum_w <- cumsum_w + w.o[i]
#     if (cumsum_w > lambda) {
#       cumsum_x <- cumsum_x + x.o[i] * w.o[i]
#       wsumlambda <- wsumlambda + w.o[i]
#     }
#     if (cumsum_w > 1 - lambda) break;
#   }
#   cumsum_x / wsumlambda
# }

# Compare unweighted version against:
# mean(x, lambda = lambda)

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
    if (attributes(x)$family != attributes(y)$family) {
      stop('x and y must belong to the same family')
    }
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
    attributes(x)$mean <- mean.diff
    attributes(x)$median <- median.diff
  }

  # Plot family
  ggplot2::ggplot(x, ggplot2::aes_string('lambda', 'mu.hat')) +
    ggplot2::geom_line(ggplot2::aes(linetype = 'Huber')) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = attributes(x)$mean, linetype = 'Mean')) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = attributes(x)$median, linetype = 'Median')) +
    ggplot2::xlab(expression(lambda)) +
    ggplot2::ylab(ifelse(is.null(y), expression(hat(mu)(lambda)),
                         expression(hat(mu)[X](lambda)-hat(mu)[Y](lambda)))) +
    ggplot2::labs(linetype = 'Center')

}
