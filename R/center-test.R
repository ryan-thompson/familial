#==================================================================================================#
# Center test function
#==================================================================================================#

#' @title Center test
#'
#' @author Ryan Thompson <ryan.thompson1@unsw.edu.au>
#'
#' @description Performs a one- or two-sample test for a family of centers.
#'
#' @param x a numeric vector of data
#' @param y an optional numeric vector of data
#' @param family the family of centers; currently only allows 'huber' for Huber family
#' @param alternative the form of the alternative hypothesis; must be one of 'two.sided' (default),
#' 'greater', or 'less'
#' @param mu the null value of the center for a one-sample test, or the
#' null value of the center of differences for a paired two-sample test, or the null value of the
#' difference of centers for an independent two-sample test; can be an interval
#' @param paired a logical indicating whether to treat \code{x} and \code{y} as paired
#' @param nboot the number of bootstraps to perform
#' @param loss an optional c×2 matrix of losses incurred from an incorrect decision, where c is the
#' number of candidate choices (typically c=3: H0, H1, or indeterminate)
#' @param cluster an optional cluster for running bootstraps in parallel; must be set up using
#' \code{parallel::makeCluster}
#' @param ... any other arguments
#'
#' @return An object of class \code{center.test}; a list with the following components:
#' \item{expected.loss}{the expected loss, calculated by post-multiplying \code{loss} with
#' \code{prob}}
#' \item{decision}{the optimal decision given the expected loss}
#' \item{loss}{the loss matrix}
#' \item{prob}{the estimated posterior probabilities of the null and alternative}
#' \item{boot}{the bootstrap output from \code{bayes.boot}}
#' \item{x}{the \code{x} that was supplied}
#' \item{y}{the \code{y} that was supplied}
#' \item{mu}{the \code{mu} that was supplied}
#' \item{family}{the \code{family} that was supplied}
#'
#' @details Uses the Bayesian bootstrap to compute posterior probabilities for the hypotheses
#' \eqn{\mathrm{H}_0:\mu(\lambda)=\mu_0} for some \eqn{\lambda\in\Lambda} vs.
#' \eqn{\mathrm{H}_1:\mu(\lambda)\neq\mu_0} for all \eqn{\lambda\in\Lambda},
#' where \eqn{\{\mu(\lambda):\lambda\in\Lambda\}} is a family of centers. \cr
#' The default loss matrix results in a decision whenever the posterior probability
#' for one of the hypotheses is greater than 0.95 and otherwise is indeterminate.
#'
#' @references Thompson, R., Forbes, C. S., MacEachern, S. N., and Peruggia, M. (2023). 'Familial
#' inference: Tests for hypotheses on a family of centres'. arXiv:
#' \href{https://arxiv.org/abs/2202.12540}{2202.12540}.
#'
#' @example R/examples/example-center-test.R
#'
#' @export

center.test <- \(x, y = NULL, family = 'huber', alternative = c('two.sided', 'less', 'greater'),
                 mu = 0, paired = FALSE, nboot = 1000, loss = NULL, cluster = NULL, ...) {

  # Check arguments are valid
  mu0 <- mu; rm(mu)
  family <- match.arg(family)
  alternative <- match.arg(alternative)
  if (anyNA(x)) stop('x must not contain NA values')
  if (anyNA(y)) stop('y must not contain NA values')

  # Set up loss matrix
  if (is.null(loss)) {
    loss <- rbind(c(0, 20), c(20, 0), c(1, 1))
    labels <- c('H0', 'H1', 'indeterminate')
    colnames(loss) <- labels[1:2]
    rownames(loss) <- labels
  }

  # Handle the two-sample paired case
  if (!is.null(y) & paired) {
    x <- x - y
    y <- NULL
  }

  # Solve the family
  if (is.null(y)) {
    boot <- bayes.boot(x, fit.family, nboot, cluster, family, ...)
  } else {
    boot.x <- bayes.boot(x, fit.family, nboot, cluster, family, ...)
    boot.x$var <- 'x'
    boot.y <- bayes.boot(y, fit.family, nboot, cluster, family, ...)
    boot.y$var <- 'y'
    boot <- rbind(boot.x, boot.y)
  }

  # Handle the two-sample unpaired case
  if (!is.null(y) & !paired) {
    interpolate <- \(b) {
      boot.b <- boot[boot$boot.id == b, ]
      lambda <- unique(boot.b$lambda)
      x.b <- boot.b[boot.b$var == 'x', ]
      y.b <- boot.b[boot.b$var == 'y', ]
      x.b <- stats::approx(x.b$lambda, x.b$mu, lambda,
                           yleft = x.b$mu[which.min(x.b$lambda)],
                           yright = x.b$mu[which.max(x.b$lambda)],
                           method = ifelse(length(x.b$mu) > 1, 'linear', 'constant'))
      y.b <- stats::approx(y.b$lambda, y.b$mu, lambda,
                           yleft = y.b$mu[which.min(y.b$lambda)],
                           yright = y.b$mu[which.max(y.b$lambda)],
                           method = ifelse(length(y.b$mu) > 1, 'linear', 'constant'))
      data.frame(boot.id = b, mu = x.b$y - y.b$y, lambda = x.b$x)
    }
    boot <- lapply(1:nboot, interpolate)
    boot <- do.call(rbind, boot)
  }

  # Compute posterior probabilities and expected loss
  prob.less <- mean(stats::aggregate(mu ~ boot.id, boot, \(x) max(x) < min(mu0))$mu)
  prob.greater <- mean(stats::aggregate(mu ~ boot.id, boot, \(x) min(x) > max(mu0))$mu)
  prob.equal <- 1 - prob.less - prob.greater
  if (alternative == 'two.sided') {
    prob <- c(prob.equal, prob.less + prob.greater)
  } else if (alternative == 'less') {
    prob <- c(prob.equal + prob.greater, prob.less)
  } else {
    prob <- c(prob.equal + prob.less, prob.greater)
  }
  names(prob) <- c('H0', 'H1')
  expected.loss <- tcrossprod(prob, loss)[, ]

  # Return result
  result <- list(expected.loss = expected.loss,
                 decision = labels[which.min(expected.loss)],
                 loss = loss,
                 prob = prob,
                 boot = boot,
                 x = x,
                 y = y,
                 mu = mu0)
  class(result) <- 'center.test'
  attributes(result)$family <- family
  return(result)

}

#==================================================================================================#
# Print function for center.test object
#==================================================================================================#

#' @title Print function for \code{center.test} object
#'
#' @author Ryan Thompson <ryan.thompson@monash.edu>
#'
#' @description Print objects of class \code{center.test}.
#'
#' @param x an object of class \code{center.test}
#' @param ... any other arguments
#'
#' @return The argument \code{x}.
#'
#' @method print center.test
#'
#' @export
#'

print.center.test <- \(x, ...) {
  cat('-----------------------------------------------\n')
  cat('familial test of centers with', attributes(x)$family, 'family\n')
  cat('-----------------------------------------------\n')
  cat('mu =', x$mu, '\n')
  cat('posterior probabilities: \n')
  print(x$prob)
  cat('optimal decision:', x$decision,  '\n')
}

#==================================================================================================#
# Plot function for center.test object
#==================================================================================================#

globalVariables(c('level', 'lower', 'upper', 'med'))

#' @title Plot function for \code{center.test} object
#'
#' @author Ryan Thompson <ryan.thompson@monash.edu>
#'
#' @description Plot the posterior distribution for the family of centers using a functional box plot.
#'
#' @param x an object of class \code{center.test}
#' @param band a vector of band limits for the functional box plot
#' @param ninterp the number of interpolation points for the functional box plot; more points lead
#' to finer resolution of the plot at the expense of additional computation
#' @param ... any other arguments
#'
#' @return A plot of the posterior distribution.
#'
#' @method plot center.test
#'
#' @export
#'
#' @importFrom graphics "plot"
#'

plot.center.test <- \(x, band = c(0.50, 0.75, 0.95), ninterp = 25, ...) {

  # Interpolate bootstrap paths at same lambda sequence
  lambda <- seq(min(x$boot$lambda), max(x$boot$lambda), length.out = ninterp)
  interpolate <- \(b) {
    x.b <- x$boot[x$boot$boot.id == b, ]
    x.b <- stats::approx(x.b$lambda, x.b$mu, lambda,
                         yleft = x.b$mu[which.min(x.b$lambda)],
                         yright = x.b$mu[which.max(x.b$lambda)],
                         method = ifelse(length(x.b$mu) > 1, 'linear', 'constant'))
    data.frame(boot.id = b, mu = x.b$y, lambda = x.b$x)
  }
  boot <- lapply(1:max(x$boot$boot.id), interpolate)
  boot <- do.call(rbind, boot)

  # Compute functional bands
  boot.wide <- as.matrix(stats::reshape(boot, idvar = 'boot.id', timevar = 'lambda',
                                        direction = 'wide')[, - 1])
  bands <- \(level) data.frame(DepthProc::fncGetBand(DepthProc::fncDepth(boot.wide), level), lambda,
                           level)
  func.bands <-  lapply(band, bands)
  func.bands <- do.call(rbind, func.bands)
  func.bands$level <- ordered(func.bands$level, levels = rev(band))
  colnames(func.bands) <- c('lower', 'upper', 'lambda', 'level')

  # Compute functional median
  func.med <- data.frame(DepthProc::fncDepthMedian(boot.wide), lambda)
  colnames(func.med) <- c('med', 'lambda')

  # Plot posterior
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(lambda, alpha = level, ymin = lower, ymax = upper),
                         func.bands) +
    ggplot2::geom_line(ggplot2::aes(lambda, med), func.med) +
    ggplot2::xlab(expression(lambda)) +
    ggplot2::ylab(ifelse(is.null(x$y), expression(mu(lambda)),
                         expression(mu[X](lambda)-mu[Y](lambda)))) +
    ggplot2::labs(alpha = 'Posterior central region')

}
