

#' check whether a weights vector satisfies constraints
#'
#' @param weights a named vector containing weight for each asset in portfolio
#' @param constraints an object of type "constraints", specifying the constraints, see \code{\link{constraint}}
#' @return TRUE, if weigths are satisfying constraints, FALSE otherwise
#' @author Anton Samoylov
CheckConstraint <- function(weights, constraints) {
  nassets <- length(constraints$assets)
  if (length(weights) != nassets) {
    warning("Length of weight vector is not equal to length of assets")
    return(FALSE)
  }
  if (any(constraints$min > weights) | any(weights > constraints$max)) {
    warning("Box constraints are not satisfied")
    return(FALSE)
  }
  min.mult     <- constraints$min_mult
  if (is.null(min.mult)) {
    min.mult   <- rep(-Inf, nassets)
  }
  max.mult     <- constraints$max_mult
  if (is.null(max.mult)) {
    max.mult   <- rep(Inf, nassets)
  }
  if (any(min.mult * constraints$assets > weights) | any(weights > max.mult * constraints$assets)) {
    warning("Box constraints are not satisfied")
    return(FALSE)
  }
  if (constraints$min_sum > sum(weights) | sum(weights) > constraints$max_sum) {
    warning("Sum constraints are not satisfied")
    return(FALSE)
  }
  return(TRUE)
}

#' generate portfolio satisfying given constraints, if it's possibe
#' additional property: if feasible portfolio exists, returned weight vector will have sum = max(min_sum, sum(min))
#'
#' @param constraints an object of type "constraints", specifying the constraints, see \code{\link{constraint}}
#' @return named weighted vector, if feasible portfolio exists, NULL otherwise
#' @author Anton Samoylov
FeasiblePortfolio <- function(constraints) {
  assets       <- constraints$assets
  nassets      <- length(assets)
  min.mult     <- constraints$min_mult
  if (is.null(min.mult)) {
    min.mult   <- rep(-Inf, nassets)
  }
  max.mult     <- constraints$max_mult
  if (is.null(max.mult)) {
    max.mult   <- rep(Inf, nassets)
  }
  min.sum      <- constraints$min_sum
  max.sum      <- constraints$max_sum
  max          <- constraints$max
  min          <- constraints$min
  portfolio    <- as.vector(assets)

  ## incapsulate two constraints in a single vector (one vector for max constraints and another for min)
  for (i in 1:nassets) {
    max[i] <- min(max[i], portfolio[i] * max.mult[i])
    min[i] <- max(min[i], portfolio[i] * min.mult[i])
  }

  ## check whether minimum possible portfolio satisfies sum constraint
  if (sum(min) > max.sum) {
    warning("No feasible portfolio exist for specified constraints - minimum possible sum exceeds max.sum")
    return(NULL)
  }
  ## check whether maximum possible portfolio satisfies sum constraint
  if (sum(max) < min.sum) {
    warning("No feasible portfolio exist for specified constraints - maximum possible sum is less than min.sum")
    return(NULL)
  }

  ## at this point we know that solution exists
  ## firstly create portfolio satisfying individual assets' constraints with minimum possible weights
  for (i in 1:nassets) {
    portfolio[i] <- min[i]
  }

  ## increase weights while the sum is outside the box
  i <- 1
  while ((sum(portfolio) < min.sum) & (i <= nassets)) {
    ## if we can increase current weight such that sum become greater than or equal to min.sum
    if ((sum(portfolio) + max[i] - portfolio[i]) > min.sum) {
      ## increase current weight s.t. sum become min.sum
      portfolio[i] <- portfolio[i] + min.sum - sum(portfolio)
    } else {
      ## else we can increase current weight to its max
      portfolio[i] <- max[i]
    }
    i <- i + 1 # increase counter
  }
  ## return found portfolio
  names(portfolio) <- names(assets)
  return(portfolio)
}

#' generate a random protfolio satisfying given constraints
#'
#' @param constraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param \dots any other passthru parameters
#' @return named weighting vector if at least one feasible portfolio exists, NULL otherwise
#' @author Anton Samoylov
#' @export
RandomizePortfolio <- function(constraints, ...) {
  ## first of all check whether at least one feasible portfolio exists
  feasible.portfolio <- FeasiblePortfolio(constraints)
  if (is.null(feasible.portfolio)) {
    return(NULL)
  }

  ## for now assume that all box constraints are finite
  assets       <- constraints$assets
  nassets      <- length(assets)
  min.mult     <- constraints$min_mult
  if (is.null(min.mult)) {
    min.mult   <- rep(-Inf, nassets)
  }
  max.mult     <- constraints$max_mult
  if (is.null(max.mult)) {
    max.mult   <- rep(Inf, nassets)
  }
  min.sum      <- constraints$min_sum
  max.sum      <- constraints$max_sum
  max          <- constraints$max
  min          <- constraints$min
  portfolio    <- as.vector(assets)

  ## incapsulate two constraints in a single vector (one vector for max constraints and another for min)
  for (i in 1:nassets) {
    max[i] <- min(max[i], portfolio[i] * max.mult[i])
    min[i] <- max(min[i], portfolio[i] * min.mult[i])
  }

  ## firstly create portfolio satisfying individual assets' constraints with minimum possible weights
  for (i in 1:nassets) {
    portfolio[i] <- min[i]
  }

  ## compute weight that should be distributed. Choose random point between minimum and maximum possible sums
  remainder <- runif(1, min = max(sum(min), min.sum), max = min(sum(max), max.sum)) - sum(min)

  ## Assign weights in two steps:
  ## 1) traverse portfolio in random order, and assign each asset random weight, s.t. it doesn't violate both
  ## its own box constraints and constraint for protfolio sum. In other words randomly distribute the remainder.
  ## 2) traverse portfolio in random order, and assign remainder where it's possible. At the end remainder must be 0

  random.index <- sample(1:nassets, nassets)
  for (i in 1:nassets) {
    cur.index <- random.index[i]
    ## assign random possible addition
    ## this distribution gives the best results
    ratio <- 1 / (rexp(1, 1 / nassets) + 1)
    add <- ratio * (min(max[cur.index] - min[cur.index], remainder))
    portfolio[cur.index] <- portfolio[cur.index] + add
    remainder <- remainder - add
  }

  ## perform step 2
  if (remainder > 0) {
    random.index <- sample(1:nassets, nassets)
    for (i in 1:nassets) {
      cur.index <- random.index[i]
      add <- min(remainder, max[cur.index] - portfolio[cur.index])
      portfolio[cur.index] <- portfolio[cur.index] + add
      remainder <- remainder - add
    }
  }

  names(portfolio) <- names(assets)
  return(portfolio)
}

#' generate an arbitary number of constrained random portfolios
#'
#' repeatedly calls \code{\link{RandomizePortfolio}} to generate an
#' arbitrary number of constrained random portfolios.
#'
#' @param constraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param n integer: number of unique constrained random portfolios to generate
#' @param \dots any other passthru parameters
#' @return matrix of random portfolio weights
#' @seealso \code{\link{constraint}}, \code{\link{RandomizePortfolio}}
#' @author Anton Samoylov
#' @export
RandomPortfolios <- function (constraints, n = 100, ...) {
  assets  <- constraints$assets
  nassets <- length(assets)
  result  <- matrix(nrow = n, ncol = nassets)

  for (i in 1:n) {
    result[i, ] <- RandomizePortfolio(constraints, ...)
  }

  if (n > 2) {
    ## add two standard portfolios if they're satisfying constraints
    ## seed portfolio
    if (CheckConstraint(assets, constraints)) {
      result[1, ] <- assets
    }
    ## random-weight protfolio
    eq.weights <- rep(1 / nassets, nassets)
    if (CheckConstraint(eq.weights, constraints)) {
      result[2, ] <- eq.weights
    }
  }

  result = unique(result)
  result = rbind(result, matrix(nrow = (n - nrow(result)), ncol = nassets))

  colnames(result) <- names(assets)
  return(result)
}
