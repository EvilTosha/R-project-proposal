#   xtsExtra: Extensions to xts during GSOC-2012
#
#   Copyright (C) 2012  Michael Weylandt: michael.weylandt@gmail.com
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

### A first attempt at multi-data-type-xts objects
### For now implemented entirely in R, move to C over time

### Implementation model:
###   1) List of xts objects, each comprising a single column and a single data type
###   2) Pseudo-inherits to data.frame with a helpful downgrade ?
###   3) Need to handle ... for both xts() and data.frame() -- right now, deferring to data.frame() mostly

xtsdf <- function(..., order.by = index(x), frequency = NULL, unique = TRUE, tzone = Sys.getenv("TZ"),
                  stringsAsFactors = default.stringsAsFactors(), check.names = TRUE) {
  # xtsdf constructor function
  # uses xts() and data.frame() code instead of rewriting all the name handling
  x <- data.frame(..., stringsAsFactors = stringsAsFactors, check.names = check.names)
  as.xtsdf(x, order.by = order.by, frequency = frequency, unique = unique, tzone = tzone)
}

is.xtsdf <- function(x) inherits(x, "xtsdf")

as.xtsdf <- function(x, ...) UseMethod("as.xtsdf")

as.xtsdf.xts <- function(x, ...){
  # Easy case -- split by list and add S3 class
  ans <- as.list(x)
  class(ans) <- "xtsdf"
  ans
}

as.xtsdf.data.frame <- function(x, order.by = "rownames", ..., frequency = NULL, unique = TRUE, tzone = Sys.getenv("TZ")){
  # Next easiest case --
  #   Take data frame and order.by argument and construct xts objects directly
  #   Also allow order.by = "rownames" to use x's rownames

  if (any(!is.timeBased(order.by))) {
    if (order.by == "rownames") {
      order.by <- rownames(x)
    }
    order.by <- as.POSIXct(order.by, ...)
  }

  ans <- lapply(as.list(x), function(x) xts(x, order.by = order.by, frequency = frequency, unique = unique, tzone = tzone))
  class(ans) <- "xtsdf"

  ans
}

as.xtsdf.matrix <- function(x, ...) as.xtsdf(as.data.frame(x), ...)

as.data.frame.xtsdf <- function(x, row.names = NULL, optional = FALSE, ...){
  row.names <- if(is.null(row.names)) index(x) else row.names

  do.call("data.frame", c(as.list(x), list(row.names = row.names, check.names = optional, ...)))
}

as.xts.xtsdf <- function(x, ...){
  xts(do.call("cbind", x), ...)
}

as.xtsdf.xtsdf <- function(x, ...) x


## Proposal code

dim.xtsdf <- function(x) c(max(sapply(x, length)), length(x))

dimnames.xtsdf <- function(x) list(rownames(x[[1]]), names(x))

print.xtsdf <- function(x, ...) {
  df <- as.data.frame(x)
  rownames(df) <- index(x[[1]])
  print(df)
}


#' basic subsetting for the xtsdf object
#'
#' Perform subsettings using numeric indexes, logical vectors or character vectors (for colnames only)
#' @param x An xtsdf object
#' @param i row index
#' @param j col index
#' @return an xtsdf object, containing required subset
#' @author Anton Samoylov
BasicSubset <- function(x, i, j) {
  nr <- nrow(x)
  nc <- ncol(x)

  if (!missing(i)) {
    ## test for negative subscripting in i
    if (is.numeric(i)) {
      if (any(i < 0)) {
        if (!all(i <= 0))
          stop('only zeros may be mixed with negative subscripts')
        i <- (1:nr)[i]
      }
      ## check boundary; length check avoids Warning from max()
      if (length(i) > 0 && max(i) > nr)
        stop('subscript out of bounds')
    }
    else if(is.logical(i)) {
      if (length(i) == 1) {
        i <- (1:nr)[rep(i, nr)]
      }
      else if (length(i) > nr) {
        stop("(subscript) logical subscript too long")
      }
      else i <- which(i)
    }

    if(!isOrdered(i, strictly = FALSE)) {
      i <- sort(i)
    }

    ## 0's in the 'i' position cause failures
    if (any(i == 0))
      i <- i[-which(i == 0)]

    if (length(i) == 0 || (length(i) == 1 && i == 0)) {
      stop("No support for zero index for now")
    }
  }

  if (missing(j)) {
    if (missing(i))
      i <- seq_len(nr)
    ans <- lapply(x, function (col) col[i])
    class(ans) <- "xtsdf"
    return(ans)
  }
  else {
    ## test for negative subscripting in j
    if (is.numeric(j)) {
      if (min(j, na.rm = TRUE) < 0) {
        if (max(j, na.rm = TRUE) > 0)
          stop('only zeros may be mixed with negative subscripts')
        j <- (1:nc)[j]
      }
      if (max(j, na.rm = TRUE) > nc)
        stop('subscript out of bounds')
    }
    else if (is.logical(j)) {
      if (length(j) == 1) {
        j <- (1:nc)[rep(j, nc)]
      }
      else if (length(j) > nc) {
        stop("(subscript) logical subscript too long")
      }
      else j <- which(j)
    }
    else if (is.character(j)) {
      j <- match(j, colnames(x), nomatch=0L)
    }

    if(length(j) == 0 || (length(j) == 1 && j == 0)) {
      stop("No support for zero index for now")
    }
    if (missing(i)) {
      ans <- x[j]
      class(ans) <- "xtsdf"
      return(ans)
    }
    cols <- x[j]
    ans <- lapply(cols, function (col) col[i])
    class(ans) <- "xtsdf"
    return(ans)
  }
}

## Some examples:

data(sample_matrix)
xdf <- as.xtsdf(sample_matrix[1:10, ])
xdf
BasicSubset(xdf, 1:5, 1:2)
BasicSubset(xdf, j = c("Open", "Close"))
BasicSubset(xdf, c(TRUE, FALSE, TRUE, TRUE), TRUE)
