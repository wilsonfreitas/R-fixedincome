#' Fixed income models, calculations and data structures
#'
#' The \code{fixedincome} package brings a set of funtions that helps with the
#' mathematics of interest rates and fixed income.
#' It handles the interest rates and term structures of interest rates as
#' objects and provides many methods to tackle specific issues
#' like computing discount factors and forward rates, interpolate term
#' structures, fit curve models and so much more.
#' This package also supports methods and models commonly used by practitioners
#' to do fixed income calculations.
#'
#' @name fixedincome-package
#' @aliases fixedincome
#' @author Wilson Freitas \email{wilson.freitas@gmail.com}
#' @references Frank Fabozzi. Fixed Income Mathematics, Wiley, 1994.
#' @references Bruce Tuckman. Fixed Income Securities, Wiley, 1994.
#' @import bizdays
#' @import methods
#' @importFrom graphics par axis abline points lines legend
#' @importFrom grDevices xy.coords
#' @importFrom stats stepfun optim approxfun splinefun
#' @importFrom utils head
#' @docType package
NULL


#' @title Datasets
#'
#' @description
#' Interest rate datasets
#'
#' @docType data
#' @keywords datasets
#' @name datasets
NULL

#' @details
#' \code{ZeroCurveBRL} Brazil's zero curve
#'
#' @rdname datasets
#' @name ZeroCurveBRL
NULL