
#' Fixed income models, calculations, data structures and instruments
#' 
#' \code{fixedincome} has a set of funtions which helps with the
#' mathematics of interest rates and fixed income.
#' It handles the interest rates and compounding 
#' factors as objects and provides many methods to tackle specific issues
#' like compute discount factors, find equivalent rates, forward rates,
#' and so on. It also has classes to represent commom fixed income entities such as
#' a term structure of interest rates, spot rates and day count rules.
#' This package also supports methods and models commom used by practitioners to 
#' do fixed income calculations.
#'
#' @name fixedincome-package
#' @aliases fixedincome
#' @author Wilson Freitas \email{wilson.freitas@@gmail.com}
#' @references Frank Fabozzi. Fixed Income Mathematics, Wiley, 1994.
#' @references Bruce Tuckman. Fixed Income Securities, Wiley, 1994.
#' @import bizdays
#' @import methods
#' @import lubridate
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

