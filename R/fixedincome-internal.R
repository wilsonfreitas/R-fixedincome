.First <-
function () 
{
    options(repos = c(CRAN = "http://cran.rstudio.com/"), browserNLdisabled = TRUE, 
        deparse.max.lines = 2)
}

#' Tools for calculations on fixed income, term structure and interest rate.
#' 
#' \code{fixedincome} brings a rich set of tools to make the life of any
#' fixed income analyst less miserable.
#' Fixed income calculations have many specific rules which deal with subjects
#' ranging from compounding regimes to business days rules to compute the 
#' amount of non-working days between maturities.
#'
#' @name fixedincome-package
#' @aliases fixedincome
#' @title Tools for fixed income calculations
#' @author Wilson Freitas \email{wilson.freitas@@gmail.com}
#' @references Frank Fabozzi. Fixed Income Mathematics, Wiley, 1994.
#' @references Bruce Tuckman. Fixed Income Securities, Wiley, 1994.
#' @docType package
NULL
