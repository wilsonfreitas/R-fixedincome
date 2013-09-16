InterestRate <- function(value, daycount='business/252', compounding='compounded', frequency='annual') {
    that <- list()
    that$value <- value
    that$daycount <- daycount
    that$compounding <- compounding
    that$frequency <- frequency
    class(that) <- 'InterestRate'
    return(that)
}

