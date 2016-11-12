
center <- function(type = c("0", "1", "2", "3", "NA"), ...) {
    
    type <- match.arg(type)
    # if(is.na(type)) {func = function(...){NA_real_}}
    func <- switch(type,
                   `0` = linear,
                   `1` = logLog,
                   `2` = semiLog,
                   `3` = logInverse,
                   `NA` = function(...){NA_real_})
    
    func(...)
}
