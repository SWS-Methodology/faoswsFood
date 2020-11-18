##' Center
##' These four different functions provide estimates for the food consumption 
##' values in year t+1 given the consumption in year t, changes in income, and 
##' the elasticity of the particular commodity.
##' 
##' @param type A character value.
##' 
##' @export
##' 

center_bc <- function(type = c("0", "1", "2", "3", "NA"), ...) {
    
    type <- match.arg(type)
    # if(is.na(type)) {func = function(...){NA_real_}}
    func <- switch(type,
                   `0` = linear_bc,
                   `1` = logLog_bc,
                   `2` = semiLog_bc,
                   `3` = logInverse_bc,
                   `NA` = function(...){NA_real_})
    
    func(...)
}
