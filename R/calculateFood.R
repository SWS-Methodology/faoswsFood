##' Calculate Food
##' 
##' @param food The food consumption at time t.
##' @param elas The elasticity of the commodity.  This will vary by country and 
##'   commodity.
##' @param gdp_pc Per person GDP (gross domestic product) at time t.
##' @param functionalForm Currently one of 0, 1, 2, or 3.  Specifies if a log-log,
##'   semi-log, or inverse-log food demand model should be used.
##' @param pop Population at time t.
##' @param trend_factor Changes due to income and other factors.
##' @param referenceYear Year of reference to compute the food estimates.
##' @param dt Table in data.table format 
##'   
##' @return A numeric vector of the estimated food consumption.
##' 
##' @export
##' 

calculateFood <- function(food, elas, gdp_pc, functionalForm, pop, trend_factor, referenceYear, dt){
    ## Data Quality Checks
    stopifnot(length(food) == length(elas))
    stopifnot(length(food) == length(gdp_pc))
    stopifnot(length(food) == length(pop))
    
    ## 0, 1, 2 and 3 are the functional forms linking the human food consumption
    ## and GDP.
    ## Elasticity parameter and functional forms were provided by Josef at FBS 
    ## aggregated level.
    ## Elasticity parameter and functional forms are commodity and country 
    ## dependant.
    if(is.na(functionalForm)){
        ## If functionalForm is missing, we have no way of computing the food. 
        ## But, to avoid an error, we'll need a function that just returns NA no
        ## matter what it is passed.
        func = function(...){NA_real_}
    } else if(functionalForm == 1){
        func = logLog
    } else if(functionalForm == 2){
        func = semiLog
    } else if(functionalForm == 3){
        func = logInverse
    } else if(functionalForm == 0){
        func = linear
    } else {
        stop("A functionalForm other than 0, 1, 2, or 3 was encountered!",
             "  This is not currently implemented.")
    }
    
    ## gdp_pc_t1 should be the same as gdp_pc but offset by 1 (i.e. one year
    ## ahead).
    food_t0 = dt[timePointYears == referenceYear, food]
    gdp_pc_t0 = dt[timePointYears == referenceYear, GDP]
    pop_t0 = dt[timePointYears == referenceYear, population]
    
    func(food_t0 = food_t0, elas = elas, gdp_pc_t1 = gdp_pc,
         gdp_pc_t0 = gdp_pc_t0, pop_t1 = pop, pop_t0 = pop_t0, 
         trend_factor = trend_factor)
}