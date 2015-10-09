##' Commodity to Functional Form
##' 
##' This function takes a commodity code and maps the commodity code to a "food 
##' demand model" code and a "food commodity model" code.  These are types of
##' models that were originally implemented by Josef Schmidhuber in SAS, and we
##' are re-using the coefficients in R in the absence of a better food model.
##' 
##' @param commodityCode A character vector of commodity codes.
##'   
##' @return A list with two objects: a vector of the corresponding food demand 
##'   codes and a vector of the corresponding food commodity codes.
##'   

commodity2FunctionalForm <- function(commodityCode){
    map = fread("~/Documents/Github/faoswsFood/Data/commodityCodeMap.csv")
    map[, commodityCode := as.character(commodityCode)]
    commodityCode = data.table(commodityCode = commodityCode,
                               index = 1:length(commodityCode))
    commodityCode[, commodityCode := as.character(commodityCode)]
    
    out = merge(map, commodityCode, by = "commodityCode", all.y = TRUE)
    out = out[order(index), ]
    return(list(foodDemand = out$foodDemand, foodCommodity = out$foodCommodity))
}
