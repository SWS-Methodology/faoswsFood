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
    ## Data Quality Checks
    if(length(commodityCode) == 0){
        return(list(foodDemand = c(), foodCommodity = c()))
    }
    
    map = fread(paste0(R_SWS_SHARE_PATH, "/browningj/food/commodityCodeMap.csv"))
    map[, fbsCode := as.character(fbsCode)]
    fbsCode = data.table(fbsCode = faoswsUtil::getFBSCode(commodityCode),
                         index = 1:length(commodityCode))
    fbsCode[, fbsCode := as.character(fbsCode)]
    
    out = merge(map, fbsCode, by = "fbsCode", all.y = TRUE)
    out = out[order(index), ]
    return(list(foodDemand = out$foodDemand, foodCommodity = out$fbsCode))
}
