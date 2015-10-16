##' Get FBS Code
##' 
##' This function takes a FCL commodity code and returns the FBS aggregate code 
##' that contains this data.
##' 
##' @param commCodeFCL
##'   
##' @return A vector of FBS codes corresponding to the passed FCL codes. 
##'   Missing codes are returned with a NA.
##'   
##' @export
##' 

getFBSCode = function(commCodeFCL){
    if(is.character(commCodeFCL)){
        commCodeFCL = as.numeric(commCodeFCL)
    }
    commCodeFCL = data.table(item = commCodeFCL)
    setkeyv(commCodeFCL, "item")
    
    map = GetTableData(schemaName = "ess", tableName = "extraction_rates")
    map[, target_code := as.character(target_code)]
    ## If we have an top node (i.e. no parents) with a target_code of NA, our
    ## while loop below will never end.  So, instead, assign some non-FBS code
    ## (like CPC-ID) and these will get filtered out later.
    map[item == parent & is.na(target_code),
        target_code := paste0("CPC", item)]
    setkeyv(map, "item")
    
    result = merge(map, commCodeFCL)
    ## Processed products won't have FBS codes.  So, assign the parent (primary 
    ## products have themselves as parents) to the item, and continue to merge
    ## back with the map until you have all FBS codes.
    while(any(is.na(result$target_code))){
        result[, item := parent]
        result = result[, list(item)] # "list" to stay data.table
        setkey(result, item)
        result = merge(map, result)
    }
    result[, target_code]
}