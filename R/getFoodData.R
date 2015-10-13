##' Get Food Data
##' 
##' This function pulls in food data from the working system given a year range.
##' It pulls data from two tables (FAOSTAT tables with the old food data and 
##' agriculture production with the new food data).  These two tables are binded
##' together after the data from the old system is converted to use codes from
##' the new system.
##' 
##' @param timePointYears A numeric vector of years for which food data is 
##'   required.
##' @param oldSystemYears A numeric vector of years for which the food data 
##'   should be pulled from the old system.  This vector must be a subset of the
##'   timePointYears parameter.  Additionally, the default (NULL) will be 
##'   updated to contain all years of timePointYears that are less than 2012. 
##'   This should be appropriate, as new data is now created after 2011 (at 
##'   least at the time of writing this function).
##'   
##' @return A data.table containing the food data.
##'   

getFoodData = function(timePointYears, oldSystemYears = NULL,
                       newFoodCode = "5141", oldFoodCode = "141"){
    ## Data quality checks
    if(is.null(oldSystemYears)){
        oldSystemYears = timePointYears[timePointYears <= 2011]
    }
    stopifnot(oldSystemYears %in% timePointYears)
    if(!exists("swsContext.baseRestUrl")){
        stop("swsContext parameters do not exist!  Have you ran ",
             "GetTestEnvironment?")
    }
    
    areaCodesFS <- GetCodeList(domain = "faostat_one", dataset = "FS1_SUA_UPD",
                               dimension = "geographicAreaFS")
    areaCodesFS <- areaCodesFS[type == "country", code]
    dimFS <- Dimension(name = "geographicAreaFS", keys = areaCodesFS)
    dimFood <- Dimension(name = "measuredElementFS", keys = oldFoodCode)
    suaCodes <- GetCodeList(domain = "faostat_one", dataset = "FS1_SUA_UPD",
                            "measuredItemFS")$code
    dimSua <- Dimension(name = "measuredItemFS", keys = suaCodes)
    dimTime <- Dimension(name = "timePointYears",
                         keys = as.character(oldSystemYears))
    keyOld <- DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD",
                         dimensions = list(dimFS, dimFood, dimSua, dimTime))
    oldData <- GetData(keyOld)
    
    ## Convert codes of old data
    oldData[, geographicAreaM49 :=
                faoswsUtil::fs2m49(as.character(geographicAreaFS))]
    ## Mapping creates some NA M49 codes.  Remove those rows, as they don't exist in
    ## the FBS domain.
    oldData <- oldData[!is.na(geographicAreaM49), ]
    oldData[, geographicAreaFS := NULL]
    oldData[, measuredItemCPC :=
                faoswsUtil::fcl2cpc(formatC(measuredItemFS, width = 4, flag = "0"))]
    oldData <- oldData[!is.na(measuredItemCPC), ]
    oldData[, measuredItemFS := NULL]
    oldData[, flagFaostat := NULL]
    oldData[, flagObservationStatus := NA_character_]
    oldData[, flagMethod := NA_character_]
    setnames(oldData, "measuredElementFS", "measuredElement")
    
    areaCodesM49 <- GetCodeList(domain = "agriculture", dataset = "agriculture",
                                dimension = "geographicAreaM49")
    areaCodesM49 <- areaCodesM49[type == "country", code]
    dimM49 <- Dimension(name = "geographicAreaM49", keys = areaCodesM49)
    dimFood <- Dimension(name = "measuredElement", keys = newFoodCode)
    cpcCodes <- GetCodeList(domain = "agriculture", dataset = "agriculture",
                            "measuredItemCPC")$code
    dimCPC <- Dimension(name = "measuredItemCPC", keys = cpcCodes)
    dimTime <- Dimension(name = "timePointYears",
                         keys = as.character(setdiff(timePointYears,
                                                     oldSystemYears)))
    keyNew <- DatasetKey(domain = "agriculture", dataset = "agriculture",
                         dimensions = list(dimM49, dimFood, dimCPC, dimTime))
    newData <- GetData(keyNew)
    
    finalData = rbind(newData, oldData)
    return(finalData)
}