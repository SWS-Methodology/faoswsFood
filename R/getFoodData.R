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
##' @param newFoodCode The element code for food in the new system.
##' @param oldFoodCode The element code for food in the old system.
##' @param commCodesCPC The commodity codes for the SUA elements.  If NULL, all
##'   food commodities will be pulled.  These codes should be in CPC.
##' @param areaCodesM49 See commCodesCPC  These codes should be M49.
##'   
##' @return A data.table containing the food data.
##'   
##' @export
##' 

getFoodData = function(timePointYears, oldSystemYears = NULL,
                       newFoodCode = "5141", oldFoodCode = "141",
                       commCodesCPC = NULL, areaCodesM49 = NULL){
    ## Data quality checks
    if(is.null(oldSystemYears)){
        oldSystemYears = timePointYears[timePointYears <= 2011]
    }
    stopifnot(oldSystemYears %in% timePointYears)
    if(!exists("swsContext.baseRestUrl")){
        stop("swsContext parameters do not exist!  Have you ran ",
             "GetTestEnvironment?")
    }
    
    allCodesFS <- GetCodeList(domain = "faostat_one", dataset = "FS1_SUA_UPD",
                              dimension = "geographicAreaFS")
    if(is.null(areaCodesM49)){
        areaCodesFS <- allCodesFS
        areaCodesFS <- areaCodesFS[type == "country", code]
    } else {
        areaCodesFS <- faoswsUtil::m492fs(areaCodesM49)
        areaCodesFS <- areaCodesFS[!is.na(areaCodesFS)]
    }
    areaCodesFS <- areaCodesFS[areaCodesFS %in% allCodesFS[, code]]
    dimFS <- Dimension(name = "geographicAreaFS", keys = areaCodesFS)
    dimFood <- Dimension(name = "measuredElementFS", keys = oldFoodCode)
    if(is.null(commCodesCPC)){
        commCodesFCL <- GetCodeList(domain = "faostat_one", dataset = "FS1_SUA_UPD",
                                    "measuredItemFS")$code
    } else {
        commCodesFCL <- faoswsUtil::cpc2fcl(commCodesCPC, returnFirst = TRUE)
        commCodesFCL <- as.character(as.numeric(commCodesFCL))
        commCodesFCL <- commCodesFCL[!is.na(commCodesFCL)]
    }
    dimSua <- Dimension(name = "measuredItemFS", keys = commCodesFCL)
    dimTime <- Dimension(name = "timePointYears",
                         keys = as.character(oldSystemYears))
    keyOld <- DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD",
                         dimensions = list(dimFS, dimFood, dimSua, dimTime))
    ## Only get data if all dimensions have keys
    keyCount <- sapply(keyOld@dimensions, function(x){length(x@keys)})
    if(all(keyCount > 0)){
        oldData <- GetData(keyOld)
        ## Convert codes of old data
        oldData[, geographicAreaM49 :=
                    faoswsUtil::fs2m49(as.character(geographicAreaFS))]
        ## Mapping creates some NA M49 codes.  Remove those rows, as they don't exist in
        ## the FBS domain.
        oldData <- oldData[!is.na(geographicAreaM49), ]
        oldData[, geographicAreaFS := NULL]
        oldData[, measuredItemCPC :=
                    faoswsUtil::fcl2cpc(formatC(as.numeric(measuredItemFS),
                                                width = 4, flag = "0"))]
        oldData <- oldData[!is.na(measuredItemCPC), ]
        oldData[, measuredItemFS := NULL]
        oldData[, flagFaostat := NULL]
        oldData[, flagObservationStatus := NA_character_]
        oldData[, flagMethod := NA_character_]
        setnames(oldData, "measuredElementFS", "measuredElement")
    } else {
        oldData <- NULL
    }
    
    ## Convert codes for new system data
    if(is.null(areaCodesM49)){
        areaCodesM49 <- faoswsUtil::fs2m49(areaCodesFS)
        areaCodesM49 <- areaCodesM49[!is.na(areaCodesM49)]
    }
    dimM49 <- Dimension(name = "geographicAreaM49", keys = areaCodesM49)
    dimFood <- Dimension(name = "measuredElement", keys = newFoodCode)
    if(is.null(commCodesCPC)){
        commCodesCPC <- faoswsUtil::fcl2cpc(formatC(as.numeric(commCodesFCL),
                                                    width = 4, flag = "0"))
        commCodesCPC <- commCodesCPC[!is.na(commCodesCPC)]
    }
    dimCPC <- Dimension(name = "measuredItemCPC", keys = commCodesCPC)
    dimTime <- Dimension(name = "timePointYears",
                         keys = as.character(setdiff(timePointYears,
                                                     oldSystemYears)))
    keyNew <- DatasetKey(domain = "agriculture", dataset = "agriculture",
                         dimensions = list(dimM49, dimFood, dimCPC, dimTime))
    keyCount <- sapply(keyNew@dimensions, function(x){length(x@keys)})
    if(all(keyCount > 0)){
        newData <- GetData(keyNew)
    } else {
        newData <- NULL
    }
    
    finalData = rbind(newData, oldData)
    ## We want to make sure we have the same commodity/country keys for all 
    ## years (instead of missing records).  So, perform a cartesian join. Note: 
    ## measuredElement must be in the same data.frame as timePointYears, as the 
    ## element code changes from old vs. new (and old vs. new are split by 
    ## different years).  Then, form the cartesian product of these years with
    ## all other country-commodity pairs observed.
    out = merge.data.frame(unique(finalData[, c("timePointYears", "measuredElement"),
                                            with = FALSE]),
                           unique(finalData[, c("geographicAreaM49", "measuredItemCPC"),
                                            with = FALSE]))
    out = merge(out, finalData, by = colnames(out), all.x = TRUE)
    out = data.table(out)
    
    return(out)
}