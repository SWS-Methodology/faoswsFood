suppressMessages({
    library(data.table)
    library(faosws)
    library(dplyr)
    library(faoswsUtil)
    library(faoswsFood)
    library(faoswsFlag)
    library(countrycode)
    library(zoo)
})

if(!CheckDebug()){
    options(error = function(){
        dump.frames()
        save(last.dump, file="/work/SWS_R_Share/caetano/last.dump.RData")
    })
}

## Set up for the test environment and parameters
R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE <- Sys.getenv("R_DEBUG_MODE")
overwritableFlags = c("M", "I")

# This return FALSE if on the Statistical Working System
if(CheckDebug()){
    
    message("Not on server, so setting up environment...")
    
    library(faoswsModules)
    SETTINGS <- ReadSettings("modules/impute_food/sws.yml")
    
    # If you're not on the system, your settings will overwrite any others
    R_SWS_SHARE_PATH <- SETTINGS[["share"]]
    
    # Define where your certificates are stored
    SetClientFiles(SETTINGS[["certdir"]])
    
    # Get session information from SWS. Token must be obtained from web interface
    GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                       token = SETTINGS[["token"]])
    files = dir("R", full.names = TRUE)
    sapply(files, source)
    
    dir_name <- "//hqlprsws2.hq.un.fao.org/sws_r_share/caetano/food/Data/"
} else {
    dir_name <- '/work/SWS_R_Share/caetano/food/Data/'
}

## Use old trade data up to
endYearOldTrade = 2013

# Parameter2004: reference year

# minReferenceYear <- as.numeric(ifelse(is.null(swsContext.computationParams$minReferenceYear), "2004",
#                                       swsContext.computationParams$minReferenceYear))

minReferenceYear <- 2011

# maxReferenceYear <- as.numeric(ifelse(is.null(swsContext.computationParams$maxReferenceYear), "2006",
#                                       swsContext.computationParams$maxReferenceYear))

maxReferenceYear <- 2013

referenceYearRange <- as.character(minReferenceYear:maxReferenceYear)
referenceYear <- round(median(as.numeric(referenceYearRange)))

if(minReferenceYear > maxReferenceYear | maxReferenceYear < minReferenceYear) 
    stop("Please check the time range for the reference years")

# Parameter: year to process

# minYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$minYearToProcess), "1990",
#                                       swsContext.computationParams$minYearToProcess))

minYearToProcess <- 1990

maxYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$maxYearToProcess), "2016",
                                      swsContext.computationParams$maxYearToProcess))

if(minYearToProcess > maxYearToProcess | maxYearToProcess < minYearToProcess) 
    stop("Please check the time range for the years to be processed")


if(referenceYear < minYearToProcess | referenceYear > maxYearToProcess) 
    stop("The reference years must be set to between the minimum and the maximum year to process")

yearCodes <- as.character(minYearToProcess:maxYearToProcess)

# referenceYear <- 2013

cat("Defining variables/dimensions/keys/...\n")

## Define the keys that we'll need for all the dimensions

##' Obtain computation parameter, this parameter determines whether only
##' selected session should be validated or the complete production domain.
validationRange = swsContext.computationParams$validation_selection

if(CheckDebug()){
    ## validationRange <- "session"
    validationRange <- "all"
}

##' Get session key and dataset configuration
sessionKey = swsContext.datasets[[1]]

##' Obtain the complete imputation Datakey
completeImputationKey = getCompleteImputationKey("food")
completeImputationKey@dimensions$timePointYears@keys = yearCodes

##' Selected the key based on the input parameter
selectedKey =
    switch(validationRange,
           "session" = sessionKey, # if "validationRange" is "session": selectedKey <- sessionKey
           "all" = completeImputationKey) # if "validationRange" is "all": selectedKey <- completeImputationkey

areaCodesM49 <- selectedKey@dimensions$geographicAreaM49@keys
itemCodesCPC <- selectedKey@dimensions$measuredItemCPC@keys

# Temp solution: use flagValidTable specific for the food module. The only difference
# is that (M,-) is not protected.
# flagValidTable <- fread("C:/Users/caetano/Documents/adhoc_food/flag_valid_table_specific/flagValidTable.csv")

## The element 21 contains the FBS population numbers
populationCodes <- "511"
## The element 141 contains the FBS food numbers
foodCodes <- "5141"

faoCodeList <- GetCodeList("faostat_datasets", "faostat_macro_ind", "geographicAreaFS")$code
dimFaoCode <- Dimension(name = "geographicAreaFS", keys = faoCodeList)

# GDP
gdpElementCode <- "6108"
gdpItemCode <- "22008"

comCodes <- GetCodeList("food", "food_factors","foodCommodityM")$code
fdmCodes <- GetCodeList("food", "food_factors","foodFdm")$code
funCodes <- GetCodeList("food", "food_factors","foodFunction")$code
varCodes <- "y_e" ## Only need elasticities from the food domain table

## Define the dimensions
dimM49 <- Dimension(name = "geographicAreaM49", keys = areaCodesM49)
dimPop <- Dimension(name = "measuredElement", keys = populationCodes)
dimElementGDP <- Dimension(name = "dim_element_fao_macro_ind", keys = gdpElementCode)
dimItemGDP <- Dimension(name = "dim_item_fao_macro_ind", keys = gdpItemCode)
dimTime <- Dimension(name = "timePointYears", keys = yearCodes)
dimFood <- Dimension(name = "measuredElement", keys = foodCodes)
dimCom <- Dimension(name = "foodCommodityM", keys = comCodes)
dimFdm <- Dimension(name = "foodFdm", keys = fdmCodes)
dimFun <- Dimension(name = "foodFunction", keys = funCodes)
dimVar <- Dimension(name = "foodVariable", keys = varCodes)

## Define the pivots.  We won't need this for all dimensions, so we'll only
## define the relevant ones.
pivotM49 <- Pivoting(code = "geographicAreaM49")
pivotPop <- Pivoting(code = "measuredElement")
pivotTime <- Pivoting(code = "timePointYears")
pivotGDP <- Pivoting(code = "wbIndicator")

## Define the keys
keyPop <- DatasetKey(domain = "population", dataset = "population_unpd",
                     dimensions = list(dimM49, dimPop, dimTime))

keyGDP <- DatasetKey(domain = "faostat_datasets", dataset = "faostat_macro_ind",
                     dimensions = list(dimFaoCode, dimElementGDP, dimItemGDP, dimTime))

keyFdm <- DatasetKey(domain = "food", dataset = "food_factors",
                     dimensions = list(dimM49, dimCom, dimFdm, dimFun, dimVar))

## Download all the datasets:
cat("Download all the datasets...\n")

## Download the population data from the SWS.  Using the pivoting argument, we 
## can specify the column order.  Since we're only pulling one key for the 
## population dimension, it makes sense to use that as the last dimension with 
## normalized = FALSE.  Doing this makes the last column the population, and
## names it Value_measuredElementPopulation_21.  We'll just rename it to
## population.
popData <- GetData(keyPop, flags=FALSE, normalized = FALSE,
                   pivoting = c(pivotM49, pivotTime, pivotPop))
setnames(popData, "Value_measuredElement_511", "population")

timeSeriesPopData <- as.data.table(expand.grid(geographicAreaM49 = unique(popData$geographicAreaM49),
                                               timePointYears = as.character(minYearToProcess:maxYearToProcess)))

popData = merge(timeSeriesPopData, popData, by = c("geographicAreaM49", "timePointYears"), 
                all.x = T)
popData[geographicAreaM49 == "156", geographicAreaM49 := "1248"]

# popData[, imputedPopulation := na.locf(population, fromLast = FALSE), 
#         by = list(geographicAreaM49)]
# 
# popData[is.na(population), population := imputedPopulation]
# popData[, c("imputedPopulation") := NULL]

# keyGDP@dimensions[[4]]@keys <- as.character(1991:2015)
# gdpData <- GetData(keyGDP, flags=FALSE)
# 
# gdpData[, geographicAreaM49 := fs2m49(geographicAreaFS)]
# setnames(gdpData, "Value", "GDP")
# gdpData[, c("geographicAreaFS", "dim_element_fao_macro_ind", "dim_item_fao_macro_ind") := NULL]
# setcolorder(gdpData, c("geographicAreaM49", "GDP", "timePointYears"))

gdpData <- ReadDatatable("gdp_usd2010")
setnames(gdpData, old = c("geographic_area_m49", "time_point_years", "gdp_usd_2010"),
         new = c("geographicAreaM49", "timePointYears", "GDP"))

gdpData[, c("fao_name", "fao_code") := NULL]

setcolorder(gdpData, c("geographicAreaM49", "timePointYears", "GDP"))

gdpData[, geographicAreaM49 := as.character(geographicAreaM49)]
gdpData[, timePointYears := as.character(timePointYears)]

# gdp_taiwan <- fread(paste0(dir_name, "gdp-taiwan.csv"))
gdp_taiwan <- ReadDatatable("gdp_taiwan_2005_prices")
gdp_taiwan[, time_point_years := as.numeric(time_point_years)]
setnames(gdp_taiwan, old = c("geographic_area_m49", "time_point_years", "gdp"),
         new = c("geographicAreaM49", "timePointYears", "GDP"))

gdp_taiwan = gdp_taiwan[timePointYears >= minYearToProcess & timePointYears <= maxYearToProcess]
gdp_taiwan[, geographicAreaM49 := as.character(geographicAreaM49)]
gdp_taiwan[, timePointYears := as.character(timePointYears)]

# Including Taiwan
gdpData <- rbind(gdpData, gdp_taiwan)
gdpData[geographicAreaM49 == "156", geographicAreaM49 := "1248"]

# timeSeriesGDPData <- as.data.table(expand.grid(geographicAreaM49 = unique(gdpData$geographicAreaM49),
#                                                timePointYears = as.character(minYearToProcess:maxYearToProcess)))
# 
# gdpData = merge(timeSeriesGDPData, gdpData, by = c("geographicAreaM49", "timePointYears"), 
#                 all.x = T)
# 
# gdpData[, imputedGDP := na.locf(GDP, fromLast = FALSE),
#         by = list(geographicAreaM49)]
#  
# gdpData[is.na(GDP), GDP := imputedGDP]
# gdpData[, c("imputedGDP") := NULL]

# m49 <- completeImputationKey@dimensions$geographicAreaM49@keys
# m49 <- m49[!(m49 %in% c("831", "832"))]
areaCodesM49 <- areaCodesM49[!(areaCodesM49 %in% c("831", "832"))]

foodData <- getFoodDataFAOSTAT1(areaCodesM49,
                                itemCodesCPC,
                                yearRange = as.character(minYearToProcess:maxYearToProcess),
                                "updated_sua_2013_data")
# foodData <- GetData(completeImputationKey, flags = TRUE)
foodData <- foodData[timePointYears <= 2013]
setnames(foodData, "Value", "food")
# foodData[, type := getCommodityClassification(as.character(measuredItemCPC))]
# food_classification_country_specific <- fread("Data/food_classification_country_specific.csv")
food_classification_country_specific <- ReadDatatable("food_classification_country_specific")
setnames(food_classification_country_specific, 
         old = c("geographic_area_m49", "measured_item_cpc", "food_classification"),
         new = c("geographicAreaM49", "measuredItemCPC", "foodClassification"))

foodData <- merge(
    foodData, food_classification_country_specific, 
    by = c("geographicAreaM49", "measuredItemCPC"),
    all.x = T)

setnames(foodData, "foodClassification", "type")
foodData = foodData[type %in% c("Food Estimate", "Food Residual")]

keys = c("flagObservationStatus", "flagMethod")
foodData = merge(foodData, flagValidTable, by = keys, all.x = T)

## Pull official food data from agriculture/aproduction

# completeImputationKey@domain <- "agriculture"
# completeImputationKey@dataset <- "aproduction"
# # completeImputationKey@dimensions$timePointYears@keys <- as.character(1991:2016)
# foodDataAgrApr <- GetData(completeImputationKey, flags = TRUE)

# Instead of pulling data from aproduction, Tomasz recommended to use SUA Updated 2015
# for the years 2014 and 2015.
foodData2014_2015 <- getFoodDataFAOSTAT1(areaCodesM49,
                                         itemCodesCPC,
                                         yearRange = as.character(2014:2015),
                                         "updated_sua_data")

keys = c("flagObservationStatus", "flagMethod")
foodData2014_2015 = merge(foodData2014_2015, flagValidTable, by = keys, all.x = T)
foodData2014_2015 = foodData2014_2015[Protected == TRUE]
# foodData2014_2015[, type := getCommodityClassification(as.character(measuredItemCPC))]
foodData2014_2015 <- merge(
    foodData2014_2015, food_classification_country_specific, 
    by = c("geographicAreaM49", "measuredItemCPC"),
    all.x = T)

setnames(foodData2014_2015, "foodClassification", "type")

foodData2014_2015 = foodData2014_2015[type %in% c("Food Estimate", "Food Residual")]
foodData2014_2015[, combineFlag := paste(flagObservationStatus, flagMethod, sep = ";")]

## Merge both food data sets
keys = c("geographicAreaM49", "timePointYears", "measuredItemCPC")
foodDataMerge = merge(foodData, foodData2014_2015[, c(keys, "Value", "combineFlag"), with = F],
                      by = keys, all = T)

# numbCasesM_ <- foodDataMerge[flagObservationStatus == "M" & flagMethod == "-", .N, timePointYears]
# flagM_2013 <- foodDataMerge[flagObservationStatus == "M" & flagMethod == "-" & timePointYears == 2013]
# 
# write.csv(flagM_2013, 
#           file = "C:/Users/caetano/Documents/adhoc_food/flag_valid_table_specific/flagM_2013.csv",
#           row.names = F)
# 
# write.csv(numbCasesM_, 
#           file = "C:/Users/caetano/Documents/adhoc_food/flag_valid_table_specific/numbCasesM_.csv",
#           row.names = F)

# Change M- to Mu

foodDataMerge[flagObservationStatus == "M" & flagMethod == "-", flagMethod := "u"]

## This data set has different flags for the same country/commodity
##test = foodDataMerge[Protected == FALSE & !is.na(Value)]

# flags from agriculture/aproduction

foodDataMerge[, flagObservationStatusValue := substr(combineFlag, 1, 1)]
foodDataMerge[combineFlag %in% c(";-", ";q", ";p"), flagObservationStatusValue := ""]
# foodDataMerge[!is.na(Value) & diff != 0 & combineFlag == ";q", updatedFlagObservationStatus := ""]
# foodDataMerge[!is.na(Value) & diff != 0 & combineFlag == ";p", updatedFlagObservationStatus := ""]

foodDataMerge[, flagMethodValue := substr(combineFlag, 3, 3)]
foodDataMerge[combineFlag == ";-", flagMethodValue := "-"]
foodDataMerge[combineFlag == ";q", flagMethodValue := "q"]
foodDataMerge[combineFlag == ";p", flagMethodValue := "p"]

## These new flags are protected or not? We need to merge this data set with flagValidTable

foodDataMerge = merge(foodDataMerge, flagValidTable, 
                      by.x = c("flagObservationStatusValue", "flagMethodValue"),
                      by.y = c("flagObservationStatus", "flagMethod"), 
                      all.x = T)

foodDataMerge[Protected.x == F & !(is.na(Value))]
foodDataMerge[, diff := food - Value]

foodDataMerge[!is.na(Value) & Protected.y == T, updatedFood := Value]
foodDataMerge[is.na(updatedFood), updatedFood := food]


foodDataMerge[!is.na(Value) & Protected.y == T, updatedFlagObservationStatus := flagObservationStatusValue]
foodDataMerge[!(!is.na(Value) & Protected.y == T), updatedFlagObservationStatus := flagObservationStatus]

foodDataMerge[!is.na(Value) & Protected.y == T, updatedFlagMethodValue := flagMethodValue]
foodDataMerge[!(!is.na(Value) & Protected.y == T), updatedFlagMethodValue := flagMethod]

# foodDataMerge[is.na(updatedFlagObservationStatus), updatedFlagObservationStatus := flagObservationStatus]
# 
# foodDataMerge[!is.na(Value) & diff != 0, updatedFlagMethod := substr(combineFlag, 3, 3)]
# foodDataMerge[is.na(updatedFlagMethod), updatedFlagMethod := flagMethod]
# 
# foodDataMerge[!is.na(Value) & diff != 0, .N, c("combineFlag", "updatedFlagObservationStatus", "updatedFlagMethod")]
# # foodDataMerge[!is.na(Value) & diff != 0 & combineFlag == ";-", updatedFlagObservationStatus := ""]
# foodDataMerge[!is.na(Value) & diff != 0 & combineFlag == ";-", updatedFlagMethod := "-"]
# 
# # foodDataMerge[!is.na(Value) & diff != 0 & combineFlag == ";q", updatedFlagObservationStatus := ""]
# foodDataMerge[!is.na(Value) & diff != 0 & combineFlag == ";q", updatedFlagMethod := "q"]
# 
# # foodDataMerge[!is.na(Value) & diff != 0 & combineFlag == ";p", updatedFlagObservationStatus := ""]
# foodDataMerge[!is.na(Value) & diff != 0 & combineFlag == ";p", updatedFlagMethod := "p"]

foodDataMerge[, c("flagObservationStatus", "flagMethod", "diff", "combineFlag", "Value", "food",
                  "Valid.x", "Valid.y", "Protected.x", "Protected.y",
                  "flagObservationStatusValue", "flagMethodValue") := NULL]
setnames(foodDataMerge, "updatedFlagObservationStatus", "flagObservationStatus")
setnames(foodDataMerge, "updatedFlagMethodValue", "flagMethod")
setnames(foodDataMerge, "updatedFood", "food")

foodDataMerge = merge(foodDataMerge, flagValidTable, 
                      by = c("flagObservationStatus", "flagMethod"), 
                      all.x = T)

## Checking countries with zero food figures
checkTotFood = foodDataMerge[, list(totFood = sum(food)), 
                             by = list(geographicAreaM49, timePointYears)]
checkTotFood = nameData("food", "fooddatafs", checkTotFood)
checkTotFood[totFood == 0, .N, c("geographicAreaM49", "geographicAreaM49_description")]
checkTotFood[timePointYears %in% referenceYearRange & totFood == 0, .N, geographicAreaM49]
excludeCountry = unique(checkTotFood[timePointYears %in% referenceYearRange & totFood == 0]$geographicAreaM49)
foodDataMerge = foodDataMerge[!(geographicAreaM49 %in% excludeCountry)]
foodDataMerge[, c("type", "Valid", "Protected") := NULL]

## Get the food classification for each commodity and select only the 
## commodities that should have figures in the food module. In this case we'll
## use just commodities with the food classification "Food Estimate". For the 
## commodities classified as a "Food residual", we need to check if there is
## net trade for them.
# foodData[, type := getCommodityClassification(measuredItemCPC)]
# foodData = foodData[type %in% c("Food Estimate", "Food residual", "Food Residual")]
# 
# 
# funcCodes <- commodity2FunctionalForm(
#         as.numeric(cpc2fcl(foodData$measuredItemCPC, returnFirst = TRUE)))
# foodData <- cbind(foodData, do.call("cbind", funcCodes))
# 
# keys = c("flagObservationStatus", "flagMethod")
# foodData <- merge(foodData, flagValidTable, all.x = T, by = keys)

## Creating time series data set
timeSeriesData <- as.data.table(expand.grid(timePointYears = as.character(minYearToProcess:maxYearToProcess),
                                            geographicAreaM49 = unique(foodDataMerge$geographicAreaM49),
                                            measuredItemCPC = unique(foodDataMerge$measuredItemCPC)))

# timeSeriesData[, type := getCommodityClassification(as.character(measuredItemCPC))]
timeSeriesData <- merge(timeSeriesData, food_classification_country_specific, 
                        by = c("geographicAreaM49", "measuredItemCPC"),
                        all.x = T)

setnames(timeSeriesData, "foodClassification", "type")

timeSeriesData <- merge(timeSeriesData, foodDataMerge, all.x = T,
                        by = c("geographicAreaM49", "timePointYears", "measuredItemCPC"))

timeSeriesData[, measuredElement := "5141"]

keys = c("flagObservationStatus", "flagMethod")
timeSeriesData <- merge(timeSeriesData, flagValidTable, all.x = T, by = keys)
timeSeriesData[, .N, Protected]
timeSeriesData[is.na(Protected), Protected := FALSE]

## Trade
tradeCode <- c("5610", "5910")
totalTradeKeySWS = DatasetKey(
    domain = "trade",
    dataset = "total_trade_cpc_m49",
    dimensions = list(
        Dimension(name = "geographicAreaM49",
                  keys = unique(timeSeriesData$geographicAreaM49)),
        Dimension(name = "measuredElementTrade", keys = tradeCode),
        Dimension(name = "timePointYears", keys = as.character((endYearOldTrade + 1):maxYearToProcess)),
        Dimension(name = "measuredItemCPC",
                  keys = unique(timeSeriesData$measuredItemCPC))
    )
)

totalTradeDataSWS = GetData(
    totalTradeKeySWS,
    flags = FALSE)

totalTradeDataSWS <- dcast.data.table(totalTradeDataSWS, geographicAreaM49 + measuredItemCPC +
                                          timePointYears ~ measuredElementTrade, value.var = "Value")


setnames(totalTradeDataSWS, "5610", "imports")
setnames(totalTradeDataSWS, "5910", "exports")

totalTradeDataFaostat <- getTotalTradeDataFAOSTAT1(unique(timeSeriesData$geographicAreaM49), 
                                                   unique(timeSeriesData$measuredItemCPC),
                                                   as.character(minYearToProcess:endYearOldTrade))

totalTradeDataFaostat <- dcast.data.table(totalTradeDataFaostat, geographicAreaM49 + measuredItemCPC + 
                                              timePointYears ~ measuredElement, value.var = "Value")

setnames(totalTradeDataFaostat, "5610", "imports")
setnames(totalTradeDataFaostat, "5910", "exports")

## Make a rbind between both total trade data from sws and faostat
totalTradeData = rbind(totalTradeDataFaostat, totalTradeDataSWS)

totalTradeData[is.na(imports), imports := 0]
totalTradeData[is.na(exports), exports := 0]
totalTradeData[, netTrade := (imports - exports)]

## Let's merge timeseries and totalTradeData
keys <- c("geographicAreaM49", "timePointYears", "measuredItemCPC")
timeSeriesData <- merge(timeSeriesData, totalTradeData, by = keys, all.x = T)
timeSeriesData[is.na(netTrade), netTrade := 0]
timeSeriesData[is.na(imports), imports := 0]
timeSeriesData[is.na(exports), exports := 0]

## Production
productionKey = DatasetKey(
    domain = "agriculture",
    dataset = "aproduction",
    dimensions = list(
        Dimension(name = "geographicAreaM49",
                  keys = unique(timeSeriesData$geographicAreaM49)),
        Dimension(name = "measuredElement",
                  keys = "5510"),
        Dimension(name = "measuredItemCPC",
                  keys = unique(timeSeriesData$measuredItemCPC)),
        Dimension(name = "timePointYears",
                  keys = as.character(minYearToProcess:maxYearToProcess)))
)

productionData = GetData(
    productionKey,
    flags = TRUE)

productionData[, c("measuredElement", "flagObservationStatus", "flagMethod") := NULL]

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")
timeSeriesData = merge(timeSeriesData, productionData, by = keys,
                       all.x = T)
setnames(timeSeriesData, "Value", "production")

timeSeriesData[is.na(production), production := 0]
timeSeriesData[, netSupply := netTrade + production]

## Preparing the dataset for computing average for food and net supply (production, imports and exports) 
selectYearsTab = timeSeriesData[timePointYears %in% referenceYearRange]

# If the figure is Protected in the reference year, we will not compute the food average for it 
selectYearsTab = 
    selectYearsTab[timePointYears == median(as.numeric(referenceYearRange)) & Protected == TRUE, 
                   protectedAux := 1, 
                   by = list(geographicAreaM49, measuredItemCPC)]

selectYearsTab[is.na(protectedAux), protectedAux := 0]
selectYearsTab[, aux := max(protectedAux, na.rm = T),
               by = list(geographicAreaM49, measuredItemCPC)]

selectYearsTabFoodEstimate = selectYearsTab[aux == 0 & type == "Food Estimate"]

## Computing food average
averageYearTab = selectYearsTabFoodEstimate[food > 0, list(
    foodAverage = mean(food, na.rm = T), 
    nObs = .N), 
    by = list(geographicAreaM49, measuredItemCPC)]

# numbItem = length(unique(averageYearTab$measuredItemCPC))
# tabCountryNan = averageYearTab[foodAverage == "NaN", .N, geographicAreaM49]
# setnames(tabCountryNan, "N", "numbFoodNaN")

## Let's exclude the country that is NaN for all the commodities
# averageYearTab = merge(averageYearTab, tabCountryNan, by = "geographicAreaM49", all.x = T)
averageYearTab = nameData("food", "fooddatafs", averageYearTab)
# averageYearTab[numbFoodNaN == numbItem, .N, c("geographicAreaM49_description", "geographicAreaM49")]

# averageYearTab = averageYearTab[numbFoodNaN < numbItem]
# averageYearTab[, .N, geographicAreaM49]
# averageYearTab[foodAverage > 0]
averageYearTab[, c("nObs", "geographicAreaM49_description", "measuredItemCPC_description") := NULL]
averageYearTab = averageYearTab[!foodAverage == "NaN"]
averageYearTab[, timePointYears := referenceYear]
averageYearTab[, timePointYears := as.character(timePointYears)]
averageYearTab[, flagObservationStatus := "I"]
averageYearTab[, flagMethod := "i"]
averageYearTab[, Protected := FALSE]

## Food Residual -  compute netSupply
selectYearsTabFoodResidual = selectYearsTab[aux == 0 & type == "Food Residual"]

## Computing netSupply average
averageYearTabResidual = selectYearsTabFoodResidual[netSupply > 0, list(
    netSupplyAverage = mean(netSupply, na.rm = T), 
    nObs = .N), 
    by = list(geographicAreaM49, measuredItemCPC)]

numbItem = length(unique(averageYearTabResidual$measuredItemCPC))
tabCountryNan = averageYearTabResidual[netSupplyAverage == "NaN", .N, geographicAreaM49]

averageYearTabResidual[, timePointYears := referenceYear]
averageYearTabResidual[, timePointYears := as.character(timePointYears)]
averageYearTabResidual[, flagObservationStatus := "I"]
averageYearTabResidual[, flagMethod := "i"]
averageYearTabResidual[, Protected := FALSE]

## Food Residual -  compute Trade
# selectYearsTabFoodResidual = selectYearsTab[aux == 0 & type == "Food Residual"]
# 
# ## Computing netTrade average
# averageYearTabResidualTrade = selectYearsTabFoodResidual[netTrade > 0, list(
#     netTradeAverage = mean(netTrade, na.rm = T), 
#     nObs = .N), 
#     by = list(geographicAreaM49, measuredItemCPC)]
# 
# numbItem = length(unique(averageYearTabResidualTrade$measuredItemCPC))
# tabCountryNan = averageYearTabResidualTrade[netTradeAverage == "NaN", .N, geographicAreaM49]
# 
# averageYearTabResidualTrade[, timePointYears := referenceYear]
# averageYearTabResidualTrade[, timePointYears := as.character(timePointYears)]
# averageYearTabResidualTrade[, flagObservationStatus := "I"]
# averageYearTabResidualTrade[, flagMethod := "e"]
# averageYearTabResidualTrade[, Protected := FALSE]

## Merge averageYearTab with timeSeriesData

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

timeSeriesData = merge(timeSeriesData, 
                       averageYearTab[, c("geographicAreaM49", "measuredItemCPC", "timePointYears", 
                                          "foodAverage", "flagObservationStatus", "flagMethod"),
                                      with =  F],
                       by = keys, all.x = T)

timeSeriesData[is.na(foodAverage), finalFood := food]
timeSeriesData[!is.na(foodAverage), finalFood := foodAverage]
timeSeriesData[!is.na(flagObservationStatus.y), flagObservationStatus.x := flagObservationStatus.y]
timeSeriesData[!is.na(flagMethod.y), flagMethod.x := flagMethod.y]

timeSeriesData[timePointYears == 2013 & !is.na(foodAverage)]
timeSeriesData[timePointYears == 2013 & !is.na(finalFood)]
timeSeriesData[, c("food", "foodAverage", "flagObservationStatus.y", "flagMethod.y") := NULL]
setnames(timeSeriesData, old = c("finalFood", "flagObservationStatus.x", "flagMethod.x"), 
         new = c("food", "flagObservationStatus", "flagMethod"))

## Merge averageYearTabResidual with timeSeriesData

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

timeSeriesData = merge(
    timeSeriesData, 
    averageYearTabResidual[, c("geographicAreaM49", "measuredItemCPC", 
                               "timePointYears", "netSupplyAverage", 
                               "flagObservationStatus", "flagMethod"), with =  F],
    by = keys, all.x = T)

timeSeriesData[is.na(netSupplyAverage), finalNetSupply := netSupply]
timeSeriesData[!is.na(netSupplyAverage), finalNetSupply := netSupplyAverage]

timeSeriesData[!is.na(flagObservationStatus.y), flagObservationStatus.x := flagObservationStatus.y]
timeSeriesData[!is.na(flagMethod.y), flagMethod.x := flagMethod.y]

# timeSeriesData[timePointYears == 2005 & !is.na(netSupplyAverage)]
# timeSeriesData[timePointYears == 2005 & !is.na(finalNetSupply)]
timeSeriesData[, c("netSupply", "netSupplyAverage", "flagObservationStatus.y", "flagMethod.y") := NULL]
setnames(timeSeriesData, old = c("finalNetSupply", "flagObservationStatus.x", "flagMethod.x"),
         new = c("netSupply", "flagObservationStatus", "flagMethod"))

## If the commodity is "Food Residual" and is not a protected figure, the amount of
## nettrade goes to food. But if nettrade is below zero, food is equal to zero.

timeSeriesData[Protected == FALSE & type == "Food Residual" & netSupply > 0, 
               food := netSupply]

timeSeriesData[Protected == FALSE & type == "Food Residual" & netSupply <= 0, 
               food := 0]

## Merge averageYearTabResidualTrade with timeSeriesData

# keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")
# 
# timeSeriesData = merge(
#     timeSeriesData, 
#     averageYearTabResidualTrade[, c("geographicAreaM49", "measuredItemCPC", 
#                                     "timePointYears", "netTradeAverage"), with =  F],
#     by = keys, all.x = T)
# 
# timeSeriesData[is.na(netTradeAverage), finalNetTrade := netTrade]
# timeSeriesData[!is.na(netTradeAverage), finalNetTrade := netTradeAverage]
# timeSeriesData[, c("netTrade", "netTradeAverage") := NULL]
# setnames(timeSeriesData, "finalNetTrade", "netTrade")

## If the commodity is "Food Residual" and is not a protected figure, the amount of
## nettrade goes to foodNetTrade But if nettrade is below zero, foodNetTrade is equal to zero.

# timeSeriesData[Protected == FALSE & type == "Food Residual" & netTrade > 0, 
#                foodNetTrade := netTrade]
# 
# timeSeriesData[Protected == FALSE & type == "Food Residual" & netTrade <= 0, 
#                foodNetTrade := 0]
# 
# timeSeriesData[type == "Food Estimate", 
#                foodNetTrade := food]

## Get initial food data for the commodities classified as a "Food Estimate" that don't
## have an initial food value in the reference year

countryCommodityZeroReferenceYear <- timeSeriesData[food %in% c(0, NA) & timePointYears == referenceYear &
                                                        type == "Food Estimate", .N, 
                                                    c("geographicAreaM49", "measuredItemCPC")]

initialFoodData <- getInitialFoodValue(
    country = countryCommodityZeroReferenceYear$geographicAreaM49, 
    commodity = countryCommodityZeroReferenceYear$measuredItemCPC,
    referenceYear = referenceYear,
    data = timeSeriesData
)
initialFoodData[, flagInitialFood := 1]

# Exclude the commodities that are Estimate but the time series for food is always zero.
initialFoodData[, .N, source]
initialFoodData <- initialFoodData[source == "food" & timePointYears >= referenceYear]

initialFoodData[, timePointYears := as.character(timePointYears)]
initialFoodData[, nrows := NULL]
keys <- c("geographicAreaM49", "measuredItemCPC", "timePointYears")
timeSeriesData <- merge(timeSeriesData, 
                        initialFoodData[, c(keys, "flagInitialFood"), with = F], 
                        by = keys, all.x = T)

# Workaround
# timeSeriesData[!is.na(initialFood), food := initialFood]
timeSeriesData[!is.na(flagInitialFood), Protected := TRUE]
# timeSeriesData[, initialFood := NULL]

# Food Commodity
funcCodes <- commodity2FunctionalForm(
    as.numeric(cpc2fcl(timeSeriesData$measuredItemCPC, returnFirst = TRUE)))
timeSeriesData <- cbind(timeSeriesData, do.call("cbind", funcCodes))
setkeyv(timeSeriesData, c("geographicAreaM49", "timePointYears"))

cat("Food data downloaded with", nrow(foodData), "rows.\n")

# Elasticity
fdmData <- GetData(keyFdm, flags=FALSE, normalized = FALSE)
setnames(fdmData, "Value_foodVariable_y_e", "elasticity")
setnames(fdmData, "foodFdm", "foodDemand")
setnames(fdmData, "foodCommodityM", "foodCommodity")

# Read map table from old code to new code
oldToNewCommodity = ReadDatatable("food_old_code_map")

fdmData <- merge(fdmData, oldToNewCommodity, all.x = T, allow.cartesian = T, 
                 by.x="foodCommodity", by.y = "old_code")
fdmData[is.na(new_code), new_code := foodCommodity]
fdmData <- fdmData[foodCommodity != "2500"]
fdmData[, c("foodCommodity") := NULL]
setnames(fdmData, old=c("new_code"), new=c("foodCommodity"))

fdmData <- fdmData[, list(elasticity = max(elasticity)), 
                   by=list(geographicAreaM49, foodDemand, foodFunction)]

## Merge the datasets together, and perform some processing.

cat("Merge population with GDP...\n")
## merge the current population and gross domestic product data into a single
## dataset
data <- merge(popData, gdpData, all = TRUE,
              by = c("geographicAreaM49", "timePointYears"))
data = data[!is.na(geographicAreaM49)]

cat("Merge in food data...\n")
data = merge(timeSeriesData, data, all.x = TRUE,
             by = c("geographicAreaM49", "timePointYears"))

cat("Merge in food demand model data...\n")
data <- merge(data, fdmData,
              by = c("foodDemand", "geographicAreaM49"),
              all.x = TRUE)

## Country income group

# countryGroup <- fread(system.file("extdata/class.csv", package = "faoswsStock"))
# countryIncomeGroup <- countryGroup[GroupCode %in% c("HIC", "LIC", "UMC", "LMC"), ]
# countryIncomeGroup[, geographicAreaM49 := as.character(countrycode(CountryCode, "wb", "iso3n"))]
# # Sudan has the wrong name (it should be former Sudan)
# countryIncomeGroup[geographicAreaM49 == "736", CountryName := "Sudan (former)"]
# # China should be 1248
# countryIncomeGroup[geographicAreaM49 == "156", geographicAreaM49 := "1248"]
# #Exclude Channel Islands and Kosovo (not separately recognised by the UN)
# countryIncomeGroup <- countryIncomeGroup[!is.na(geographicAreaM49)]
# setnames(countryIncomeGroup, "GroupName", "incomeGroup")
countryIncomeGroup <- ReadDatatable("country_income_group")
setnames(countryIncomeGroup, 
         old = c("geographic_area_m49", "country_name", "country_code", "group_code", "income_group"),
         new = c("geographicAreaM49", "CountryName", "CountryCode", "GroupCode", "incomeGroup"))

## We need to fill the gaps of the elasticity for the combination country/commodity

key = "geographicAreaM49"
data = merge(data, countryIncomeGroup[, c("geographicAreaM49", "incomeGroup"), with = F],
             by = key, all.x = T) 

## Take the elasticity average for each combination commodity/income group

data[, foodFunctionAux := as.numeric(foodFunction)]
elastAverage = data[, list(elasticityAverage = mean(elasticity, na.rm = T),
                           foodFunctionAux = round(mean(foodFunctionAux, na.rm = T))
                           # nObs = as.numeric(length(unique(geographicAreaM49)))
),
by = list(measuredItemCPC, incomeGroup)]

elastAverage = elastAverage[!is.na(elasticityAverage)]

elastAverage[, foodFunctionAux := as.character(foodFunctionAux)]
data[, "foodFunctionAux" := NULL]
## Merge elastAverage with data

keys = c("measuredItemCPC", "incomeGroup")
data = merge(data, elastAverage, by = keys, all.x = T)

## If elasticity is NA, we take the figure from elasticityAverage

data[is.na(elasticity), updatedElast := elasticityAverage]
data[!is.na(elasticity), updatedElast := elasticity]

data[is.na(foodFunction), updatedFoodFunction := foodFunctionAux]
data[!is.na(foodFunction), updatedFoodFunction := foodFunction]

## Analysing elasticity

tabSD <- data[timePointYears == referenceYear, list(
    minUpdatedElast = min(updatedElast, na.rm = T),
    averageUpdatedElast = mean(updatedElast, na.rm = T),
    sdUpdatedElast = sd(updatedElast, na.rm = T)), 
    by = list(incomeGroup, measuredItemCPC)]

tabSD[, lowerTreshold := averageUpdatedElast - 2 * sdUpdatedElast]
tabSD[, upperTreshold := averageUpdatedElast + 2 * sdUpdatedElast]

data <- merge(data, tabSD[, c("incomeGroup", "measuredItemCPC", "lowerTreshold",
                              "upperTreshold", "averageUpdatedElast"), with = F],
              by = c("incomeGroup", "measuredItemCPC"))

data[updatedElast > upperTreshold | updatedElast < lowerTreshold, flagOutlier := 1]
data[!(updatedElast > upperTreshold | updatedElast < lowerTreshold), flagOutlier := 0]

data[flagOutlier == 1, newElasticity := averageUpdatedElast]
data[flagOutlier == 0, newElasticity := updatedElast]

# The country/commodity that has no food classification will be classified as
# "Food Estimate".
data[is.na(type), type := "Food Estimate"]
##

# ## Workaround: let's make a test changing the classification of two commodities
# ## (meat of pig and meat of chicken) only for Mexico to Food Residual (Prod + Net trade)
# 
# data[geographicAreaM49 == "484" & measuredItemCPC %in% c("21113.01", "21121"), 
#               type := "Food Residual"]

if(nrow(data) == 0){
    warning("data has no rows, so the module is ending...  Are you sure there ",
            "are observations for food for these commodities?")
    stats = list(inserted = 0, ignored = 0, discarded = 0)
} else {
    
    ## First, sort the data by area and time.  The time sorting is important as we
    ## will later assume row i+1 is one time step past row i.
    setkeyv(data, c("geographicAreaM49", "timePointYears"))
    
    cat("Estimate food using the food model...\n")
    ## The funcional form 4 (originally presented in Josef's data) was replaced by
    ## functional form 3 The functional form 32 is a typo. It was replaced by
    ## functional form 2 in Food Factors database.
    # data[, foodFunction := ifelse(foodFunction == 4, 3, foodFunction)]
    data[, updatedFoodFunction := ifelse(updatedFoodFunction == 4, 3, updatedFoodFunction)]
    data[, foodHat := computeFoodForwardBackward(food = food,
                                                 pop = population,
                                                 elas = newElasticity,
                                                 gdp = GDP/population, 
                                                 netTrade = netSupply,
                                                 functionalForm = updatedFoodFunction,
                                                 timePointYears = as.numeric(timePointYears),
                                                 protected = Protected,
                                                 type = type, 
                                                 referenceYear = referenceYear),
         by = list(geographicAreaM49, measuredItemCPC)]
    
    data[, error := food - foodHat]
    
    dataToSave <- data[!is.na(foodHat)]
    
    ## Prepare data and save it to SWS
    cat("Restructure and filter data to save to SWS...\n")
    setnames(dataToSave, "foodHat", "Value")
    dataToSave[, measuredElement := "5141"]
    dataToSave[Protected == FALSE, flagObservationStatus := "I"]
    dataToSave[type == "Food Residual" & Protected == FALSE, flagMethod := "i"]
    dataToSave[type == "Food Estimate" & Protected == FALSE, flagMethod := "e"]
    
    dataToSave <- dataToSave[, c("geographicAreaM49", "measuredElement",
                                 "measuredItemCPC", "timePointYears", "Value", "flagObservationStatus", "flagMethod", "Protected"),
                             with = FALSE]
    
    keys = c("geographicAreaM49", "measuredElement",
             "measuredItemCPC", "timePointYears")
    
    # dataToSave[Protected == FALSE, flagObservationStatus := "I"]
    # dataToSave[Protected == FALSE, flagMethod := "e"]
    dataToSave <- dataToSave[, "Protected" := NULL]
    
    setcolorder(dataToSave,
                c("timePointYears", "geographicAreaM49", "measuredItemCPC",
                  "measuredElement", "Value", "flagObservationStatus", "flagMethod"))
    
    cat("Save the final data...\n")
    
    # country48 <- c("4","24","50","68","104","120","140","144","148","1248","170","178","218","320",
    #                "324","332","356","360","368","384","404","116","408","450","454","484","508",
    #                "524","562","566","586","604","608","716","646","686","762","834","764","800",
    #                "854","704","231","887","894","760","862","860")
    
    # dataToSave <- dataToSave[geographicAreaM49 %in% country48]
    
    
    # write.csv(dataToSave, file = "C:/Users/caetano/Documents/food-ad-hoc/reference_period_2012/foodDataReferenceYear2012_updated_final_2.csv",
    # row.names = F)
    
    
    # save a .csv to make analysis for the new food classification
    # write.csv(dataToSave, file = "C:/Users/caetano/Documents/food-ad-hoc/analysis_food_classification/foodNewClassification.csv",
    #           row.names = F)
    
    #dataToSave <- dataToSave[geographicAreaM49 %in% c(360, 454, 484, 686, 1248, 392, 716)]
    stats = SaveData(domain = "food", dataset = "fooddata", data = dataToSave, waitTimeout = 1800)
}

paste0("Food module completed successfully!!! ",
       stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")


