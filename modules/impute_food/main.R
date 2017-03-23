suppressMessages({
    library(data.table)
    library(faosws)
    library(dplyr)
    library(faoswsUtil)
    library(faoswsFood)
    library(faoswsFlag)
    library(countrycode)
    library(zoo)
    #library(reshape2)
})

if(!CheckDebug()){
    options(error = function(){
        dump.frames()
        save(last.dump, file="/work/SWS_R_Share/caetano/last.dump.RData")
    })
}


## To do:
## - Check the input dataset from Josef against what's on the server: ask Nick for
## the file that Jim gave him with the elasticities, compare with Josef's file
## and figure out why we have differences.  Reload if necessary.

## set up for the test environment and parameters
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
    
}

# if(swsContext.computationParams$yearToProcess < 1991)
#     stop("This module was designed for imputation on years after 1990 only!")
referenceYear <- as.numeric(ifelse(is.null(swsContext.computationParams$referenceYear), "1998",
                                   swsContext.computationParams$referenceYear))

yearAverage <- c("2004", "2005", "2006")

# minYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$minYearToProcess), "1991",
#                                    swsContext.computationParams$minYearToProcess))

minYearToProcess = "1991"
maxYearToProcess = "2014"

# maxYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$maxYearToProcess), "2014",
#                                       swsContext.computationParams$maxYearToProcess))

# yearMinProcess <- min(referenceYear, minYearToProcess, maxYearToProcess)
# yearMaxProcess <- max(referenceYear, minYearToProcess, maxYearToProcess)

if(referenceYear < minYearToProcess | referenceYear > maxYearToProcess) 
    stop("The reference year must be set to between the minimum and the maximum year")

yearCodes <- minYearToProcess:maxYearToProcess
yearCodes <- as.character(yearCodes)

cat("Defining variables/dimensions/keys/...\n")

## Define the keys that we'll need for all the dimensions
## set the keys to get the population data from the FAO working system
#areaCodesM49 <- swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys

## #######

##' Obtain computation parameter, this parameter determines whether only
##' selected session should be validated or the complete production domain.
validationRange = swsContext.computationParams$validation_selection

if(CheckDebug()){
## validationRange <- "session"
validationRange <- "all"
}

##' Get session key and dataset configuration
sessionKey = swsContext.datasets[[1]]
## datasetConfig = GetDatasetConfig(domainCode = sessionKey@domain,
##                                  datasetCode = sessionKey@dataset)


##' Obtain the complete imputation Datakey
completeImputationKey = getCompleteImputationKey("food")
# completeImputationKey@dataset = "fooddatafs_"

##' Selected the key based on the input parameter
selectedKey =
    switch(validationRange,
           "session" = sessionKey, # if "validationRange" is "session": selectedKey <- sessionKey
           "all" = completeImputationKey) # if "validationRange" is "all": selectedKey <- completeImputationkey

areaCodesM49 <- selectedKey@dimensions$geographicAreaM49@keys
itemCodesCPC <- selectedKey@dimensions$measuredItemCPC@keys
# referenceYear <- as.numeric(sessionKey@dimensions$timePointYears@keys)

## ####

# country codes for GDP
# worldbankAreaCode = GetCodeList(domain = "WorldBank",
#                                 dataset = "worldbank_indicator",
#                                 dimension = "worldbankArea")[, code]
# 
# dimWorldbankArea <- Dimension(name = "worldbankArea", keys = worldbankAreaCode)

#areaCodesM49 <- GetCodeList("agriculture", "aproduction", "geographicAreaM49")[type == "country",code]

## We need different area codes for the SUA domain
# yearsForSD <- as.numeric(swsContext.computationParams$yearsForVar)
# yearsForSD <- as.numeric(swsContext.datasets[[1]]@dimensions$timePointYears@keys)
## We will need the year of imputation as well as previous years to compute the
## standard deviation.  We'll grab as many years as specified by the user.
# yearCodes <- as.numeric(swsContext.computationParams$yearToProcess) +
#     (-yearsForSD:0)
# yearCodes <- as.character(yearCodes)
# yearCodes <- swsContext.datasets[[1]]@dimensions$timePointYears@keys
## We need the previous year to compute change in GDP.  This allows us to
## calculate food in the new year.
# yearCodes <- as.numeric(swsContext.computationParams$yearToProcess) + 0:22
# yearCodes <- as.character(yearCodes)
## GDP per capita (constant 2500 US$) is under this key
# gdpCodes <- "NY.GDP.PCAP.KD"
## The element 21 contains the FBS population numbers
populationCodes <- "21"
## The element 141 contains the FBS food numbers
foodCodes <- "5141"

comCodes <- GetCodeList("food", "food_factors","foodCommodityM")$code
fdmCodes <- GetCodeList("food", "food_factors","foodFdm")$code
funCodes <- GetCodeList("food", "food_factors","foodFunction")$code
varCodes <- "y_e" ## Only need elasticities from the food domain table

## Define the dimensions
dimM49 <- Dimension(name = "geographicAreaM49", keys = areaCodesM49)
dimPop <- Dimension(name = "measuredElementPopulation", keys = populationCodes)
dimTime <- Dimension(name = "timePointYears", keys = yearCodes)
# dimGDP <- Dimension(name = "wbIndicator", keys = gdpCodes)
dimFood <- Dimension(name = "measuredElement", keys = foodCodes)


dimCom <- Dimension(name = "foodCommodityM", keys = comCodes)
dimFdm <- Dimension(name = "foodFdm", keys = fdmCodes)
dimFun <- Dimension(name = "foodFunction", keys = funCodes)
dimVar <- Dimension(name = "foodVariable", keys = varCodes)

# dimCPC <- Dimension(name = "measuredElement", keys = itemCodesCPC)


## Define the pivots.  We won't need this for all dimensions, so we'll only
## define the relevant ones.
pivotM49 <- Pivoting(code = "geographicAreaM49")
pivotPop <- Pivoting(code = "measuredElementPopulation")
pivotTime <- Pivoting(code = "timePointYears")
pivotGDP <- Pivoting(code = "wbIndicator")

## Define the keys

# dimTime@keys <- as.character(1991:2016)
keyPop <- DatasetKey(domain = "population", dataset = "population",
                     dimensions = list(dimM49, dimPop, dimTime))
# keyGDP <- DatasetKey(domain = "WorldBank", dataset = "worldbank_indicator",
#                      dimensions = list(dimWorldbankArea, dimGDP, dimTime))
keyFdm <- DatasetKey(domain = "food", dataset = "food_factors",
                     dimensions = list(dimM49, dimCom, dimFdm, dimFun, dimVar))

## Download all the datasets:
cat("Download all the datasets...\n")

## download the population data from the SWS.  Using the pivoting argument, we 
## can specify the column order.  Since we're only pulling one key for the 
## population dimension, it makes sense to use that as the last dimension with 
## normalized = FALSE.  Doing this makes the last column the population, and
## names it Value_measuredElementPopulation_21.  We'll just rename it to
## population.
popData <- GetData(keyPop, flags=FALSE, normalized = FALSE,
                   pivoting = c(pivotM49, pivotTime, pivotPop))
setnames(popData, "Value_measuredElementPopulation_21", "population")


timeSeriesPopData <- as.data.table(expand.grid(geographicAreaM49 = unique(popData$geographicAreaM49),
                                               timePointYears = as.character(minYearToProcess:maxYearToProcess)))

popData = merge(timeSeriesPopData, popData, by = c("geographicAreaM49", "timePointYears"), 
                all.x = T)

popData[, imputedPopulation := na.locf(population, fromLast = FALSE), 
        by = list(geographicAreaM49)]

popData[is.na(population), population := imputedPopulation]
popData[, c("imputedPopulation") := NULL]

# popData[, shiftPop := shift(population),
#         by = list(geographicAreaM49)]
# 
# popData[, percent := (population - shiftPop)/shiftPop,
#         by = list(geographicAreaM49)]
# 
# popData[, pop2016 := population * (1 + percent)]
# 
# pop2016 = as.data.table(expand.grid(geographicAreaM49 = unique(popData$geographicAreaM49),
#                                     timePointYears = as.character(2016)))
# 
# pop2016[, pop2015 := popData[timePointYears == 2015, population]]
# pop2016[, percent := popData[timePointYears == 2015, percent]]
# 
# pop2016[, population := pop2015 * (1 + percent)]
# pop2016[, c("pop2015", "percent") := NULL]
# 
# popData[, c("shiftPop", "percent", "pop2016") := NULL]
# 
# popData = rbind(popData, pop2016)
# popData[, geographicAreaM49 := as.character(geographicAreaM49)]
# popData[, timePointYears := as.character(timePointYears)]

## download the gdp data from the SWS.  We're again only pulling one wbIndicator
# gdpData <- GetData(keyGDP, flags=FALSE, normalized = T)
# gdpData[, geographicAreaM49 := as.character(countrycode(worldbankArea, "iso2c", "iso3n"))]
# # Sudan has the wrong name (it should be former Sudan)
# # gdpData[geographicAreaM49 == "736", CountryName := "Sudan (former)"]
# # China should be 1248
# gdpData[geographicAreaM49 == "156", geographicAreaM49 := "1248"]
# 
# setnames(gdpData, "Value", "GDP")
# gdpData[, c("worldbankArea", "wbIndicator") := NULL]
# setcolorder(gdpData, c("geographicAreaM49", "timePointYears", "GDP"))
# gdpData <- gdpData[!is.na(geographicAreaM49)]
# 
# timeSeriesGDP <- as.data.table(expand.grid(geographicAreaM49 = unique(gdpData$geographicAreaM49),
#                                            timePointYears = unique(gdpData$timePointYears)))
# 
# gdpData = merge(timeSeriesGDP, gdpData, by = c("geographicAreaM49", "timePointYears"),
#                 all.x = T)
# 
# gdpData[, imputedGDP := na.locf(GDP, fromLast = TRUE)]
# gdpData[is.na(GDP), GDP := imputedGDP]
# gdpData[, c("imputedGDP", "diff") := NULL]

## GDP comes from unsd (Marie had sent to us this file. In March it will be available also in SWS)
gdp = fread("sandbox/planB/gdp_forecast.csv", head = T)
# gdpData = gdp[, c("FAOName", "year", "GDPPC_USD2005"), with = F]
# gdpData = gdpData[year >= minYearToProcess & year <= maxYearToProcess]
# setnames(gdpData, "GDPPC_USD2005", "GDP")
# setnames(gdpData, "year", "timePointYears")
gdp[, geographicAreaM49 := as.character(countrycode(FAOName, "country.name", "iso3n"))]
gdp[, c("FAOName", "FAOCode") := NULL]
# Sudan has the wrong name (it should be former Sudan)
# gdpData[geographicAreaM49 == "736", FAOName := "Sudan (former)"]
gdp[geographicAreaM49 == "156", geographicAreaM49 := "1248"]
gdpData = melt.data.table(gdp, id.vars = "geographicAreaM49")
setnames(gdpData, "variable", "timePointYears")
setnames(gdpData, "value", "GDP")

gdpData = gdpData[!(timePointYears %in% c("1990", "2015", "2016"))]
gdpData[, timePointYears := as.character(timePointYears)]
gdpData[, GDP := as.numeric(GDP)]
gdpData = gdpData[!is.na(GDP)]
gdpData[, dupl := duplicated(timePointYears), 
        by = list(geographicAreaM49)]

gdpData = gdpData[dupl == FALSE]
gdpData[, dupl := NULL]

## download the food data from the SWS
# foodData <- getFoodData(timePointYears = yearCodes, areaCodesM49 = areaCodesM49,
# #                        commCodesCPC = swsContext.datasets[[1]]@dimensions$measuredItemCPC@keys)
#                          commCodesCPC = itemCodesCPC)

foodData <- GetData(completeImputationKey, flags = TRUE)
setnames(foodData, "Value", "food")
foodData[, type := getCommodityClassification(as.character(measuredItemCPC))]
foodData = foodData[type %in% c("Food Estimate", "Food Residual")]

keys = c("flagObservationStatus", "flagMethod")
foodData = merge(foodData, flagValidTable, by = keys, all.x = T)

## Pull official food data from agriculture/aproduction

completeImputationKey@domain <- "agriculture"
completeImputationKey@dataset <- "aproduction"
# completeImputationKey@dimensions$timePointYears@keys <- as.character(1991:2016)
foodDataAgrApr <- GetData(completeImputationKey, flags = TRUE)

keys = c("flagObservationStatus", "flagMethod")
foodDataAgrApr = merge(foodDataAgrApr, flagValidTable, by = keys, all.x = T)
foodDataAgrApr = foodDataAgrApr[Protected == TRUE]
foodDataAgrApr[, type := getCommodityClassification(as.character(measuredItemCPC))]
foodDataAgrApr = foodDataAgrApr[type %in% c("Food Estimate", "Food Residual")]
foodDataAgrApr[, combineFlag := paste(flagObservationStatus, flagMethod, sep = ";")]

## Merge both food data sets
keys = c("geographicAreaM49", "timePointYears", "measuredItemCPC")
foodDataMerge = merge(foodData, foodDataAgrApr[, c(keys, "Value", "combineFlag"), with = F],
      by = keys, all = T)

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
checkTotFood[timePointYears %in% yearAverage & totFood == 0, .N, geographicAreaM49]
excludeCountry = unique(checkTotFood[timePointYears %in% yearAverage & totFood == 0]$geographicAreaM49)
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

timeSeriesData[, type := getCommodityClassification(as.character(measuredItemCPC))]

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
        Dimension(name = "timePointYears", keys = as.character(2000:maxYearToProcess)),
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
                                                   as.character(minYearToProcess:1999))

totalTradeDataFaostat <- dcast.data.table(totalTradeDataFaostat, geographicAreaM49 + measuredItemCPC + 
                                              timePointYears ~ measuredElement, value.var = "Value")

setnames(totalTradeDataFaostat, "5610", "imports")
setnames(totalTradeDataFaostat, "5910", "exports")


## Make a rbind between both total trade data from sws and faostat
totalTradeData = rbind(totalTradeDataFaostat, totalTradeDataSWS)

totalTradeData[is.na(imports), imports := 0]
totalTradeData[is.na(exports), exports := 0]
totalTradeData[, netTrade := (imports - exports)]

# load("sandbox/planB/totalTradeDataForecasted.RData")
# 
# # Rbind
# totalTradeData = rbind(totalTradeData, totalTradeDataForecasted)

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

# load("sandbox/planB/productionDataForecasted.RData")
# 
# productionData = rbind(productionData, productionDataForecasted)
# productionData

## 
keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")
timeSeriesData = merge(timeSeriesData, productionData, by = keys,
                       all.x = T)
setnames(timeSeriesData, "Value", "production")

timeSeriesData[is.na(production), production := 0]
timeSeriesData[, netSupply := netTrade + production]


## Preparing the dataset for computing average for food and net supply (production, imports and exports) 

selectYearsTab = timeSeriesData[timePointYears %in% yearAverage]

# If the figure is Protected in 2012, we will not compute the food average for it 
selectYearsTab = 
    selectYearsTab[timePointYears == median(as.numeric(yearAverage)) & Protected == TRUE, 
                   protectedAux := 1, 
                   by = list(geographicAreaM49, measuredItemCPC)]

selectYearsTab[is.na(protectedAux), protectedAux := 0]
selectYearsTab[, aux := max(protectedAux, na.rm = T),
               by = list(geographicAreaM49, measuredItemCPC)]

selectYearsTabFoodEstimate = selectYearsTab[aux == 0 & type == "Food Estimate"]

## Computing food average
averageYearTab = selectYearsTabFoodEstimate[, list(
    foodAverage = mean(food, na.rm = T), 
    nObs = .N), 
    by = list(geographicAreaM49, measuredItemCPC)]

numbItem = length(unique(averageYearTab$measuredItemCPC))
tabCountryNan = averageYearTab[foodAverage == "NaN", .N, geographicAreaM49]
setnames(tabCountryNan, "N", "numbFoodNaN")

## Let's exclude the country that is NaN for all the commodities
averageYearTab = merge(averageYearTab, tabCountryNan, by = "geographicAreaM49", all.x = T)
averageYearTab = nameData("food", "fooddatafs", averageYearTab)
averageYearTab[numbFoodNaN == numbItem, .N, c("geographicAreaM49_description", "geographicAreaM49")]

averageYearTab = averageYearTab[numbFoodNaN < numbItem]
averageYearTab[, .N, geographicAreaM49]
averageYearTab[foodAverage > 0]
averageYearTab[, c("nObs", "numbFoodNaN", "geographicAreaM49_description", "measuredItemCPC_description") := NULL]
averageYearTab = averageYearTab[!foodAverage == "NaN"]
averageYearTab[, timePointYears := median(as.numeric(yearAverage))]
averageYearTab[, timePointYears := as.character(timePointYears)]
averageYearTab[, flagObservationStatus := "I"]
averageYearTab[, flagMethod := "e"]
averageYearTab[, Protected := FALSE]

## Food Residual -  compute netSupply
selectYearsTabFoodResidual = selectYearsTab[aux == 0 & type == "Food Residual"]

## Computing netSupply average
averageYearTabResidual = selectYearsTabFoodResidual[, list(
    netSupplyAverage = mean(netSupply, na.rm = T), 
    nObs = .N), 
    by = list(geographicAreaM49, measuredItemCPC)]

numbItem = length(unique(averageYearTabResidual$measuredItemCPC))
tabCountryNan = averageYearTabResidual[netSupplyAverage == "NaN", .N, geographicAreaM49]

averageYearTabResidual[, timePointYears := median(as.numeric(yearAverage))]
averageYearTabResidual[, timePointYears := as.character(timePointYears)]
averageYearTabResidual[, flagObservationStatus := "I"]
averageYearTabResidual[, flagMethod := "e"]
averageYearTabResidual[, Protected := FALSE]

## Food Residual -  compute Trade
selectYearsTabFoodResidual = selectYearsTab[aux == 0 & type == "Food Residual"]

## Computing netTrade average
averageYearTabResidualTrade = selectYearsTabFoodResidual[, list(
    netTradeAverage = mean(netTrade, na.rm = T), 
    nObs = .N), 
    by = list(geographicAreaM49, measuredItemCPC)]

numbItem = length(unique(averageYearTabResidualTrade$measuredItemCPC))
tabCountryNan = averageYearTabResidualTrade[netTradeAverage == "NaN", .N, geographicAreaM49]

averageYearTabResidualTrade[, timePointYears := median(as.numeric(yearAverage))]
averageYearTabResidualTrade[, timePointYears := as.character(timePointYears)]
averageYearTabResidualTrade[, flagObservationStatus := "I"]
averageYearTabResidualTrade[, flagMethod := "e"]
averageYearTabResidualTrade[, Protected := FALSE]

## Merge averageYearTab with timeSeriesData

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

timeSeriesData = merge(timeSeriesData, 
                       averageYearTab[, c("geographicAreaM49", "measuredItemCPC", "timePointYears", "foodAverage"), with =  F],
                       by = keys, all.x = T)

timeSeriesData[is.na(foodAverage), finalFood := food]
timeSeriesData[!is.na(foodAverage), finalFood := foodAverage]
timeSeriesData[timePointYears == 2005 & !is.na(foodAverage)]
timeSeriesData[timePointYears == 2005 & !is.na(finalFood)]
timeSeriesData[, c("food", "foodAverage") := NULL]
setnames(timeSeriesData, "finalFood", "food")

## Merge averageYearTabResidual with timeSeriesData

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

timeSeriesData = merge(
    timeSeriesData, 
    averageYearTabResidual[, c("geographicAreaM49", "measuredItemCPC", 
                               "timePointYears", "netSupplyAverage"), with =  F],
    by = keys, all.x = T)

timeSeriesData[is.na(netSupplyAverage), finalNetSupply := netSupply]
timeSeriesData[!is.na(netSupplyAverage), finalNetSupply := netSupplyAverage]
# timeSeriesData[timePointYears == 2005 & !is.na(netSupplyAverage)]
# timeSeriesData[timePointYears == 2005 & !is.na(finalNetSupply)]
timeSeriesData[, c("netSupply", "netSupplyAverage") := NULL]
setnames(timeSeriesData, "finalNetSupply", "netSupply")

## If the commodity is "Food Residual" and is not a protected figure, the amount of
## nettrade goes to food. But if nettrade is below zero, food is equal to zero.

timeSeriesData[Protected == FALSE & type == "Food Residual" & netSupply > 0, 
               food := netSupply]

timeSeriesData[Protected == FALSE & type == "Food Residual" & netSupply <= 0, 
               food := 0]

## Merge averageYearTabResidualTrade with timeSeriesData

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

timeSeriesData = merge(
    timeSeriesData, 
    averageYearTabResidualTrade[, c("geographicAreaM49", "measuredItemCPC", 
                                    "timePointYears", "netTradeAverage"), with =  F],
    by = keys, all.x = T)

timeSeriesData[is.na(netTradeAverage), finalNetTrade := netTrade]
timeSeriesData[!is.na(netTradeAverage), finalNetTrade := netTradeAverage]
# timeSeriesData[timePointYears == 2005 & !is.na(netSupplyAverage)]
# timeSeriesData[timePointYears == 2005 & !is.na(finalNetSupply)]
timeSeriesData[, c("netTrade", "netTradeAverage") := NULL]
setnames(timeSeriesData, "finalNetTrade", "netTrade")


## If the commodity is "Food Residual" and is not a protected figure, the amount of
## nettrade goes to foodNetTrade But if nettrade is below zero, foodNetTrade is equal to zero.

timeSeriesData[Protected == FALSE & type == "Food Residual" & netTrade > 0, 
               foodNetTrade := netTrade]

timeSeriesData[Protected == FALSE & type == "Food Residual" & netTrade <= 0, 
               foodNetTrade := 0]

timeSeriesData[type == "Food Estimate", 
               foodNetTrade := food]


##

funcCodes <- commodity2FunctionalForm(
    as.numeric(cpc2fcl(timeSeriesData$measuredItemCPC, returnFirst = TRUE)))
timeSeriesData <- cbind(timeSeriesData, do.call("cbind", funcCodes))
setkeyv(timeSeriesData, c("geographicAreaM49", "timePointYears"))

# foodData <- foodData[!is.na(foodDemand), ]
cat("Food data downloaded with", nrow(foodData), "rows.\n")

## download the food dimension data (elasticities) from the SWS
fdmData <- GetData(keyFdm, flags=FALSE, normalized = FALSE)
setnames(fdmData, "Value_foodVariable_y_e", "elasticity")
setnames(fdmData, "foodFdm", "foodDemand")
setnames(fdmData, "foodCommodityM", "foodCommodity")

# read map table from old code to new code
oldToNewCommodity = ReadDatatable("food_old_code_map")

fdmData <- merge(fdmData, oldToNewCommodity, all.x = T, allow.cartesian = T, 
                 by.x="foodCommodity", by.y = "old_code")
fdmData[is.na(new_code), new_code := foodCommodity]
fdmData <- fdmData[foodCommodity != "2500"]
fdmData[, c("foodCommodity") := NULL]
setnames(fdmData, old=c("new_code"), new=c("foodCommodity"))

fdmData <- fdmData[, list(elasticity = max(elasticity)), 
                   by=list(geographicAreaM49, foodDemand, foodFunction)]

# timeSeriesData[Protected == FALSE & type == "Food Residual" & netSupply > 0, 
#                food := netSupply]
# 
# timeSeriesData[Protected == FALSE & type == "Food Residual" & netSupply <= 0, 
#                food := 0]

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

countryGroup <- fread(system.file("extdata/class.csv", package = "faoswsStock"))
countryIncomeGroup <- countryGroup[GroupCode %in% c("HIC", "LIC", "UMC", "LMC"), ]
countryIncomeGroup[, geographicAreaM49 := as.character(countrycode(CountryCode, "wb", "iso3n"))]
# Sudan has the wrong name (it should be former Sudan)
countryIncomeGroup[geographicAreaM49 == "736", CountryName := "Sudan (former)"]
# China should be 1248
countryIncomeGroup[geographicAreaM49 == "156", geographicAreaM49 := "1248"]
#Exclude Channel Islands and Kosovo (not separately recognised by the UN)
countryIncomeGroup <- countryIncomeGroup[!is.na(geographicAreaM49)]
setnames(countryIncomeGroup, "GroupName", "incomeGroup")

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


# data <- data[!(is.na(GDP) | is.na(population) | is.na(elasticity))]

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
                                                 elas = updatedElast,
                                                 gdp = GDP,
                                                 netTrade = netSupply,
                                                 functionalForm = updatedFoodFunction,
                                                 timePointYears = as.numeric(timePointYears),
                                                 protected = Protected,
                                                 type = type, 
                                                 referenceYear = median(as.numeric(yearAverage))),
         by = list(geographicAreaM49, measuredItemCPC)]

    # data[, foodHatNetTrade := computeFoodForwardBackward(food = foodNetTrade,
    #                                              pop = population,
    #                                              elas = updatedElast,
    #                                              gdp = GDP,
    #                                              netTrade = netTrade,
    #                                              functionalForm = updatedFoodFunction,
    #                                              timePointYears = as.numeric(timePointYears),
    #                                              protected = Protected,
    #                                              type = type, 
    #                                              referenceYear = median(as.numeric(yearAverage))),
    #      by = list(geographicAreaM49, measuredItemCPC)]
    
    
        
    # In statistics, a forecast error is the difference between the actual or real
    # and the predicted or forecast value of a time series or any other phenomenon
    # of interest.
    # In simple cases, a forecast is compared with an outcome at a single
    # time-point and a summary of forecast errors is constructed over a collection
    # of such time-points. Here the forecast may be assessed using the difference
    # or using a proportional error.
    # By convention, the error is defined using the value of the outcome minus the
    # value of the forecast.
    data[, error := food - foodHat]
    
    # Geographic Area, measuredElement = 141, measuredItem = SUA item code, Dist
    # Param = log(Mu) or log(Sigma), Year = Year
    # dataToSave <- data_base[,
    #     ## Since we're ordered by time, foodHat[.N] will give the last estimate for
    #     ## the food element, and this is exactly what we want.
    #     list(mean = foodHat[.N],
    #          var = mean(error^2, na.rm = TRUE),
    #          timePointYears = max(timePointYears)),
    #     by = c("geographicAreaM49", "measuredElement", "measuredItemCPC")]
    # dataToSave <- data[Protected == FALSE & timePointYears != referenceYear, ]
    # dataToSave <- data[Protected == FALSE, ]
    
    dataToSave <- data[!is.na(foodHat)]
    # save(dataToSave, file = "sandbox/comparison_netTrade_netSupply/dataToSave.RData")
    # dataToSave[, "foodHatNetTrade" := NULL]
    
    ## To convert from mu/sigma to logmu/logsigma, we can use the method of moments
    ## estimators (which express the parameters of the log-normal distribution in
    ## terms of the moments).  The formula are:
    ## logmu = 2*log(E(X)) - 1/2*log(E(X^2))
    ## logsigma = log(E(X^2)) - 2*log(E(X))
    ## We can write E(X^2) = sigma^2 + E(X)^2, and so we can now easily express
    ## logmu and logsigma from the estimated expected value and variance.
    
    # dataToSave[, lmu := 2*log(mean)-1/2*log(var+mean^2)]
    # dataToSave[, lsi := log(var+mean^2) - 2*log(mean)]
    # ## Remove mean and var now that we've calculate lmu and lsi
    # dataToSave[, c("mean", "var") := NULL]
    # dataToSave = data.table:::melt.data.table(dataToSave,
    #     measure.vars = c("lmu", "lsi"))
    
    ## Prepare data and save it to SWS
    cat("Restructure and filter data to save to SWS...\n")
    setnames(dataToSave, "foodHat", "Value")
    dataToSave[, measuredElement := "5141"]
    dataToSave <- dataToSave[, c("geographicAreaM49", "measuredElement",
                                 "measuredItemCPC", "timePointYears", "Value", "flagObservationStatus", "flagMethod", "Protected"),
                             with = FALSE]

    keys = c("geographicAreaM49", "measuredElement",
             "measuredItemCPC", "timePointYears")
    
    # Remove official data from dataToSave.  foodData has the official data, just
    # need to remove missing and imputed.
    
    # By now there are no data for food in the agriculture/aproduction before 2011.
    # This table should be populate by the table in the old system (food code 141).
    # As the flagObservationStatus and flagMethod are only NA, we assume that these 
    # figures are not officials. Thus, we'll save all the data computed for 2011.

    # foodData = foodData[!flagObservationStatus %in% overwritableFlags &
    #                     !is.na(flagObservationStatus), ]
    # dataToSave = merge(dataToSave, foodData[, c(keys, "food"), with = FALSE],
    #           all.x = TRUE, by = keys)
    # dataToSave = dataToSave[is.na(food), ]
    # dataToSave[, food := NULL]
    
    dataToSave[Protected == FALSE, flagObservationStatus := "I"]
    dataToSave[Protected == FALSE, flagMethod := "e"]
    dataToSave <- dataToSave[, "Protected" := NULL]
    
    setcolorder(dataToSave,
                c("timePointYears", "geographicAreaM49", "measuredItemCPC",
                  "measuredElement", "Value", "flagObservationStatus", "flagMethod"))
    
    cat("Save the final data...\n")
    
    stats = SaveData(domain = "food", dataset = "fooddata", data = dataToSave)
}

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")
