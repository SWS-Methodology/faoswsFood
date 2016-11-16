suppressMessages({
    library(data.table)
    library(faosws)
    library(dplyr)
    library(faoswsUtil)
    library(faoswsFood)
    library(faoswsFlag)
    library(countrycode)
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
    files = dir("~/Github/faoswsFood/R", full.names = TRUE)
    sapply(files, source)
    
}

# if(swsContext.computationParams$yearToProcess < 1991)
#     stop("This module was designed for imputation on years after 1990 only!")
referenceYear <- as.numeric(ifelse(is.null(swsContext.computationParams$referenceYear), "1998",
                                   swsContext.computationParams$referenceYear))

minYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$minYearToProcess), "1991",
                                   swsContext.computationParams$minYearToProcess))

maxYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$maxYearToProcess), "2013",
                                      swsContext.computationParams$maxYearToProcess))

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
worldbankAreaCode = GetCodeList(domain = "WorldBank",
                                dataset = "worldbank_indicator",
                                dimension = "worldbankArea")[, code]

dimWorldbankArea <- Dimension(name = "worldbankArea", keys = worldbankAreaCode)

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
gdpCodes <- "NY.GDP.PCAP.KD"
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
dimGDP <- Dimension(name = "wbIndicator", keys = gdpCodes)
dimFood <- Dimension(name = "measuredElement", keys = foodCodes)


dimCom <- Dimension(name = "foodCommodityM", keys = comCodes)
dimFdm <- Dimension(name = "foodFdm", keys = fdmCodes)
dimFun <- Dimension(name = "foodFunction", keys = funCodes)
dimVar <- Dimension(name = "foodVariable", keys = varCodes)

dimCPC <- Dimension(name = "measuredElement", keys = itemCodesCPC)


## Define the pivots.  We won't need this for all dimensions, so we'll only
## define the relevant ones.
pivotM49 <- Pivoting(code = "geographicAreaM49")
pivotPop <- Pivoting(code = "measuredElementPopulation")
pivotTime <- Pivoting(code = "timePointYears")
pivotGDP <- Pivoting(code = "wbIndicator")

## Define the keys
keyPop <- DatasetKey(domain = "population", dataset = "population",
                     dimensions = list(dimM49, dimPop, dimTime))
keyGDP <- DatasetKey(domain = "WorldBank", dataset = "worldbank_indicator",
                     dimensions = list(dimWorldbankArea, dimGDP, dimTime))
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

## download the gdp data from the SWS.  We're again only pulling one wbIndicator
gdpData <- GetData(keyGDP, flags=FALSE, normalized = T)
gdpData[, geographicAreaM49 := as.character(countrycode(worldbankArea, "iso2c", "iso3n"))]
# Sudan has the wrong name (it should be former Sudan)
# gdpData[geographicAreaM49 == "736", CountryName := "Sudan (former)"]
# China should be 1248
gdpData[geographicAreaM49 == "156", geographicAreaM49 := "1248"]

setnames(gdpData, "Value", "GDP")
gdpData[, c("worldbankArea", "wbIndicator") := NULL]
setcolorder(gdpData, c("geographicAreaM49", "timePointYears", "GDP"))
gdpData <- gdpData[!is.na(geographicAreaM49)]

## download the food data from the SWS
# foodData <- getFoodData(timePointYears = yearCodes, areaCodesM49 = areaCodesM49,
# #                        commCodesCPC = swsContext.datasets[[1]]@dimensions$measuredItemCPC@keys)
#                          commCodesCPC = itemCodesCPC)
foodData <- GetData(completeImputationKey, flags = TRUE)
setnames(foodData, "Value", "food")

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
timeSeriesData <- as.data.table(expand.grid(timePointYears = yearCodes,
                             geographicAreaM49 = areaCodesM49,
                             measuredItemCPC = itemCodesCPC))

timeSeriesData[, type := getCommodityClassification(as.character(measuredItemCPC))]
timeSeriesData = timeSeriesData[type %in% c("Food Estimate", "Food Residual")]


timeSeriesData <- merge(timeSeriesData, foodData, all.x = T,
                        by = c("geographicAreaM49", "timePointYears", "measuredItemCPC"))

timeSeriesData[, measuredElement := "5141"]

keys = c("flagObservationStatus", "flagMethod")
timeSeriesData <- merge(timeSeriesData, flagValidTable, all.x = T, by = keys)
timeSeriesData[, .N, Protected]
timeSeriesData[is.na(Protected), Protected := FALSE]

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

## Trade

totalTradeData <- getTotalTradeDataFAOSTAT1(areaCodesM49, itemCodesCPC, yearCodes)

totalTradeData <- dcast.data.table(totalTradeData, geographicAreaM49 + measuredItemCPC + 
                     timePointYears ~ measuredElement, value.var = "Value")

setnames(totalTradeData, "5610", "imports")
setnames(totalTradeData, "5910", "exports")

totalTradeData[is.na(imports), imports := 0]
totalTradeData[is.na(exports), exports := 0]
totalTradeData[, netTrade := (imports - exports)]

## Merge the datasets together, and perform some processing.

cat("Merge population with GDP...\n")
## merge the current population and gross domestic product data into a single
## dataset
data <- merge(popData, gdpData, all = TRUE,
              by = c("geographicAreaM49", "timePointYears"))

cat("Merge in food data...\n")
data = merge(timeSeriesData, data, all.x = TRUE,
             by = c("geographicAreaM49", "timePointYears"))

cat("Merge in food demand model data...\n")
data <- merge(data, fdmData,
              by = c("foodDemand", "geographicAreaM49"),
              all.x = TRUE)

## Let's merge data and totalTradeData
keys <- c("geographicAreaM49", "timePointYears", "measuredItemCPC")
data <- merge(data, totalTradeData[, c("geographicAreaM49", "timePointYears",
                                       "measuredItemCPC", "netTrade"), with = F],
              by = keys, all.x = T)
data[is.na(netTrade), netTrade := 0]

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
    data[, foodFunction := ifelse(foodFunction == 4, 3, foodFunction)]
    data[, foodHat := computeFoodForwardBackward(food = food,
                                                 pop = population,
                                                 elas = elasticity,
                                                 gdp = GDP,
                                                 netTrade = netTrade,
                                                 functionalForm = foodFunction,
                                                 timePointYears = as.numeric(timePointYears),
                                                 protected = Protected,
                                                 type = type, 
                                                 referenceYear = referenceYear),
         by = list(geographicAreaM49, measuredItemCPC)]
    
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
    dataToSave <- data[Protected == FALSE & timePointYears != referenceYear, ]
    
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
                                 "measuredItemCPC", "timePointYears", "Value"),
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
    
    dataToSave[, flagObservationStatus := "I"]
    dataToSave[, flagMethod := "e"]
    dataToSave <- dataToSave[!is.na(Value), ]
    
    setcolorder(dataToSave,
                c("timePointYears", "geographicAreaM49", "measuredItemCPC",
                  "measuredElement", "Value", "flagObservationStatus", "flagMethod"))
    
    cat("Save the final data...\n")
    
    stats = SaveData(domain = "agriculture", dataset = "aproduction", data = dataToSave)
}

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")
