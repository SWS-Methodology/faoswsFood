suppressMessages({
    library(data.table)
    library(faosws)
    library(dplyr)
    library(faoswsUtil)
    library(faoswsFood)
    library(faoswsFlag)
    #library(countrycode)
    library(zoo)
})

## Set up for the test environment and parameters
R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE <- Sys.getenv("R_DEBUG_MODE")

# This return FALSE if on the Statistical Working System
if (CheckDebug()) {

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
    files <- dir("R", full.names = TRUE)
    sapply(files, source)

} else {
    R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
    options(error = function(){
        dump.frames()
        save(last.dump, file = "/work/SWS_R_Share/caetano/last.dump.RData")
    })
}

## Use old trade data up to
# endYearOldTrade = 2013

minReferenceYear <- as.numeric(ifelse(is.null(swsContext.computationParams$minReferenceYear), "2011",
                                      swsContext.computationParams$minReferenceYear))

maxReferenceYear <- as.numeric(ifelse(is.null(swsContext.computationParams$maxReferenceYear), "2013",
                                      swsContext.computationParams$maxReferenceYear))

# referenceYear <- round(median(as.numeric(referenceYearRange)))
if (minReferenceYear > maxReferenceYear | maxReferenceYear < minReferenceYear)
    stop("Please check the time range for the reference years")

referenceYearRange <- as.character(minReferenceYear:maxReferenceYear)

# Parameter: year to process

minYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$minYearToProcess), "1990",
                                      swsContext.computationParams$minYearToProcess))

maxYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$maxYearToProcess), "2017",
                                      swsContext.computationParams$maxYearToProcess))

if (minYearToProcess > maxYearToProcess | maxYearToProcess < minYearToProcess)
    stop("Please check the time range for the years to be processed")

referenceYear <- floor(median(minYearToProcess:maxYearToProcess))

yearCodes <- as.character(minYearToProcess:maxYearToProcess)

cat("Defining variables/dimensions/keys/...\n")

## Define the keys that we'll need for all the dimensions

##' Obtain computation parameter, this parameter determines whether only
##' selected session should be validated or the complete production domain.
validationRange <- swsContext.computationParams$validation_selection

if (CheckDebug()) {
    ## validationRange <- "session"
    validationRange <- "all"
}

##' Get session key and dataset configuration
sessionKey <- swsContext.datasets[[1]]

##' Obtain the complete imputation Datakey
completeImputationKey <- getCompleteImputationKey("food")

# Use countries and cpcs from agriculture aproduction
codesM49 <- GetCodeList('agriculture', 'aproduction', 'geographicAreaM49')[type == 'country', code]
cpcItems <- GetCodeList("suafbs", "sua_validated_2015", "measuredItemFbsSua")[, code]

completeImputationKey@dimensions$geographicAreaM49@keys <- codesM49
completeImputationKey@dimensions$measuredItemCPC@keys <- cpcItems
completeImputationKey@dimensions$timePointYears@keys <- yearCodes

##' Selected the key based on the input parameter
selectedKey <-
    switch(validationRange,
           "session" = sessionKey, # if "validationRange" is "session": selectedKey <- sessionKey
           "all" = completeImputationKey) # if "validationRange" is "all": selectedKey <- completeImputationkey

areaCodesM49 <- selectedKey@dimensions$geographicAreaM49@keys
itemCodesCPC <- selectedKey@dimensions$measuredItemCPC@keys

# Exclude those codes
areaCodesM49 <- areaCodesM49[!(areaCodesM49 %in% c("831", "832"))]

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
dimTime <- Dimension(name = "timePointYears", keys = yearCodes)
dimCom <- Dimension(name = "foodCommodityM", keys = comCodes)
dimFdm <- Dimension(name = "foodFdm", keys = fdmCodes)
dimFun <- Dimension(name = "foodFunction", keys = funCodes)
dimVar <- Dimension(name = "foodVariable", keys = varCodes)
# Curently not used
#dimElementGDP <- Dimension(name = "dim_element_fao_macro_ind", keys = gdpElementCode)
#dimItemGDP <- Dimension(name = "dim_item_fao_macro_ind", keys = gdpItemCode)
#dimFood <- Dimension(name = "measuredElement", keys = foodCodes)

# Currently not used
#pivotGDP <- Pivoting(code = "wbIndicator")

############################# Define the keys ######################

# Keys for population
keyPop <- DatasetKey(domain = "population", dataset = "population_unpd",
                     dimensions = list(dimM49, dimPop, dimTime))

############# Currently not used
#keyGDP <- DatasetKey(domain = "faostat_datasets", dataset = "faostat_macro_ind",
#                     dimensions = list(dimFaoCode, dimElementGDP, dimItemGDP, dimTime))

# Keys for food parameters
keyFdm <- DatasetKey(domain = "food", dataset = "food_factors",
                     dimensions = list(dimM49, dimCom, dimFdm, dimFun, dimVar))

# Key for food
foodKey <- DatasetKey(
    domain = "suafbs",
    dataset = "sua_validated_2015",
    dimensions = list(
        Dimension(name = "geographicAreaM49", keys = areaCodesM49),
        Dimension(name = "measuredElementSuaFbs", keys = '5141'),
        Dimension(name = "measuredItemFbsSua", keys = itemCodesCPC),
        Dimension(name = "timePointYears", keys = yearCodes)
    )
)

# Key for trade
tradeCode <- c("5610", "5910")
totalTradeKey <- DatasetKey(
    domain = "trade",
    dataset = "total_trade_cpc_m49",
    dimensions = list(
        Dimension(name = "geographicAreaM49", keys = areaCodesM49),
        Dimension(name = "measuredElementTrade", keys = tradeCode),
        Dimension(name = "timePointYears", keys = yearCodes),
        Dimension(name = "measuredItemCPC",
                  keys = intersect(itemCodesCPC,
                                   GetCodeList("trade", "total_trade_cpc_m49", "measuredItemCPC")$code))
    )
)

# Keys for production
productionKey <- DatasetKey(
    domain = "agriculture",
    dataset = "aproduction",
    dimensions = list(
        Dimension(name = "geographicAreaM49", keys = areaCodesM49),
        Dimension(name = "measuredElement", keys = "5510"),
        Dimension(name = "timePointYears", keys = yearCodes),
        Dimension(name = "measuredItemCPC",
                  keys = intersect(itemCodesCPC,
                                   GetCodeList("agriculture", "aproduction", "measuredItemCPC")$code))
    )
)

############################# Get data ######################

# Population

popData <- GetData(keyPop, flags = FALSE)

stopifnot(nrow(popData) > 0)


# GDP

# XXX: should use dataset/datatable

gdp <- read.csv(paste0(R_SWS_SHARE_PATH, "/wanner/gdp/","GDP.csv"))
gdp <- as.data.table(gdp)
#gdp[, geographicAreaM49 := as.character(countrycode(Country.Code, "iso3c", "iso3n"))]
gdp[geographicAreaM49 == "156", geographicAreaM49 := "1248"]

gdp <- dplyr::filter(gdp,!is.na(geographicAreaM49))
#gdp <- dplyr::select(gdp,-Country.Name,-Country.Code,-Indicator.Name,-Indicator.Code)
gdp <- as.data.table(gdp)
gdpData <- melt.data.table(gdp, id.vars = "geographicAreaM49")
setnames(gdpData, "variable", "timePointYears")
setnames(gdpData, "value", "GDP")
gdpData <- as.data.frame(gdpData)
gdpData$timePointYears = substr(gdpData$timePointYears,2,5)
gdpData <- as.data.table(gdpData)

# There are no data for Taiwan. So we get this table from Taiwan website.
gdp_taiwan <- ReadDatatable("gdp_taiwan_2005_prices")

gdp_taiwan[, time_point_years := as.numeric(time_point_years)]

setnames(gdp_taiwan, old = c("geographic_area_m49", "time_point_years", "gdp"),
         new = c("geographicAreaM49", "timePointYears", "GDP"))

gdp_taiwan <- gdp_taiwan[timePointYears >= minYearToProcess & timePointYears <= maxYearToProcess]
gdp_taiwan[, geographicAreaM49 := as.character(geographicAreaM49)]
gdp_taiwan[, timePointYears := as.character(timePointYears)]

gdpData <- rbind(gdpData, gdp_taiwan)

gdpData[geographicAreaM49 == "156", geographicAreaM49 := "1248"]

stopifnot(nrow(gdpData) > 0)


# Food before 2000

# The sua_validated_2015 starts from 2000. In order to get the data from 1990-1999,
# we will keep pulling data from updated_sua_2013_data. If in future we not need the
# data before 2000, just ignore this piece of code.

foodDataUpTo1999 <- getFoodDataFAOSTAT1(areaCodesM49,
                                itemCodesCPC,
                                yearRange = as.character(1990:1999),
                                "updated_sua_2013_data")

stopifnot(nrow(foodDataUpTo1999) > 0)


# Food data from 2000

foodDataFrom2000 <- GetData(foodKey, flags = TRUE)

stopifnot(nrow(foodDataFrom2000) > 0)


# Trade data

totalTradeData <- GetData(totalTradeKey, flags = FALSE)

stopifnot(nrow(totalTradeData) > 0)


# Production

productionData <- GetData(productionKey, flags = TRUE)

stopifnot(nrow(productionData) > 0)


# Elasticities & co.

fdmData <- GetData(keyFdm, flags = FALSE, normalized = FALSE)

stopifnot(nrow(fdmData) > 0)


# Read map table from old code to new code
oldToNewCommodity <- ReadDatatable("food_old_code_map")

stopifnot(nrow(oldToNewCommodity) > 0)


## Country income group
countryIncomeGroup <- ReadDatatable("country_income_group")

stopifnot(nrow(countryIncomeGroup) > 0)


# Read the food_classification table
food_classification_country_specific <- ReadDatatable("food_classification_country_specific")

stopifnot(nrow(food_classification_country_specific) > 0)


############### Set names


setnames(popData, "Value", "population")

setnames(foodDataFrom2000, old = c("measuredElementSuaFbs", "measuredItemFbsSua", "Value"),
         new = c("measuredElement", "measuredItemCPC", "food"))

setnames(foodDataUpTo1999, old = "Value", new = "food")

setnames(food_classification_country_specific,
         old = c("geographic_area_m49", "measured_item_cpc", "food_classification"),
         new = c("geographicAreaM49", "measuredItemCPC", "type"))




timeSeriesPopData <- as.data.table(expand.grid(geographicAreaM49 = unique(popData$geographicAreaM49),
                                               timePointYears = as.character(minYearToProcess:maxYearToProcess)))

popData <- merge(timeSeriesPopData, popData, by = c("geographicAreaM49", "timePointYears"), all.x = TRUE)

popData[geographicAreaM49 == "156", geographicAreaM49 := "1248"]




setcolorder(foodDataUpTo1999, c("geographicAreaM49", "measuredElement", "measuredItemCPC",
                                "food", "timePointYears", "flagObservationStatus", "flagMethod"))

# Bind the two food data sets
foodData <- rbind(foodDataUpTo1999, foodDataFrom2000)


foodData <- merge(
    foodData,
    food_classification_country_specific,
    by = c("geographicAreaM49", "measuredItemCPC"),
    all.x = TRUE)

foodData <- foodData[type %in% c("Food Estimate", "Food Residual")]

keys <- c("flagObservationStatus", "flagMethod")

foodDataMerge <- merge(foodData, flagValidTable, by = keys, all.x = TRUE)

# Discussed in a meeting: change from M- to Mu
foodDataMerge[flagObservationStatus == "M" & flagMethod == "-", flagMethod := "u"]

## Checking countries with zero food figures
checkTotFood <- foodDataMerge[, list(totFood = sum(food)),
                             by = list(geographicAreaM49, timePointYears)]

# checkTotFood <- nameData("food", "fooddatafs", checkTotFood)
# checkTotFood[totFood == 0, .N, c("geographicAreaM49", "geographicAreaM49_description")]
# checkTotFood[timePointYears %in% referenceYearRange & totFood == 0, .N, geographicAreaM49]

excludeCountry <- unique(checkTotFood[timePointYears %in% referenceYearRange & totFood == 0]$geographicAreaM49)

foodDataMerge <- foodDataMerge[!(geographicAreaM49 %in% excludeCountry)]
# foodDataMerge[, c("type", "Valid", "Protected") := NULL]

## Creating time series data set
timeSeriesData <- as.data.table(expand.grid(timePointYears = as.character(minYearToProcess:maxYearToProcess),
                                            geographicAreaM49 = unique(foodDataMerge$geographicAreaM49),
                                            measuredItemCPC = unique(foodDataMerge$measuredItemCPC)))

timeSeriesData <-
  merge(
    timeSeriesData,
    food_classification_country_specific,
    by = c("geographicAreaM49", "measuredItemCPC"),
    all.x = TRUE
  )

timeSeriesData <-
  merge(
    timeSeriesData,
    foodDataMerge,
    all.x = TRUE,
    by = c("geographicAreaM49", "timePointYears", "measuredItemCPC", "type")
  )

timeSeriesData[, measuredElement := "5141"]

keys <- c("flagObservationStatus", "flagMethod")

timeSeriesData[is.na(Protected), Protected := FALSE]

## Trade


totalTradeData <-
  dcast.data.table(
    totalTradeData,
    geographicAreaM49 + measuredItemCPC + timePointYears ~ measuredElementTrade,
    value.var = "Value",
    fill = 0
  )

setnames(totalTradeData, c("5610", "5910"), c("imports", "exports"))

totalTradeData[, netTrade := (imports - exports)]

## Let's merge timeseries and totalTradeData
keys <- c("geographicAreaM49", "timePointYears", "measuredItemCPC")
timeSeriesData <- merge(timeSeriesData, totalTradeData, by = keys, all.x = TRUE)
timeSeriesData[is.na(netTrade), netTrade := 0]
timeSeriesData[is.na(imports), imports := 0]
timeSeriesData[is.na(exports), exports := 0]

## Production


productionData[, c("measuredElement", "flagObservationStatus", "flagMethod") := NULL]

keys <- c("geographicAreaM49", "measuredItemCPC", "timePointYears")

timeSeriesData <-
  merge(
    timeSeriesData,
    productionData,
    by = keys,
    all.x = TRUE
  )

setnames(timeSeriesData, "Value", "production")

timeSeriesData[is.na(production), production := 0]
timeSeriesData[, netSupply := netTrade + production]

## Preparing the dataset for computing average for food and net supply (production, imports and exports)
selectYearsTab <- timeSeriesData[timePointYears %in% referenceYearRange]

# If the figure is Protected in the reference year, we will not compute the food average for it

selectYearsTab <-
  selectYearsTab[,
    protectedAux := ifelse(timePointYears == median(as.numeric(referenceYearRange)) & Protected == TRUE, 1, 0),
    by = list(geographicAreaM49, measuredItemCPC)
  ]

selectYearsTab[is.na(protectedAux), protectedAux := 0]

selectYearsTab[, aux := max(protectedAux, na.rm = TRUE), by = list(geographicAreaM49, measuredItemCPC)]

selectYearsTabFoodEstimate <- selectYearsTab[aux == 0 & type == "Food Estimate"]

## Computing food average
averageYearTab <-
  selectYearsTabFoodEstimate[
    food > 0,
    .(foodAverage = mean(food, na.rm = TRUE)),
    by = list(geographicAreaM49, measuredItemCPC)
  ][,
    `:=`(
      timePointYears = as.character(referenceYear),
      flagObservationStatus = "I",
      flagMethod = "i",
      Protected = FALSE
    )
  ]

## Food Residual -  compute netSupply
selectYearsTabFoodResidual <- selectYearsTab[aux == 0 & type == "Food Residual"]

## Computing netSupply average
averageYearTabResidual <-
  selectYearsTabFoodResidual[
    netSupply > 0,
    .(netSupplyAverage = mean(netSupply, na.rm = TRUE)),
    by = list(geographicAreaM49, measuredItemCPC)
  ][,
    `:=`(
      timePointYears = as.character(referenceYear),
      flagObservationStatus = "I",
      flagMethod = "i",
      Protected = FALSE
    )
  ]

## Merge averageYearTab with timeSeriesData
keys <- c("geographicAreaM49", "measuredItemCPC", "timePointYears")

timeSeriesData <-
  merge(
    timeSeriesData,
    averageYearTab[, c("geographicAreaM49", "measuredItemCPC", "timePointYears",
                       "foodAverage", "flagObservationStatus", "flagMethod"), with =  FALSE],
    by = keys,
    all.x = TRUE
  )

timeSeriesData[, finalFood := ifelse(is.na(foodAverage), food, foodAverage)]

timeSeriesData[!is.na(flagObservationStatus.y), flagObservationStatus.x := flagObservationStatus.y]
timeSeriesData[!is.na(flagMethod.y), flagMethod.x := flagMethod.y]

timeSeriesData[, c("food", "foodAverage", "flagObservationStatus.y", "flagMethod.y") := NULL]

setnames(timeSeriesData, old = c("finalFood", "flagObservationStatus.x", "flagMethod.x"),
         new = c("food", "flagObservationStatus", "flagMethod"))

## Merge averageYearTabResidual with timeSeriesData
keys <- c("geographicAreaM49", "measuredItemCPC", "timePointYears")

timeSeriesData <-
  merge(
    timeSeriesData,
    averageYearTabResidual[, c("geographicAreaM49", "measuredItemCPC",
                               "timePointYears", "netSupplyAverage",
                               "flagObservationStatus", "flagMethod"), with =  FALSE],
    by = keys,
    all.x = TRUE
  )

timeSeriesData[, finalNetSupply := ifelse(is.na(netSupplyAverage), netSupply, netSupplyAverage)]

timeSeriesData[!is.na(flagObservationStatus.y), flagObservationStatus.x := flagObservationStatus.y]
timeSeriesData[!is.na(flagMethod.y), flagMethod.x := flagMethod.y]

timeSeriesData[, c("netSupply", "netSupplyAverage", "flagObservationStatus.y", "flagMethod.y") := NULL]

setnames(timeSeriesData, old = c("finalNetSupply", "flagObservationStatus.x", "flagMethod.x"),
         new = c("netSupply", "flagObservationStatus", "flagMethod"))

## If the commodity is "Food Residual" and is not a protected figure, the amount of
## nettrade goes to food. But if nettrade is below zero, food is equal to zero.

timeSeriesData[
  Protected == FALSE & type == "Food Residual",
  food := ifelse(netSupply > 0, netSupply, 0)
]

## Get initial food data for the commodities classified as a "Food Estimate" that don't
## have an initial food value in the reference year

countryCommodityZeroReferenceYear <-
  timeSeriesData[
    food %in% c(0, NA) & timePointYears == referenceYear & type == "Food Estimate",
    .N,
    c("geographicAreaM49", "measuredItemCPC")
  ]

initialFoodData <- getInitialFoodValue(
    country = countryCommodityZeroReferenceYear$geographicAreaM49,
    commodity = countryCommodityZeroReferenceYear$measuredItemCPC,
    referenceYear = referenceYear,
    data = timeSeriesData
)

initialFoodData[, flagInitialFood := 1]

# Exclude the commodities that are Estimate but the time series for food is always zero.
initialFoodData <- initialFoodData[source == "food" & timePointYears >= referenceYear]

initialFoodData[, timePointYears := as.character(timePointYears)]
initialFoodData[, nrows := NULL]

keys <- c("geographicAreaM49", "measuredItemCPC", "timePointYears")

timeSeriesData <-
  merge(
    timeSeriesData,
    initialFoodData[, c(keys, "flagInitialFood"), with = FALSE],
    by = keys,
    all.x = TRUE
  )

# Workaround
timeSeriesData[!is.na(flagInitialFood), Protected := TRUE]

# Food Commodity
funcCodes <- commodity2FunctionalForm(
    as.numeric(cpc2fcl(timeSeriesData$measuredItemCPC, returnFirst = TRUE)))

timeSeriesData <- cbind(timeSeriesData, do.call("cbind", funcCodes))

setkeyv(timeSeriesData, c("geographicAreaM49", "timePointYears"))


# Elasticity

setnames(fdmData, old = c("Value_foodVariable_y_e", "foodFdm", "foodCommodityM"),
         new = c("elasticity", "foodDemand", "foodCommodity"))


fdmData <-
  merge(
    fdmData,
    oldToNewCommodity,
    all.x = TRUE,
    allow.cartesian = TRUE,
    by.x = "foodCommodity",
    by.y = "old_code"
  )

fdmData[is.na(new_code), new_code := foodCommodity]
fdmData <- fdmData[foodCommodity != "2500"]
fdmData[, c("foodCommodity") := NULL]

setnames(fdmData, old=c("new_code"), new=c("foodCommodity"))

fdmData <-
  fdmData[,
    .(elasticity = max(elasticity)),
    by = list(geographicAreaM49, foodDemand, foodFunction)
  ]

## Merge the datasets together, and perform some processing.
data <- merge(popData, gdpData, all = TRUE, by = c("geographicAreaM49", "timePointYears"))

data <- data[!is.na(geographicAreaM49)]

cat("Merge in food data...\n")
data <- merge(timeSeriesData, data, all.x = TRUE,
             by = c("geographicAreaM49", "timePointYears"))

cat("Merge in food demand model data...\n")
data <- merge(data, fdmData,
              by = c("foodDemand", "geographicAreaM49"),
              all.x = TRUE)

setnames(countryIncomeGroup,
         old = c("geographic_area_m49", "country_name", "country_code", "group_code", "income_group"),
         new = c("geographicAreaM49", "CountryName", "CountryCode", "GroupCode", "incomeGroup"))

## We need to fill the gaps of the elasticity for the combination country/commodity

key <- "geographicAreaM49"
data <- merge(data, countryIncomeGroup[, c("geographicAreaM49", "incomeGroup"), with = FALSE],
             by = key, all.x = TRUE)

## Take the elasticity average for each combination commodity/income group
data[, foodFunctionAux := as.numeric(foodFunction)]

elastAverage <-
  data[,
    list(
      elasticityAverage = mean(elasticity, na.rm = TRUE),
      foodFunctionAux = round(mean(foodFunctionAux, na.rm = TRUE))
    ),
    by = list(measuredItemCPC, incomeGroup)
  ][
    !is.na(elasticityAverage)
  ][,
    foodFunctionAux := as.character(foodFunctionAux)
  ]

data[, "foodFunctionAux" := NULL]

## Merge elastAverage with data
keys <- c("measuredItemCPC", "incomeGroup")
data <- merge(data, elastAverage, by = keys, all.x = TRUE)

## If elasticity is NA, we take the figure from elasticityAverage

data[, updatedElast := ifelse(is.na(elasticity), elasticityAverage, eleasticity)]

data[, updatedFoodFunction := ifelse(is.na(foodFunction), foodFunctionAux, foodFunction)]

## Analysing elasticity

tabSD <-
  data[
    timePointYears == referenceYear,
    list(
      minUpdatedElast = min(updatedElast, na.rm = TRUE),
      averageUpdatedElast = mean(updatedElast, na.rm = TRUE),
      sdUpdatedElast = sd(updatedElast, na.rm = TRUE)
    ),
    by = list(incomeGroup, measuredItemCPC)
  ]

tabSD[, lowerTreshold := averageUpdatedElast - 2 * sdUpdatedElast]
tabSD[, upperTreshold := averageUpdatedElast + 2 * sdUpdatedElast]

tabSD[is.na(lowerTreshold), lowerTreshold := averageUpdatedElast]
tabSD[is.na(upperTreshold), upperTreshold := averageUpdatedElast]

data <- merge(data, tabSD[, c("incomeGroup", "measuredItemCPC", "lowerTreshold",
                              "upperTreshold", "averageUpdatedElast"), with = FALSE],
              by = c("incomeGroup", "measuredItemCPC"))

# If the condition is TRUE, it's an outlier
data[,
  newElasticity :=
    ifelse(
      updatedElast > upperTreshold | updatedElast < lowerTreshold,
      averageUpdatedElast,
      updatedElast
    )
]

# The country/commodity that has no food classification will be classified as
# "Food Estimate".
data[is.na(type), type := "Food Estimate"]

if (nrow(data) == 0){
    warning("data has no rows, so the module is ending...  Are you sure there ",
            "are observations for food for these commodities?")
    stats <- list(inserted = 0, ignored = 0, discarded = 0)
} else {

    ## First, sort the data by area and time.  The time sorting is important as we
    ## will later assume row i+1 is one time step past row i.
    setkeyv(data, c("geographicAreaM49", "timePointYears"))

    cat("Estimate food using the food model...\n")
    ## The funcional form 4 (originally presented in Josef's data) was replaced by
    ## functional form 3. The functional form 32 is a typo. It was replaced by
    ## functional form 2 in Food Factors database.
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

    keys <- c("geographicAreaM49", "measuredElement",
             "measuredItemCPC", "timePointYears")

    dataToSave <- dataToSave[, "Protected" := NULL]

    setcolorder(dataToSave,
                c("timePointYears", "geographicAreaM49", "measuredItemCPC",
                  "measuredElement", "Value", "flagObservationStatus", "flagMethod"))

    cat("Save the final data...\n")

    stats <- SaveData(domain = "food", dataset = "fooddata", data = dataToSave, waitTimeout = 180000)
}

paste0("Food module completed successfully!!! ",
       stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")

