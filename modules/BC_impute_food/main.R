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
    SETTINGS <- ReadSettings("Modules/impute_food/sws.yml")
    
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

minReferenceYear <- as.numeric(ifelse(is.null(swsContext.computationParams$minReferenceYear), "2014",
                                      swsContext.computationParams$minReferenceYear))

maxReferenceYear <- as.numeric(ifelse(is.null(swsContext.computationParams$maxReferenceYear), "2016",
                                      swsContext.computationParams$maxReferenceYear))

# referenceYear <- round(median(as.numeric(referenceYearRange)))
if (minReferenceYear > maxReferenceYear | maxReferenceYear < minReferenceYear)
    stop("Please check the time range for the reference years")

referenceYearRange <- as.character(minReferenceYear:maxReferenceYear)

# Parameter: year to process

minYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$minYearToProcess), "1990",
                                      swsContext.computationParams$minYearToProcess))

maxYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$maxYearToProcess), "2018",
                                      swsContext.computationParams$maxYearToProcess))

if (minYearToProcess > maxYearToProcess | maxYearToProcess < minYearToProcess)
    stop("Please check the time range for the years to be processed")

# e.g.: 2012 for range 2011, 2012, 2013
referenceYear <- floor(median(as.numeric(referenceYearRange)))

yearCodes <- as.character(minYearToProcess:maxYearToProcess)

yearCodes_sua_unbalanced <- as.character(2000:maxYearToProcess)

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
cpcItems <- GetCodeList("suafbs", "sua_unbalanced", "measuredItemFbsSua")[, code]

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
# TOP 53
# areaCodesM49 <- c("356",	"1248",	"586",	"50",	"231",	"360",	"180",	"834",	"608",	"800",	"566",
#                   "704",	"408",	"729",	"368",	"404",	"450",	"4",	"24",	"508",	"894",	"887",
#                   "716",	"104",	"764",	"484",	"332",	"148",	"384",	"170",	"454",	"854",	"646",
#                   "760",	"604",	"116",	"320",	"710",	"524",	"144",	"68",	"860",	"140",	"324",
#                   "862",	"562",	"178",	"12",	"686",	"694",	"120",	"218",	"762")

# TOP 62 countries

# areaCodesM49_top_62<-c("4","12","24","50","68","104","116","120","140","144","148","170","178","180","218","231","320","324","332","340","356","360","364",
#                 "368","384","404","408","418","430","450","454","466","484","508","524","562","566","586","598","604","608","646","686","694","704","706",
#                 "710","716","729","748","760","762","764","768","800","834","854","860","862","887","894","1248" )
#  
# areaCodesM49 <-setdiff(areaCodesM49,areaCodesM49_top_62)

# areaCodesM49 <- c(areaCodesM49,areaCodesM49_top_62)


#all countries

areaCodesM49 <-  c("4","8","12","24","28","32",
"51","36","40","31","44","50","52","112","56","84","204","68","70","72","76","100","854","108","116","120","124","132",
"140","148","152","158","344","446","1248","170","174","178","180","188","384","191","192","196","203","208","262",
"212","214","218","818","222","233","748","231","242","246","250","258","266","270","268","276","288","300","308",
"320","324","624","328","332","340","348","352","356","360","364","368","372","376","380","388","392",
"400","398","404","296","408","410","414","417","418","428","422","426","430","434","440","442","450","454",
"458","462","466","470","478","480","484","498","496","499","504","508","104","516","524","528","540","554","558",
"562","566","807","578","512","586","275","591","598","600","604","608","616","620","642","643","646","659","662",
"882","678","682","686","688","690","694","703","705","90","706","710","724","144","670","729","740","752","756",
"760","762","834","764","626","768","780","788","792","795","800","804","784","826","840","858","860","548","862",
"704","887","894","716")


# Exclude those codes
areaCodesM49 <- areaCodesM49[!(areaCodesM49 %in% c("831", "832"))]


# #test for one country
# areaCodesM49 <- c("654","792", "434")
# 

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
dimElementGDP <- Dimension(name = "dim_element_fao_macro_ind", keys = gdpElementCode)
dimItemGDP <- Dimension(name = "dim_item_fao_macro_ind", keys = gdpItemCode)
# Curently not used
#pivotGDP <- Pivoting(code = "wbIndicator")
#dimFood <- Dimension(name = "measuredElement", keys = foodCodes)

############################# Define the keys ######################

# Keys for population
keyPop <- DatasetKey(domain = "population", dataset = "population_unpd",
                     dimensions = list(dimM49, dimPop, dimTime))

# Keys for GDP
keyGDP <- DatasetKey(domain = "faostat_datasets", dataset = "faostat_macro_ind",
                     dimensions = list(dimFaoCode, dimElementGDP, dimItemGDP, dimTime))

# Keys for food parameters
keyFdm <- DatasetKey(domain = "food", dataset = "food_factors",
                     dimensions = list(dimM49, dimCom, dimFdm, dimFun, dimVar))

# Key for food
foodKey_unbalanced <- DatasetKey(
    domain = "suafbs",
    dataset = "sua_unbalanced",
    dimensions = list(
        Dimension(name = "geographicAreaM49", keys = areaCodesM49),
        Dimension(name = "measuredElementSuaFbs", keys = '5141'),
        Dimension(name = "measuredItemFbsSua", keys = itemCodesCPC),
        Dimension(name = "timePointYears", keys = as.character(2000:2009))
    )
)

foodKey <- DatasetKey(
    domain = "suafbs",
    dataset = "sua_balanced",
    dimensions = list(
        Dimension(name = "geographicAreaM49", keys = areaCodesM49),
        Dimension(name = "measuredElementSuaFbs", keys = '5141'),
        Dimension(name = "measuredItemFbsSua", keys = itemCodesCPC),
        Dimension(name = "timePointYears", keys = as.character(2010:maxYearToProcess))
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

# XXX: should use dataset/datatable. For now old data will be updated
# with the groth rate of new data during last year.


#@@@@@@@@@@@ "old"

gdpData_old <- ReadDatatable("gdp_usd2010")

stopifnot(nrow(gdpData_old) > 0)

setnames(gdpData_old, old = c("geographic_area_m49", "time_point_years", "gdp_usd_2010"),
         new = c("geographicAreaM49", "timePointYears", "GDP"))

gdpData_old[, c("fao_name", "fao_code") := NULL]
setcolorder(gdpData_old, c("geographicAreaM49", "timePointYears", "GDP"))
gdpData_old[, geographicAreaM49 := as.character(geographicAreaM49)]
gdpData_old[, timePointYears := as.character(timePointYears)]

# There are no data for Taiwan. So we get this table from Taiwan website.
gdp_taiwan <- ReadDatatable("gdp_taiwan_2005_prices")
gdp_taiwan[, time_point_years := as.numeric(time_point_years)]
setnames(gdp_taiwan, old = c("geographic_area_m49", "time_point_years", "gdp"),
         new = c("geographicAreaM49", "timePointYears", "GDP"))

gdp_taiwan = gdp_taiwan[timePointYears >= minYearToProcess & timePointYears <= maxYearToProcess]
gdp_taiwan[, geographicAreaM49 := as.character(geographicAreaM49)]
gdp_taiwan[, timePointYears := as.character(timePointYears)]

# Including Taiwan
gdpData_old <- rbind(gdpData_old, gdp_taiwan)
gdpData_old[geographicAreaM49 == "156", geographicAreaM49 := "1248"]

setnames(gdpData_old, "GDP", "GDP_old")


#@@@@@@@@@@@ "new"

# gdp <- read.csv(paste0(R_SWS_SHARE_PATH, "/wanner/gdp/","GDP.csv"))

# gdp<-read.csv("GDP_2427927.csv")

# gdp<- read.csv("Z:/wanner/gdp/GDP.csv")

# gdp<- read.csv("modules/Food Module/Data/GDP.csv")
# gdp <- as.data.table(gdp)
#gdp[, geographicAreaM49 := as.character(countrycode(Country.Code, "iso3c", "iso3n"))]
# gdp[geographicAreaM49 == "156", geographicAreaM49 := "1248"]

# gdp <- dplyr::filter(gdp,!is.na(geographicAreaM49))
#gdp <- dplyr::select(gdp,-Country.Name,-Country.Code,-Indicator.Name,-Indicator.Code)
# gdp <- as.data.table(gdp)
# gdpData_new <- melt.data.table(gdp, id.vars = "geographicAreaM49")
# setnames(gdpData_new, "variable", "timePointYears")
# setnames(gdpData_new, "value", "GDP")
# gdpData_new <- as.data.frame(gdpData_new)
# gdpData_new$timePointYears = substr(gdpData_new$timePointYears,2,5)
# gdpData_new <- as.data.table(gdpData_new)


gdpData_new <- ReadDatatable("gdp_food_imputation")

setnames(gdpData_new,names(gdpData_new),c("geographicAreaM49","timePointYears","GDP"))
gdpData_new <- gdpData_new[!is.na(geographicAreaM49)]
gdpData_new[geographicAreaM49 == "156", geographicAreaM49 := "1248"]

# There are no data for Taiwan. So we get this table from Taiwan website.
gdp_taiwan <- ReadDatatable("gdp_taiwan_2005_prices")

gdp_taiwan[, time_point_years := as.numeric(time_point_years)]

setnames(gdp_taiwan, old = c("geographic_area_m49", "time_point_years", "gdp"),
         new = c("geographicAreaM49", "timePointYears", "GDP"))

gdp_taiwan <- gdp_taiwan[timePointYears >= minYearToProcess & timePointYears <= maxYearToProcess]
gdp_taiwan[, geographicAreaM49 := as.character(geographicAreaM49)]
gdp_taiwan[, timePointYears := as.character(timePointYears)]

gdpData_new <- rbind(gdpData_new, gdp_taiwan)

gdpData_new[geographicAreaM49 == "156", geographicAreaM49 := "1248"]

gdpData_new[, GDP := as.numeric(GDP)]

stopifnot(nrow(gdpData_new) > 0)


#@@@@@@@@@@@ "old" + "new"

gdpData_new <- gdpData_new[order(geographicAreaM49, timePointYears)]

gdpData_new[, growth := GDP / shift(GDP), geographicAreaM49]

# In the "new" data, Taiwan was not updated. Below it was updated by
# inserting directly the "GDP Growht Rate (%)" found via query to:
# http://statdb.dgbas.gov.tw/pxweb/dialog/statfile1L.asp

gdpData_new <-
    rbind(
        gdpData_new,
        data.table(geographicAreaM49 = "158", timePointYears = c("2017","2018"),
                   GDP = NA_real_, growth = 1 + 3.08/100)
    )

gdpData <-
    dplyr::full_join(
        gdpData_old,
        gdpData_new,
        by = c("geographicAreaM49", "timePointYears"),
        suffix = c("_old", "_new")
    )

setDT(gdpData)

gdpData <- gdpData[order(geographicAreaM49, timePointYears)]

gdpData[, GDP1 := ifelse(timePointYears == '2017', shift(GDP_old) * growth, GDP_old), .(geographicAreaM49)]

gdpData[, GDP_old := ifelse(is.na(GDP_old) & !is.na(GDP1),GDP1 , GDP_old), .(geographicAreaM49)]

gdpData[, GDP1 := ifelse(timePointYears == '2018', shift(GDP_old) * growth, GDP_old), .(geographicAreaM49)]


gdpData <- gdpData[, .(geographicAreaM49, timePointYears, GDP = GDP1)]


# Food before 2000

# The sua_validated_2015 starts from 2000. In order to get the data from 1990-1999,
# we will keep pulling data from updated_sua_2013_data. If in future we not need the
# data before 2000, just ignore this piece of code.

foodDataUpTo1999 <- getFoodDataFAOSTAT1(areaCodesM49,
                                        itemCodesCPC,
                                        yearRange = as.character(1990:1999),
                                        "updated_sua_2013_data")

stopifnot(nrow(foodDataUpTo1999) > 0)

print("line 387")
# Food data from 2000

foodDataFrom2000_2009 <- GetData(foodKey_unbalanced, flags = TRUE) # from sua unbalanced 2000 to 2009

foodDataFrom2000 <- GetData(foodKey, flags = TRUE) # sua balanced from 2010 to date

foodDataFrom2000 <- rbind(foodDataFrom2000_2009,foodDataFrom2000)

stopifnot(nrow(foodDataFrom2000) > 0)





###################################################################################################################################################

#Sumeda : 05/03/2020 As a temporary action we are deleting (E,f) food data from sua unbalance from 2014 to 2017 of 53 countries mentioned above. 
#however, this will be a temporary action (Salar requested as we are re-validating 2014 -2017 apart 2018) 
# (E,f) flags of these years (2014-2017) of  these 53 coutnries have been deleted in food domain as well.



foodDataFrom2000[timePointYears %in% c(2010:2013) & flagObservationStatus == "E" &  flagMethod == "f",
                 c("Value","flagObservationStatus","flagMethod") := NA] #back compilation 

                                             



##################################################################################################################################


# Trade data

totalTradeData <- GetData(totalTradeKey, flags = FALSE)

stopifnot(nrow(totalTradeData) > 0)


# Production

productionData <- GetData(productionKey, flags = TRUE)

stopifnot(nrow(productionData) > 0)


# Elasticities & co.Two m49 158 , 512 are missing

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


#Sumeda : It decided to estimate all food items as "food estimate". 24/05/2019
food_classification_country_specific[food_classification == "Food Residual", food_classification:= "Food Estimate"]
food_classification_country_specific <- food_classification_country_specific[!geographic_area_m49 == "\\N"]

############### Set names


setnames(popData, "Value", "population")

setnames(foodDataFrom2000, old = c("measuredElementSuaFbs", "measuredItemFbsSua", "Value"),
         new = c("measuredElement", "measuredItemCPC", "food"))

setnames(foodDataUpTo1999, old = "Value", new = "food")

setnames(food_classification_country_specific,
         old = c("geographic_area_m49", "measured_item_cpc", "food_classification"),
         new = c("geographicAreaM49", "measuredItemCPC", "type"))

setnames(fdmData, old = c("Value_foodVariable_y_e", "foodFdm", "foodCommodityM"),
         new = c("elasticity", "foodDemand", "foodCommodity"))

setnames(countryIncomeGroup,
         old = c("geographic_area_m49", "country_name", "country_code", "group_code", "income_group"),
         new = c("geographicAreaM49", "CountryName", "CountryCode", "GroupCode", "incomeGroup"))



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

#if data is taken from SUA Balanced keep it in mind that E,h is protected in flagValideTable. 
# E,h flag is protected in flagValidTable. Hence , it is needed to change to FALSE. 

flagValidTable <-flagValidTable[flagObservationStatus == "E" & flagMethod == "h", Protected := FALSE]



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

print("line 582")
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

print("line 653")

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


print("line 725")

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


fdmData <-
    merge(
        fdmData,
        oldToNewCommodity,
        all.x = TRUE,
        allow.cartesian = TRUE,
        by.x = "foodCommodity",
        by.y = "old_code"
    )

print("line 768")

fdmData[is.na(new_code), new_code := foodCommodity]
fdmData <- fdmData[foodCommodity != "2500"]
fdmData[, c("foodCommodity") := NULL]

setnames(fdmData, old = "new_code", new = "foodCommodity")

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

data[, updatedElast := ifelse(is.na(elasticity), elasticityAverage, elasticity)]

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

print("line 844")


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

#Sumeda : it is decided to use the simple linear model for the commodities that does not have a functional form. 04/03/2020
data[,updatedFoodFunction := ifelse(is.na(updatedFoodFunction), 0, updatedFoodFunction)]


print("line 875")

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
    
    #data[, error := food - foodHat]
    
    dataToSave <- data[!is.na(foodHat)]
    
    ## Prepare data and save it to SWS
    cat("Restructure and filter data to save to SWS...\n")
    
    setnames(dataToSave, "foodHat", "Value")
    
    dataToSave[, measuredElement := "5141"]
    
    dataToSave[
        Protected == FALSE,
        `:=`(
            flagObservationStatus = "I",
            flagMethod = ifelse(type == "Food Estimate", "e", "i") # else => residual
        )
        ]
    
    dataToSave <- dataToSave[, c("timePointYears", "geographicAreaM49", "measuredItemCPC",
                                 "measuredElement", "Value", "flagObservationStatus", "flagMethod"),
                             with = FALSE]
    
    
    dataToSave <- subset(dataToSave, timePointYears %in% c(2010:2013))
    
    cat("Save the final data...\n")
    
    stats <- SaveData(domain = "food", dataset = "fooddata", data = dataToSave, waitTimeout = 180000)
}

paste0("Food module completed successfully!!! ",
       stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")

