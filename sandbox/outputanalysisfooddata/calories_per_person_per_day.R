suppressMessages({
    library(data.table)
    library(faosws)
    library(dplyr)
    library(faoswsUtil)
    library(faoswsFood)
    library(faoswsFlag)
    library(countrycode)
    library(zoo)
    library(ggplot2)
    library(scales)
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

completeImputationKey = getCompleteImputationKey("food")
completeImputationKey@dataset <- "fooddata"

newFoodData <- GetData(completeImputationKey, flags = TRUE)
setnames(newFoodData, "Value", "food")
newFoodData[, type := getCommodityClassification(as.character(measuredItemCPC))]

## computing calories per person per day from the old food data

caloriesData <- faoswsUtil::getNutritiveFactors(geographicAreaM49 = newFoodData$geographicAreaM49,
                                                measuredElement = "261",
                                                measuredItemCPC = newFoodData$measuredItemCPC,
                                                timePointYearsSP = newFoodData$timePointYears)
setnames(caloriesData,
         c("timePointYearsSP", "Value"),
         c("timePointYears", "valueCal"))


## Pop
minYearToProcess = "1991"
maxYearToProcess = "2014"
yearCodes <- minYearToProcess:maxYearToProcess
yearCodes <- as.character(yearCodes)
areaCodesM49 <- completeImputationKey@dimensions$geographicAreaM49@keys
dimM49 <- Dimension(name = "geographicAreaM49", keys = areaCodesM49)
dimTime <- Dimension(name = "timePointYears", keys = yearCodes)
populationCodes <- "21"
dimPop <- Dimension(name = "measuredElementPopulation", keys = populationCodes)
pivotM49 <- Pivoting(code = "geographicAreaM49")
pivotPop <- Pivoting(code = "measuredElementPopulation")
pivotTime <- Pivoting(code = "timePointYears")
keyPop <- DatasetKey(domain = "population", dataset = "population",
                     dimensions = list(dimM49, dimPop, dimTime))

popData <- GetData(keyPop, flags=FALSE, normalized = FALSE,
                   pivoting = c(pivotM49, pivotTime, pivotPop))
setnames(popData, "Value_measuredElementPopulation_21", "population")


timeSeriesPopData <- as.data.table(expand.grid(geographicAreaM49 = unique(popData$geographicAreaM49),
                                               timePointYears = unique(popData$timePointYears)))

popData = merge(timeSeriesPopData, popData, by = c("geographicAreaM49", "timePointYears"), 
                all.x = T)

popData[, imputedPopulation := na.locf(population, fromLast = TRUE)]
popData[is.na(population), population := imputedPopulation]
popData[, c("imputedPopulation", "diff") := NULL]

## Merge the food data with calories data
keys = c("geographicAreaM49",  "measuredItemCPC", "timePointYears")
newFoodData <- merge(
    newFoodData, 
    caloriesData[, c(keys, "valueCal"), with = F],
    by = keys, all.x=T)

newFoodData

## merge with pop
keys = c("geographicAreaM49", "timePointYears")
newFoodData <- merge(
    newFoodData, 
    popData[, c(keys, "population"), with = F],
    by = keys, all.x=T)

newFoodData

newFoodData[, calPerPersonPerDay := (food * valueCal * 10000) / 365 / (population * 1000)]

gdp = fread("Data/gdp_pc_usd_2005.csv")
gdpData = gdp[, c("FAOName", "year", "GDPPC_USD2005"), with = F]
gdpData = gdpData[year >= minYearToProcess & year <= maxYearToProcess]
setnames(gdpData, "GDPPC_USD2005", "GDP")
setnames(gdpData, "year", "timePointYears")
gdpData[, geographicAreaM49 := as.character(countrycode(FAOName, "country.name", "iso3n"))]
gdpData[, "FAOName" := NULL]
# Sudan has the wrong name (it should be former Sudan)
# gdpData[geographicAreaM49 == "736", FAOName := "Sudan (former)"]
gdpData[geographicAreaM49 == "156", geographicAreaM49 := "1248"]
setcolorder(gdpData, c("geographicAreaM49", "timePointYears", "GDP"))
gdpData = gdpData[!is.na(GDP)]
gdpData[, dupl := duplicated(timePointYears), 
        by = list(geographicAreaM49)]

gdpData = gdpData[dupl == FALSE]
gdpData[, dupl := NULL]

## merge with GDP
keys = c("geographicAreaM49", "timePointYears")
newFoodData <- merge(
    newFoodData, 
    gdpData[, c(keys, "GDP"), with = F],
    by = keys, all.x=T)

newFoodData



## FBS groups

## Food Groups
fbsTree = fread("C:/Users/caetano/Documents/fbs_tree/FbsTree.csv")
fbsTree[, .N, fbsItemTOT_description]
fbsTree[, .N, fbsItemAggregated_description]
setnames(fbsTree, "measuredItem", "measuredItemCPC")

## Merge
newFoodData = merge(newFoodData, 
                        fbsTree[, c("measuredItemCPC", "fbsItemTOT_description", "fbsItemAggregated_description"), with = F],
                        by = "measuredItemCPC", all.x = T)

newFoodData <- nameData("food", "fooddata", newFoodData)

newFoodData[, c("timePointYears_description", "measuredElement", 
                    "measuredElement_description", "flagObservationStatus", 
                    "flagMethod") := NULL]

newFoodData[, foodPerCapita := food/365/(population * 1000)]
###########

tradeCode <- c("5610", "5910")
totalTradeKeySWS = DatasetKey(
    domain = "trade",
    dataset = "total_trade_cpc_m49",
    dimensions = list(
        Dimension(name = "geographicAreaM49",
                  keys = unique(newFoodData$geographicAreaM49)),
        Dimension(name = "measuredElementTrade", keys = tradeCode),
        Dimension(name = "timePointYears", keys = as.character(2000:maxYearToProcess)),
        Dimension(name = "measuredItemCPC",
                  keys = unique(newFoodData$measuredItemCPC))
    )
)

totalTradeDataSWS = GetData(
    totalTradeKeySWS,
    flags = FALSE)

totalTradeDataSWS <- dcast.data.table(totalTradeDataSWS, geographicAreaM49 + measuredItemCPC +
                                          timePointYears ~ measuredElementTrade, value.var = "Value")


setnames(totalTradeDataSWS, "5610", "imports")
setnames(totalTradeDataSWS, "5910", "exports")

totalTradeDataFaostat <- getTotalTradeDataFAOSTAT1(unique(newFoodData$geographicAreaM49), 
                                                   unique(newFoodData$measuredItemCPC),
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


newFoodData = merge(newFoodData, 
      totalTradeData[, 
                     c("geographicAreaM49", "measuredItemCPC", "timePointYears", "netTrade"), with = F], 
      all.x = T, by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"))


calorieCountryDay <- newFoodData[, list(totalCalDay = sum(calPerPersonPerDay, na.rm=T),
                                        netTrade = sum(netTrade, na.rm = T),
                                            numbCommodities = .N),
                                     by = list(geographicAreaM49, geographicAreaM49_description, timePointYears)]


keys = c("geographicAreaM49", "timePointYears")
calorieCountryDay <- calorieCountryDay[!(totalCalDay == Inf)]
calorieCountryDay[, timePointYears := as.numeric(timePointYears)]



ggplot(calorieCountryDay[geographicAreaM49 == 276], 
       aes(x = timePointYears, y = totalCalDay, group = 1)) + 
    geom_line(stat="identity", colour = "blue") + 
    #geom_area(stat="identity") + 
    # scale_fill_manual(values=c("#FA8258", "#088A08")) +
    #scale_color_manual(values=c("#A9F5A9", "#81BEF7")) +
    # scale_fill_grey() +
    #geom_area(position = 'stack') +
    ylab('Total calories per person per day') + xlab('Year') +
    #labs(x="Year", y="Value") +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    scale_x_continuous(limits = c(1991, 2014), breaks = seq(1991, 2014, 1)) +
    scale_y_continuous(limits = c(0, 4000), breaks = seq(0, 4000, 500)) +
    theme(legend.title = element_blank()) +
    # ggtitle(country_name) +
    theme(axis.text=element_text(size=12, face="bold"),
          axis.title=element_text(size=14,face="bold")) +
    theme(legend.position="top")



ggplot(newFoodData[geographicAreaM49 == 840 & measuredItemCPC == "0115"], 
       aes(x = timePointYears, y = calPerPersonPerDay, group = 1)) + 
    geom_line(stat="identity", colour = "blue") #+ 
    #ylab('Tonnes') + xlab('Year')

countriesZeroCal = calorieCountryDay[totalCalDay == 0, .N, geographicAreaM49][, geographicAreaM49]

calorieCountryDay = calorieCountryDay[!(geographicAreaM49 %in% countriesZeroCal)]


# country = calorieCountryDay[, .N, c("geographicAreaM49_description", "faoCode")]
# country[, "N" := NULL]
# 
# save(country, file = "sandbox/country.RData")


calorieCountryDayItem <- newFoodData[, list(totalCalDay = sum(calPerPersonPerDay, na.rm=T),
                                            netTrade = sum(netTrade, na.rm = T),
                                        numbCommodities = .N),
                                 by = list(geographicAreaM49, geographicAreaM49_description, timePointYears, type)]


keys = c("geographicAreaM49", "timePointYears")
calorieCountryDayItem <- calorieCountryDayItem[!(totalCalDay == Inf)]
calorieCountryDayItem[, timePointYears := as.numeric(timePointYears)]

ggplot(calorieCountryDayItem[geographicAreaM49 == 276], 
       aes(x = timePointYears, y = totalCalDay, group = type, col = type)) + 
    geom_line(stat="identity") #+ 
#ylab('Tonnes') + xlab('Year')


ggplot(calorieCountryDayItem[geographicAreaM49 == 76], 
       aes(x = timePointYears, y = netTrade, group = type, col = type)) + 
    geom_line(stat="identity")

