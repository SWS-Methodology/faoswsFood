
library('forecast')
library("xlsx")
library("dplyr")
library('reshape2')

## New Net Trade Data
tradeCode <- c("5610", "5910")
totalTradeKeySWS = DatasetKey(
    domain = "trade",
    dataset = "total_trade_cpc_m49",
    dimensions = list(
        Dimension(name = "geographicAreaM49",
                  keys = unique(timeSeriesData$geographicAreaM49)),
        Dimension(name = "measuredElementTrade", keys = tradeCode),
        Dimension(name = "timePointYears", keys = as.character(2000:2013)),
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

# totalTradeData[, shiftImports := shift(imports),
#                by = list(geographicAreaM49, measuredItemCPC)]
# 
# totalTradeData[, percent := (imports - shiftImports)/shiftImports,
#                by = list(geographicAreaM49, measuredItemCPC)]


averageTab = totalTradeData[timePointYears %in% c("2011", "2012", "2013"), 
               list(#averagePercenPoints = mean(percent, na.rm = T),
                    imports = mean(imports, na.rm = T),
                    exports = mean(exports, na.rm = T)),
               by = list(geographicAreaM49, measuredItemCPC)]


averageTab[, timePointYears := "2014"]
setcolorder(averageTab, c("geographicAreaM49", "measuredItemCPC", 
                          "timePointYears", "imports", "exports"))

averageTab[, netTrade := (imports - exports)]

totalTradeData = rbind(totalTradeData, averageTab)

# 2012 - 2013 - 2014
averageTab = totalTradeData[timePointYears %in% c("2012", "2013", "2014"), 
                            list(#averagePercenPoints = mean(percent, na.rm = T),
                                imports = mean(imports, na.rm = T),
                                exports = mean(exports, na.rm = T)),
                            by = list(geographicAreaM49, measuredItemCPC)]


averageTab[, timePointYears := "2015"]
setcolorder(averageTab, c("geographicAreaM49", "measuredItemCPC", 
                          "timePointYears", "imports", "exports"))

averageTab[, netTrade := (imports - exports)]

totalTradeData = rbind(totalTradeData, averageTab)

# 2013 - 2014 - 2015
averageTab = totalTradeData[timePointYears %in% c("2013", "2014", "2015"), 
                            list(#averagePercenPoints = mean(percent, na.rm = T),
                                imports = mean(imports, na.rm = T),
                                exports = mean(exports, na.rm = T)),
                            by = list(geographicAreaM49, measuredItemCPC)]


averageTab[, timePointYears := "2016"]
setcolorder(averageTab, c("geographicAreaM49", "measuredItemCPC", 
                          "timePointYears", "imports", "exports"))

averageTab[, netTrade := (imports - exports)]

totalTradeData = rbind(totalTradeData, averageTab)

totalTradeDataForecasted = totalTradeData[timePointYears %in% c("2014", "2015", "2016")]

save(totalTradeDataForecasted, file = "sandbox/planB/totalTradeDataForecasted.RData")


## Imports

# SECTION I. --------------------------------------------------------------
# Prepare Workspace

imports = totalTradeData[, c("geographicAreaM49", "measuredItemCPC", 
                             "timePointYears", "imports"), with = F]

importsTimeSeries = as.data.table(expand.grid(timePointYears = as.character(unique(imports$timePointYears)),
                                              geographicAreaM49 = as.character(unique(imports$geographicAreaM49)),
                                              measuredItemCPC = as.character(unique(imports$measuredItemCPC))))

mergeImports <- merge(importsTimeSeries, imports, 
      by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"),
      all.x = T)

mergeImports[is.na(imports), imports := 0]


imports = dcast.data.table(mergeImports, geographicAreaM49 + measuredItemCPC ~ timePointYears, 
                 value.var = "imports")

importsTest = head(imports, 1000)



matrixImports = data.matrix(importsTest[, !(c("geographicAreaM49", "measuredItemCPC")), with = F],
                            rownames.force = NA)
dim(matrixImports)
nrows = dim(matrixImports)[1]
ncols = dim(matrixImports)[2]



# SECTION II. --------------------------------------------------------------
# Create empty matrix to store forecated values
store_forecast <- matrix( NA, nrow = nrows, ncol = ncols + 3)

# Loop over all countries, select best ARIMA model and produce forecast
for (i in 1:nrows){
    #1. Delta-log transformation
    ldata <- log(matrixImports[i,] + 1 )
    ldata.ts <- as.ts(ldata)
    #2. Outlier detection and replacement
    x <- tsoutliers(ldata.ts)
    ldata.ts[x$index] <- x$replacements
    dldata.ts <- diff(ldata.ts, lag=1) 
    dldata <- 100*dldata.ts
    # 3. Select best ARIMA model and use it for forecasting
    fit1 <- forecast::auto.arima(x=dldata)
    fit1
    temp <- forecast( fit1, h=3 ) 
    plot.forecast( forecast( fit1, h=3 ))
    # Revert the delta log forecasted values to levels and store them
    level_forecast <- matrixImports[i,ncols]*exp(temp$mean[1:3]/100)
    store_forecast[i,1:ncols] <- exp(ldata.ts) - 1 
    store_forecast[i,(ncols+1):(ncols+3)] <- level_forecast[1:3]
    
    rm(level_forecast, fit1, temp, x)
}

