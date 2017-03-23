
# Compare food figures estimated for commodities classified as a 
# "Food Residual" with net trade and net supply(production + netTrade)
# This data set was saved in March 3, 2017.

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

# Load data set
load("sandbox/comparison_netTrade_netSupply/dataToSave.RData")

# foodHat: computed using netSupply (netTrade + production)
# foodHatNetTrade: computed using netSupply (netTrade)

## Computing calories per person per day from the old food data

caloriesData <- faoswsUtil::getNutritiveFactors(geographicAreaM49 = dataToSave$geographicAreaM49,
                                                measuredElement = "261",
                                                measuredItemCPC = dataToSave$measuredItemCPC,
                                                timePointYearsSP = dataToSave$timePointYears)
setnames(caloriesData,
         c("timePointYearsSP", "Value"),
         c("timePointYears", "valueCal"))

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

## Merge the food data with calories data

foodData <- merge(
    dataToSave, 
    caloriesData[, c(keys, "valueCal"), with = F],
    by = keys, all.x=T)

foodData

foodData[, calPerPersonPerDayNetSupply := (foodHat * valueCal * 10000) / 365 / (population * 1000)]
foodData[, calPerPersonPerDayNetTrade := (foodHatNetTrade * valueCal * 10000) / 365 / (population * 1000)]
foodData[, diffCalories := calPerPersonPerDayNetSupply - calPerPersonPerDayNetTrade, ]
foodData[diffCalories > 0]

foodData = nameData("food", "fooddata", foodData)
foodData
foodData[diffCalories > 500, .N, geographicAreaM49_description]
summary(foodData$diffCalories)

## Compute the calorie consumption per country, per person and per day
calorieCountryDay <- foodData[, list(totalCalDayNetSupply = sum(calPerPersonPerDayNetSupply, na.rm=T),
                                     totalCalDayNetTrade = sum(calPerPersonPerDayNetTrade, na.rm=T),
                                     numbCommodities = .N),
                              by = list(geographicAreaM49, geographicAreaM49_description, timePointYears)]

calorieCountryDay[, diff := totalCalDayNetSupply - totalCalDayNetTrade,]
calorieCountryDay[, percent := totalCalDayNetSupply/totalCalDayNetTrade - 1,]

calorieCountryDay[diff > 100, .N, geographicAreaM49_description]
# calorieCountryDay[, c("numbCommodities", "diff") := NULL]

## Exclude countries that have just one value
# calorieCountryDay[, .N, geographicAreaM49_description][order(N)]
# calorieCountryDay = calorieCountryDay[!geographicAreaM49_description == "American Samoa"]

library(ggplot2)
library(scales)


calorieCountryDay <- calorieCountryDay[!(totalCalDayNetSupply %in% c("Inf", 0))]

totalCalories <- melt.data.table(calorieCountryDay, 
                                 id.vars = c(
                                         "geographicAreaM49", "geographicAreaM49_description",
                                         "timePointYears"), 
                                     measure.vars = c("totalCalDayNetTrade", "diff"))

totalCalories[variable == "diff", variable2 := "Production"]
totalCalories[variable == "totalCalDayNetTrade", variable2 := "Net Trade"]

totalCalories$variable2 <- factor(totalCalories$variable2, levels = c("Production",
                                                                    "Net Trade"))
space <- function(x, ...) { 
    format(x, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

countryName = unique(totalCalories$geographicAreaM49_description)
countryName = countryName[order(countryName)]
dput(countryName)

keys = c("geographicAreaM49", "timePointYears")
totalCalories = merge(totalCalories, calorieCountryDay[, c(keys, "totalCalDayNetSupply"), with = F],
      by = keys, all.x = T)

exclude = totalCalories[totalCalDayNetSupply < 1000, .N, geographicAreaM49][, geographicAreaM49]

totalCalories[, aux := totalCalDayNetSupply %% 200]
totalCalories[, auxTot := totalCalDayNetSupply + (200 - aux)]

totalCalories = totalCalories[!(geographicAreaM49 %in% exclude)]
countryName = unique(totalCalories$geographicAreaM49_description)
countryName = countryName[order(countryName)]
dput(countryName)

# 

dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsFood", "sandbox")
pdffile <- file.path(dlpath, "comparison_netTrade_netSupply", "total-calories.pdf")

pdf(file = pdffile, height = 11, width = 16)

system.time(for (country_name in c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda", 
                                   "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", 
                                   "Bahamas", "Bangladesh", "Barbados", "Bel-Lux(-1999)", "Belarus", 
                                   "Belgium", "Belize", "Benin", "Bermuda", "Bolivia (Plurinational State of)", 
                                   "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei Darussalam", 
                                   "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", 
                                   "Cameroon", "Canada", "Central African Republic", "Chad", "Chile", 
                                   "China, Macao", "China, Main", "China,H.Kong", "Colombia", "Comoros", 
                                   "Congo", "Costa Rica", 
                                   #"Côte d'Ivoire",
                                   "Croatia", "Cuba", "Cyprus", 
                                   "Czechia", "Democratic People's Republic of Korea", "Democratic Republic of the Congo", 
                                   "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", 
                                   "Egypt", "El Salvador", "Estonia", "Ethiopia", "Fiji", "Finland", 
                                   "France", "French Polynesia", "Gabon", "Gambia", "Georgia", "Germany", 
                                   "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", 
                                   "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", 
                                   "Iran (Islamic Republic of)", "Iraq", "Ireland", "Israel", "Italy", 
                                   "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", 
                                   "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", "Latvia", 
                                   "Lebanon", "Lesotho", "Liberia", "Libya", "Lithuania", "Luxembourg", 
                                   "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", 
                                   "Mauritania", "Mauritius", "Mexico", "Mongolia", "Morocco", "Mozambique", 
                                   "Myanmar", "Namibia", "Nepal", "Netherlands", "New Caledonia", 
                                   "New Zealand", "Nicaragua", "Niger", "Nigeria", "Norway", "Pakistan", 
                                   "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", 
                                   "Poland", "Portugal", "Republic of Korea", "Republic of Moldova", 
                                   "Romania", "Russian Federation", "Rwanda", "Saint Kitts and Nevis", 
                                   "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "Sao Tome and Principe", 
                                   "Saudi Arabia", "Senegal", "Seychelles", "Sierra Leone", "Slovakia", 
                                   "Solomon Islands", "Somalia", "South Africa", "Spain", "Sri Lanka", 
                                   "Suriname", "Swaziland", "Sweden", "Switzerland", "Syrian Arab Republic", 
                                   "Tajikistan", "Thailand", "The former Yugoslav Republic of Macedonia", 
                                   "Togo", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", 
                                   "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", 
                                   "United Republic of Tanzania", "United States of America", "Uruguay", 
                                   "Uzbekistan", "Vanuatu", "Venezuela (Bolivarian Republic of)", 
                                   "Viet Nam", "Yemen", "Zambia", "Zimbabwe")) {
    
    p = ggplot(totalCalories[geographicAreaM49_description == country_name], 
               aes(x = timePointYears, y = value, fill = as.factor(variable2))) + 
        geom_bar(stat="identity") +
        scale_fill_manual(values=c("#FA8258", "#088A08")) +
        ylab('Total calories per person per day') + xlab('Year') +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
        #scale_x_continuous(limits = c(1991, 2013), breaks = seq(1991, 2013, 1)) +
        #scale_y_continuous(limits = c(0, 4500), breaks = seq(0, 4500, 250), labels = space) +
        scale_y_continuous(limits = c(0, round(max(totalCalories[geographicAreaM49_description == country_name,]$auxTot)) + 200), 
                           breaks = seq(0, round(max(totalCalories[geographicAreaM49_description == country_name,]$auxTot)), 200),
                           labels = space) +
        theme(legend.title = element_blank()) +
        ggtitle(country_name) +
        theme(plot.title = element_text(size=20)) +
        theme(legend.text=element_text(size=14)) +
        
        theme(axis.text=element_text(size=16, face="bold"),
              axis.title=element_text(size=16,face="bold")) +
        theme(legend.position="top") +
        geom_hline(aes(yintercept=4000))
    print(p)
    
}

)


dev.off()


calorieCountryDay

dataFaostat = read.csv("C:/Users/caetano/Downloads/FAOSTAT_data_3-6-2017.csv")
dataFaostat = data.table(dataFaostat)
dataFaostat[, geographicAreaM49 := fs2m49(as.character(Country.Code))]
dataFaostat[is.na(geographicAreaM49)] # China
setnames(dataFaostat, "Year", "timePointYears")
setnames(dataFaostat, "Value", "caloriesFaostat")
dataFaostat[, timePointYears := as.character(timePointYears)]

keys = c("geographicAreaM49", "timePointYears")

sapply(calorieCountryDay, class)
sapply(dataFaostat, class)

calorieCountryDayMergeFaostat = merge(calorieCountryDay, 
      dataFaostat[, c(keys, "caloriesFaostat"), with = F],
      by = keys, all.x = T)

calorieCountryDayMergeFaostat

calorieCountryDayMergeFaostat[is.na(caloriesFaostat) & timePointYears != 2014, .N, geographicAreaM49_description]

calorieCountryDayMergeFaostatMelt <- melt.data.table(calorieCountryDayMergeFaostat, 
                                 id.vars = c(
                                     "geographicAreaM49", "geographicAreaM49_description",
                                     "timePointYears"), 
                                 measure.vars = c("totalCalDayNetSupply", "totalCalDayNetTrade", "caloriesFaostat"))

calorieCountryDayMergeFaostatMelt[variable == "totalCalDayNetSupply", variable2 := ""]

ggplot(calorieCountryDayMergeFaostatMelt[geographicAreaM49_description == country_name], 
       aes(x = timePointYears, y = value, colour=variable, group = variable)) + 
    # geom_bar(stat="identity") +
    geom_line(size = 1.2) +
    # scale_fill_manual(values=c("#FA8258", "#088A08")) +
    ylab('Total calories per person per day') + xlab('Year') +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    #scale_x_continuous(limits = c(1991, 2013), breaks = seq(1991, 2013, 1)) +
    #scale_y_continuous(limits = c(0, 4500), breaks = seq(0, 4500, 250), labels = space) +
    scale_y_continuous(limits = c(0, round(max(totalCalories[geographicAreaM49_description == country_name,]$auxTot)) + 200), 
                       breaks = seq(0, round(max(totalCalories[geographicAreaM49_description == country_name,]$auxTot)), 200),
                       labels = space) +
    theme(legend.title = element_blank()) +
    ggtitle(country_name) +
    theme(plot.title = element_text(size=20)) +
    theme(legend.text=element_text(size=14)) +
    
    theme(axis.text=element_text(size=16, face="bold"),
          axis.title=element_text(size=16,face="bold")) +
    theme(legend.position="top") +
    geom_hline(aes(yintercept=4000))


###

## pulling the old food data

minYearToProcess = "1991"
maxYearToProcess = "2014"

completeImputationKey = getCompleteImputationKey("food")


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



foodDataMerge[!is.na(Value) & Protected.y == T, updatedFood := Value]
foodDataMerge[is.na(updatedFood), updatedFood := food]


foodDataMerge[!is.na(Value) & Protected.y == T, updatedFlagObservationStatus := flagObservationStatusValue]
foodDataMerge[!(!is.na(Value) & Protected.y == T), updatedFlagObservationStatus := flagObservationStatus]

foodDataMerge[!is.na(Value) & Protected.y == T, updatedFlagMethodValue := flagMethodValue]
foodDataMerge[!(!is.na(Value) & Protected.y == T), updatedFlagMethodValue := flagMethod]

foodDataMerge[, c("flagObservationStatus", "flagMethod", "diff", "combineFlag", "Value", "food",
                  "Valid.x", "Valid.y", "Protected.x", "Protected.y",
                  "flagObservationStatusValue", "flagMethodValue") := NULL]
setnames(foodDataMerge, "updatedFlagObservationStatus", "flagObservationStatus")
setnames(foodDataMerge, "updatedFlagMethodValue", "flagMethod")
setnames(foodDataMerge, "updatedFood", "food")

foodDataMerge = merge(foodDataMerge, flagValidTable, 
                      by = c("flagObservationStatus", "flagMethod"), 
                      all.x = T)


## Computing calories per person per day from the old food data

caloriesDataOldFood <- faoswsUtil::getNutritiveFactors(geographicAreaM49 = foodDataMerge$geographicAreaM49,
                                                measuredElement = "261",
                                                measuredItemCPC = foodDataMerge$measuredItemCPC,
                                                timePointYearsSP = foodDataMerge$timePointYears)
setnames(caloriesDataOldFood,
         c("timePointYearsSP", "Value"),
         c("timePointYears", "valueCal"))

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

## Merge the food data with calories data

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


foodDataMerge <- merge(
    foodDataMerge, 
    caloriesDataOldFood[, c(keys, "valueCal"), with = F],
    by = keys, all.x=T)

foodDataMerge


## Population
minYearToProcess = "1991"
maxYearToProcess = "2014"

yearCodes <- minYearToProcess:maxYearToProcess
yearCodes <- as.character(yearCodes)

populationCodes <- "21"
dimPop <- Dimension(name = "measuredElementPopulation", keys = populationCodes)
pivotPop <- Pivoting(code = "measuredElementPopulation")
pivotM49 <- Pivoting(code = "geographicAreaM49")
pivotTime <- Pivoting(code = "timePointYears")


dimTime <- Dimension(name = "timePointYears", keys = yearCodes)

selectedKey =
    switch(validationRange,
           "session" = sessionKey, # if "validationRange" is "session": selectedKey <- sessionKey
           "all" = completeImputationKey) # if "validationRange" is "all": selectedKey <- completeImputationkey

areaCodesM49 <- selectedKey@dimensions$geographicAreaM49@keys
dimM49 <- Dimension(name = "geographicAreaM49", keys = areaCodesM49)

keyPop <- DatasetKey(domain = "population", dataset = "population",
                     dimensions = list(dimM49, dimPop, dimTime))


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


## Merge food and pop

keys = c("geographicAreaM49", "timePointYears")
foodDataMerge = merge(foodDataMerge, popData, by = keys, all.x = T)

foodDataMerge[, calPerPersonPerDay := (food * valueCal * 10000) / 365 / (population * 1000)]
oldFoodDataCalories = foodDataMerge[, list(oldTotalCal = sum(calPerPersonPerDay, na.rm = T)),
              by = list(geographicAreaM49, timePointYears)]

oldFoodDataCalories = nameData("food", "fooddata", oldFoodDataCalories)
oldFoodDataCalories








## Food Residual -  compute Trade
selectYearsTabFoodResidual = selectYearsTab[aux == 0 & type == "Food Residual"]

## Computing netSupply average
averageYearTabResidualTrade = selectYearsTabFoodResidual[, list(
    netTradeAverage = mean(netTrade, na.rm = T), 
    nObs = .N), 
    by = list(geographicAreaM49, measuredItemCPC)]

numbItem = length(unique(averageYearTabResidualTrade$measuredItemCPC))
tabCountryNan = averageYearTabResidualTrade[netSupplyAverage == "NaN", .N, geographicAreaM49]

averageYearTabResidualTrade[, timePointYears := median(as.numeric(yearAverage))]
averageYearTabResidualTrade[, timePointYears := as.character(timePointYears)]
averageYearTabResidualTrade[, flagObservationStatus := "I"]
averageYearTabResidualTrade[, flagMethod := "e"]
averageYearTabResidualTrade[, Protected := FALSE]


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
## nettrade goes to food. But if nettrade is below zero, food is equal to zero.

timeSeriesData[Protected == FALSE & type == "Food Residual" & netTrade > 0, 
               foodNetTrade := netTrade]

timeSeriesData[Protected == FALSE & type == "Food Residual" & netTrade <= 0, 
               foodNetTrade := 0]

timeSeriesData[type == "Food Estimate", 
               foodNetTrade := food]


