
# Compare food figures estimated for commodities classified as a 
# "Food Residual" with net trade and net supply(production + netTrade). We will
# also compare with the old food data and Faostat data.
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

# if(swsContext.computationParams$yearToProcess < 1991)
#     stop("This module was designed for imputation on years after 1990 only!")
referenceYear <- as.numeric(ifelse(is.null(swsContext.computationParams$referenceYear), "1998",
                                   swsContext.computationParams$referenceYear))

yearAverage <- c("2004", "2005", "2006")

minYearToProcess = "1991"
maxYearToProcess = "2014"

if(referenceYear < minYearToProcess | referenceYear > maxYearToProcess) 
    stop("The reference year must be set to between the minimum and the maximum year")

yearCodes <- minYearToProcess:maxYearToProcess
yearCodes <- as.character(yearCodes)

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
dimFood <- Dimension(name = "measuredElement", keys = foodCodes)

dimCom <- Dimension(name = "foodCommodityM", keys = comCodes)
dimFdm <- Dimension(name = "foodFdm", keys = fdmCodes)
dimFun <- Dimension(name = "foodFunction", keys = funCodes)
dimVar <- Dimension(name = "foodVariable", keys = varCodes)

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

## Population data
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

## Food data (from food domain and agriculture aproduction)

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

# flags from agriculture/aproduction

foodDataMerge[, flagObservationStatusValue := substr(combineFlag, 1, 1)]
foodDataMerge[combineFlag %in% c(";-", ";q", ";p"), flagObservationStatusValue := ""]

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

foodDataMerge[, c("flagObservationStatus", "flagMethod", "diff", "combineFlag", "Value", "food",
                  "Valid.x", "Valid.y", "Protected.x", "Protected.y",
                  "flagObservationStatusValue", "flagMethodValue") := NULL]
setnames(foodDataMerge, "updatedFlagObservationStatus", "flagObservationStatus")
setnames(foodDataMerge, "updatedFlagMethodValue", "flagMethod")
setnames(foodDataMerge, "updatedFood", "food")

foodDataMerge = merge(foodDataMerge, flagValidTable, 
                      by = c("flagObservationStatus", "flagMethod"), 
                      all.x = T)

## Merge food and pop

keys = c("geographicAreaM49", "timePointYears")
foodDataMerge = merge(foodDataMerge, popData, by = keys, all.x = T)

# Calories

## Computing calories per person per day from the old food data

caloriesDataOldFood <- faoswsUtil::getNutritiveFactors(geographicAreaM49 = foodDataMerge$geographicAreaM49,
                                                       measuredElement = "261",
                                                       measuredItemCPC = foodDataMerge$measuredItemCPC,
                                                       timePointYearsSP = foodDataMerge$timePointYears)
setnames(caloriesDataOldFood,
         c("timePointYearsSP", "Value"),
         c("timePointYears", "valueCal"))

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

foodDataMerge <- merge(
    foodDataMerge, 
    caloriesDataOldFood[, c(keys, "valueCal"), with = F],
    by = keys, all.x=T)

foodDataMerge

foodDataMerge[, calPerPersonPerDay := (food * valueCal * 10000) / 365 / (population * 1000)]
oldFoodDataCalories = foodDataMerge[, list(oldTotalCal = sum(calPerPersonPerDay, na.rm = T)),
                                    by = list(geographicAreaM49, timePointYears)]

oldFoodDataCalories = nameData("food", "fooddata", oldFoodDataCalories)
oldFoodDataCalories <- oldFoodDataCalories[!(oldTotalCal %in% c("Inf", 0))]

## Pull new Food data that I saved on March 3

# Load data set
load("sandbox/comparison_netTrade_netSupply/dataToSave.RData")

caloriesData <- faoswsUtil::getNutritiveFactors(geographicAreaM49 = dataToSave$geographicAreaM49,
                                                measuredElement = "261",
                                                measuredItemCPC = dataToSave$measuredItemCPC,
                                                timePointYearsSP = dataToSave$timePointYears)
setnames(caloriesData,
         c("timePointYearsSP", "Value"),
         c("timePointYears", "valueCal"))

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

## Merge the food data with calories data

newFoodData <- merge(
    dataToSave, 
    caloriesData[, c(keys, "valueCal"), with = F],
    by = keys, all.x=T)

newFoodData[, calPerPersonPerDayNetSupply := (foodHat * valueCal * 10000) / 365 / (population * 1000)]
newFoodData[, calPerPersonPerDayNetTrade := (foodHatNetTrade * valueCal * 10000) / 365 / (population * 1000)]
newFoodData[, diffCalories := calPerPersonPerDayNetSupply - calPerPersonPerDayNetTrade, ]

newFoodData = nameData("food", "fooddata", newFoodData)

## Compute the calorie consumption per country, per person and per day
newCalorieCountryDay <- newFoodData[, list(totalCalDayNetSupply = sum(calPerPersonPerDayNetSupply, na.rm=T),
                                     totalCalDayNetTrade = sum(calPerPersonPerDayNetTrade, na.rm=T),
                                     numbCommodities = .N),
                              by = list(geographicAreaM49, geographicAreaM49_description, timePointYears)]

newCalorieCountryDay <- newCalorieCountryDay[!(totalCalDayNetSupply %in% c("Inf", 0))]

## Merge both total cal data: from the old system and the new one
keys = c("geographicAreaM49", "timePointYears")
oldNewCalories <- merge(newCalorieCountryDay, oldFoodDataCalories[, c(keys, "oldTotalCal"), with = F], 
                        by = keys, all.x = T)

## Data from faostat (website) - FBS level

dataFBS = read.csv("C:/Users/caetano/Downloads/FAOSTAT_data_3-6-2017.csv")
dataFBS = data.table(dataFBS)
dataFBS[, geographicAreaM49 := fs2m49(as.character(Country.Code))]
dataFBS[is.na(geographicAreaM49)] # China
setnames(dataFBS, "Year", "timePointYears")
setnames(dataFBS, "Value", "caloriesFBS")
dataFBS[, timePointYears := as.character(timePointYears)]

keys = c("geographicAreaM49", "timePointYears")

# Merge with dataFBS
oldNewCalories = merge(oldNewCalories, 
                       dataFBS[, c(keys, "caloriesFBS"), with = F],
                                      by = keys, all.x = T)

## Melt
oldNewTotalCaloriesMelt <- melt.data.table(
    oldNewCalories, 
    id.vars = c(
        "geographicAreaM49", "geographicAreaM49_description", "timePointYears"), 
    measure.vars = c("totalCalDayNetSupply", "totalCalDayNetTrade", "oldTotalCal", "caloriesFBS"))

oldNewTotalCaloriesMelt[variable == "totalCalDayNetSupply", variable2 := "New Food: Net Trade + Produc"]
oldNewTotalCaloriesMelt[variable == "totalCalDayNetTrade", variable2 := "New Food: Net Trade"]
oldNewTotalCaloriesMelt[variable == "oldTotalCal", variable2 := "Old Food (cpc level)"]
oldNewTotalCaloriesMelt[variable == "caloriesFBS", variable2 := "FBS"]

exclude = oldNewTotalCaloriesMelt[value < 500, .N, geographicAreaM49][, geographicAreaM49]

oldNewTotalCaloriesMelt[, aux := value %% 200]
oldNewTotalCaloriesMelt[, auxTot := value + (200 - aux)]

oldNewTotalCaloriesMelt = oldNewTotalCaloriesMelt[!(geographicAreaM49 %in% exclude)]
oldNewTotalCaloriesMelt = oldNewTotalCaloriesMelt[!(is.na(value))]

countryName = unique(oldNewTotalCaloriesMelt$geographicAreaM49_description)
countryName = countryName[order(countryName)]
dput(countryName)

space <- function(x, ...) { 
    format(x, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

## plot
library(ggplot2)
library(scales)

dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsFood", "sandbox")
pdffile <- file.path(dlpath, "comparison_netTrade_netSupply", "total-calories-old-new-food.pdf")

pdf(file = pdffile, height = 11, width = 16)

system.time(for (country_name in c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda", 
                                   "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", 
                                   "Bahamas", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", 
                                   "Benin", "Bermuda", "Bolivia (Plurinational State of)", "Bosnia and Herzegovina", 
                                   "Botswana", "Brazil", "Brunei Darussalam", "Bulgaria", "Burkina Faso", 
                                   "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Central African Republic", 
                                   "Chad", "Chile", "China, Macao", "China, Main", "China,H.Kong", 
                                   "Colombia", "Comoros", "Congo", "Costa Rica", 
                                   #"Côte d'Ivoire", 
                                   "Croatia", "Cuba", "Cyprus", "Czechia", "Democratic People's Republic of Korea", 
                                   "Democratic Republic of the Congo", "Denmark", "Djibouti", "Dominica", 
                                   "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Estonia", 
                                   "Ethiopia", "Fiji", "Finland", "France", "French Polynesia", 
                                   "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", 
                                   "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", 
                                   "Hungary", "Iceland", "India", "Iran (Islamic Republic of)", 
                                   "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", 
                                   "Kazakhstan", "Kenya", "Kiribati", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", 
                                   "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Lithuania", 
                                   "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", 
                                   "Mali", "Malta", "Mauritania", "Mauritius", "Mexico", "Mongolia", 
                                   "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Netherlands", 
                                   "New Caledonia", "New Zealand", "Nicaragua", "Niger", "Nigeria", 
                                   "Norway", "Pakistan", "Panama", "Papua New Guinea", "Paraguay", 
                                   "Peru", "Philippines", "Poland", "Portugal", "Republic of Korea", 
                                   "Republic of Moldova", "Romania", "Russian Federation", "Rwanda", 
                                   "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", 
                                   "Samoa", "Sao Tome and Principe", "Saudi Arabia", "Senegal", 
                                   "Seychelles", "Sierra Leone", "Slovakia", "Solomon Islands", 
                                   "Somalia", "South Africa", "Spain", "Sri Lanka", "Suriname", 
                                   "Swaziland", "Sweden", "Switzerland", "Syrian Arab Republic", 
                                   "Tajikistan", "Thailand", "The former Yugoslav Republic of Macedonia", 
                                   "Timor-Leste", "Togo", "Trinidad and Tobago", "Tunisia", "Turkey", 
                                   "Turkmenistan", "Uganda", "Ukraine", "United Arab Emirates", 
                                   "United Kingdom", "United Republic of Tanzania", "United States of America", 
                                   "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela (Bolivarian Republic of)", 
                                   "Viet Nam", "Yemen", "Zambia", "Zimbabwe")) {
    
    p = ggplot(oldNewTotalCaloriesMelt[geographicAreaM49_description == country_name], 
               aes(x = timePointYears, y = value, colour=variable2, group = variable2)) + 
        # geom_bar(stat="identity") +
       geom_line(position=position_dodge(width=0.4), size = 1.2) +
        # scale_fill_manual(values=c("#FA8258", "#088A08")) +
        ylab('Total calories per person per day') + xlab('Year') +
        theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
        #scale_x_continuous(limits = c(1991, 2013), breaks = seq(1991, 2013, 1)) +
        #scale_y_continuous(limits = c(0, 4500), breaks = seq(0, 4500, 250), labels = space) +
        scale_y_continuous(limits = c(0, round(max(oldNewTotalCaloriesMelt[geographicAreaM49_description == country_name,]$auxTot)) + 200), 
                           breaks = seq(0, round(max(oldNewTotalCaloriesMelt[geographicAreaM49_description == country_name,]$auxTot)), 200),
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

overviewTab = oldNewTotalCaloriesMelt[, list(caloriesAverage = mean(value)),
                        by = list(variable, variable2, timePointYears)]

ggplot(overviewTab, 
       aes(x = timePointYears, y = caloriesAverage, colour=variable2, group = variable2)) + 
    # geom_bar(stat="identity") +
    geom_line(position=position_dodge(width=0.4), size = 1.2) +
    # scale_fill_manual(values=c("#FA8258", "#088A08")) +
    ylab('Total calories per person per day') + xlab('Year') +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    #scale_x_continuous(limits = c(1991, 2013), breaks = seq(1991, 2013, 1)) +
    #scale_y_continuous(limits = c(0, 4500), breaks = seq(0, 4500, 250), labels = space) +
    scale_y_continuous(limits = c(2000, 3200),
                       breaks = seq(2000, 3200, 100),
                       labels = space) +
    theme(legend.title = element_blank()) +
    ggtitle("World") +
    theme(plot.title = element_text(size=20)) +
    theme(legend.text=element_text(size=12)) +
    theme(axis.text=element_text(size=12, face="bold"),
          axis.title=element_text(size=12,face="bold")) +
    theme(legend.position="top") +
    geom_hline(aes(yintercept=4000))


ggplot(overviewTab, aes(factor(variable2), caloriesAverage)) + 
    geom_boxplot(aes(fill = factor(variable2))) + geom_jitter() + coord_flip() +
    xlab('') + ylab('Total calories per person per day') +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    #scale_x_continuous(limits = c(1991, 2013), breaks = seq(1991, 2013, 1)) +
    #scale_y_continuous(limits = c(0, 4500), breaks = seq(0, 4500, 250), labels = space) +
    scale_y_continuous(limits = c(2000, 3200),
                       breaks = seq(2000, 3200, 100),
                       labels = space) +
    theme(legend.title = element_blank()) +
    ggtitle("World") +
    theme(plot.title = element_text(size=20)) + 
    theme(legend.position="none")


oldNewTotalCaloriesMelt[, list(caloriesAverage = mean(value)),
                        by = list(variable, variable2)]




system.time(for (country_name in c("Afghanistan", "Brazil", "China, Macao", "China, Main", "India", 
                                   "Indonesia", "Thailand", "United Republic of Tanzania", "United States of America")) {
    
ggplot(oldNewTotalCaloriesMelt[geographicAreaM49_description %in% c("Brazil", "China, Main", "India",
                                                                    "United States of America")], 
               aes(x = as.numeric(timePointYears), y = value, colour=variable2, group = variable2)) + 
        # geom_bar(stat="identity") +
        geom_line(position=position_dodge(width=0.4), size = 1.2) +
        #facet_wrap(~ geographicAreaM49_description, scales = "free") +
        facet_wrap(~ geographicAreaM49_description) +
        # scale_fill_manual(values=c("#FA8258", "#088A08")) +
        ylab('Total calories per person per day') + xlab('Year') +
        theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
        scale_x_continuous(limits = c(1990, 2014), breaks = seq(1990, 2014, 2)) +
        scale_y_continuous(labels = space) +
        theme(legend.title = element_blank()) +
        # ggtitle(country_name) +
        theme(plot.title = element_text(size=20)) +
        theme(legend.text=element_text(size=14)) +
        theme(axis.text=element_text(size=12, face="bold"),
              axis.title=element_text(size=12,face="bold")) +
        theme(legend.position="top") +
        geom_hline(aes(yintercept=4000))
    
    print(p)
    
}

)


dev.off()



ggplot(oldNewFoodData[region == reg & measuredItemCPC_description == item], 
       aes(x = GDP , y = newFoodDataPC, group = Official, colour = Official)) +
    geom_line(aes(group=1), size = 1) +
    facet_wrap(~ countryElast2, scales = "free") +
    xlab('GDP per capita (constant 2005 US$)') + ylab('Food Estimate per capita (kg/person)') +
    ggtitle(paste(reg, unique(oldNewFoodData[measuredItemCPC_description == item]$measuredItemCPC_description), sep=" - "))

