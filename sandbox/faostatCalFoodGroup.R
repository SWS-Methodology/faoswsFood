
completeImputationKey@dataset <- "fooddatafs_"
completeImputationKey@dimensions$timePointYears@keys <- as.character(1990:2014)

foodDataFaostat <- GetData(completeImputationKey, flags = TRUE)
setnames(foodDataFaostat, "Value", "food")
foodDataFaostat[, type := getCommodityClassification(as.character(measuredItemCPC))]
foodDataFaostat = foodDataFaostat[type %in% c("Food Estimate", "Food Residual")]


## computing calories per person per day from the old food data

caloriesData <- faoswsUtil::getNutritiveFactors(geographicAreaM49 = foodDataFaostat$geographicAreaM49,
                                                measuredElement = "261",
                                                measuredItemCPC = foodDataFaostat$measuredItemCPC,
                                                timePointYearsSP = foodDataFaostat$timePointYears)
setnames(caloriesData,
         c("timePointYearsSP", "Value"),
         c("timePointYears", "valueCal"))

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

## Merge the food data with calories data

foodDataFaostat <- merge(
    foodDataFaostat, 
    caloriesData[, c(keys, "valueCal"), with = F],
    by = keys, all.x=T)

foodDataFaostat

## merge with pop
keys = c("geographicAreaM49", "timePointYears")
foodDataFaostat <- merge(
    foodDataFaostat, 
    popData[, c(keys, "population"), with = F],
    by = keys, all.x=T)

foodDataFaostat

foodDataFaostat[, calPerPersonPerDay := (food * valueCal * 10000) / 365 / (population * 1000)]

## Food Groups
fbsTree = fread("C:/Users/caetano/Documents/fbs_tree/FbsTree.csv")
fbsTree[, .N, fbsItemTOT_description]
fbsTree[, .N, fbsItemAggregated_description]
setnames(fbsTree, "measuredItem", "measuredItemCPC")

## Merge
foodDataFaostat = merge(foodDataFaostat, 
                    fbsTree[, c("measuredItemCPC", "fbsItemTOT_description", "fbsItemAggregated_description"), with = F],
                    by = "measuredItemCPC", all.x = T)

foodDataFaostat <- nameData("food", "fooddatafs_", foodDataFaostat)
foodDataFaostat[, c("timePointYears_description", "measuredElement", 
                    "measuredElement_description", "flagObservationStatus", 
                    "flagMethod", "type") := NULL]

calorieCountryDay <- foodDataFaostat[, list(totalCalDay = sum(calPerPersonPerDay, na.rm=T),
                                     numbCommodities = .N),
                              by = list(geographicAreaM49, geographicAreaM49_description, timePointYears)]


keys = c("geographicAreaM49", "timePointYears")

## calories by country
countriesZeroCal = calorieCountryDay[
    totalCalDay == 0, .N, c("geographicAreaM49_description", 
                            "timePointYears")][, c("geographicAreaM49_description",
                                                   "timePointYears"),with = F]


foodDataFaostat = merge(foodDataFaostat, calorieCountryDay[, c("geographicAreaM49", "timePointYears", "totalCalDay", "numbCommodities"), with = F],
      by = keys, all.x = T)

foodDataFaostat = foodDataFaostat[totalCalDay > 1000]
foodDataFaostat[, c("numbCommodities", "totalCalDay", "valueCal", "population", "food") := NULL]

setcolorder(foodDataFaostat, c("geographicAreaM49", "geographicAreaM49_description",
                               "measuredItemCPC", "measuredItemCPC_description", "timePointYears",
                               "food", "calPerPersonPerDay", "fbsItemTOT_description", 
                               "fbsItemAggregated_description"))

foodDataFaostat = foodDataFaostat[!is.na(fbsItemAggregated_description)]

calCountryFoodGroup <- foodDataFaostat[, list(calPerPersonPerDay = sum(calPerPersonPerDay, na.rm = T)),
                by = list(geographicAreaM49, geographicAreaM49_description, 
                          timePointYears, fbsItemTOT_description, fbsItemAggregated_description)]

foodDataFaostat
save(calCountryFoodGroup, file = "calCountryFoodGroup.RData")


