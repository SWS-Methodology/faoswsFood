

## forecast production from 2014 to 2016

## Production

productionKey = DatasetKey(
    domain = "agriculture",
    dataset = "aproduction",
    dimensions = list(
        Dimension(name = "geographicAreaM49",
                  keys = unique(dataToSave$geographicAreaM49)),
        Dimension(name = "measuredElement",
                  keys = "5510"),
        Dimension(name = "measuredItemCPC",
                  keys = unique(dataToSave$measuredItemCPC)),
        Dimension(name = "timePointYears",
                  keys = as.character(minYearToProcess:2013)))
)

productionData = GetData(
    productionKey,
    flags = TRUE)


productionData[, .N, timePointYears]
productionData[, c("measuredElement", "flagObservationStatus", "flagMethod") := NULL]


averageTab = productionData[timePointYears %in% c("2011", "2012", "2013"), 
                            list(#averagePercenPoints = mean(percent, na.rm = T),
                                Value = mean(Value, na.rm = T)),
                            by = list(geographicAreaM49, measuredItemCPC)]

averageTab[, timePointYears := "2014"]
setcolorder(averageTab, c("geographicAreaM49", "measuredItemCPC", 
                          "timePointYears", "Value"))
productionData = rbind(productionData, averageTab)

# 2012 - 2013 - 2014
averageTab = productionData[timePointYears %in% c("2012", "2013", "2014"), 
                            list(#averagePercenPoints = mean(percent, na.rm = T),
                                Value = mean(Value, na.rm = T)),
                            by = list(geographicAreaM49, measuredItemCPC)]


averageTab[, timePointYears := "2015"]
setcolorder(averageTab, c("geographicAreaM49", "measuredItemCPC", 
                          "timePointYears", "Value"))
productionData = rbind(productionData, averageTab)


# 2013 - 2014 - 2015
averageTab = productionData[timePointYears %in% c("2013", "2014", "2015"), 
                            list(#averagePercenPoints = mean(percent, na.rm = T),
                                Value = mean(Value, na.rm = T)),
                            by = list(geographicAreaM49, measuredItemCPC)]


averageTab[, timePointYears := "2016"]
setcolorder(averageTab, c("geographicAreaM49", "measuredItemCPC", 
                          "timePointYears", "Value"))
productionData = rbind(productionData, averageTab)

productionData

productionDataForecasted = productionData[timePointYears %in% c("2014", "2015", "2016")]

save(productionDataForecasted, file = "sandbox/planB/productionDataForecasted.RData")

