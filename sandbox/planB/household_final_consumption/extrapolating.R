
## Here we're going to extrapolate Household Final Consumption for 2016.
## The approch used was the same that Marie used for extrapolating GDP for 2014-2016.

## Load and prepare data

## Our variable is: 
# CONS_S14_USD2005:	Household consumption expenditure (including Non-profit institutions serving households)

library('forecast')
library("xlsx")
library("dplyr")
library('reshape2')


householdConsumpExpend = read.csv(
    "sandbox/planB/household_final_consumption/Routput_CONS_dataCapture.csv"
    # , dec = ".", sep = ","
    )

head(householdConsumpExpend)

hhConsExp <- select(householdConsumpExpend, FAOCode, FAOName, year, CONS_S14_USD2005)
hhConsExp <- dcast(hhConsExp, FAOCode + FAOName ~ year, value.var = "CONS_S14_USD2005")
head(hhConsExp)

# Drop former economies
hhConsExp <- hhConsExp[!(is.na(hhConsExp$'2015')), ]

a <- which(colnames(hhConsExp) == 1970)
b <- dim(hhConsExp)[2]

dataHhConsExp <- data.matrix(hhConsExp[,a:b], rownames.force = NA)
nrows <- dim(dataHhConsExp)[1]
col2015 <- dim(dataHhConsExp)[2]

# SECTION II. --------------------------------------------------------------
# Create empty matrix to store forecated values
store_forecast <- matrix( NA, nrow = nrows, ncol = col2015+1)

# Loop over all countries, select best ARIMA model and produce forecast
for (i in 1:nrows){
    #1. Delta-log transformation
    ldata <- log(dataHhConsExp[i,])
    ldata.ts <- as.ts(ldata)
    #2. Outlier detection and replacement
    x <- tsoutliers(ldata.ts)
    ldata.ts[x$index] <- x$replacements
    dldata.ts <- diff(ldata.ts, lag=1) 
    dldata <- 100*dldata.ts
    # 3. Select best ARIMA model and use it for forecasting
    fit1 <- forecast::auto.arima(x=dldata)
    fit1
    temp <- forecast( fit1, h=1 ) 
    plot.forecast( forecast( fit1, h=1 ))
    # Revert the delta log forecasted values to levels and store them
    level_forecast <- dataHhConsExp[i,col2015]*exp(temp$mean[1]/100)
    store_forecast[i,1:col2015] <- exp(ldata.ts)
    store_forecast[i,col2015+1] <- level_forecast
    
    rm(level_forecast, fit1, temp, x)
}


# SECTION III. --------------------------------------------------------------
# Save results

# Original GDP series with 2016 forecast appended
results <- cbind(dataHhConsExp, store_forecast[,47])
colnames(results)[47] <- "2016"
results <- cbind(hhConsExp[,1:2], results)
library(data.table)
results = data.table(results)
hhConsExpForecasted = melt.data.table(results, id.vars = c("FAOCode", "FAOName"))
setnames(hhConsExpForecasted, "variable", "timePointYears")
setnames(hhConsExpForecasted, "value", "hhConsExp")
hhConsExpForecasted[, timePointYears := as.numeric(as.character(timePointYears))]


# hhConsExpForecasted[, previousValue := shift(hhConsExp),
#                     by = list(FAOCode, FAOName)]
# 
# hhConsExpForecasted[, percent := hhConsExp/previousValue, 
#                     by = list(FAOCode, FAOName)]
# 
# ggplot(hhConsExpForecasted[FAOName == "India"],
#        aes(x=as.numeric(timePointYears), y=hhConsExp, group=1, col = type)) +
#     geom_line(colour = "#5882FA", size = .8) +
#     scale_x_continuous(limits = c(1991, 2016), breaks = seq(1991, 2016, 1)) 
# 
# hist(hhConsExpForecasted[timePointYears == 2016]$percent)
# 
# hhConsExpForecasted[percent > 1.05 & timePointYears == 2016]
# hhConsExpForecasted[FAOName == "United States of America"]
# 
# ggplot(hhConsExpForecasted[timePointYears == 2016],
#        aes(y=percent)) +
#     geom_line() +
#     scale_x_continuous(limits = c(1991, 2016), breaks = seq(1991, 2016, 1)) 


write.table(
    hhConsExpForecasted, 
    file = "sandbox/planB/household_final_consumption/household_final_consumptionhhConsExpForecasted.csv", 
    row.names = F)

