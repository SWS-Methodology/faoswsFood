library(data.table)
library(faosws)
library(dplyr)
library(faoswsUtil)
#library(reshape2)

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

if(swsContext.computationParams$yearToProcess < 1999)
    stop("This module was designed for imputation on years after 1998 only!")

cat("Defining variables/dimensions/keys/...\n")

## Define the keys that we'll need for all the dimensions
## set the keys to get the population data from the FAO working system
# areaCodesM49 <- swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
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
# yearCodes <- as.numeric(swsContext.computationParams$yearToProcess) + -1:15
# yearCodes <- as.character(yearCodes)
## GDP per capita (constant 2500 US$) is under this key
# gdpCodes <- "NY.GDP.PCAP.KD"
## The element 21 contains the FBS population numbers
# populationCodes <- "21"
## The element 141 contains the FBS food numbers

areaCodesFoodFactors <- GetCodeList("food", "food_factors","geographicAreaM49")$code
comCodesFoodFactors <- GetCodeList("food", "food_factors","foodCommodityM")$code
fdmCodesFoodFactors <- GetCodeList("food", "food_factors","foodFdm")$code
funCodesFoodFactors <- GetCodeList("food", "food_factors","foodFunction")$code
varCodesFoodFactors <- "y_e" ## Only need elasticities from the food domain table

## Define the dimensions
dimM49FoodFactors <- Dimension(name = "geographicAreaM49", keys = areaCodesFoodFactors)
dimComFoodFactors <- Dimension(name = "foodCommodityM", keys = comCodesFoodFactors)
dimFdmFoodFactors <- Dimension(name = "foodFdm", keys = fdmCodesFoodFactors)
dimFunFoodFactors <- Dimension(name = "foodFunction", keys = funCodesFoodFactors)
dimVarFoodFactors <- Dimension(name = "foodVariable", keys = varCodesFoodFactors)

## Define the keys
keyFdmFoodFactors <- DatasetKey(domain = "food", dataset = "food_factors",
                     dimensions = list(dimM49FoodFactors, dimComFoodFactors, 
                                       dimFdmFoodFactors, dimFunFoodFactors, 
                                       dimVarFoodFactors))

## download the food dimension data (elasticities) from the SWS
foodFactors <- GetData(keyFdmFoodFactors, flags=FALSE, normalized = FALSE)
foodFactors <- nameData("food", "food_factors", foodFactors)
setnames(foodFactors, "Value_foodVariable_y_e", "elasticity")
setnames(foodFactors, "foodFdm", "foodDemand")
setnames(foodFactors, "foodCommodityM", "foodCommodity")


foodFactors[, .N, geographicAreaM49]
foodFactors[, .N, foodCommodity]
foodFactors[, .N, foodDemand]
foodFactors[, .N, foodFunction]

foodFactors[, list(geographicAreaM49 = length(unique(geographicAreaM49)),
                   foodCommodity = length(unique(foodCommodity)),
                   foodDemand = length(unique(foodDemand)),
                   foodFunction = length(unique(foodFunction)),
                   nrows = nrow(foodFactors))]

foodFactors[, c("foodFdm_description", "foodFunction_description") := NULL]

## Food Factors Nat

areaCodesFoodFactorsNat <- GetCodeList("food", "food_factors_n","geographicAreaM49")$code
comCodesFoodFactorsNat <- GetCodeList("food", "food_factors_n","foodCommodityM")$code
fdmCodesFoodFactorsNat <- GetCodeList("food", "food_factors_n","foodFdm")$code
funCodesFoodFactorsNat <- GetCodeList("food", "food_factors_n","foodFunction")$code
varCodesFoodFactorsNat <- "y_e" ## Only need elasticities from the food domain table

## Define the dimensions
dimM49FoodFactorsNat <- Dimension(name = "geographicAreaM49", keys = areaCodesFoodFactorsNat)
dimComFoodFactorsNat <- Dimension(name = "foodCommodityM", keys = comCodesFoodFactorsNat)
dimFdmFoodFactorsNat <- Dimension(name = "foodFdm", keys = fdmCodesFoodFactorsNat)
dimFunFoodFactorsNat <- Dimension(name = "foodFunction", keys = funCodesFoodFactorsNat)
dimVarFoodFactorsNat <- Dimension(name = "foodVariable", keys = varCodesFoodFactorsNat)



keyFdmNat <- DatasetKey(domain = "food", dataset = "food_factors_n",
                     dimensions = list(dimM49FoodFactorsNat, dimComFoodFactorsNat,
                                       dimFdmFoodFactorsNat, dimFunFoodFactorsNat,
                                       dimVarFoodFactorsNat))

foodFactorsNat <- GetData(keyFdmNat, flags=FALSE, normalized = FALSE)
foodFactorsNat <- nameData("food", "food_factors_n", foodFactorsNat)
setnames(foodFactorsNat, "Value_foodVariable_y_e", "elasticityNat")
setnames(foodFactorsNat, "foodFdm", "foodDemand")
setnames(foodFactorsNat, "foodCommodityM", "foodCommodity")

foodFactorsNat[, c("foodFdm_description", "foodFunction_description") := NULL]

foodFactorsNat[, list(geographicAreaM49 = length(unique(geographicAreaM49)),
                   foodCommodity = length(unique(foodCommodity)),
                   foodDemand = length(unique(foodDemand)),
                   foodFunction = length(unique(foodFunction)),
                   nrows = nrow(foodFactorsNat))]



foodFactorsTab <- foodFactors[, list(geographicAreaM49 = length(unique(geographicAreaM49)),
                   foodCommodity = length(unique(foodCommodity)),
                   foodDemand = length(unique(foodDemand)),
                   foodFunction = length(unique(foodFunction)),
                   nrows = nrow(foodFactors))]

foodFactorsTab <- foodFactorsTab[, dataset:= "food_factors"]
setcolorder(foodFactorsTab, c("dataset", "nrows", "geographicAreaM49", "foodCommodity", "foodDemand", "foodFunction"))



foodFactorsNatTab <- foodFactorsNat[, list(geographicAreaM49 = length(unique(geographicAreaM49)),
                      foodCommodity = length(unique(foodCommodity)),
                      foodDemand = length(unique(foodDemand)),
                      foodFunction = length(unique(foodFunction)),
                      nrows = nrow(foodFactorsNat))]
foodFactorsNatTab <- foodFactorsNatTab[, dataset:= "food_factors_nat"]
setcolorder(foodFactorsNatTab, c("dataset", "nrows", "geographicAreaM49", "foodCommodity", "foodDemand", "foodFunction"))

rbind(foodFactorsTab, foodFactorsNatTab)

kable(rbind(foodFactorsTab, foodFactorsNatTab), 
      format = "markdown", padding = 0)


# merge the datasets

keys = c("geographicAreaM49", "geographicAreaM49_description", "foodCommodity", 
         "foodCommodityM_description", "foodDemand", "foodFunction")
test <- merge(foodFactors, foodFactorsNat, by = keys, all = T)
test[, diff := elasticity - elasticityNat]
test[diff != 0]
summary(test$diff, na.rm=T)
min(test$diff, na.rm = T)

foodFactors[foodCommodity == "2500", .N, geographicAreaM49_description]
foodFactorsNat[foodCommodity == "2500", .N, geographicAreaM49_description]


test[, .N, geographicAreaM49]
test[is.na(elasticity), .N, geographicAreaM49_description]
test[is.na(elasticityNat), .N, geographicAreaM49_description]

