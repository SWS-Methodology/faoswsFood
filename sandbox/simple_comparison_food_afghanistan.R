
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
    SETTINGS <- ReadSettings("C:/Users/caetano/Documents/Github/faoswsFood/modules/impute_food/sws.yml")
    
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



####
getwd()

#setwd("Libraries\Documents")

load("foodData.RData")
library(data.table)
names(foodData)

foodData[geographicAreaM49 == 4 & measuredItemCPC == "23110"]

oldFoodDataAfghanistan = foodData[geographicAreaM49 == 4]


load("newFoodDataAfghanistan.RData")

setnames(newFoodDataAfghanistan, "Value", "newValue")
newFoodDataAfghanistan


afghanistanMerge = merge(newFoodDataAfghanistan, 
oldFoodDataAfghanistan[, c("measuredItemCPC", "timePointYears", "Value"), with = F], 
by = c("measuredItemCPC", "timePointYears"), all.x = T)


afghanistanMerge[, diff := newValue - Value] 

afghanistanMerge = nameData("agriculture", "aproduction", afghanistanMerge)
afghanistanMerge[is.na(diff), .N, measuredItemCPC]

afghanistanMerge[, type := getCommodityClassification(as.character(measuredItemCPC))]
afghanistanMerge[is.na(diff), .N, c("measuredItemCPC", "measuredItemCPC_description", "type")]

afghanistanMerge 
