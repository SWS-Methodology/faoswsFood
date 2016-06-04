# install.packages("XLConnect")
library(XLConnect)
library(data.table)
library(faosws)
library(faoswsUtil)
library(xlsx)

## set up for the test environment and parameters
R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE <- Sys.getenv("R_DEBUG_MODE")
overwritableFlags = c("M", "I")

if(!exists("DEBUG_MODE") || DEBUG_MODE == "") {
    if(Sys.info()[7] == "josh"){ # Josh's work computer
        R_SWS_SHARE_PATH <- "/media/hqlprsws1_qa/"
        SetClientFiles(dir = "~/R certificate files/QA/")
        files = dir("~/Documents/Github/faoswsFood/R",
                    full.names = TRUE)
        token = "557f0e65-5f84-43b0-a021-fe3bf3f02316"
    } else if(Sys.info()[7] == "caetano"){ # bruno's work computer
        SetClientFiles(dir = "~/.R/QA/")
        R_SWS_SHARE_PATH = "//hqlprsws1.hq.un.fao.org/sws_r_share"
        files = dir("~/Github/faoswsFood/R", full.names = TRUE)
        token = "66a36f31-1a29-4a49-8626-ae62117c251a"
    } else {
        stop("User not yet implemented!")
    }  
    
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = token
    )
    #R_SWS_SHARE_PATH <- "/media/hqlprsws1_qa/"
    sapply(files, source)
}


## This table is the last version of the food group revised for "Gigi and Daniela"
foodGroup20160426 <- readWorksheet(
    loadWorkbook(
        "C:\\Users\\caetano\\Desktop\\food_commodity_list\\ReferenceFile_SUACommodityList_26042016 (002).xlsx"
        ), 
    sheet = 4)


head(foodGroup20160426, 20)

foodGroup20160426 <- foodGroup20160426[18:nrow(foodGroup20160426), ]
foodGroup20160426 <- data.table(foodGroup20160426)
foodGroup20160426 <- foodGroup20160426[, c("Col5", "Col6", "Col7", "Col11"), with = F]
setnames(foodGroup20160426, old = c("Col5", "Col6", "Col7", "Col11"),
         new = c("measuredItemFCL", "measuredItemCPC", "description", "type"))

foodGroup20160426[, .N, measuredItemCPC][order(-N)]
foodGroup20160426[, .N, description][order(-N)]

## This table has two problematic commodities (maize (corn) and tomato juice)
foodGroup20160426[measuredItemCPC == "0112", 
                  type := "Food Estimate"]

foodGroup20160426[measuredItemCPC == "21321", 
                  type := "Food residual"]

## Making sure everything is correct
foodGroup20160426[, list(n = length(unique(type))),
                  by = measuredItemCPC][order(n)]

foodGroup20160426[, list(n = length(unique(type))),
                  by = description][order(-n)]

## Excluding duplicated

foodGroup20160426 <- foodGroup20160426[, list(type = max(type)),
                  by = list(measuredItemCPC, measuredItemFCL, description)]

foodGroup20160426

# Shares table

SharesKey = DatasetKey(domain = "agriculture", dataset = "aupus_share", dimensions = list(
    Dimension(name = "geographicAreaM49", keys = GetCodeList("agriculture", "aupus_share", "geographicAreaM49")[, code]),
    Dimension(name = "measuredShare", keys = "1"),
    Dimension(name = "timePointYearsSP", keys = as.character(2007:2011)),
    Dimension(name = "measuredItemParentCPC", keys = GetCodeList("agriculture", "aupus_share", "measuredItemParentCPC")[, code]),
    Dimension(name = "measuredItemChildCPC", keys = GetCodeList("agriculture", "aupus_share", "measuredItemChildCPC")[, code]))
)
sharesData = GetData(SharesKey)

## Levels of commodities
commodityLevel <- faoswsUtil:::getCommodityLevel(
    commodityTree = sharesData, 
    parentColname = 'measuredItemParentCPC', 
    childColname = 'measuredItemChildCPC'
    )

commodityLevel[, .N, node]
setnames(commodityLevel, "node", "measuredItemCPC")

# merge with the commodity level table
foodGroup20160426 <- merge(foodGroup20160426, commodityLevel, 
                           by = "measuredItemCPC", 
                           all.x = T
                           )

setcolorder(foodGroup20160426, 
            c("measuredItemCPC", "measuredItemFCL", "description", "level", "type")
            )

foodGroup20160426[, .N, measuredItemCPC][order(-N)]
foodGroup20160426[, .N, measuredItemFCL][order(-N)]
foodGroup20160426[, .N, type]

foodGroup20160426[type == "Food estimate", type := "Food Estimate"]
foodGroup20160426[type == "food estimate", type := "Food Estimate"]
foodGroup20160426[type == "Standardized commodity", type := "Standardized Commodity"]
foodGroup20160426[type == "Non-food commodity", type := "Non-food Commodity"]
foodGroup20160426[type == "Non-food commodity", type := "Non-food Commodity"]


setwd("C:\\Users\\caetano\\Desktop\\food_commodity_list")
write.xlsx(foodGroup20160426, "food_group.xlsx", row.names = F)


## Let's compare the first version and the last one

## This table is the first  version of the food group 

firstVersion <- fread("C:\\Users\\caetano\\Desktop\\food_commodity_list\\food_group.csv")


head(firstVersion)


## the last version

lastVersion <- foodGroup20160426[, list(type = max(type)),
                                 by=list(measuredItemCPC, description)]



setnames(firstVersion, "foodGroup", "first_version")

setnames(lastVersion, "type", "last_Version")

dataMerge <- merge(lastVersion, firstVersion[, c("description", "first_version"), with = FALSE], 
                   by = "description", all.x = T)



head(dataMerge)

table(dataMerge$first_version, dataMerge$last_Version)

getwd()

write.xlsx(dataMerge, "food_group_last_first_version.xlsx", row.names = F)
