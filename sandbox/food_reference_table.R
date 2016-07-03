
## EA
GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:9453/sws", "b4d57573-7cbd-46bd-94b0-a4b10f6e7d66")

## Read the new version 

library(data.table)
library(faosws)

## old version : SWS

oldFoodClassification = ReadDatatable("food_classification", readOnly = F)
oldFoodClassification
oldFoodClassification[measured_item_cpc == "0112"]

## new version : .csv
dlpath <- file.path("T:", "Team_working_folder", "A", "FBS-Modules", "Food", "reference_table", "food_group_2016_06_02.csv")

newFoodClassification <- fread(dlpath, colClasses = c("character", "character", "character",
                                          "character", "character"))
newFoodClassification[measuredItemCPC == "0112"]
setnames(newFoodClassification, "measuredItemCPC", "measured_item_cpc")

## find the level
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
setnames(commodityLevel, "node", "measured_item_cpc")

# merge with the commodity level table
newFoodClassification <- merge(newFoodClassification, commodityLevel, 
                           by = "measured_item_cpc", 
                           all.x = T
)

setnames(newFoodClassification, "last_Version_2/June", "type")
newFoodClassification[, c("last_Version", "first_version") := NULL]
newFoodClassification[measured_item_cpc != "0112", measured_item_fcl := cpc2fcl(measured_item_cpc)]

setcolorder(newFoodClassification, c("measured_item_cpc", "measured_item_fcl", 
                                     "description", "level", "type"))

# Step 1: Delete

oldFoodClassification = ReadDatatable("food_classification", readOnly = F)

changeset = Changeset("food_classification", type = "write")

AddDeletions(changeset, oldFoodClassification[1:nrow(oldFoodClassification), ])
Finalise(changeset)


# Step 2; Put new data

AddInsertions(changeset, newFoodClassification)
Finalise(changeset)


