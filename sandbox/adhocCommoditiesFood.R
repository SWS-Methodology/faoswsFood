
library(data.table)
library(faosws)
library(dplyr)
library(faoswsUtil)
#library(reshape2)

## To do:
## - We have to figure it out which commodities we should have in the food module.
## We are using the historical data and shares table to find a solution.

## set up for the test environment and parameters
R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE <- Sys.getenv("R_DEBUG_MODE")
overwritableFlags = c("M", "I")

if(!exists("DEBUG_MODE") || DEBUG_MODE == "") {
  if(Sys.info()[7] == "josh"){ # Josh's work computer
    #R_SWS_SHARE_PATH <- "/media/hqlprsws1_qa/"
    SetClientFiles(dir = "~/R certificate files/QA/")
    files = dir("~/Documents/Github/faoswsFood/R",
                full.names = TRUE)
    token = "b42c86bc-ee6c-418c-9741-6747786e9bc1"
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

elements = c("51", "61", "71", "91", "101", "131", "141")

# 51 production (Mt)
# 61 imports (Mt)
# 71 stock variation (Mt) 
# 91 exports (Mt)  
# 101 feed (Mt)
# 131 processed (Mt)
# 141 food (Mt)

# Data
key = DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD", dimensions = list(
  Dimension(name = "geographicAreaFS", keys = GetCodeList("faostat_one", "FS1_SUA_UPD", "geographicAreaFS")[, code]),
  Dimension(name = "measuredElementFS", keys = elements),
  Dimension(name = "timePointYears", keys = as.character(2005:2011)),
  Dimension(name = "measuredItemFS", keys = GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredItemFS")[, code]))
)
data = GetData(key)
data <- dcast.data.table(data,
                         geographicAreaFS + measuredItemFS + timePointYears ~ measuredElementFS,
                         value.var = "Value")

setnames(data, old=c("101", "131", "141", "51", "61", "71", "91"),
         new=c("feed", "processed", "food", "production", "imports", "stockVariation", "exports"))


data[, geographicAreaM49 := fs2m49(as.character(geographicAreaFS))]

data[, measuredItemFS := formatC(as.numeric(measuredItemFS), width = 4,
                                    flag = "0")]
data[, measuredItemCPC := fcl2cpc(as.character(measuredItemFS))]
data

# Checking

for(cname in c("feed", "processed", "food", "production", "imports",
                 "exports", "stockVariation")){
  data[is.na(get(cname)), c(cname) := 0]
}

data[, foodByProduction := food/production]
data[, foodByImports := food/imports]
data[, foodByProcessingFood := food/(processed + food)]
data[, foodBySupply := food/(production + imports - exports + stockVariation)]
data[, foodBySupply2 := food/(production + imports - exports)]



data[foodBySupply > 0]

data[, .N, measuredItemCPC]
data[, .N, geographicAreaM49]


data[measuredItemCPC == "0111" & food > 0 & geographicAreaFS == "170"]


# Shares table

SharesKey = DatasetKey(domain = "agriculture", dataset = "aupus_share", dimensions = list(
  Dimension(name = "geographicAreaM49", keys = GetCodeList("agriculture", "aupus_share", "geographicAreaM49")[, code]),
  Dimension(name = "measuredShare", keys = "1"),
  Dimension(name = "timePointYearsSP", keys = as.character(2007:2011)),
  Dimension(name = "measuredItemParentCPC", keys = GetCodeList("agriculture", "aupus_share", "measuredItemParentCPC")[, code]),
  Dimension(name = "measuredItemChildCPC", keys = GetCodeList("agriculture", "aupus_share", "measuredItemChildCPC")[, code]))
)
sharesData = GetData(SharesKey)

# levels of commodities
commodityLevel <- getCommodityLevel(commodityTree=sharesData, parentColname ='measuredItemParentCPC', 
                                    childColname='measuredItemChildCPC')

commodityLevel[, .N, node]
setnames(commodityLevel, "node", "measuredItemCPC")

commodityLevel[, .N, measuredItemCPC]

# merge with the commodity level table
data <- merge(data, commodityLevel, by="measuredItemCPC", all.x=T)
head(data)

data[!is.na(level)]

data[is.na(foodBySupply2) & level == 0, .N, measuredItemCPC]

# Aggregating ratios

data = data[!is.na(measuredItemCPC), ]
data = data[foodBySupply <= 1 | is.na(foodBySupply), ]

data[, cpcSubCode := substring(measuredItemCPC, 1, 3)]
stats = data[, list(foodByProduction = median(foodByProduction, na.rm = TRUE),
                    foodBySupply = median(foodBySupply, na.rm = TRUE),
                    count = .N),
             by = c("cpcSubCode", "level")]
statsCPC = data[, list(foodByProduction = median(foodByProduction, na.rm = TRUE),
                    foodBySupply = median(foodBySupply, na.rm = TRUE),
                    count = .N),
             by = c("measuredItemCPC", "level")]


library(rpart)
treeModel = rpart(foodBySupply ~ measuredItemCPC, data = data[!is.na(foodBySupply), ])
names(treeModel)
usedCPCcodes = unique(data[!is.na(foodBySupply), measuredItemCPC])
predsWithLevel = commodityLevel[measuredItemCPC %in% usedCPCcodes, ]
predsWithLevel$predictions = predict(treeModel, newdata = predsWithLevel)
predsWithLevel[, fcl := cpc2fcl(measuredItemCPC, returnFirst = TRUE)]

predsWithLevel[substring(measuredItemCPC, 1, 3) == "215", ][order(predictions)]
data[, prodSmallPosTrade := exports < imports & production / imports < .1]
data[, productionByNetTrade := production / (exports - imports)]


data[, foodByTrade := food/abs(imports - exports)]
library(ggplot2)
foodCPCCodes = predsWithLevel[predictions > .5, measuredItemCPC]
subData = data[measuredItemCPC %in% foodCPCCodes, ]
qplot(subData[, foodByTrade], binwidth = .1) + scale_x_log10(breaks = 10^(-3:5))

subData[, foodByTradeLog := log(foodByTrade)]

treeModel2 = rpart(foodByTradeLog ~ measuredItemCPC,
                   data = subData[abs(foodByTradeLog) < Inf &
                                 !is.na(foodByTradeLog) &
                               level != 0, ])
#                   control = rpart.control(cp = 0.001))
library(rpart.plot)
prp(treeModel2, type = 1)

quantile(subData$foodByTradeLog[abs(subData$foodByTradeLog) < Inf &
                                  subData$level != 0],
         na.rm = TRUE, probs = 0:39/40)
fit = kmeans(subData[!is.na(foodByTradeLog) & abs(foodByTradeLog) < Inf & level != 0,
                     foodByTradeLog], centers = 4, nstart = 10)
fit$centers
fit = lm(foodByTradeLog ~ measuredItemCPC,
         data = subData[!is.na(foodByTradeLog) & abs(foodByTradeLog) < Inf &
                          level != 0, ])
summary(fit)


