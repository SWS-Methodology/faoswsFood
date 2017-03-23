## comparison between old net trade, old food and new net trade

suppressMessages({
    library(data.table)
    library(faosws)
    library(dplyr)
    library(faoswsUtil)
    library(faoswsFood)
    library(faoswsFlag)
    library(countrycode)
    library(zoo)
    library(ggplot2)
    library(scales)
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

completeImputationKey = getCompleteImputationKey("food")
completeImputationKey@dataset <- "fooddatafs_"
completeImputationKey@dimensions$timePointYears@keys <- as.character(2000:2014)

foodFAOSTAT <- GetData(completeImputationKey, flags = TRUE)
setnames(foodFAOSTAT, "Value", "food")
foodFAOSTAT[, type := getCommodityClassification(as.character(measuredItemCPC))]

## New Net Trade Data

tradeCode <- c("5610", "5910")
newTotalTradeKey = DatasetKey(
    domain = "trade",
    dataset = "total_trade_cpc_m49",
    dimensions = list(
        Dimension(name = "geographicAreaM49",
                  keys = unique(foodFAOSTAT$geographicAreaM49)),
        Dimension(name = "measuredElementTrade", keys = tradeCode),
        Dimension(name = "timePointYears", keys = as.character(2000:2014)),
        Dimension(name = "measuredItemCPC",
                  keys = unique(foodFAOSTAT$measuredItemCPC))
    )
)

newTotalTradeData = GetData(
    newTotalTradeKey,
    flags = FALSE)

newTotalTradeData <- dcast.data.table(newTotalTradeData, geographicAreaM49 + measuredItemCPC +
                                          timePointYears ~ measuredElementTrade, value.var = "Value")


setnames(newTotalTradeData, "5610", "imports")
setnames(newTotalTradeData, "5910", "exports")

newTotalTradeData[is.na(imports), imports := 0]
newTotalTradeData[is.na(exports), exports := 0]
newTotalTradeData[, newNetTrade := (imports - exports)]

## Old Net Trade Data

oldTotalTradeData <- getTotalTradeDataFAOSTAT1(unique(foodFAOSTAT$geographicAreaM49), 
                                                   unique(foodFAOSTAT$measuredItemCPC),
                                                   as.character(2000:2014))

oldTotalTradeData <- dcast.data.table(oldTotalTradeData, geographicAreaM49 + measuredItemCPC + 
                                              timePointYears ~ measuredElement, value.var = "Value")

setnames(oldTotalTradeData, "5610", "imports")
setnames(oldTotalTradeData, "5910", "exports")

oldTotalTradeData[is.na(imports), imports := 0]
oldTotalTradeData[is.na(exports), exports := 0]
oldTotalTradeData[, oldNetTrade := (imports - exports)]

## Merge between foodFAOSTAT, oldTotalTrade and the newTotalTrade

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")
foodFAOSTAT = merge(
    foodFAOSTAT, 
    oldTotalTradeData[, c(keys, "oldNetTrade"), with = F],
      by = keys, all.x = T)

foodFAOSTAT = merge(
    foodFAOSTAT, 
    newTotalTradeData[, c(keys, "newNetTrade"), with = F],
    by = keys, all.x = T)

foodResidual <- foodFAOSTAT[type == "Food Residual"]

## checking food figures == 0

calorieCountryDay <- foodResidual[, list(totalFood = sum(food, na.rm=T)),
                                  by = list(geographicAreaM49, timePointYears)]

countriesZeroFood = calorieCountryDay[totalFood == 0, .N, geographicAreaM49][, geographicAreaM49]

foodResidual = foodResidual[!(geographicAreaM49 %in% countriesZeroFood)]

foodResidualMelt <- melt.data.table(
    foodResidual, 
    id.vars = c("geographicAreaM49", "measuredItemCPC", "timePointYears"), 
    measure.vars = c("food", "oldNetTrade", "newNetTrade")
    )


foodResidualMelt <- nameData("food", "fooddatafs_", foodResidualMelt)
foodResidualMelt[geographicAreaM49 == 276 & measuredItemCPC == "01214"]

foodResidualMelt[variable == "food", variableAux := "Food FAOSTAT"]
foodResidualMelt[variable == "oldNetTrade", variableAux := "Net Trade FAOSTAT"]
foodResidualMelt[variable == "newNetTrade", variableAux := "New Net Trade"]

foodResidualMelt[, countryCPC := paste(geographicAreaM49_description,
                                       measuredItemCPC_description, sep = " - ")]


ggplot(foodResidualMelt[geographicAreaM49_description == "Bulgaria" & measuredItemCPC == "F1232"],
       aes(x=as.numeric(timePointYears), y=value, colour = variableAux)) +
   # geom_line(size = .8) +
    geom_line(position=position_dodge(width=0.2)) +
    #facet_grid(country ~ variableNames) +
    #facet_wrap(measuredItemCPC_description ~ variableNames, scales = "free") +
    #geom_blank(data=meltOldNewFoodData[geographicAreaM49 == 356 & measuredItemCPC_description == item]) +
    #ggtitle(paste(countryName, itemName, sep = "-")) +
    ylab('Tonnes') + xlab('Year') +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    scale_x_continuous(limits = c(2000, 2014), breaks = seq(2000, 2014, 1)) +
    theme(legend.title = element_blank()) +
    ggtitle(countryCPC) +
    theme(axis.text=element_text(size=12, face="bold"),
          axis.title=element_text(size=14,face="bold"))

geographicAreaM49_description = unique(foodResidualMelt$geographicAreaM49_description)
geographicAreaM49_description = geographicAreaM49_description[order(geographicAreaM49_description)]
dput(unique(geographicAreaM49_description))

## Food Groups
fbsTree = fread("C:/Users/caetano/Documents/fbs_tree/FbsTree.csv")
fbsTree[, .N, fbsItemTOT_description]
fbsTree[, .N, fbsItemAggregated_description]
setnames(fbsTree, "measuredItem", "measuredItemCPC")

## Merge
foodResidualMelt = merge(foodResidualMelt, 
                    fbsTree[, c("measuredItemCPC", "fbsItemTOT_description", "fbsItemAggregated_description"), with = F],
                    by = "measuredItemCPC", all.x = T)

fbsItemAggregated_description = unique(foodResidualMelt$fbsItemAggregated_description)
fbsItemAggregated_description = fbsItemAggregated_description[order(fbsItemAggregated_description)]
dput(unique(fbsItemAggregated_description))


foodResidualMelt <- foodResidualMelt[!is.na(fbsItemAggregated_description)]



dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsFood", "sandbox")
pdffile <- file.path(dlpath, "comparison-oldfood-nettrade", "food-faostat-vs-netTrade.pdf")

pdf(file = pdffile, height = 11, width = 16)

system.time(for (countryName in c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda", 
                                  "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", 
                                  "Bahamas", "Bangladesh", "Barbados", "Bel-Lux(-1999)", "Belarus", 
                                  "Belgium", "Belize", "Benin", "Bermuda", "Bolivia (Plurinational State of)", 
                                  "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei Darussalam"
                                  #,
                                  # 
                                  # "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", 
                                  # "Cameroon", "Canada", "Central African Republic", "Chad", "Chile", 
                                  # "China, Macao", "China, Main", "China,H.Kong", "China,Taiwan", 
                                  # "Colombia", "Comoros", "Congo", "Costa Rica", 
                                  # #"Côte d'Ivoire", 
                                  # "Croatia", "Cuba", "Cyprus", "Czechia", "Democratic People's Republic of Korea", 
                                  # "Democratic Republic of the Congo", "Denmark", "Djibouti", "Dominica", 
                                  # "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", 
                                  # "Estonia", "Ethiopia", "Fiji", "Finland", "France", "French Polynesia", 
                                  # "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", 
                                  # "Guadeloupe", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", 
                                  # "Haiti", "Honduras", "Hungary", "Iceland", "India", "Iran (Islamic Republic of)", 
                                  # "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", 
                                  # "Kazakhstan", "Kenya", "Kiribati", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", 
                                  # "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Lithuania", 
                                  # "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", 
                                  # "Mali", "Malta", "Martinique", "Mauritania", "Mauritius", "Mexico", 
                                  # "Mongolia", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", 
                                  # "NethAntilles", "Netherlands", "New Caledonia", "New Zealand", 
                                  # "Nicaragua", "Niger", "Nigeria", "Norway", "Oman", "Pakistan", 
                                  # "Palestine(1996-)", "Panama", "Papua New Guinea", "Paraguay", 
                                  # "Peru", "Philippines", "Poland", "Portugal", "Republic of Korea", 
                                  # "Republic of Moldova", 
                                  # #"Réunion", 
                                  # "Romania", "Russian Federation", 
                                  # "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", 
                                  # "Samoa", "Sao Tome and Principe", "Saudi Arabia", "Senegal", 
                                  # "Seychelles", "Sierra Leone", "Slovakia", "Slovenia", "Solomon Islands", 
                                  # "Somalia", "South Africa", "Spain", "Sri Lanka", "Suriname", 
                                  # "Swaziland", "Sweden", "Switzerland", "Syrian Arab Republic", 
                                  # "Tajikistan", "Thailand", "The former Yugoslav Republic of Macedonia", 
                                  # "Timor-Leste", "Togo", "Trinidad and Tobago", "Tunisia", "Turkey", 
                                  # "Turkmenistan", "Uganda", "Ukraine", "United Arab Emirates", 
                                  # "United Kingdom", "United Republic of Tanzania", "United States of America", 
                                  # "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela (Bolivarian Republic of)", 
                                  # "Viet Nam", "Yemen", "Zambia", "Zimbabwe"
                                  )){ 
    for (fbsItemName in c("ALCOHOL (INCL BEER&WINE)", "ANIMAL FATS & PRODUCTS", "BEVERAGE CROPS", 
                       "CEREALS &PROD. EXCL BEER", "EGGS & PRODUCTS", "FRUITS &PROD. (EXCL WINE", 
                       "MEAT (SLAUGHTERED) & PRD", "MILK & PRODUCTS", "OFFALS EDIBLE", 
                       "OILCROPS (EXCL. PROD.)", "SPICES", "STARCHY ROOTS & PRODUCTS", 
                       "SWEETENERS", "TREENUTS & PRODUCTS", "VEGETABLE OILS & PROD.", 
                       "VEGETABLES & PRODUCTS")) {
        
        if(nrow(foodResidualMelt[countryName == geographicAreaM49_description & 
                                 fbsItemName == fbsItemAggregated_description]) > 0) {
    
    p = ggplot(foodResidualMelt[countryName == geographicAreaM49_description & 
                                    fbsItemName == fbsItemAggregated_description],
              aes(x=as.numeric(timePointYears), y=value, colour = variableAux)) +
    facet_wrap(~ measuredItemCPC_description , scales = "free") +
        geom_line(size = .8) + ylab('Tonnes') + xlab('Year') +
        theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
        scale_x_continuous(limits = c(2000, 2014), breaks = seq(2000, 2014, 2)) +
        theme(legend.title = element_blank()) +
        ggtitle(paste(countryName, fbsItemName, sep=" - ")) +
        theme(axis.text=element_text(size=12, face="bold"),
              axis.title=element_text(size=14,face="bold"))
    
    print(p)
    
    }
    }
}

)

dev.off()




###########################################################################


ggplot(foodResidualMelt[countryName == geographicAreaM49_description & 
                            fbsItemName == fbsItemAggregated_description],
       aes(x=as.numeric(timePointYears), y=value, colour = variableAux)) +
    facet_wrap(~ measuredItemCPC_description , scales = "free") +
    geom_line(size = .8) +  
    geom_point(aes(shape=variableAux),   # Shape depends on cond
               size = 4) +
    ylab('Tonnes') + xlab('Year') +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    scale_x_continuous(limits = c(2000, 2014), breaks = seq(2000, 2014, 2)) +
    theme(legend.title = element_blank()) +
    ggtitle(paste(countryName, fbsItemName, sep=" - ")) +
    theme(axis.text=element_text(size=12, face="bold"),
          axis.title=element_text(size=14,face="bold"))


