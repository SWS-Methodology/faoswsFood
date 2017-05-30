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
# install.packages("matrixStats")
library(matrixStats)
library(ggplot2)
library(scales)

space <- function(x, ...) { 
    format(x, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

################################################################################

# Connect to SWS
if(!CheckDebug()){
    options(error = function(){
        dump.frames()
        save(last.dump, file="/work/SWS_R_Share/caetano/last.dump.RData")
    })
}


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


# We'll analyse 5 outlooks here!
# outlook 1: net trade, GDP and no corrections in the elasticity
# outlook 2: net trade + prod, GDP and no corrections in the elasticity
# outlook 3: net trade, household final consump expenditure and no corrections in the elasticity
# outlook 4: net trade, GDP and with corrections in the elasticity
# outlook 5: net trade + prod, household final consump expenditure and with corrections in the elasticity

load("sandbox/presentation/data_outlooks.RData")
head(data_outlooks)

## Exclude countries that does not exist in FAOSTAT website
faostat = fread("C:/Users/caetano/Downloads/FAOSTAT_data_4-19-2017.csv")
faostat


setnames(faostat, "Country", "geographicAreaM49_description")
setnames(faostat, "Year", "timePointYears")
setnames(faostat, "Country Code", "fsCode")
faostat[, timePointYears := as.character(timePointYears)]

data_outlooks[, fsCode := m492fs(geographicAreaM49)]

##
t2 = faostat[, .N, c("fsCode", "timePointYears")]
t2[, fsCode := as.character(fsCode)]

t1 = data_outlooks[, .N, c("fsCode", "timePointYears")]

keys = c("fsCode", "timePointYears")
t3 = merge(t1, t2, by = keys, all = T)

t3[, geographicAreaM49 := fs2m49(fsCode)]
t3 = nameData("agriculture", "aproduction", t3)
t3[is.na(N.x), .N, geographicAreaM49_description]
t3[is.na(N.y), .N, geographicAreaM49_description][N == 24]

# countryLevel[, fsCode := m492fs(geographicAreaM49)]

final = t3[, list(tot = sum(N.y, na.rm = T)), by = list(fsCode, geographicAreaM49_description, geographicAreaM49)]
exclude = final[tot == 0, geographicAreaM49] # countries that does not exist in FBS

data_outlooks = data_outlooks[!geographicAreaM49 %in% exclude]

# Calories

## Computing calories per person per day from the old food data

calories <- faoswsUtil::getNutritiveFactors(geographicAreaM49 = data_outlooks$geographicAreaM49,
                                                       measuredElement = "261",
                                                       measuredItemCPC = data_outlooks$measuredItemCPC,
                                                       timePointYearsSP = data_outlooks$timePointYears)
setnames(calories,
         c("timePointYearsSP", "Value"),
         c("timePointYears", "valueCal"))

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

data_outlooks <- merge(
    data_outlooks, 
    calories[, c(keys, "valueCal"), with = F],
    by = keys, all.x=T)

data_outlooks[, calories_Outlook_1 := (foodHat_Outlook_1 * valueCal * 10000) / 365 / (population * 1000)]
data_outlooks[, calories_Outlook_2 := (foodHat_Outlook_2 * valueCal * 10000) / 365 / (population * 1000)]
data_outlooks[, calories_Outlook_3 := (foodHat_Outlook_3 * valueCal * 10000) / 365 / (population * 1000)]
data_outlooks[, calories_Outlook_4 := (foodHat_Outlook_4 * valueCal * 10000) / 365 / (population * 1000)]
data_outlooks[, calories_Outlook_5 := (foodHat_Outlook_5 * valueCal * 10000) / 365 / (population * 1000)]

data_outlooks = nameData("food", "fooddata", data_outlooks)

# Checking country level
countryLevel = data_outlooks[, list(totCalOutook1 = sum(calories_Outlook_1, na.rm = T),
                     totCalOutook2 = sum(calories_Outlook_2, na.rm = T),
                     totCalOutook3 = sum(calories_Outlook_3, na.rm = T),
                     totCalOutook4 = sum(calories_Outlook_4, na.rm = T),
                     totCalOutook5 = sum(calories_Outlook_5, na.rm = T)),
              list(timePointYears, geographicAreaM49, geographicAreaM49_description)]

countryLevel[, `:=`(averageTotCal = rowMeans(.SD, na.rm = T)), 
             .SDcols = c("totCalOutook1", "totCalOutook2", "totCalOutook3", 
                         "totCalOutook4", "totCalOutook5")]

# exclude = countryLevel[averageTotCal < 1000, .N, c("geographicAreaM49", "timePointYears")]
# exclude[, "N" := NULL]
# exclude[, flagExc := 1]

## exclude other countries

countryLevel = countryLevel[!(geographicAreaM49_description %in% 
                                c("Czechoslovak(-1992)", "Ethiopia PDR(-1992)", "USSR(-1991)", 
                                  "Serbia-Monte(1992-2005)", "Sudan (former)", "Bel-Lux(-1999)"))]

keys = c("geographicAreaM49", "timePointYears")
# countryLevel = merge(countryLevel, exclude, by = keys, all.x = T)
# countryLevel[is.na(flagExc), flagExc := 0]
# countryLevel = countryLevel[!flagExc == 1 & !(averageTotCal == Inf)]
# countryLevel[, "flagExc" := NULL]

countryLevel[, percent_1_2 := (totCalOutook2 - totCalOutook1)/totCalOutook1]
countryLevel[percent_1_2 == 0]

countryLevel[, percent_1_3 := (totCalOutook3 - totCalOutook1)/totCalOutook1]
countryLevel[percent_1_3 == 0]


countryLevel[, percent_1_4 := (totCalOutook4 - totCalOutook1)/totCalOutook1]
countryLevel[percent_1_4 > 0]

countryLevel[, percent_1_5 := (totCalOutook5 - totCalOutook1)/totCalOutook1]
countryLevel[percent_1_5 == 0]

changesByCountry = countryLevel[, list(averagePercent1_2 = sum(totCalOutook2, na.rm = T)/sum(totCalOutook1, na.rm = T),
                    averagePercent1_3 = sum(totCalOutook3, na.rm = T)/sum(totCalOutook1, na.rm = T),
                    averagePercent1_4 = sum(totCalOutook4, na.rm = T)/sum(totCalOutook1, na.rm = T),
                    averagePercent1_5 = sum(totCalOutook5, na.rm = T)/sum(totCalOutook1, na.rm = T)),
             by = list(geographicAreaM49, geographicAreaM49_description)]


summary(changesByCountry$averagePercent1_2)
# 1 vs 2
quantile1_2 = changesByCountry[, list(classes = seq(0, 9, 1), 
                        quantile = quantile(averagePercent1_2, probs = c(0:9/10)) - 1), ]

quantile1_2 = quantile1_2[quantile > 0]
# quantile1_2 = data.frame(quantile(changesByCountry$averagePercent1_2, probs = c(0:9/10)))
changesByCountry[averagePercent1_2 > 1.4]


ggplot(quantile1_2, aes(x = as.factor(classes), y = quantile)) + 
           geom_bar(stat="identity", fill = "#000099") + 
    # geom_line(size = 1.2, col= "#000099") +
    xlab("Deciles") + ylab("Percentage increase") +
    # scale_y_continuous(name="%", labels = percent) +
    ggtitle("Scenario 2 vs 1") +
    scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 0.05), labels = percent) +
    
    theme(axis.text.x = element_text(face = "bold", angle = 0, vjust = .5)) +
    theme(axis.text=element_text(size=14, face="bold"),
          axis.title=element_text(size=14,face="bold"))


countriesLastPart_1_2 = changesByCountry[averagePercent1_2 > 1.34278158, .N, geographicAreaM49][, geographicAreaM49]
countriesMoreImpacted_1_2 = countryLevel[geographicAreaM49 %in% countriesLastPart_1_2]
countriesMoreImpacted_1_2 = countriesMoreImpacted_1_2[, c("timePointYears", "geographicAreaM49",
                                                          "geographicAreaM49_description",
                                                          "totCalOutook1", "totCalOutook2"), with = F]
countriesMoreImpacted_1_2 <- melt(countriesMoreImpacted_1_2, 
                                  id=c("timePointYears", "geographicAreaM49", "geographicAreaM49_description"))

                       
countriesMoreImpacted_1_2 = countriesMoreImpacted_1_2[geographicAreaM49_description != "China,Taiwan"]
                 
countriesMoreImpacted_1_2[variable == "totCalOutook1", Scenario := "1"]
countriesMoreImpacted_1_2[variable == "totCalOutook2", Scenario := "2"]

## at country level - scenario 1 vs 2
ggplot(countriesMoreImpacted_1_2, 
       aes(x = as.numeric(timePointYears), y = value, colour=Scenario, group = Scenario)) + 
    # geom_bar(stat="identity") +
    geom_line(position=position_dodge(width=0.4), size = 1.2) +
    facet_wrap(~ geographicAreaM49_description) +
    # facet_grid(~ geographicAreaM49_description, scales = "free") +
    # scale_fill_manual(values=c("#A3A500", "#F8766D")) +
    scale_color_manual(values=c("#F8766D", "#A3A500")) +
    ylab('Total calories per person per day') + xlab('Year') + 
    
    scale_y_continuous(limits = c(1500, 5600), breaks = seq(1500, 5600, 500), labels = comma) +
    scale_x_continuous(limits = c(1991, 2015), breaks = seq(1991, 2015, 4)) +
    theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
    # ggtitle("Scenario 1 vs 2 - Global level: Net trade + Production") +
    ggtitle("Scenario 2 vs 1 - Country level: Net trade + Production") +
    geom_hline(yintercept = 4000) +
    geom_hline(yintercept = 2500)

## 1 vs 3
quantile1_3 = changesByCountry[, list(classes = seq(0, 9, 1), 
                                      quantile = quantile(averagePercent1_3, probs = c(0:9/10)) - 1), ]

quantile1_3 = quantile1_3[classes != 0]
# quantile1_2 = data.frame(quantile(changesByCountry$averagePercent1_2, probs = c(0:9/10)))
changesByCountry[averagePercent1_3 < 0.9]


ggplot(quantile1_3, aes(x = as.factor(classes), y = quantile)) + 
    geom_bar(stat="identity", fill = "#000099") + 
    # geom_line(size = 1.2, col= "#000099") +
    xlab("Deciles") + ylab("Percentage increase") +
    # scale_y_continuous(name="%", labels = percent) +
    ggtitle("Scenario 1 vs 3") +
    scale_y_continuous(limits = c(-0.02, 0.02), breaks = seq(-0.02, 0.02, 0.01), labels = percent) +
    
    theme(axis.text.x = element_text(face = "bold", angle = 0, vjust = .5)) +
    theme(axis.text=element_text(size=14, face="bold"),
          axis.title=element_text(size=14,face="bold"))

countriesFirstPart_1_3 = changesByCountry[averagePercent1_3 < 0.9825611, .N, geographicAreaM49][, geographicAreaM49]
countriesMoreImpactedNeg_1_3 = countryLevel[geographicAreaM49 %in% countriesFirstPart_1_3]
countriesMoreImpactedNeg_1_3 = countriesMoreImpactedNeg_1_3[, c("timePointYears", "geographicAreaM49",
                                                          "geographicAreaM49_description",
                                                          "totCalOutook1", "totCalOutook3"), with = F]
countriesMoreImpactedNeg_1_3 <- melt(countriesMoreImpactedNeg_1_3, 
                                  id=c("timePointYears", "geographicAreaM49", "geographicAreaM49_description"))


countriesMoreImpactedNeg_1_3[variable == "totCalOutook1", Scenario := "1"]
countriesMoreImpactedNeg_1_3[variable == "totCalOutook3", Scenario := "3"]

## at country level - scenario 1 vs 2
ggplot(countriesMoreImpactedNeg_1_3, 
       aes(x = as.numeric(timePointYears), y = value, colour=Scenario, group = Scenario)) + 
    # geom_bar(stat="identity") +
    geom_line(position=position_dodge(width=0.4), size = 1.2) +
    facet_wrap(~ geographicAreaM49_description) +
    # facet_grid(~ geographicAreaM49_description, scales = "free") +
    # scale_fill_manual(values=c("#A3A500", "#F8766D")) +
    scale_color_manual(values=c("#F8766D", "#A3A500")) +
    ylab('Total calories per person per day') + xlab('Year') + 
    
    scale_y_continuous(limits = c(900, 4500), breaks = seq(900, 4500, 500), labels = comma) +
    scale_x_continuous(limits = c(1991, 2015), breaks = seq(1991, 2015, 4)) +
    theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
    # ggtitle("Scenario 1 vs 2 - Global level: Net trade + Production") +
    ggtitle("Scenario 1 vs 2 - Country level: Net trade + Production") +
    geom_hline(yintercept = 4000) +
    geom_hline(yintercept = 2500)

##

## 1 vs 5
quantile1_5 = changesByCountry[, list(classes = seq(0, 9, 1), 
                                      quantile = quantile(averagePercent1_5, probs = c(0:9/10)) - 1), ]

quantile1_5 = quantile1_5[classes != 0]
# quantile1_2 = data.frame(quantile(changesByCountry$averagePercent1_2, probs = c(0:9/10)))
changesByCountry[averagePercent1_5 < 0.9]


ggplot(quantile1_5, aes(x = as.factor(classes), y = quantile)) + 
    geom_bar(stat="identity", fill = "#000099") + 
    # geom_line(size = 1.2, col= "#000099") +
    xlab("Deciles") + ylab("Percentage increase") +
    # scale_y_continuous(name="%", labels = percent) +
    ggtitle("Scenario 5 vs 1") +
    scale_y_continuous(limits = c(0, 0.35), breaks = seq(0, 0.35, 0.05), labels = percent) +
    
    theme(axis.text.x = element_text(face = "bold", angle = 0, vjust = .5)) +
    theme(axis.text=element_text(size=14, face="bold"),
          axis.title=element_text(size=14,face="bold"))

countriesFirstPart_1_5 = changesByCountry[averagePercent1_5 < 1, .N, geographicAreaM49][, geographicAreaM49]
countriesMoreImpactedNeg_1_5 = countryLevel[geographicAreaM49 %in% countriesFirstPart_1_5]
countriesMoreImpactedNeg_1_5 = countriesMoreImpactedNeg_1_5[, c("timePointYears", "geographicAreaM49",
                                                                "geographicAreaM49_description",
                                                                "totCalOutook1", "totCalOutook5"), with = F]
countriesMoreImpactedNeg_1_5 <- melt(countriesMoreImpactedNeg_1_5, 
                                     id=c("timePointYears", "geographicAreaM49", "geographicAreaM49_description"))


countriesMoreImpactedNeg_1_5[variable == "totCalOutook1", Scenario := "1"]
countriesMoreImpactedNeg_1_5[variable == "totCalOutook5", Scenario := "5"]


## at country level - scenario 1 vs 5 - affected negatively
ggplot(countriesMoreImpactedNeg_1_5, 
       aes(x = as.numeric(timePointYears), y = value, colour=Scenario, group = Scenario)) + 
    # geom_bar(stat="identity") +
    geom_line(position=position_dodge(width=0.4), size = 1.2) +
    facet_wrap(~ geographicAreaM49_description) +
    # facet_grid(~ geographicAreaM49_description, scales = "free") +
    # scale_fill_manual(values=c("#A3A500", "#F8766D")) +
    scale_color_manual(values=c("#F8766D", "#E76BF3")) +
    ylab('Total calories per person per day') + xlab('Year') + 
    
    scale_y_continuous(limits = c(900, 4500), breaks = seq(900, 4500, 500), labels = comma) +
    scale_x_continuous(limits = c(1991, 2015), breaks = seq(1991, 2015, 4)) +
    theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
    # ggtitle("Scenario 1 vs 2 - Global level: Net trade + Production") +
    # ggtitle("Scenario 1 vs 5 - Country level: Net trade + Production") +
    ggtitle("Scenario 5 vs 1 - Global level: Net trade + Production, Household \n Final Consump Expenditure and  Elasticity matrix updated") +
    geom_hline(yintercept = 4000) +
    geom_hline(yintercept = 2500)

# more than the last decile
countriesLastPart_1_5 = changesByCountry[averagePercent1_5 > 1.3407798, .N, geographicAreaM49][, geographicAreaM49]
countriesMoreImpactedPosit_1_5 = countryLevel[geographicAreaM49 %in% countriesLastPart_1_5]
countriesMoreImpactedPosit_1_5 = countriesMoreImpactedPosit_1_5[, c("timePointYears", "geographicAreaM49",
                                                                "geographicAreaM49_description",
                                                                "totCalOutook1", "totCalOutook5"), with = F]
countriesMoreImpactedPosit_1_5 <- melt(countriesMoreImpactedPosit_1_5, 
                                     id=c("timePointYears", "geographicAreaM49", "geographicAreaM49_description"))

countriesMoreImpactedPosit_1_5 = countriesMoreImpactedPosit_1_5[geographicAreaM49_description != "China,Taiwan"]


countriesMoreImpactedPosit_1_5[variable == "totCalOutook1", Scenario := "1"]
countriesMoreImpactedPosit_1_5[variable == "totCalOutook5", Scenario := "5"]

## at country level - scenario 1 vs 5 - more than the last decile
ggplot(countriesMoreImpactedPosit_1_5, 
       aes(x = as.numeric(timePointYears), y = value, colour=Scenario, group = Scenario)) + 
    # geom_bar(stat="identity") +
    geom_line(position=position_dodge(width=0.4), size = 1.2) +
    facet_wrap(~ geographicAreaM49_description) +
    # facet_grid(~ geographicAreaM49_description, scales = "free") +
    # scale_fill_manual(values=c("#A3A500", "#F8766D")) +
    scale_color_manual(values=c("#F8766D", "#E76BF3")) +
    ylab('Total calories per person per day') + xlab('Year') + 
    
    scale_y_continuous(limits = c(0, 5550), breaks = seq(0, 5550, 1000), labels = comma) +
    scale_x_continuous(limits = c(1991, 2015), breaks = seq(1991, 2015, 4)) +
    theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
    # ggtitle("Scenario 1 vs 2 - Global level: Net trade + Production") +
    # ggtitle("Scenario 1 vs 5 - Country level: Net trade + Production") +
    ggtitle("Scenario 5 vs 1 - Global level: Net trade + Production, Household \n Final Consump Expenditure and  Elasticity matrix updated") +
    geom_hline(yintercept = 4000) +
    geom_hline(yintercept = 2500)



###

ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
    geom_bar(stat="identity")

summary(changesByCountry$averagePercent1_3)

changesByCountry[averagePercent1_3 < 0.9]

summary(changesByCountry$averagePercent1_4)
summary(changesByCountry$averagePercent1_5)


# global level
globalLevel = countryLevel[, list(avgCalOutook1 = mean(totCalOutook1, na.rm = T),
                                  avgCalOutook2 = mean(totCalOutook2, na.rm = T),
                                  avgCalOutook3 = mean(totCalOutook3, na.rm = T),
                                  avgCalOutook4 = mean(totCalOutook4, na.rm = T),
                                  avgCalOutook5 = mean(totCalOutook5, na.rm = T),
                                  numCountry = .N),
                             by = list(timePointYears)]


globalLevelMelt = melt.data.table(globalLevel, id.vars = c("timePointYears", "numCountry"), 
                measure.vars = c("avgCalOutook1", "avgCalOutook2", "avgCalOutook3",
                                 "avgCalOutook4", "avgCalOutook5"))

globalLevelMelt[variable == "avgCalOutook1", Scenario := "1"]
globalLevelMelt[variable == "avgCalOutook2", Scenario := "2"]
globalLevelMelt[variable == "avgCalOutook3", Scenario := "3"]
globalLevelMelt[variable == "avgCalOutook4", Scenario := "4"]
globalLevelMelt[variable == "avgCalOutook5", Scenario := "5"]

globalLevel[, percent_1_2 := (avgCalOutook2 - avgCalOutook1)/avgCalOutook1]
globalLevel[, percent_1_3 := (avgCalOutook3 - avgCalOutook1)/avgCalOutook1]
globalLevel[, percent_1_4 := (avgCalOutook4 - avgCalOutook1)/avgCalOutook1]
globalLevel[, percent_1_5 := (avgCalOutook5 - avgCalOutook1)/avgCalOutook1]

globalLevel[, list(percent_1_2 = (sum(avgCalOutook2) - sum(avgCalOutook1))/sum(avgCalOutook2),
                   percent_1_3 = (sum(avgCalOutook3) - sum(avgCalOutook1))/sum(avgCalOutook3),
                   percent_1_4 = (sum(avgCalOutook4) - sum(avgCalOutook1))/sum(avgCalOutook4),
                   percent_1_5 = (sum(avgCalOutook5) - sum(avgCalOutook1))/sum(avgCalOutook5))]

# ggplot(globalLevelMelt[variable %in% c("avgCalOutook1", "avgCalOutook2")], 
#        aes(x = timePointYears, y = value, fill = variable)) + 
#     # geom_bar(aes(fill = variable), stat = "identity") +
#     geom_bar(stat="identity", position=position_dodge()) +
#     # geom_bar(position=position_dodge(width=0.4), size = 1.2) +
#     # scale_fill_manual(values=c("#FA8258", "#088A08")) +
#     ylab('Total calories per person per day') + xlab('Year') +
#     theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) 

## all outlooks together
p = ggplot(globalLevelMelt, 
       aes(x = timePointYears, y = value, colour=Scenario, group = Scenario)) + 
    # geom_bar(stat="identity") +
    geom_line(position=position_dodge(width=0.4), size = 1.2) +
    # scale_fill_manual(values=c("#FA8258", "#088A08")) +
    ylab('Total calories per person per day') + xlab('Year') + 
    scale_y_continuous(limits = c(2000, 3200), breaks = seq(2000, 3200, 100), labels = comma) +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    ggtitle("Global level") +
    theme(axis.text=element_text(size=14, face="bold"),
          axis.title=element_text(size=14,face="bold")) 
    
ggplot_build(p)$data
my_y_title <- expression(paste("Scenario 1", italic("vs"), " 2 - Global level: Net trade + Production"))

ggplot(globalLevelMelt[variable %in% c("avgCalOutook1", "avgCalOutook2")], 
       aes(x = timePointYears, y = value, colour=Scenario, group = Scenario)) + 
    # geom_bar(stat="identity") +
    geom_line(position=position_dodge(width=0.4), size = 1.2) +
    # scale_fill_manual(values=c("#A3A500", "#F8766D")) +
    scale_color_manual(values=c("#F8766D", "#A3A500")) +
    ylab('Total calories per person per day') + xlab('Year') + 
    scale_y_continuous(limits = c(2000, 3200), breaks = seq(2000, 3200, 100), labels = space) +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    # ggtitle("Scenario 1 vs 2 - Global level: Net trade + Production") +
    ggtitle(my_y_title) +
    theme(axis.text=element_text(size=14, face="bold"),
          axis.title=element_text(size=14,face="bold"))

my_y_title <- expression(paste("Scenario 1", italic("vs"), " 3 - Global level: Household Final Consump Expenditure"))
ggplot(globalLevelMelt[variable %in% c("avgCalOutook1", "avgCalOutook3")], 
       aes(x = timePointYears, y = value, colour=Scenario, group = Scenario)) + 
    # geom_bar(stat="identity") +
    geom_line(position=position_dodge(width=0.4), size = 1.2) +
    # scale_fill_manual(values=c("#A3A500", "#F8766D")) +
    scale_color_manual(values=c("#F8766D", "#00BF7D")) +
    ylab('Total calories per person per day') + xlab('Year') + 
    scale_y_continuous(limits = c(2000, 3200), breaks = seq(2000, 3200, 100), labels = space) +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    # ggtitle("Global level: Household Final Consump Expenditure") +
    ggtitle(my_y_title) +
    theme(axis.text=element_text(size=14, face="bold"),
          axis.title=element_text(size=14,face="bold"))

my_y_title <- expression(paste("Scenario 1", italic("vs"), " 4 - Global level: Elasticity matrix updated"))

ggplot(globalLevelMelt[variable %in% c("avgCalOutook1", "avgCalOutook4")], 
       aes(x = timePointYears, y = value, colour=Scenario, group = Scenario)) + 
    # geom_bar(stat="identity") +
    geom_line(position=position_dodge(width=0.4), size = 1.2) +
    # scale_fill_manual(values=c("#A3A500", "#F8766D")) +
    scale_color_manual(values=c("#F8766D", "#00B0F6")) +
    ylab('Total calories per person per day') + xlab('Year') + 
    scale_y_continuous(limits = c(2000, 3200), breaks = seq(2000, 3200, 100), labels = space) +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    # ggtitle("Global level: Elasticity matrix updated") +
    ggtitle(my_y_title) +
    theme(axis.text=element_text(size=14, face="bold"),
          axis.title=element_text(size=14,face="bold"))

my_y_title <- expression(paste("Scenario 1", italic("vs"), " 5 - Global level:Net trade + Production, Household \n Final Consump Expenditure and Elasticity matrix updated\n"))

ggplot(globalLevelMelt[variable %in% c("avgCalOutook1", "avgCalOutook5")], 
       aes(x = timePointYears, y = value, colour=Scenario, group = Scenario)) + 
    # geom_bar(stat="identity") +
    geom_line(position=position_dodge(width=0.4), size = 1.2) +
    # scale_fill_manual(values=c("#A3A500", "#F8766D")) +
    scale_color_manual(values=c("#F8766D", "#E76BF3")) +
    ylab('Total calories per person per day') + xlab('Year') + 
    scale_y_continuous(limits = c(2000, 3200), breaks = seq(2000, 3200, 100), labels = space) +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    ggtitle("Scenario 1 vs 5 - Global level: Net trade + Production, Household \n Final Consump Expenditure and  Elasticity matrix updated") +
    # ggtitle(my_y_title) +
    theme(axis.text=element_text(size=14, face="bold"),
          axis.title=element_text(size=14,face="bold"))

## total production

prodTab = data_outlooks[, list(totalProd = sum(production, na.rm = T)),
                        by = list(timePointYears)]

ggplot(prodTab, 
       aes(x = timePointYears, y = totalProd, group = 1)) + 
    # geom_bar(stat="identity") +
    geom_line(size = 1.2, col= "#000099") +
    xlab("Year") +
    scale_y_continuous(name="Production [t]", labels = comma) +
    ggtitle("Global level: Production") +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    theme(axis.text=element_text(size=14, face="bold"),
          axis.title=element_text(size=14,face="bold")) 


## Country level
# 
# countryLevel = countryLevel[, list(avgCalOutook1 = mean(totCalOutook1, na.rm = T),
#                                   avgCalOutook2 = mean(totCalOutook2, na.rm = T),
#                                   avgCalOutook3 = mean(totCalOutook3, na.rm = T),
#                                   avgCalOutook4 = mean(totCalOutook4, na.rm = T),
#                                   avgCalOutook5 = mean(totCalOutook5, na.rm = T),
#                                   numCountry = .N),
#                            by = list(timePointYears)]
# 
# 
# globalLevelMelt = melt.data.table(globalLevel, id.vars = c("timePointYears", "numCountry"), 
#                                   measure.vars = c("avgCalOutook1", "avgCalOutook2", "avgCalOutook3",
#                                                    "avgCalOutook4", "avgCalOutook5"))
