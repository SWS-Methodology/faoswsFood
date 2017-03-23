library(ggplot2)
library(scales)
#install.packages("gtable")
library(gtable)
library(data.table)
library("yarrr")
library(RColorBrewer)


## Food Per Capita vs GDP

#save(data, file = "C:/Users/caetano/Documents/foodData25_01-2017.RData")

load("C:/Users/caetano/Documents/foodData25_01-2017.RData")
data = data.table(data)

newFoodData = data[!is.na(foodHat)]
head(newFoodData)

newFoodData = nameData("agriculture", "aproduction", newFoodData)

setnames(newFoodData, "geographicAreaM49_description", "country")
setnames(newFoodData, "timePointYears", "year")
newFoodData[, "timePointYears_description" := NULL]

## Reading FBS tree

fbsTree = fread("C:/Users/caetano/Documents/fbs_tree/FbsTree.csv")
fbsTree[, .N, fbsItemTOT_description]
fbsTree[, .N, fbsItemAggregated_description]
setnames(fbsTree, "measuredItem", "measuredItemCPC")

## Merge
newFoodData = merge(newFoodData, 
      fbsTree[, c("measuredItemCPC", "fbsItemTOT_description", "fbsItemAggregated_description"), with = F],
      by = "measuredItemCPC", all.x = T)

## Calories

caloriesData <- faoswsUtil::getNutritiveFactors(geographicAreaM49 = newFoodData$geographicAreaM49,
                                                measuredElement = "261",
                                                measuredItemCPC = newFoodData$measuredItemCPC,
                                                timePointYearsSP = newFoodData$timePointYears)
setnames(caloriesData,
         c("timePointYearsSP", "Value"),
         c("year", "valueCal"))

keys = c("geographicAreaM49", "measuredItemCPC", "year")


newFoodData <- merge(newFoodData, 
    caloriesData[, c(keys, "valueCal"), with = F],
    by = keys, all.x=T)

newFoodData[, calPerPersonPerDay := (foodHat * valueCal * 10000) / 365 / (population * 1000)]

newFoodData = newFoodData[!is.na(fbsItemTOT_description)]

## Compute the calorie consumption per country, per person and per day
calorieCountryDay <- newFoodData[, list(totalCalDay = sum(calPerPersonPerDay, na.rm=T),
                                     numbCommodities = .N),
                              by = list(geographicAreaM49, country, year, fbsItemTOT_description)]

calorieCountryDay[, percent := round(totalCalDay/sum(totalCalDay), 3),
                  by = list(geographicAreaM49, country, year)]

calorieCountryDay[, pos := round(cumsum(percent) - (0.05 * percent), 4),
                  by = list(geographicAreaM49, country, year)]

calorieCountryDay[, pos2 := round(cumsum(totalCalDay) - (0.05 * totalCalDay), 4),
                  by = list(geographicAreaM49, country, year)]

space <- function(x, ...) { 
    format(x, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

countryName = unique(calorieCountryDay$country)
countryName = countryName[order(countryName)]
dput(countryName)

## time series by country and group (vegetables or animal products)

dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsFood", "sandbox", "calories_per_person_per_day")
pdffile <- file.path(dlpath, "calories_by_food_group", "total-calories-by-group.pdf")

pdf(file = pdffile, height = 11, width = 16)

system.time(for (country_name in c("Afghanistan", "Brazil", "China, Macao", "China, Main", "China,H.Kong", 
                                   #"China,Taiwan",
                                   "India", "Indonesia","Thailand",
                                   "United Republic of Tanzania", "United States of America")) {
    
p = ggplot(calorieCountryDay[country == country_name], 
       aes(x = year, y = totalCalDay, fill = as.factor(fbsItemTOT_description))) + 
    geom_bar(stat="identity") + 
    #geom_area(stat="identity") + 
    scale_fill_manual(values=c("#FA8258", "#088A08")) +
    #scale_color_manual(values=c("#A9F5A9", "#81BEF7")) +
    # scale_fill_grey() +
    geom_text(aes(x = year, y = pos2, label = paste0(100*percent,"%")),
              size=3) +
    #geom_area(position = 'stack') +
    ylab('Total calories per person per day') + xlab('Year') +
    #labs(x="Year", y="Value") +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    #scale_x_continuous(limits = c(1991, 2013), breaks = seq(1991, 2013, 1)) +
    scale_y_continuous(limits = c(0, 4250), breaks = seq(0, 4250, 250), labels = space) +
    theme(legend.title = element_blank()) +
    ggtitle(country_name) +
    theme(axis.text=element_text(size=12, face="bold"),
          axis.title=element_text(size=14,face="bold")) +
    theme(legend.position="top") +
    geom_hline(aes(yintercept=4000)) 
print(p)

 }

)


dev.off()



## By fbs group level

## Compute the calorie consumption per country, per person and per day
calorieCountryDayFbsItem <- newFoodData[, list(totalCalDay = sum(calPerPersonPerDay, na.rm=T),
                                        numbCommodities = .N),
                                 by = list(geographicAreaM49, country, year, 
                                           fbsItemTOT_description, fbsItemAggregated_description)]

calorieCountryDayFbsItem[, percent := round(totalCalDay/sum(totalCalDay, na.rm = T), 3),
                  by = list(geographicAreaM49, country, year)]

calorieCountryDayFbsItem[, pos := round(cumsum(percent) - (0.05 * percent), 4),
                  by = list(geographicAreaM49, country, year)]

calorieCountryDayFbsItem[, pos2 := round(totalCalDay - (0.05 * totalCalDay), 4),
                  by = list(geographicAreaM49, country, year)]


calorieCountryDayFbsItem[, totCalPerPersonPerday := sum(totalCalDay, na.rm = T),
                         by = list(country, year)]

calorieCountryDayFbsItem$fbsItemAggregated_description <- 
    factor(
        calorieCountryDayFbsItem$fbsItemAggregated_description, 
        levels = c("ALCOHOL (INCL BEER&WINE)", "BEVERAGE CROPS", "CEREALS &PROD. EXCL BEER", 
                   "FRUITS &PROD. (EXCL WINE", "OILCROPS (EXCL. PROD.)", "PULSES & PRODUCTS", 
                   "SPICES", "STARCHY ROOTS & PRODUCTS", "SUGARCROPS (EXCL. PROD.)", 
                   "SWEETENERS", "TREENUTS & PRODUCTS", "VEGETABLE OILS & PROD.", 
                   "VEGETABLES & PRODUCTS", "ANIMAL FATS & PRODUCTS", "EGGS & PRODUCTS", "MEAT (SLAUGHTERED) & PRD", 
                   "MILK & PRODUCTS", "OFFALS EDIBLE"))


calorieCountryDayFbsItem[, totCalAdj := totCalPerPersonPerday - totalCalDay]
caloriesFbsMelt = melt.data.table(calorieCountryDayFbsItem, 
                             id.vars = c("geographicAreaM49", "country", "year",
                                         "fbsItemTOT_description", "fbsItemAggregated_description", "percent", "pos2"),
                             measure.vars = c("totalCalDay", "totCalAdj"))


caloriesFbsMelt[variable == "totalCalDay", variable2 := "FBS - Item Aggregated"]
caloriesFbsMelt[variable == "totCalAdj", variable2 := "Total"]
caloriesFbsMelt[, year := as.numeric(year)]




## creating a pdf by country and fbs item


caloriesFbsMelt[, percentAverage := round(mean(percent), 3),
                by = list(geographicAreaM49, fbsItemAggregated_description)]

dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsFood", "sandbox", "calories_per_person_per_day")
pdffile <- file.path(dlpath, "calories_by_food_group", "total-calories-by-fbs-level.pdf")

pdf(file = pdffile, height = 11, width = 16)


system.time(for (country_name in c("Afghanistan", "Brazil", "China, Macao", "China, Main", "China,H.Kong", 
                                   #"China,Taiwan",
                                   "India", "Indonesia","Thailand",
                                   "United Republic of Tanzania", "United States of America"
                                   
)) { 
    for (fbsItem in c("VEGETABLE PROD. (DEMAND)", "ANIMAL PRODUCTS - DEMAND")) {
    
    if(nrow(caloriesFbsMelt[country_name == country & fbsItem == fbsItemTOT_description]) > 0) {          
    
     p = ggplot(data = caloriesFbsMelt[country_name == country & fbsItem == fbsItemTOT_description], 
               aes(x = as.numeric(year), y = as.numeric(value), 
                   fill = as.factor(variable2))) +
            geom_bar(stat="identity") + 
            #scale_fill_manual(values=c("#FA8258", "#088A08")) +
         facet_wrap(~ fbsItemAggregated_description , scales = "free") +
            geom_text(aes(x = year[12], y = pos2[12], label = paste0(100*percentAverage,"%")),
                      size=4, check_overlap = TRUE, fontface = "bold") +
         # geom_hline(aes(yintercept=mean(pos2),  label = 10)) +
         
            ylab('Total calories per person per day') + xlab('Year') +
            theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
            # scale_x_continuous(limits = c(1990, 2014), breaks = seq(1991, 2013, 1)) +
         scale_x_continuous(limits = c(1990, 2014), breaks = c(1991, 1994,
                                                               1997, 
                                                               2000,
                                                               2003, 2006,
                                                               2009, 
                                                               2013)) +
            scale_y_continuous(limits = c(0, 4250), breaks = seq(0, 4250, 500), labels = space) +
            
            theme(legend.title = element_blank()) +
            ggtitle(paste(country_name, fbsItem, sep = "-")) +
            theme(axis.text=element_text(face="bold"),
                  axis.title=element_text(size=14,face="bold")) +
            theme(legend.position="top") +
            geom_hline(aes(yintercept=4000)) 
     print(p)
        
}
    
}
}
)
dev.off()

## Trying to compare data with HHS

## By fbs group level

## Compute the calorie consumption per country, per person and per day

newFoodData[fbsItemAggregated_description %in% c("MEAT (SLAUGHTERED) & PRD", "OFFALS EDIBLE", "ANIMAL FATS & PRODUCTS"), 
                fbsGroupLevelModified := "MEAT (SLAUGHTERED) & PRD, OFFALS EDIBLE, ANIMAL FATS & PRODUCTS"]

newFoodData[fbsItemAggregated_description %in% c("SUGARCROPS (EXCL. PROD.)", "SWEETENERS"), 
                fbsGroupLevelModified := "SUGARCROPS (EXCL. PROD.) & SWEETENERS"]

newFoodData[fbsItemAggregated_description %in% c("OILCROPS (EXCL. PROD.)", "VEGETABLE OILS & PROD."), 
                fbsGroupLevelModified := "OILCROPS (EXCL. PROD.), VEGETABLE OILS & PROD."]

newFoodData[is.na(fbsGroupLevelModified), fbsGroupLevelModified := fbsItemAggregated_description]
newFoodData[, .N, fbsGroupLevelModified][order(fbsGroupLevelModified)]


caloriesFbsGroupModified <- newFoodData[, list(totalCalDay = sum(calPerPersonPerDay, na.rm=T),
                                               numbCommodities = .N),
                                        by = list(geographicAreaM49, country, year, 
                                                  fbsItemTOT_description, fbsGroupLevelModified)]

caloriesFbsGroupModified[, percent := round(totalCalDay/sum(totalCalDay, na.rm = T), 3),
                         by = list(geographicAreaM49, country, year)]

caloriesFbsGroupModified[, pos := round(cumsum(percent) - (0.05 * percent), 4),
                         by = list(geographicAreaM49, country, year)]

caloriesFbsGroupModified[, pos2 := round(totalCalDay - (0.05 * totalCalDay), 4),
                         by = list(geographicAreaM49, country, year)]


caloriesFbsGroupModified[, totCalPerPersonPerday := sum(totalCalDay, na.rm = T),
                         by = list(country, year)]

fbsGroupLevelModified = unique(caloriesFbsGroupModified$fbsGroupLevelModified)
fbsGroupLevelModified = fbsGroupLevelModified[order(fbsGroupLevelModified)]
dput(unique(fbsGroupLevelModified))

caloriesFbsGroupModified$fbsGroupLevelModified <- 
    factor(
        caloriesFbsGroupModified$fbsGroupLevelModified, 
        levels = c("ALCOHOL (INCL BEER&WINE)", "BEVERAGE CROPS", "CEREALS &PROD. EXCL BEER", 
                   "EGGS & PRODUCTS", "FRUITS &PROD. (EXCL WINE", 
                   "MEAT (SLAUGHTERED) & PRD, OFFALS EDIBLE, ANIMAL FATS & PRODUCTS", 
                   "MILK & PRODUCTS", "OILCROPS (EXCL. PROD.), VEGETABLE OILS & PROD.", 
                   "PULSES & PRODUCTS", "SPICES", "STARCHY ROOTS & PRODUCTS", 
                   "SUGARCROPS (EXCL. PROD.) & SWEETENERS", 
                   "TREENUTS & PRODUCTS", "VEGETABLES & PRODUCTS"))


caloriesFbsGroupModified[, totCalAdj := totCalPerPersonPerday - totalCalDay]

    
afghanistanFoodModuleFbsGroupMod = caloriesFbsGroupModified[geographicAreaM49 == 4 & year == 2008]

afghanistanFoodModuleFbsGroupMod[, origin := "Food Module"]

summaryTotalCal[, origin := "HHS"]

tabAfghanistan = merge(afghanistanFoodModuleFbsGroupMod, 
      summaryTotalCal[, c("fbsGroupLevelModified", "origin", "totalCalHS", "percentCalHS"), with = F],
      by = "fbsGroupLevelModified", all = T)

tabAfghanistan = na.omit(tabAfghanistan)
tabAfghanistan[, percentDiff := (totalCalDay - totalCalHS)/totalCalHS]

tabAfghanistan[, percentagePoints := 100*round(totalCalDay/sum(totalCalDay) - totalCalHS/sum(totalCalHS), 3)]



tabAfghanistan[, group := 1:nrow(tabAfghanistan)]

tabAfghanistan$fbsGroupLevelModified2 <-sub(",", "- \n ", tabAfghanistan$fbsGroupLevelModified)

# In order to compare both data sets we can put them in the same scale. 


commodiry = unique(caloriesFbsGroupModified$fbsGroupLevelModified)
fbsGroupLevelModified = fbsGroupLevelModified[order(fbsGroupLevelModified)]
dput(unique(comparingDataAfgh$HHBS))





ggplot(tabAfghanistan[order(tabAfghanistan$fbsGroupLevelModified2)], aes(y = as.factor(fbsGroupLevelModified2))) + 
    geom_point(aes(x = 0, size = totalCalHS, color = "totalCalHS")) + 
    geom_point(aes(x = percentagePoints, size = totalCalDay, color = "totalCalDay")) +
    # scale_colour_gradient(low = "blue") +
    labs(size = "[kcal/person/day]", colour="") +
    # scale_size(range = c(1, 10)) +
    # scale_size_continuous(range = c(0, 9)) +
    # scale_size_area(max_size = 9) +
    scale_color_manual(
        labels = c("Food Module", "HHS"),
        values = colors) +
    geom_segment(aes(x = 0, y = as.numeric(group), xend = 0 + percentagePoints,
                     yend = group + 0), size = 1) +
    scale_x_continuous(#labels = percent, 
                       limits = c(-8, 8), breaks = seq(-8, 8, 1)) +
    xlab("Percentage Points") + ylab("Commodity Group") + theme(legend.position = "bottom") + 
    ggtitle("Afghanistan - 2008 \n Food Module vs HHS")


################################################################################

setwd()

comparingDataAfgh = fread("C:/Users/caetano/Documents/DES/hhs-faostat-food-calories.csv")
comparingDataAfgh

# comparingDataAfgh[, percentagePointsHHSFoodMod := 100*round(caloriesFoodModule/2168.879 - caloriesHHS/2511.207, 3)]
comparingDataAfgh[, percentageHHS := caloriesHHS/2511.207]
comparingDataAfgh[, percentageFoodMod := caloriesFoodModule/2168.879]
comparingDataAfgh[, percentageFAOSTAT := caloriesFAOSTAT/2037]

comparingDataAfgh[, percentagePointsHHSFoodMod := 100*round(percentageFoodMod - percentageHHS, 3)]
comparingDataAfgh[, percentagePointsHHSFaostatMod := 100*round(percentageFAOSTAT - percentageHHS, 3)]
comparingDataAfgh[,group := 1:nrow(comparingDataAfgh)]



commodity = unique(comparingDataAfgh$HHBS)
fbsGroupLevelModified = fbsGroupLevelModified[order(fbsGroupLevelModified)]
dput(unique(fbsGroupLevelModified))

comparingDataAfgh$HHBS <- 
    factor(
        comparingDataAfgh$HHBS, 
        levels = c("Cereals and products", "Oils and fats", "Meat", "Eggs", "Fruits and products", 
                   "Milk and cheese", "Pulses", "Spices & additives", "Roots and tubers", 
                   "Sugars and syrups", "Tree nuts", "Vegetables and products"))

ggplot(comparingDataAfgh, aes(y = as.factor(HHBS))) + 
    geom_point(aes(x = 0, size = caloriesHHS, color = "caloriesHHS")) + 
    geom_point(aes(x = percentagePointsHHSFoodMod, size = caloriesFoodModule, color = "caloriesFoodModule")) +
    # scale_colour_gradient(low = "blue") +
    labs(size = "[kcal/person/day]", colour="") +
    # scale_size(range = c(1, 10)) +
    # scale_size_continuous(range = c(0, 9)) +
    # scale_size_area(max_size = 9) +
    scale_color_manual(
        labels = c("Food Module", "HHS"),
        values = colors) +
    geom_segment(aes(x = 0, y = as.numeric(group), xend = 0 + percentagePointsHHSFoodMod,
                     yend = group + 0), size = 1) +
    scale_x_continuous(#labels = percent, 
        limits = c(-11, 11), breaks = seq(-11, 11, 1)) +
    xlab("Percentage Points") + ylab("Commodity Group") + theme(legend.position = "bottom") + 
    ggtitle("Afghanistan - 2008 \n Food Module vs HHS")



ggplot(comparingDataAfgh, aes(y = as.factor(HHBS))) + 
    geom_point(aes(x = 0, size = caloriesHHS, color = "caloriesHHS")) + 
    geom_point(aes(x = percentagePointsHHSFaostatMod, size = caloriesFoodModule, color = "caloriesFoodModule")) +
    # scale_colour_gradient(low = "blue") +
    labs(size = "[kcal/person/day]", colour="") +
    # scale_size(range = c(1, 10)) +
    # scale_size_continuous(range = c(0, 9)) +
    # scale_size_area(max_size = 9) +
    scale_color_manual(
        labels = c("FAOSTAT", "HHS"),
        values = colors) +
    geom_segment(aes(x = 0, y = as.numeric(group), xend = 0 + percentagePointsHHSFaostatMod,
                     yend = group + 0), size = 1) +
    scale_x_continuous(#labels = percent, 
        limits = c(-11, 11), breaks = seq(-11, 11, 1)) +
    xlab("Percentage Points") + ylab("Commodity Group") + theme(legend.position = "bottom") + 
    ggtitle("Afghanistan - 2008 \n FAOSTAT vs HHS")





#####################
tabAfghanistanMelt = melt.data.table(tabAfghanistan, 
                id.vars = c("geographicAreaM49", "country", "year",
                            "fbsItemTOT_description", "fbsGroupLevelModified", "origin"),
                measure.vars = c("totalCalDay", "totalCalHS"))

tabAfghanistanMelt[, percent := value/sum(value, na.rm = T),
               by = list(variable)]




ggplot(data = tabAfghanistanMelt, 
       aes(x = fbsGroupLevelModified, y = value, fill=fbsItemTOT_description)) + 
    geom_bar(stat="identity") + facet_wrap(~ fbsItemTOT_description)


################################################################################
colfunc <- colorRampPalette(c("green", "white"))
dput(colfunc(13))


colfunc <- colorRampPalette(c("red", "pink"))
dput(colfunc(5))

colfunc<-colorRampPalette(c("green", "blue", "red"))
dput(colfunc(18))

library("viridis")# nice colors

ggplot(brazil[order(brazil$fbsItemAggregated_description)], 
       aes(x = year, y = totalCalDay, fill = as.factor(fbsItemAggregated_description))) + 
    geom_bar(stat="identity") + 
    scale_fill_viridis(discrete = TRUE, option = "plasma")
    # geom_bar(stat='identity') + scale_colour_brewer(palette = "Greens")
    scale_fill_manual(
        values=colfunc(18)) +
    #geom_area(stat="identity") + 
    # scale_fill_manual(values=c("#FA8258", "#088A08")) +
    #scale_color_manual(values=c("#A9F5A9", "#81BEF7")) +
    # scale_fill_grey() +
    geom_text(aes(x = year, y = pos2, label = paste0(100*percent,"%")),
              size=3) +
    #geom_area(position = 'stack') +
    ylab('Total calories per person per day') + xlab('Year') +
    #labs(x="Year", y="Value") +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    #scale_x_continuous(limits = c(1991, 2013), breaks = seq(1991, 2013, 1)) +
    scale_y_continuous(limits = c(0, 4250), breaks = seq(0, 4250, 250), labels = space) +
    theme(legend.title = element_blank()) +
    ggtitle(country_name) +
    theme(axis.text=element_text(size=12, face="bold"),
          axis.title=element_text(size=14,face="bold")) +
    theme(legend.position="top") +
    geom_hline(aes(yintercept=4000)) 
