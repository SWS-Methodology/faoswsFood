library(ggplot2)
library(scales)
#install.packages("gtable")
library(gtable)

## Food Per Capita vs GDP

#save(data, file = "C:/Users/caetano/Documents/foodData25_01-2017.RData")

load("C:/Users/caetano/Documents/foodData25_01-2017.RData")
newFoodData = data[!is.na(foodHat)]
head(newFoodData)

newFoodData = nameData("agriculture", "aproduction", newFoodData)

setnames(newFoodData, "geographicAreaM49_description", "country")
setnames(newFoodData, "timePointYears", "year")
newFoodData[, "timePointYears_description" := NULL]

## GDP

gdp_usd_2005 = fread("C:/Users/caetano/Documents/pc_usd_2005.csv")
setnames(gdp_usd_2005, "FAOName", "country")

gdp_usd_2005[, .N, region]

## merge 2 files

keys = c("country", "year")

newFoodData = merge(newFoodData, gdp_usd_2005[, c("country", "year", "region"), with = F],
                 by = keys, all.x = T)

newFoodData = newFoodData[type == "Food Estimate"]
newFoodData[, c("foodDemand", "type", "measuredElement", "measuredElement_description", "Valid",
                "foodCommodity", "netTrade", "food") := NULL]

setnames(newFoodData, "foodHat", "newFoodData")

## Pull old food data from food domain

OldFoodDataKey = DatasetKey(
        domain = "food",
        dataset = "fooddatafs",
        dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = areaCodesM49),
            Dimension(name = "measuredElement",
                      keys = "5141"),
            Dimension(name = "measuredItemCPC",
                      keys = itemCodesCPC),
            Dimension(name = "timePointYears",
                      keys = yearCodes))
    )

oldFoodData = GetData(OldFoodDataKey, flags = TRUE)
oldFoodData[, type := getCommodityClassification(as.character(measuredItemCPC))]
oldFoodData = oldFoodData[type %in% c("Food Estimate")]
setnames(oldFoodData, "timePointYears", "year")
setnames(oldFoodData, "flagObservationStatus", "oldFlagObservationStatus")
setnames(oldFoodData, "flagMethod", "oldFlagMethod")
setnames(oldFoodData, "Value", "oldFoodData")

## Merge Food Data
keys = c("geographicAreaM49", "measuredItemCPC", "year")
oldNewFoodData = merge(newFoodData, oldFoodData[, c("geographicAreaM49", "measuredItemCPC", "year",
                                                "oldFoodData", "oldFlagObservationStatus",
                                                "oldFlagMethod"), with = F], by = keys, all.x = T)


ggplot(oldNewFoodData[region  == "Americas" & measuredItemCPC == "21115"], 
       aes(x = GDP , y = newFoodDataPC
           ,group = Protected
           ,colour = Protected
           )) +
geom_line(aes(group=1), size = 1) + 
    #geom_smooth(method="lm")  +
    # geom_line(aes(), stat = "identity", position=position_dodge(), size=1) +
    facet_wrap(~ country, scales = "free") +
    xlab('GDP per capita (constant 2005 US$)') + ylab('Food Estimates') +
    ggtitle(paste(unique(oldNewFoodData[measuredItemCPC == "21115"]$measuredItemCPC_description)))


setwd("C:/Users/caetano/Documents/Github/faoswsFood/sandbox/food_per_capita_vs_gdp")

oldNewFoodData[country %in% c("China, Main", "China,Taiwan", "China,H.Kong", "China, Macao", "Palestine(1996-)"), 
               region := "Asia"]

oldNewFoodData[country %in% c("Côte d'Ivoire", "United Republic of Tanzania"), 
               region := "Africa"]

oldNewFoodData[country %in% c("NethAntilles"), 
               region := "Americas"]


oldNewFoodData[geographicAreaM49 == 384, 
               region := "Africa"]

oldNewFoodData[geographicAreaM49 == 891, 
               region := "Europe"]

## creating a pdf

oldNewFoodData[, totalFood := sum(newFoodData, na.rm = T),
               by = list(geographicAreaM49, measuredItemCPC)]

oldNewFoodData <- oldNewFoodData[!(totalFood == 0 | is.na(totalFood))]

oldNewFoodData <- oldNewFoodData[!country %in% c("Myanmar", "China,Taiwan", "French Polynesia",
                                                 "Democratic People's Republic of Korea", 
                                                 "NethAntilles", "New Caledonia",
                                                 "Somalia", "Syrian Arab Republic", 
                                                 "Serbia-Monte(1992-2005)")]

setnames(oldNewFoodData, "Protected", "Official")

oldNewFoodData[, countryElast := paste(country, "elasticity", sep = ", ")]
oldNewFoodData[, countryElast2 := paste(countryElast, elasticity, sep = ": ")]


oldNewFoodData[, popMillion := population * 1000]

oldNewFoodData[, newFoodDataKg := newFoodData * 1000]

oldNewFoodData[, foodPerCapitaKgPerPerson := newFoodDataKg/popMillion]

# if the time series is not complete (23 figures) it will be removed

checkTimeSeries = oldNewFoodData[, .N, c("geographicAreaM49", "measuredItemCPC")]
checkTimeSeries

oldNewFoodData <- merge(oldNewFoodData, checkTimeSeries, by = c("geographicAreaM49", "measuredItemCPC"),
                    all.x = T)

oldNewFoodData <- oldNewFoodData[N == 23]
oldNewFoodData[, "N" := NULL]

item = unique(oldNewFoodData$measuredItemCPC_description)
item = item[order(item)]
dput(item)


dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsFood", "sandbox")
pdffile <- file.path(dlpath, "food_per_capita_vs_gdp", "foodvsgdp.pdf")

pdf(file = pdffile, height = 11, width = 16)


system.time(for (item in c("Almonds, in shell", "Animal Oils and Fats nes", "Apples", 
                           "Apricots", "Areca nuts", "Artichokes", "Asparagus", "Avocados", 
                           "Bambara beans, dry", "Bananas", "Barley", "Barley Flour and Grits", 
                           "Barley, Pearled", "Beans, dry", "Beer of Barley, malted", "Beer of Maize, malted", 
                           "Beer of Millet, malted", "Beer of Sorghum, malted", "Blueberries", 
                           "Bran of Buckwheat", "Bran of Rice", "Bran of Wheat", "Brazil nuts, in shell", 
                           "Broad beans and horse beans, dry", "Broad beans and horse beans, green", 
                           "Buckwheat", "Buffalo fat, unrendered", "Bulgur", "Butter and Ghee of Sheep Milk", 
                           "Butter of Buffalo Milk", "Butter of Karite Nuts", "Buttermilk, Curdled Milk, Acidifie", 
                           "Cabbages", "Cane sugar, non-centrifugal", "Cantaloupes and other melons", 
                           "Carrots and turnips", "Casein", "Cashew nuts, in shell", "Cashewapple", 
                           "Cassava, dried", "Cassava, fresh", "Cattle fat, unrendered", 
                           "Cattle, Butcher Fat", "Cauliflowers and broccoli", "Cheese from milk of buffalo, fresh or processed", 
                           "Cheese from milk of goats, fresh or processed", "Cheese from milk of sheep, fresh or processed", 
                           "Cheese from Skimmed Cow Milk", "Cherries", "Chick peas, dry", 
                           "Chicory roots", "Chillies and peppers, green (Capsicum spp. and Pimenta spp.)", 
                           "Cinnamon (canella), raw", "Cocoa butter, fat and oil", "Cocoa paste not defatted", 
                           "Coconut oil", "Coconuts, in shell", "Cottonseed oil", "Cow peas, dry", 
                           "Cranberries and other fruits of the genus Vaccinium", "Cucumbers and gherkins", 
                           "Currants", "Dairy products n.e.", "Dates", "Dry Buttermilk", 
                           "Dry Whey", "Edible offal of buffalo, fresh, chilled or frozen", 
                           "Edible offal of cattle, fresh, chilled or frozen", "Edible offal of goat, fresh, chilled or frozen", 
                           "Edible offal of pigs, fresh, chilled or frozen", "Edible offal of sheep, fresh, chilled or frozen", 
                           "Edible offals of camels and other camelids, fresh, chilled or frozen", 
                           "Edible offals of horses and other equines,  fresh, chilled or frozen", 
                           "Edible roots and tubers with high starch or inulin content, n.e., fresh", 
                           "Eggplants (aubergines)", "Eggs from other birds in shell, fresh, n.e.", 
                           #"Extracts, essences and concentrates of tea or mate, and preparations with a basis thereof or with a basis of tea or mat<U+FFFD>", 
                           "Fat of camels", "Fat of other camelids", "Fat of pigs", "Fat of poultry", 
                           "fat preparations n.e.", "Figs", "Flour of Buckwheat", "Flour of Cassava", 
                           "Flour of Cereals nes", "Flour of Fonio", "Flour of Fruits", 
                           "Flour of Maize", "Flour of Millet", "Flour of Mixed Grain", 
                           "Flour of Mustard Seed", "Flour of Pulses", "Flour of Rice", 
                           "Flour of Roots and Tubers nes", "Flour of Rye", "Flour of Sorghum", 
                           "Flour of Triticale", "Flour, meal, powder, flakes, granules and pellets of potatoes", 
                           "Fructose, Chemically Pure", "fruit prepared n.e.", "Game meat, fresh or chilled", 
                           "Germ of Wheat", "Ghee, from Buffalo Milk", "Goat fat, unrendered", 
                           "Gooseberries", "Grapes", "Green corn (maize)", "Green garlic", 
                           "Groundnut oil", "Groundnuts, excluding shelled", "Groundnuts, shelled", 
                           "Hazelnuts, in shell", "Hen eggs in shell, fresh", "Horse meat (fresh)", 
                           "Husked rice", "Isoglucose", "Leeks and other alliaceous vegetables", 
                           "Lemons and limes", "Lentils, dry", "Linseed", "Lupins", "Maize (corn)", 
                           "Maize Gluten", "Malt, whether or not roasted", "Maltose, Chemically Pure", 
                           "Mangoes, guavas, mangosteens", 
                           #"Mat<U+FFFD> leaves", 
                           "Meat of asses (fresh)", 
                           "Meat of buffalo, fresh or chilled", "Meat of camels (fresh)", 
                           "Meat of cattle boneless, fresh or chilled", "Meat of cattle, fresh or chilled", 
                           "Meat of chickens, fresh or chilled", "Meat of ducks, fresh or chilled", 
                           "Meat of geese, fresh or chilled", "Meat of goat, fresh or chilled", 
                           "Meat of mules (fresh)", "Meat of other domestic camelids (fresh)", 
                           "Meat of other domestic rodents (fresh)", "Meat of pig boneless (pork), fresh or chilled", 
                           "Meat of pig, fresh or chilled", "Meat of pigeons and other birds n.e., fresh or chilled", 
                           "Meat of rabbits and hares, fresh or chilled", "Meat of sheep, fresh or chilled", 
                           "Meat of turkeys, fresh or chilled", "Melonseed", "Millet", "Molasses (from beet, cane and maize)", 
                           "Mushrooms and truffles", "Must of Grape", "Mustard seed", "Mustard seed oil, crude", 
                           "Natural honey", "Oats", "Oats, Rolled", "Offals n.e. (excluding mammals)", 
                           "Oil of Linseed", "Oil of Maize", "Oil of Palm Kernel", "Oil of Rice Bran", 
                           "Oil of Sesame Seed", "Okra", "Olive oil", "Olives", "Onions and shallots, dry (excluding dehydrated)", 
                           "Onions and shallots, green", "Oranges", "Other berries and fruits of the genus Vaccinium n.e.", 
                           "Other cereals n.e.", "Other citrus fruit, n.e.", "Other fruits, n.e.", 
                           "Other nuts (excluding wild edible nuts and groundnuts), in shell, n.e.", 
                           "Other oil of Vegetable Origin, crude n.e.", "Other stimulant, spice and aromatic crops, n.e.", 
                           "Other sugar crops n.e.", "Other tropical and subtropical fruits, n.e.", 
                           "Other vegetables, fresh n.e.", "Palm oil", "Papayas", "Peaches and nectarines", 
                           "Pears", "Peas, dry", "Peas, green", "Persimmons", "Pig fat, rendered", 
                           "Pig meat, cuts, salted, dried or smoked (bacon and ham)", "Pig, Butcher Fat", 
                           "Pigeon peas, dry", "Pineapples", "Pistachios, in shell", "Plantains and others", 
                           "Plums and sloes", "Pomelos and grapefruits", "Poppy seed", "Pot Barley", 
                           "Potatoes", "Pulses n.e.", "Pumpkins, squash and gourds", "Quinces", 
                           "Quinoa", "Rapeseed or canola oil, crude", "Raspeberries", "Raw milk of buffalo", 
                           "Raw milk of cattle", "Raw milk of goats", "Raw milk of sheep", 
                           "Rice", "Rice, Broken", "Rice, Milled", "Rice, Milled (Husked)", 
                           "Safflower-seed oil, crude", "Sesame seed", "Sheep fat, unrendered", 
                           "Skim Milk of Buffalo", "Skim Sheep Milk", "Snails, fresh, chilled, frozen, dried, salted or in brine, except sea snails", 
                           "Sorghum", "Sour cherries", "Soya bean oil", "Soya beans", "Soya Curd", 
                           "Soya Paste", "Spinach", "Starch of Cassava", "Starch of Maize", 
                           "Starch of Potatoes", "Starch of Rice", "Starch of Wheat", "Stone fruit, n.e.", 
                           "Strawberries", "String beans", "Sugar beet", "Sugar cane", "Sunflower-seed oil, crude", 
                           "Sunflower seed", "Sweet potatoes", "Tallow", "Tangerines, mandarins, clementines", 
                           "Tapioca of Cassava", "Tapioca of Potatoes", "Taro", "Tomatoes", 
                           "Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80% vol; spirits, liqueurs and other spirituous beverages", 
                           "Vetches", "Walnuts, in shell", "Watermelons", "Wheat", "Wheat-Fermented Beverages", 
                           "Wheat and meslin flour", "Wheat Gluten", "Whey, Fresh", "Yams", 
                           "Yautia", "Yoghurt, Concentrated or Unconcent"
                 )) {
    for (reg in c("Europe", "Africa", "Asia", "Americas", "Oceania")) {
        
        if(nrow(oldNewFoodData[region == reg & measuredItemCPC_description == item]) > 0) {    
            
            p = ggplot(oldNewFoodData[region == reg & measuredItemCPC_description == item], 
                       aes(x = GDP , y = foodPerCapitaKgPerPerson, group = Official, colour = Official)) +
                geom_line(aes(group=1), size = 1) +
                facet_wrap(~ countryElast2, scales = "free") +
                xlab('GDP per capita (constant 2005 US$)') + ylab('Food Estimate per capita (kg/person)') +
                ggtitle(paste(reg, unique(oldNewFoodData[measuredItemCPC_description == item]$measuredItemCPC_description), sep=" - "))
            
            print(p)
        }
        
    }
})


dev.off()




## Another plot

meltOldNewFoodData <- melt.data.table(oldNewFoodData,
                                      id.vars = c("geographicAreaM49","country",
                                                  "measuredItemCPC", "measuredItemCPC_description",
                                                  "year", "countryElast2", "region"),
                                      measure.vars = c("GDP", "foodPerCapitaKgPerPerson"))

meltOldNewFoodData[variable == "GDP", 
                   variableNames := "GDP per capita (constant 2005 US$)"]
meltOldNewFoodData[variable == "foodPerCapitaKgPerPerson", 
                   variableNames := "Food Estimate per capita (kg/person)"]


meltOldNewFoodData[variable == "GDP", 
                   variable2 := "GDP"]
meltOldNewFoodData[variable == "foodPerCapitaKgPerPerson", 
                   variable2 := "Food"]

## with time series

item = unique(meltOldNewFoodData$measuredItemCPC_description)
item = item[order(item)]
dput(item)


dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsFood", "sandbox")
pdffile <- file.path(dlpath, "food_per_capita_vs_gdp", "foodvsgdp-timeseries.pdf")

pdf(file = pdffile, height = 11, width = 16)

system.time(for (item in c("Almonds, in shell", "Animal Oils and Fats nes", "Apples", 
                           "Apricots", "Areca nuts", "Artichokes", "Asparagus", "Avocados", 
                           "Bambara beans, dry", "Bananas", "Barley", "Barley Flour and Grits", 
                           "Barley, Pearled", "Beans, dry", "Beer of Barley, malted", "Beer of Maize, malted", 
                           "Beer of Millet, malted", "Beer of Sorghum, malted", "Blueberries", 
                           "Bran of Buckwheat", "Bran of Rice", "Bran of Wheat", "Brazil nuts, in shell", 
                           "Broad beans and horse beans, dry", "Broad beans and horse beans, green", 
                           "Buckwheat", "Buffalo fat, unrendered", "Bulgur", "Butter and Ghee of Sheep Milk", 
                           "Butter of Buffalo Milk", "Butter of Karite Nuts", "Buttermilk, Curdled Milk, Acidifie", 
                           "Cabbages", "Cane sugar, non-centrifugal", "Cantaloupes and other melons", 
                           "Carrots and turnips", "Casein", "Cashew nuts, in shell", "Cashewapple", 
                           "Cassava, dried", "Cassava, fresh", "Cattle fat, unrendered", 
                           "Cattle, Butcher Fat", "Cauliflowers and broccoli", "Cheese from milk of buffalo, fresh or processed", 
                           "Cheese from milk of goats, fresh or processed", "Cheese from milk of sheep, fresh or processed", 
                           "Cheese from Skimmed Cow Milk", "Cherries", "Chick peas, dry", 
                           "Chicory roots", "Chillies and peppers, green (Capsicum spp. and Pimenta spp.)", 
                           "Cinnamon (canella), raw", "Cocoa butter, fat and oil", "Cocoa paste not defatted", 
                           "Coconut oil", "Coconuts, in shell", "Cottonseed oil", "Cow peas, dry", 
                           "Cranberries and other fruits of the genus Vaccinium", "Cucumbers and gherkins", 
                           "Currants", "Dairy products n.e.", "Dates", "Dry Buttermilk", 
                           "Dry Whey", "Edible offal of buffalo, fresh, chilled or frozen", 
                           "Edible offal of cattle, fresh, chilled or frozen", "Edible offal of goat, fresh, chilled or frozen", 
                           "Edible offal of pigs, fresh, chilled or frozen", "Edible offal of sheep, fresh, chilled or frozen", 
                           "Edible offals of camels and other camelids, fresh, chilled or frozen", 
                           "Edible offals of horses and other equines,  fresh, chilled or frozen", 
                           "Edible roots and tubers with high starch or inulin content, n.e., fresh", 
                           "Eggplants (aubergines)", "Eggs from other birds in shell, fresh, n.e.", 
                           #"Extracts, essences and concentrates of tea or mate, and preparations with a basis thereof or with a basis of tea or mat<U+FFFD>", 
                           "Fat of camels", "Fat of other camelids", "Fat of pigs", "Fat of poultry", 
                           "fat preparations n.e.", "Figs", "Flour of Buckwheat", "Flour of Cassava", 
                           "Flour of Cereals nes", "Flour of Fonio", "Flour of Fruits", 
                           "Flour of Maize", "Flour of Millet", "Flour of Mixed Grain", 
                           "Flour of Mustard Seed", "Flour of Pulses", "Flour of Rice", 
                           "Flour of Roots and Tubers nes", "Flour of Rye", "Flour of Sorghum", 
                           "Flour of Triticale", "Flour, meal, powder, flakes, granules and pellets of potatoes", 
                           "Fructose, Chemically Pure", "fruit prepared n.e.", "Game meat, fresh or chilled", 
                           "Germ of Wheat", "Ghee, from Buffalo Milk", "Goat fat, unrendered", 
                           "Gooseberries", "Grapes", "Green corn (maize)", "Green garlic", 
                           "Groundnut oil", "Groundnuts, excluding shelled", "Groundnuts, shelled", 
                           "Hazelnuts, in shell", "Hen eggs in shell, fresh", "Horse meat (fresh)", 
                           "Husked rice", "Isoglucose", "Leeks and other alliaceous vegetables", 
                           "Lemons and limes", "Lentils, dry", "Linseed", "Lupins", "Maize (corn)", 
                           "Maize Gluten", "Malt, whether or not roasted", "Maltose, Chemically Pure", 
                           "Mangoes, guavas, mangosteens", 
                           #"Mat<U+FFFD> leaves", 
                           "Meat of asses (fresh)", 
                           "Meat of buffalo, fresh or chilled", "Meat of camels (fresh)", 
                           "Meat of cattle boneless, fresh or chilled", "Meat of cattle, fresh or chilled", 
                           "Meat of chickens, fresh or chilled", "Meat of ducks, fresh or chilled", 
                           "Meat of geese, fresh or chilled", "Meat of goat, fresh or chilled", 
                           "Meat of mules (fresh)", "Meat of other domestic camelids (fresh)", 
                           "Meat of other domestic rodents (fresh)", "Meat of pig boneless (pork), fresh or chilled", 
                           "Meat of pig, fresh or chilled", "Meat of pigeons and other birds n.e., fresh or chilled", 
                           "Meat of rabbits and hares, fresh or chilled", "Meat of sheep, fresh or chilled", 
                           "Meat of turkeys, fresh or chilled", "Melonseed", "Millet", "Molasses (from beet, cane and maize)", 
                           "Mushrooms and truffles", "Must of Grape", "Mustard seed", "Mustard seed oil, crude", 
                           "Natural honey", "Oats", "Oats, Rolled", "Offals n.e. (excluding mammals)", 
                           "Oil of Linseed", "Oil of Maize", "Oil of Palm Kernel", "Oil of Rice Bran", 
                           "Oil of Sesame Seed", "Okra", "Olive oil", "Olives", "Onions and shallots, dry (excluding dehydrated)", 
                           "Onions and shallots, green", "Oranges", "Other berries and fruits of the genus Vaccinium n.e.", 
                           "Other cereals n.e.", "Other citrus fruit, n.e.", "Other fruits, n.e.", 
                           "Other nuts (excluding wild edible nuts and groundnuts), in shell, n.e.", 
                           "Other oil of Vegetable Origin, crude n.e.", "Other stimulant, spice and aromatic crops, n.e.", 
                           "Other sugar crops n.e.", "Other tropical and subtropical fruits, n.e.", 
                           "Other vegetables, fresh n.e.", "Palm oil", "Papayas", "Peaches and nectarines", 
                           "Pears", "Peas, dry", "Peas, green", "Persimmons", "Pig fat, rendered", 
                           "Pig meat, cuts, salted, dried or smoked (bacon and ham)", "Pig, Butcher Fat", 
                           "Pigeon peas, dry", "Pineapples", "Pistachios, in shell", "Plantains and others", 
                           "Plums and sloes", "Pomelos and grapefruits", "Poppy seed", "Pot Barley", 
                           "Potatoes", "Pulses n.e.", "Pumpkins, squash and gourds", "Quinces", 
                           "Quinoa", "Rapeseed or canola oil, crude", "Raspeberries", "Raw milk of buffalo", 
                           "Raw milk of cattle", "Raw milk of goats", "Raw milk of sheep", 
                           "Rice", "Rice, Broken", "Rice, Milled", "Rice, Milled (Husked)", 
                           "Safflower-seed oil, crude", "Sesame seed", "Sheep fat, unrendered", 
                           "Skim Milk of Buffalo", "Skim Sheep Milk", "Snails, fresh, chilled, frozen, dried, salted or in brine, except sea snails", 
                           "Sorghum", "Sour cherries", "Soya bean oil", "Soya beans", "Soya Curd", 
                           "Soya Paste", "Spinach", "Starch of Cassava", "Starch of Maize", 
                           "Starch of Potatoes", "Starch of Rice", "Starch of Wheat", "Stone fruit, n.e.", 
                           "Strawberries", "String beans", "Sugar beet", "Sugar cane", "Sunflower-seed oil, crude", 
                           "Sunflower seed", "Sweet potatoes", "Tallow", "Tangerines, mandarins, clementines", 
                           "Tapioca of Cassava", "Tapioca of Potatoes", "Taro", "Tomatoes", 
                           "Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80% vol; spirits, liqueurs and other spirituous beverages", 
                           "Vetches", "Walnuts, in shell", "Watermelons", "Wheat", "Wheat-Fermented Beverages", 
                           "Wheat and meslin flour", "Wheat Gluten", "Whey, Fresh", "Yams", 
                           "Yautia", "Yoghurt, Concentrated or Unconcent")) {
    for (reg in c("Europe", "Africa", "Asia", "Americas", "Oceania")) {
        
        if(nrow(meltOldNewFoodData[region == reg & measuredItemCPC_description == item]) > 0) {    

         p = ggplot(meltOldNewFoodData[region == reg & measuredItemCPC_description == item],
                   aes(x=as.numeric(year), y=value, group=variableNames, col=variableNames)) +
                geom_line() +
             #facet_grid(country ~ variableNames) +
         facet_wrap(country ~ variable2, scales = "free") +
                geom_blank(data=meltOldNewFoodData[region == reg & measuredItemCPC_description == item]) +
                #ggtitle(paste(countryName, itemName, sep = "-")) +
                xlab('GDP per capita (constant 2005 US$)') + ylab('Food Estimate per capita (kg/person)')+
                labs(x="Year", y="Value") +
                theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
                scale_x_continuous(limits = c(1991, 2013), breaks = c(1991, 1994,
                                                                      1997, 
                                                                      2000,
                                                                      2003, 2006,
                                                                      2009, 
                                                                      2013)) +
                theme(legend.title = element_blank()) +
                ggtitle(paste(reg, unique(meltOldNewFoodData[measuredItemCPC_description == item]$measuredItemCPC_description), sep=" - "))
            
            

print(p)
        }
        
    }
})

dev.off()

################################################################################

# Include type of commodity

meltOldNewFoodData[, cpc3digits := substr(measuredItemCPC, 1, 3)]

meltOldNewFoodData[, .N, cpc3digits]

meltOldNewFoodData[cpc3digits == "011", foodGroup := "Cereals"]
meltOldNewFoodData[cpc3digits == "012", foodGroup := "Vegetables"]
meltOldNewFoodData[cpc3digits == "013", foodGroup := "Fruit and nuts"]
meltOldNewFoodData[cpc3digits == "014", foodGroup := "Oilseeds and oleaginous fruits"]
meltOldNewFoodData[cpc3digits == "015", foodGroup := "Edible roots and tubers"]
meltOldNewFoodData[cpc3digits == "016", foodGroup := "Spice and aromatic crops"]
meltOldNewFoodData[cpc3digits == "017", foodGroup := "Pulses"]
meltOldNewFoodData[cpc3digits == "018", foodGroup := "Sugar Crops"]
meltOldNewFoodData[cpc3digits == "019", foodGroup := "Fibres, living plants"]
meltOldNewFoodData[cpc3digits == "021", foodGroup := "Live animals"]
meltOldNewFoodData[cpc3digits == "022", foodGroup := "Raw milk"]
meltOldNewFoodData[cpc3digits == "023", foodGroup := "Eggs"]
meltOldNewFoodData[cpc3digits == "029", foodGroup := "Other animal products"]
meltOldNewFoodData[cpc3digits == "211", foodGroup := "Meat and meat products"]
meltOldNewFoodData[cpc3digits == "212", foodGroup := "Fish, crustaceans, molluscs"]
meltOldNewFoodData[cpc3digits == "213", foodGroup := "Prepared and preserved vegetables, pulses and potatoes"]
meltOldNewFoodData[cpc3digits == "214", foodGroup := "Prepared and preserved fruit and nuts"]
meltOldNewFoodData[cpc3digits == "215", foodGroup := "Animal fats"]
meltOldNewFoodData[cpc3digits == "216", foodGroup := "Vegetable oils"]
meltOldNewFoodData[cpc3digits == "221", foodGroup := "Processed liquid milk and cream"]
meltOldNewFoodData[cpc3digits == "222", foodGroup := "Other dairy products"]
meltOldNewFoodData[cpc3digits == "223", foodGroup := "Eggs, in shell, preserved or cooked"]
meltOldNewFoodData[cpc3digits == "231", foodGroup := "Grain mill products"]
meltOldNewFoodData[cpc3digits == "239", foodGroup := "Food products n.e./n"]
meltOldNewFoodData[cpc3digits == "241", foodGroup := "Ethyl alcohol, spirits"]
meltOldNewFoodData[cpc3digits == "242", foodGroup := "Wines"]
meltOldNewFoodData[cpc3digits == "243", foodGroup := "Malt liquors and malt"]
meltOldNewFoodData[cpc3digits == "239", foodGroup := "Food products n.e./n"]
meltOldNewFoodData[is.na(foodGroup), foodGroup := "Other products"]

foodGroups = unique(meltOldNewFoodData$foodGroup)
foodGroups = foodGroups[order(foodGroups)]
dput(foodGroups)

## creating a pdf by country

dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsFood", "sandbox")
pdffile <- file.path(dlpath, "food_per_capita_vs_gdp", "foodvsgdp-timeseries-USA.pdf")

pdf(file = pdffile, height = 11, width = 16)

system.time(for (group in c("Animal fats", "Cereals", "Edible roots and tubers", "Eggs", 
                            "Ethyl alcohol, spirits", "Food products n.e./n", "Fruit and nuts", 
                            "Grain mill products", "Malt liquors and malt", "Meat and meat products", 
                            "Oilseeds and oleaginous fruits", "Other animal products", "Other dairy products", 
                            "Other products", "Prepared and preserved fruit and nuts", "Prepared and preserved vegetables, pulses and potatoes", 
                            "Processed liquid milk and cream", "Pulses", "Raw milk", "Spice and aromatic crops", 
                            "Sugar Crops", "Vegetable oils", "Vegetables", "Wines"
                   
 )) {
        
        if(nrow(meltOldNewFoodData[foodGroup == group & geographicAreaM49 == 840]) > 0) {    
            
            p = ggplot(meltOldNewFoodData[foodGroup == group & geographicAreaM49 == 840],
                       aes(x=as.numeric(year), y=value, group=variableNames, col=variableNames)) +
                geom_line() +
                #facet_grid(country ~ variableNames) +
                facet_wrap(measuredItemCPC_description ~ variableNames, scales = "free") +
                #geom_blank(data=meltOldNewFoodData[geographicAreaM49 == 356 & measuredItemCPC_description == item]) +
                #ggtitle(paste(countryName, itemName, sep = "-")) +
                xlab('GDP per capita (constant 2005 US$)') + ylab('Food Estimate per capita (kg/person)')+
                labs(x="Year", y="Value") +
                theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
                scale_x_continuous(limits = c(1991, 2013), breaks = c(1991, 1994,
                                                                      1997, 
                                                                      2000,
                                                                      2003, 2006,
                                                                      2009, 
                                                                      2013)) +
                theme(legend.title = element_blank()) +
                ggtitle(paste("United States of America",
                    group, sep=" - ")) + theme(legend.position="top")

            print(p)
        }
        
    }
)

dev.off()








################################################################################
# Run function
ggplot_dual_axis(p1, p2, "y")




multiplot(p1, p2)

p1 = ggplot(meltOldNewFoodData[geographicAreaM49 == 840 & measuredItemCPC == "23110"],
       aes(x=as.numeric(year), y=value, group=variableNames, col=variableNames)) +
    geom_line() +
    facet_wrap(country ~ variableNames, scales = "free") +
geom_blank(data=meltOldNewFoodData[geographicAreaM49 == 840 & measuredItemCPC == "23110"]) +
#ggtitle(paste(countryName, itemName, sep = "-")) +
xlab('GDP per capita (constant 2005 US$)') + ylab('Food Estimate per capita (kg/person)')+
labs(x="Year", y="Value") +
theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
scale_x_continuous(limits = c(1991, 2013), breaks = seq(1991, 2013, 3)) +
theme(legend.title = element_blank()) +
ggtitle(paste(reg, unique(meltOldNewFoodData[measuredItemCPC_description == item]$measuredItemCPC_description), sep=" - "))



# require(gridExtra) # also loads grid
require(lattice)

p1 = levelplot(oldNewFoodData[geographicAreaM49 == 840 & measuredItemCPC == "23110"]$year, 
          oldNewFoodData[geographicAreaM49 == 840 & measuredItemCPC == "23110"]$foodPerCapitaKgPerPerson, type="l", col="red3")


par(mar = c(5,5,2,5))

par(mfrow=c(2,2))

oldNewFoodData[, .N, c("region", "measuredItemCPC")]

oldNewFoodData[, list(numCountry = length(unique(geographicAreaM49))), 
               by=list(region, measuredItemCPC_description)]


###


# par(mfrow=c(8, 7))
par(mfrow=c(2,3))
with(oldNewFoodData[geographicAreaM49 == 840 & measuredItemCPC == "23110"], 
     plot(as.numeric(year), foodPerCapitaKgPerPerson, type="l", col="red3"))
mtext(side = 4, line = 3, 'Food Estimate per capita (kg/person)')

par(new = T)
with(oldNewFoodData[geographicAreaM49 == 840 & measuredItemCPC == "23110"], 
     plot(as.numeric(year), GDP, type="l", col="blue", ylab=NA, axes=F))
axis(side = 4)
mtext(side = 4, line = 3, 'GDP per capita (constant 2005 US$)')
legend("topleft",
       legend=c("Food", "GDP"),
       lty=c(1,1), pch=c(NA, NA), col=c("red3", "blue"))





p2 = ggplot(meltOldNewFoodData[geographicAreaM49 == 76 & measuredItemCPC == "23110"],
            aes(x=as.numeric(year), y=value, group=variableNames, col=variableNames)) +
    geom_line() +
    facet_wrap(country ~ variableNames, scales = "free") +
    #ggtitle(paste(countryName, itemName, sep = "-")) +
    xlab('GDP per capita (constant 2005 US$)') + ylab('Food Estimate per capita (kg/person)')+
    labs(x="Year", y="Value") +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    scale_x_continuous(limits = c(1991, 2013), breaks = seq(1991, 2013, 3)) +
    theme(legend.title = element_blank()) + theme(legend.position="top")


par(xpd = T, mar = par()$mar + c(0,0,0,7))
with(oldNewFoodData[geographicAreaM49 == 840 & measuredItemCPC == "23110"], 
     plot(as.numeric(year), foodPerCapitaKgPerPerson, type="l", col="red3"))
             #mtext(side = 4, line = 3, 'Food Estimate per capita (kg/person)')
             
#par(mfrow=c(8, 7))
par(new = T)
with(oldNewFoodData[geographicAreaM49 == 840 & measuredItemCPC == "23110"], 
                  plot(as.numeric(year), GDP, type="l", col="blue", ylab=NA, axes=F))
axis(side = 4)
mtext(side = 4, line = 3, 'GDP per capita (constant 2005 US$)')
legend("topleft", legend=c("Food", "GDP"),
lty=c(1,1), pch=c(NA, NA), col=c("red3", "blue"))
             

###############################################################################

# par(xpd = T, mar = par()$mar + c(0,7))
# par(mar = c(5, 4, 4, 8), xpd = T)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(as.numeric(test[country == i & measuredItemCPC_description == "Bananas"]$year),
     test[country == i & measuredItemCPC_description == "Bananas"]$foodPerCapitaKgPerPerson,
     type="l", col="red3", ylab="Food Estimate per capita (kg/person)", xlab = "Year", bty='L')
#mtext(side = 4, line = 3, 'GDP per capita (constant 2005 US$)')
par(new = T)
plot(as.numeric(test[country == i & measuredItemCPC_description == "Bananas"]$year),
     test[country == i & measuredItemCPC_description == "Bananas"]$GDP,
     type="l", col="blue", ylab=NA, axes = F, xlab="Year")
axis(side = 4)
mtext(side = 4, line = 3, 'GDP per capita (constant 2005 US$)')

legend("top", c("Food", "GDP"), xpd = TRUE, horiz = TRUE, 
       bty = "n", pch = c(NA, NA), lty=c(1,1), col = 1:2, cex = 1)


# par(xpd = TRUE)

# legend("topleft", inset=c(0,.5), legend=c("Food", "GDP"), pch=c(1,3),
#        title="", col=c("red3", "blue"))
# par(mar=c(5, 2) + 0.1)


















             
