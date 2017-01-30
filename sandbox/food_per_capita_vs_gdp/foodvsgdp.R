
dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsFood", "sandbox")
pdffile <- file.path(dlpath, "food_per_capita_vs_gdp", "foodvsgdp.pdf")

pdf(file = pdffile, height = 11, width = 16)


oldNewFoodData[, totalFoodperCapita := sum(newFoodDataPC, na.rm = T),
                      by = list(geographicAreaM49, measuredItemCPC)]

oldNewFoodData <- oldNewFoodData[!(totalFoodperCapita == 0 | is.na(totalFoodperCapita))]

item = unique(oldNewFoodData$measuredItemCPC_description)
item = item[order(item)]
dput(item)

# oldNewFoodData[is.na(newFoodDataPC), foodFlagNA := 1,
#                by=list(geographicAreaM49, measuredItemCPC)]
# 
# oldNewFoodData[!is.na(newFoodDataPC), foodFlagNA := 0,
#                by=list(geographicAreaM49, measuredItemCPC)]
# 
# oldNewFoodData[, maxFoodFlagNA := max(foodFlagNA),
#                by=list(geographicAreaM49, measuredItemCPC)]
# 
# 
# oldNewFoodData[is.na(GDP), gdpFlagNA := 1,
#                by=list(geographicAreaM49, measuredItemCPC)]
# 
# oldNewFoodData[!is.na(GDP), gdpFlagNA := 0,
#                by=list(geographicAreaM49, measuredItemCPC)]
# 
# oldNewFoodData[, maxGDPFlagNA := max(gdpFlagNA),
#                by=list(geographicAreaM49, measuredItemCPC)]



oldNewFoodData <- oldNewFoodData[!country %in% c("Myanmar", "China,Taiwan", "French Polynesia", "Democratic People's Republic of Korea", 
                                         "NethAntilles", "New Caledonia", "Somalia", "Syrian Arab Republic", 
                                         "Serbia-Monte(1992-2005)")]

setnames(oldNewFoodData, "Protected", "Official")


oldNewFoodData[, c("regionNumber", "foodFlagNA", "maxFoodFlagNA", "gdpFlagNA", "maxGDPFlagNA") := NULL]
oldNewFoodData[, countryElast := paste(country, "elasticity", sep = ", ")]
oldNewFoodData[, countryElast2 := paste(countryElast, elasticity, sep = ": ")]


oldNewFoodData[, imputedPopulation := na.locf(population, fromLast = TRUE),
               by = list(geographicAreaM49)]

oldNewFoodData[, popMillion := population * 1000]
oldNewFoodData[, imputedPopMillion := na.locf(popMillion, fromLast = TRUE),
               by = list(geographicAreaM49)]

oldNewFoodData[, newFoodDataKg := newFoodData * 1000]

oldNewFoodData[, foodPerCapitaKgPerPerson := newFoodDataKg/imputedPopMillion]


###

# timeSeries <- as.data.table(expand.grid(geographicAreaM49 = unique(oldNewFoodData$geographicAreaM49),
#                                         measuredItemCPC = unique(oldNewFoodData$measuredItemCPC),
#                                         year = unique(oldNewFoodData$year)))
# 
# keys = c("geographicAreaM49", "measuredItemCPC", "year")
# timeSeries = merge(timeSeries, oldNewFoodData[, c("geographicAreaM49", "measuredItemCPC", "year",
#                                    "Protected", "newFoodDataPC", "GDP", "region",
#                                    "measuredItemCPC_description", "country"), with = F],
#       by = keys, all = T)
# 
# # timeSeries[is.na(maxFoodFlagNA), maxFoodFlagNA := 1]
# # timeSeries[is.na(maxGDPFlagNA), maxGDPFlagNA := 1]
# 
# timeSeries[is.na(newFoodDataPC), newFoodDataPC := 0]
# 
# gdpCountry = oldNewFoodData[, list(GDP = max(GDP)),
#                by = list(geographicAreaM49, year)]
# 
# keys = c("geographicAreaM49", "year")
# timeSeries = merge(timeSeries, gdpCountry, by= keys, all.x = T)
# setnames(timeSeries, "GDP.y", "GDP")
# 
# timeSeries[, "GDP.x" := NULL]
# 
# #timeSeries[is.na(GDP), GDP := 0]
# 
# timeSeries[is.na(Protected), Protected := FALSE]
# 
# timeSeries[, c("measuredItemCPC_description", "country") := NULL]
# 
# timeSeries = nameData("agriculture", "aproduction", timeSeries)
# setnames(timeSeries, "geographicAreaM49_description", "country")
# 
# tabRegion = timeSeries[!is.na(region), .N, c("geographicAreaM49", "region")]
# 
# timeSeries = merge(timeSeries, tabRegion[, c("geographicAreaM49", "region"), with = FALSE],
#       by = "geographicAreaM49", all.x = T)
# 
# timeSeries[, "region.x" := NULL]
# setnames(timeSeries, "region.y", "region")
# 
# timeSeries <- timeSeries[!country %in% c("Myanmar", "China,Taiwan", "French Polynesia", "Democratic People's Republic of Korea", 
#                                         "NethAntilles", "New Caledonia", "Somalia", "Sudan (former)", "Syrian Arab Republic", 
#                                         "Serbia-Monte(1992-2005)")]
# 
# setnames(timeSeries, "Protected", "Official")



system.time(for (item in c("Almonds, in shell", "Animal Oils and Fats nes", "Apples", 
                           "Apricots", "Areca nuts", "Artichokes", "Asparagus", "Avocados", 
                           "Bambara beans, dry", "Bananas", "Barley", "Barley Flour and Grits", 
                           "Barley, Pearled", "Beans, dry", "Beer of Barley, malted"
                           # , "Beer of Maize, malted", 
                           # "Beer of Millet, malted", "Beer of Sorghum, malted", "Blueberries", 
                           # "Bran of Buckwheat", "Bran of Rice", "Bran of Rye", "Bran of Wheat", 
                           # "Brazil nuts, in shell", "Broad beans and horse beans, dry", 
                           # "Broad beans and horse beans, green", "Buckwheat", "Buffalo fat, unrendered", 
                           # "Bulgur", "Butter and Ghee of Sheep Milk", "Butter of Buffalo Milk", 
                           # "Butter of Karite Nuts", "Buttermilk, Curdled Milk, Acidifie", 
                           # "Cabbages", "Cane sugar, non-centrifugal", "Cantaloupes and other melons", 
                           # "Carrots and turnips", "Casein", "Cashew nuts, in shell", "Cashewapple", 
                           # "Cassava, dried", "Cassava, fresh", "Cattle fat, unrendered", 
                           # "Cattle, Butcher Fat", "Cauliflowers and broccoli", "Cheese from milk of buffalo, fresh or processed", 
                           # "Cheese from milk of goats, fresh or processed", "Cheese from milk of sheep, fresh or processed", 
                           # "Cheese from Skimmed Cow Milk", "Cherries", "Chick peas, dry", 
                           # "Chicory roots", "Chillies and peppers, green (Capsicum spp. and Pimenta spp.)", 
                           # "Cinnamon (canella), raw", "Cocoa butter, fat and oil", "Cocoa paste not defatted", 
                           # "Coconut oil", "Coconuts, in shell", "Cottonseed oil", "Cow peas, dry", 
                           # "Cranberries and other fruits of the genus Vaccinium", "Cucumbers and gherkins", 
                           # "Currants", "Dairy products n.e.", "Dates", "Dry Buttermilk", 
                           # "Dry Whey", "Edible offal of buffalo, fresh, chilled or frozen", 
                           # "Edible offal of cattle, fresh, chilled or frozen", "Edible offal of goat, fresh, chilled or frozen", 
                           # "Edible offal of pigs, fresh, chilled or frozen", "Edible offal of sheep, fresh, chilled or frozen", 
                           # "Edible offals of camels and other camelids, fresh, chilled or frozen", 
                           # "Edible offals of horses and other equines,  fresh, chilled or frozen", 
                           # "Edible roots and tubers with high starch or inulin content, n.e., dried", 
                           # "Edible roots and tubers with high starch or inulin content, n.e., fresh", 
                           # "Eggplants (aubergines)", "Eggs from other birds in shell, fresh, n.e.", 
                           # #"Extracts, essences and concentrates of tea or mate, and preparations with a basis thereof or with a basis of tea or mat<U+FFFD>", 
                           # "Fat of camels", "Fat of other camelids", "Fat of pigs", "Fat of poultry", 
                           # "fat preparations n.e.", "Figs", "Flour of Buckwheat", "Flour of Cassava", 
                           # "Flour of Cereals nes", "Flour of Fonio", "Flour of Fruits", 
                           # "Flour of Maize", "Flour of Millet", "Flour of Mixed Grain", 
                           # "Flour of Mustard Seed", "Flour of Pulses", "Flour of Rice", 
                           # "Flour of Roots and Tubers nes", "Flour of Rye", "Flour of Sorghum", 
                           # "Flour of Triticale", "Flour, meal, powder, flakes, granules and pellets of potatoes", 
                           # "Fonio", "Fructose, Chemically Pure", "fruit prepared n.e.", 
                           # "Game meat, fresh or chilled", "Germ of Wheat", "Ghee, from Buffalo Milk", 
                           # "Goat fat, unrendered", "Gooseberries", "Grapes", "Green corn (maize)", 
                           # "Green garlic", "Groundnut oil", "Groundnuts, excluding shelled", 
                           # "Groundnuts, shelled", "Hazelnuts, in shell", "Hen eggs in shell, fresh", 
                           # "Hop cones (fresh and dried)", "Horse meat (fresh)", "Husked rice", 
                           # "Isoglucose", "Lard Stearine and Lard Oil", "Leeks and other alliaceous vegetables", 
                           # "Lemons and limes", "Lentils, dry", "Linseed", "Lupins", "Maize (corn)", 
                           # "Maize Gluten", "Malt, whether or not roasted", "Maltose, Chemically Pure", 
                           # "Mangoes, guavas, mangosteens", 
                           # #"Mat<U+FFFD> leaves", 
                           # "Meat meal", 
                           # "Meat of asses (fresh)", "Meat of buffalo, fresh or chilled", 
                           # "Meat of camels (fresh)", "Meat of cattle boneless, fresh or chilled", 
                           # "Meat of cattle, fresh or chilled", "Meat of chickens, fresh or chilled", 
                           # "Meat of ducks, fresh or chilled", "Meat of geese, fresh or chilled", 
                           # "Meat of goat, fresh or chilled", "Meat of mules (fresh)", "Meat of other domestic camelids (fresh)", 
                           # "Meat of other domestic rodents (fresh)", "Meat of pig boneless (pork), fresh or chilled", 
                           # "Meat of pig, fresh or chilled", "Meat of pigeons and other birds n.e., fresh or chilled", 
                           # "Meat of rabbits and hares, fresh or chilled", "Meat of sheep, fresh or chilled", 
                           # "Meat of turkeys, fresh or chilled", "Melonseed", "Millet", "Molasses (from beet, cane and maize)", 
                           # "Mushrooms and truffles", "Must of Grape", "Mustard seed", "Mustard seed oil, crude", 
                           # "Natural honey", "Oats", "Oats, Rolled", "Offals n.e. (excluding mammals)", 
                           # "oil from fish and marine mammals", "Oil of Linseed", "Oil of Maize", 
                           # "Oil of Palm Kernel", "Oil of Rice Bran", "Oil of Sesame Seed", 
                           # "Okra", "Olive oil", "Olive Residues", "Olives", "Onions and shallots, dry (excluding dehydrated)", 
                           # "Onions and shallots, green", "Oranges", "Other beans, green", 
                           # "Other berries and fruits of the genus Vaccinium n.e.", "Other cereals n.e.", 
                           # "Other citrus fruit, n.e.", "Other fruit n.e.c., dried", "Other fruits, n.e.", 
                           # "Other meat n.e. (excluding mammals)", "Other nuts (excluding wild edible nuts and groundnuts), in shell, n.e.", 
                           # "Other oil of Vegetable Origin, crude n.e.", "Other stimulant, spice and aromatic crops, n.e.", 
                           # "Other sugar crops n.e.", "Other tropical and subtropical fruits, n.e.", 
                           # "Other tropical fruit, dried", "Other vegetables, fresh n.e.", 
                           # "Palm oil", "Papayas", "Peaches and nectarines", "Pears", "Peas, dry", 
                           # "Peas, green", "Persimmons", "Pig fat, rendered", "Pig meat, cuts, salted, dried or smoked (bacon and ham)", 
                           # "Pig, Butcher Fat", "Pigeon peas, dry", "Pineapples", "Pistachios, in shell", 
                           # "Plantains and others", "Plums and sloes", "Pomelos and grapefruits", 
                           # "Poppy seed", "Pot Barley", "Potatoes", "Pulses n.e.", "Pumpkins, squash and gourds", 
                           # "Quinces", "Quinoa", "Rapeseed or canola oil, crude", "Raspeberries", 
                           # "Raw cane or beet sugar (centrifugal only)", "Raw milk of buffalo", 
                           # "Raw milk of cattle", "Raw milk of goats", "Raw milk of sheep", 
                           # "Rice", "Rice, Broken", "Rice, Milled", "Rice, Milled (Husked)", 
                           # "Safflower-seed oil, crude", "Sesame seed", "Sheep fat, unrendered", 
                           # "Skim Milk of Buffalo", "Skim Sheep Milk", "Snails, fresh, chilled, frozen, dried, salted or in brine, except sea snails", 
                           # "Sorghum", "Sour cherries", "Soya bean oil", "Soya beans", "Soya Curd", 
                           # "Soya Paste", "Spinach", "Starch of Cassava", "Starch of Maize", 
                           # "Starch of Potatoes", "Starch of Rice", "Starch of Wheat", "Stone fruit, n.e.", 
                           # "Strawberries", "String beans", "Sugar beet", "Sugar cane", "Sunflower-seed oil, crude", 
                           # "Sunflower seed", "Sweet potatoes", "Tallow", "Tangerines, mandarins, clementines", 
                           # "Tapioca of Cassava", "Tapioca of Potatoes", "Taro", "Tomatoes", 
                           # "Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80% vol; spirits, liqueurs and other spirituous beverages", 
                           # "Vetches", "Walnuts, in shell", "Watermelons", "Wheat", "Wheat-Fermented Beverages", 
                           # "Wheat and meslin flour", "Wheat Gluten", "Whey, Fresh", "Yams", 
                           # "Yautia", "Yoghurt, Concentrated or Unconcent"
                           )) {
    
        for (reg in c("Europe", "Asia", "Africa", "Americas", "Oceania")) {
    
        if(nrow(oldNewFoodData[region == reg & measuredItemCPC_description == item]) > 0) {    
            
    p = ggplot(oldNewFoodData[region == reg & measuredItemCPC_description == item], 
               aes(x = GDP , y = newFoodDataPC, group = Official, colour = Official)) +
        geom_line(aes(group=1), size = 1) +
        facet_wrap(~ countryElast2, scales = "free") +
        xlab('GDP per capita (constant 2005 US$)') + ylab('Food Estimate per capita (kg/person)') +
        ggtitle(paste(reg, unique(oldNewFoodData[measuredItemCPC_description == item]$measuredItemCPC_description), sep=" - "))
    
    print(p)
        }

    }
})


dev.off()
