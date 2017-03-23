## domain: food
## data set: fooddatafs

foodData <- GetData(completeImputationKey, flags = TRUE)
setnames(foodData, "Value", "food")
foodData[, type := getCommodityClassification(as.character(measuredItemCPC))]
foodData = foodData[type %in% c("Food Estimate", "Food Residual")]

## computing calories per person per day from the old food data

caloriesData <- faoswsUtil::getNutritiveFactors(geographicAreaM49 = foodData$geographicAreaM49,
                                                measuredElement = "261",
                                                measuredItemCPC = foodData$measuredItemCPC,
                                                timePointYearsSP = foodData$timePointYears)
setnames(caloriesData,
         c("timePointYearsSP", "Value"),
         c("timePointYears", "valueCal"))

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

## Merge the food data with calories data

foodData <- merge(
    foodData, 
    caloriesData[, c(keys, "valueCal"), with = F],
    by = keys, all.x=T)

foodData

## merge with pop
keys = c("geographicAreaM49", "timePointYears")
foodData <- merge(
    foodData, 
    popData[, c(keys, "population"), with = F],
    by = keys, all.x=T)

foodData


foodData[, calPerPersonPerDay := (food * valueCal * 10000) / 365 / (population * 1000)]

## Compute the calorie consumption per country, per person and per day
calorieCountryDay <- foodData[, list(totalCalDay = sum(calPerPersonPerDay, na.rm=T),
                                     numbCommodities = .N),
                                        by = list(geographicAreaM49, timePointYears)]


calorieCountryDay[geographicAreaM49 == 840]

calorieCountryDay = nameData("food", "fooddatafs", calorieCountryDay)
calorieCountryDay[, .N, geographicAreaM49]

countriesZeroCal = calorieCountryDay[totalCalDay == 0, .N, geographicAreaM49][, geographicAreaM49]

calorieCountryDay = calorieCountryDay[!(geographicAreaM49 %in% countriesZeroCal)]

## Exclude countries that have just one value
calorieCountryDay[, .N, geographicAreaM49_description][order(N)]
calorieCountryDay = calorieCountryDay[!geographicAreaM49_description == "American Samoa"]

################################################################################

dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsFood", "sandbox")
pdffile <- file.path(dlpath, "calories_per_person_per_day", "calories-old-food-data.pdf")

pdf(file = pdffile, height = 11, width = 16)

countries = unique(calorieCountryDay$geographicAreaM49_description)
countries = countries[order(countries)]
dput(countries)

system.time(for (country in c("Albania", "Algeria", "Angola", "Antigua and Barbuda", 
                              "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", 
                              "Bahamas", "Bangladesh", "Bel-Lux(-1999)", "Belarus", "Belize", 
                              "Benin", "Bermuda", "Bolivia (Plurinational State of)", "Botswana", 
                              "Brazil", "Brunei Darussalam", "Bulgaria", "Cambodia", "Cameroon", 
                              "Canada", "Chad", "Chile", "China, Main", "China,Taiwan", "Colombia", 
                              "Congo", "Costa Rica", 
                              #"Côte d'Ivoire", 
                              "Cuba", "Cyprus", "Czech Republic", 
                              "Democratic Republic of the Congo", "Denmark", "Dominica", "Ecuador", 
                              "El Salvador", "Estonia", "Fiji", "Finland", "France", "French Guiana", 
                              "French Polynesia", "Gabon", "Gambia", "Georgia", "Germany", 
                              "Ghana", "Greece", "Grenada", "Guadeloupe", "Guam", "Guatemala", 
                              "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", 
                              "India", "Indonesia", "Iran (Islamic Republic of)", "Iraq", "Ireland", 
                              "Israel", "Italy", "Japan", "Kazakhstan", "Kenya", "Kuwait", 
                              "Kyrgyzstan", "Latvia", "Libya", "Lithuania", "Madagascar", "Malaysia", 
                              "Maldives", "Malta", "Martinique", "Mauritania", "Mauritius", 
                              "Mexico", "Mongolia", "Morocco", "Myanmar", "Namibia", "NethAntilles", 
                              "Netherlands", "New Caledonia", "New Zealand", "Nicaragua", "Nigeria", 
                              "Norway", "Oman", "Pakistan", "Panama", "Papua New Guinea", "Paraguay", 
                              "Peru", "Philippines", "Poland", "Portugal", "Republic of Korea", 
                              "Republic of Moldova", 
                              #"Réunion", 
                              "Romania", "Russian Federation", 
                              "Saint Kitts and Nevis", "Saint Lucia", "Sao Tome and Principe", 
                              "Saudi Arabia", "Senegal", "Serbia-Monte(1992-2005)", "Seychelles", 
                              "Solomon Islands", "South Africa", "Spain", "Sri Lanka", "Sudan", 
                              "Suriname", "Sweden", "Switzerland", "Syrian Arab Republic", 
                              "Tajikistan", "Thailand", "The former Yugoslav Republic of Macedonia", 
                              "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", 
                              "Turkmenistan", "Ukraine", "United Arab Emirates", "United Kingdom", 
                              "United States of America", "Uruguay", "Uzbekistan", "Vanuatu", 
                              "Venezuela (Bolivarian Republic of)", "Wallis and Futuna Islands", 
                              "Westn Sahara", "Yemen", "Yemen Dem", "Zimbabwe")) {
    

        p = ggplot(calorieCountryDay[geographicAreaM49_description == country],
                   aes(x=as.numeric(timePointYears), y=totalCalDay, group=1)) +
            geom_line(colour = "#5882FA", size = .8) +
            #facet_grid(country ~ variableNames) +
            #facet_wrap(measuredItemCPC_description ~ variableNames, scales = "free") +
            #geom_blank(data=meltOldNewFoodData[geographicAreaM49 == 356 & measuredItemCPC_description == item]) +
            #ggtitle(paste(countryName, itemName, sep = "-")) +
            ylab('Total calories per person per day') + xlab('Year') +
            #labs(x="Year", y="Value") +
            theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
            scale_x_continuous(limits = c(1991, 2013), breaks = seq(1991, 2013, 1)) +
            theme(legend.title = element_blank()) +
            ggtitle(paste(country, sep=" - ")) +
            theme(axis.text=element_text(size=12, face="bold"),
                  axis.title=element_text(size=14,face="bold"))
        
        print(p)
    
}
)

dev.off()

