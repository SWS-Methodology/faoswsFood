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
## Compute calories per person per day

load("sandbox/planB/dataToSave.RData")

caloriesData <- faoswsUtil::getNutritiveFactors(geographicAreaM49 = dataToSave$geographicAreaM49,
                                                measuredElement = "261",
                                                measuredItemCPC = dataToSave$measuredItemCPC,
                                                timePointYearsSP = dataToSave$timePointYears)
setnames(caloriesData,
         c("timePointYearsSP", "Value"),
         c("timePointYears", "valueCal"))

keys = c("geographicAreaM49", "measuredItemCPC", "timePointYears")

## Merge the dataToSave with calories data

dataToSave <- merge(
    dataToSave, 
    caloriesData[, c(keys, "valueCal"), with = F],
    by = keys, all.x=T)

dataToSave


dataToSave[, calPerPersonPerDay := (foodHat * valueCal * 10000) / 365 / (population * 1000)]

## getCommodityTree 

tree = getCommodityTree(geographicAreaM49 = unique(dataToSave$geographicAreaM49),
                        timePointYears = "2012")
tree

level = getCommodityLevel(tree, parentColname = "measuredItemParentCPC",
                          childColname = "measuredItemChildCPC")

level

setnames(level, "node", "measuredItemParentCPC")

tree = merge(tree, level, by = "measuredItemParentCPC", all.x = T)
tree[level == 0, .N, measuredItemParentCPC]

setnames(level, "measuredItemParentCPC", "measuredItemCPC")

dataToSave = merge(dataToSave, level, by = "measuredItemCPC", all.x = T)
dataToSave <- dataToSave[order(-dataToSave$calPerPersonPerDay)]

dataToSave = dataToSave[!(is.na(calPerPersonPerDay) | calPerPersonPerDay == "Inf")]
dataToSave = dataToSave[calPerPersonPerDay > 0]

dataToSave[geographicAreaM49 == 276 & level == 0 & timePointYears == 2012]

dataToSave[geographicAreaM49 == 276 & level == 0 & timePointYears == 2012, 
           c("geographicAreaM49", "measuredItemCPC", "timePointYears", "calPerPersonPerDay"), with = F]



## wheat tree

dataToSave[geographicAreaM49 == 40 &
               measuredItemCPC %in% c("0111"
                       #                "23110", "23140.01", "23140.02", "23140.03", 
                       # "23140.08", "24110", "2413", "24320", "39120.01", "39160"
                       ),
           list(calories = sum(calPerPersonPerDay, na.rm = T)),
           by = list(timePointYears)]

## refined sugar
measuredItemCPC %in% c("23520", "23210.04", "23670.01", "2413", "24490", "2351", "2351f")


dataToSave[geographicAreaM49 == 40 &
               measuredItemCPC %in% c(#"23520"
                                      "23210.04", "23670.01", "2413", "24490", "2351", "2351f"
                                      ),
           list(calories = sum(calPerPersonPerDay, na.rm = T)),
           by = list(timePointYears)]



## sugar beet 01801

dataToSave[geographicAreaM49 == 40 & timePointYears == 2012 &
               measuredItemCPC %in% c("01801", "23512", "23540", "24110", "2413", "39140.01", "39160")]

dataToSave[geographicAreaM49 == 276 &
               measuredItemCPC %in% c(#"01801" 
                                       "23512", "23540", "24110", "2413", "39140.01", "39160"
                                      ),
           list(calories = sum(calPerPersonPerDay, na.rm = T)),
           by = list(timePointYears)]



# maize 0112
dataToSave[geographicAreaM49 == 40 & timePointYears == 2012 & 
               measuredItemCPC %in% c("0112", "23120.03", "23140.03", "23140.06",
                                      "23140.08", "23220.04", "24110", "2413", "24310.01",
                                      "24310.02", "39120.04", "39130.02", "39160")]

## raw milk - 02211

tree[geographicAreaM49 == 276 & measuredItemParentCPC == "02211"]

dataToSave[geographicAreaM49 == 40 & timePointYears == 2012 & 
               measuredItemCPC %in% c("02211", "22110.01", "22110.02", "22120", "22130.01", "22211", "22212", 
                                      "22221.01", "22222.01", "22230.01", "22230.02", "22230.03", "22241.01", 
                                      "22241.02", "22251.01", "22251.02")]


## excluir
dataToSave[geographicAreaM49 == 276 & 
               !measuredItemCPC %in% c("23120.03", "23140.03", "23140.06",
                                "23140.08", "23220.04", "24110", "2413", "24310.01",
                                "24310.02", "39120.04", "39130.02", "39160",
                                "23512", "23540", "24110", "2413", "39140.01", "39160",
                                "23210.04", "23670.01", "2413", "24490", "2351", "2351f",
                                "23110", "23140.01", "23140.02", "23140.03", 
                                "23140.08", "24110", "2413", "24320", "39120.01", "39160",
                                "22110.01", "22110.02", "22120", "22130.01", "22211", "22212", 
                                "22221.01", "22222.01", "22230.01", "22230.02", "22230.03", "22241.01", 
                                "22241.02", "22251.01", "22251.02"), sum(calPerPersonPerDay, na.rm = T),
           by = timePointYears]


## which countries have wheat

dataToSave[maxFlagWheat == 1, .N, geographicAreaM49]


dataToSave[measuredItemCPC == "0111", flagWheat := 1]

dataToSave[is.na(flagWheat), flagWheat := 0]

dataToSave[, maxFlagWheat := max(flagWheat),
           by = list(geographicAreaM49, timePointYears)]

dataToSave[maxFlagWheat == 1, flagWheatChild := 1,
           by = list(geographicAreaM49)]


dataToSave[ifelse(maxFlagWheat == 1,
                  measuredItemCPC %in% c("23110", "23140.01", "23140.02", "23140.03", 
                                                             "23140.08", "24110",
                                         "2413", "24320", "39120.01", "39160")),  1,
           by = list(geographicAreaM49)]


dataToSave[maxFlagWheat != 1, flagWheatChild := 0,
           by = list(geographicAreaM49)]

dataToSave[measuredItemCPC == "23520", flagRefinedSugar := 1,
           by = list(geographicAreaM49)]

dataToSave[is.na(flagRefinedSugar), flagRefinedSugar := 0,
           by = list(geographicAreaM49)]

dataToSave[, maxFlagRefinedSugar := max(flagRefinedSugar),
           by = list(geographicAreaM49)]


dataToSave[measuredItemCPC == "0112", flagMaize := 1,
           by = list(geographicAreaM49)]

dataToSave[is.na(flagMaize), flagMaize := 0,
           by = list(geographicAreaM49)]

dataToSave[, maxFlagMaize := max(flagMaize),
           by = list(geographicAreaM49)]



dataToSave[measuredItemCPC == "01801", flagSugarBeet := 1,
           by = list(geographicAreaM49)]

dataToSave[is.na(flagSugarBeet), flagSugarBeet := 0,
           by = list(geographicAreaM49)]

dataToSave[, maxFlagSugarBeet := max(flagSugarBeet),
           by = list(geographicAreaM49)]


dataToSave[measuredItemCPC == "02211", flagRawMilk := 1,
           by = list(geographicAreaM49)]

dataToSave[is.na(flagRawMilk), flagRawMilk := 0,
           by = list(geographicAreaM49)]

dataToSave[, maxFlagRawMilk := max(flagRawMilk),
           by = list(geographicAreaM49)]

dataToSave[maxFlagWheat == 1, 
           !(measuredItemCPC %in% c("23110", "23140.01", "23140.02",
                                    "23140.03", "23140.08", "24110", "2413", 
                                    "24320", "39120.01", "39160")),
           # c("geographicAreaM49", "timePointYears", "measuredItemCPC", "foodHat", "calPerPersonPerDay"),
           by = list(geographicAreaM49)]


dataToSave[ifelse(maxFlagWheat == 1), !(measuredItemCPC %in% c("23110", "23140.01", "23140.02",
                                                                 "23140.03", "23140.08", "24110", "2413", 
                                                                 "24320", "39120.01", "39160")),
           by = list(geographicAreaM49)]


###############################################################################

## Compute the calorie consumption per country, per person and per day

## exclude wheat (0111) for germany and colombia
dataToSave = dataToSave[!(geographicAreaM49 %in% c(276, 170, 616, 40) & measuredItemCPC == "0111")]

dataToSave = nameData("food", "fooddata", dataToSave)

calorieCountryDay <- dataToSave[, list(totalCalPersonDay = sum(calPerPersonPerDay, na.rm=T),
                                     numbCommodities = .N),
                              by = list(geographicAreaM49_description, geographicAreaM49, timePointYears)]

calorieCountryDay = calorieCountryDay[order(-calorieCountryDay$timePointYears)]

calorieCountryDay[geographicAreaM49 == 276]

# calorieCountryDay = nameData("food", "fooddatafs_", calorieCountryDay)
# calorieCountryDay[, .N, geographicAreaM49]

countriesZeroCal = calorieCountryDay[totalCalPersonDay == 0, .N, geographicAreaM49][, geographicAreaM49]

calorieCountryDay = calorieCountryDay[!(geographicAreaM49 %in% countriesZeroCal)]


## to exclude countries with more than 4,400 kcal and less than 1,400 kcal

lessthan1400kcal <- calorieCountryDay[totalCalPersonDay < 1400, .N, geographicAreaM49][, geographicAreaM49]

morethan4400kcal <- calorieCountryDay[totalCalPersonDay > 4500, .N, geographicAreaM49][, geographicAreaM49]


calorieCountryDay = calorieCountryDay[!(geographicAreaM49 %in% lessthan1400kcal)]
calorieCountryDay = calorieCountryDay[!(geographicAreaM49 %in% morethan4400kcal)]
calorieCountryDay[, .N, geographicAreaM49_description]

calorieCountryDay[geographicAreaM49 == 616]

summary(calorieCountryDay$totalCalDay)

calorieCountryDay[, "numbCommodities" := NULL]


write.csv(calorieCountryDay, file = "sandbox/planB/kcal_by_country.csv", row.names = F)

library(ggplot2)
library(scales)

ggplot(calorieCountryDay[geographicAreaM49 == 616],
       aes(x=as.numeric(timePointYears), y=totalCalDay, group=1, col = type)) +
     geom_line(colour = "#5882FA", size = .8) +
    # geom_line(size = .8) +
    #facet_grid(country ~ variableNames) +
    #facet_wrap(measuredItemCPC_description ~ variableNames, scales = "free") +
    #geom_blank(data=meltOldNewFoodData[geographicAreaM49 == 356 & measuredItemCPC_description == item]) +
    #ggtitle(paste(countryName, itemName, sep = "-")) +
    ylab('Total calories per person per day') + xlab('Year') +
    #labs(x="Year", y="Value") +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    scale_x_continuous(limits = c(1991, 2016), breaks = seq(1991, 2016, 1)) +
    scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 500)) +
    theme(legend.title = element_blank()) +
    # ggtitle(paste(country, sep=" - ")) +
    theme(axis.text=element_text(size=12, face="bold"),
          axis.title=element_text(size=14,face="bold"))

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
