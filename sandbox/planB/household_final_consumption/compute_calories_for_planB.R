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

load("sandbox/planB/household_final_consumption/dataToSave.RData")

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


dataToSave[, calPerPersonPerDayhhConsExp := (foodHathhConsExp * valueCal * 10000) / 365 / (population * 1000)]
dataToSave[, calPerPersonPerDayGDP := (foodHatGDP * valueCal * 10000) / 365 / (population * 1000)]

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
dataToSave <- dataToSave[order(-dataToSave$calPerPersonPerDayhhConsExp)]

dataToSave = dataToSave[!(is.na(calPerPersonPerDayhhConsExp) | calPerPersonPerDayhhConsExp == "Inf")]
dataToSave = dataToSave[calPerPersonPerDayhhConsExp > 0]

dataToSave[geographicAreaM49 == 276 & level == 0 & timePointYears == 2012]

dataToSave[geographicAreaM49 == 276 & level == 0 & timePointYears == 2012, 
           c("geographicAreaM49", "measuredItemCPC", "timePointYears", 
             "calPerPersonPerDayhhConsExp", "calPerPersonPerDayGDP"), with = F]


###############################################################################

## Compute the calorie consumption per country, per person and per day

## exclude wheat (0111) for germany and colombia
dataToSave = dataToSave[!(geographicAreaM49 %in% c(276, 170, 616, 40) & measuredItemCPC == "0111")]

dataToSave = nameData("food", "fooddata", dataToSave)

calorieCountryDay <- dataToSave[, list(totalCalPersonDayhhConsExp = sum(calPerPersonPerDayhhConsExp, na.rm=T),
                                       totalCalPersonDayGDP = sum(calPerPersonPerDayGDP, na.rm=T),
                                     numbCommodities = .N),
                              by = list(geographicAreaM49_description, geographicAreaM49, timePointYears)]

calorieCountryDay = calorieCountryDay[order(-calorieCountryDay$timePointYears)]

calorieCountryDay[geographicAreaM49 == 76]

# calorieCountryDay = nameData("food", "fooddatafs_", calorieCountryDay)
# calorieCountryDay[, .N, geographicAreaM49]

countriesZeroCal = calorieCountryDay[totalCalPersonDayhhConsExp == 0, .N, geographicAreaM49][, geographicAreaM49]

calorieCountryDay = calorieCountryDay[!(geographicAreaM49 %in% countriesZeroCal)]


## to exclude countries with more than 4,400 kcal and less than 1,400 kcal

lessthan1400kcal <- calorieCountryDay[totalCalPersonDayhhConsExp < 1400, .N, geographicAreaM49][, geographicAreaM49]

morethan4400kcal <- calorieCountryDay[totalCalPersonDayhhConsExp > 4500, .N, geographicAreaM49][, geographicAreaM49]


calorieCountryDay = calorieCountryDay[!(geographicAreaM49 %in% lessthan1400kcal)]
calorieCountryDay = calorieCountryDay[!(geographicAreaM49 %in% morethan4400kcal)]
calorieCountryDay[, .N, geographicAreaM49_description]

calorieCountryDay[geographicAreaM49 == 616]

calorieCountryDay[, "numbCommodities" := NULL]

setnames(calorieCountryDay, "totalCalPersonDayhhConsExp", "totalCalPersonDay_hhConsExp")
setnames(calorieCountryDay, "totalCalPersonDayGDP", "totalCalPersonDay_GDP")


write.csv(calorieCountryDay, file = "sandbox/planB/household_final_consumption/kcal_by_country.csv", row.names = F)

library(ggplot2)
library(scales)

calorieCountryDayMelt = melt.data.table(
    calorieCountryDay, 
    id.vars = c("geographicAreaM49_description", "geographicAreaM49", "timePointYears"), 
    value.vars = c("totalCalPersonDay_hhConsExp", "totalCalPersonDay_GDP"))


ggplot(data=test_data_long,
       aes(x=date, y=value, colour=variable)) +
    geom_line()

 
calorieCountryDayMelt[variable == "totalCalPersonDay_GDP", variableAux := "GDP"]
calorieCountryDayMelt[variable == "totalCalPersonDay_hhConsExp", variableAux := "Household Final\n Consump Expend"]
calorieCountryDayMelt[, timePointYears := as.numeric(timePointYears)]


ggplot(calorieCountryDayMelt[geographicAreaM49 == 840],
       aes(x=timePointYears, y=value, group=variableAux, colour = variableAux)) +
    #geom_line(size = .8) +
    #geom_line(position=position_dodge(width=0.2), size = .4) +
    geom_line(size = 1.2, linetype = 2) +
    #facet_grid(country ~ variableNames) +
    #facet_wrap(measuredItemCPC_description ~ variableNames, scales = "free") +
    #geom_blank(data=meltOldNewFoodData[geographicAreaM49 == 356 & measuredItemCPC_description == item]) +
    #ggtitle(paste(countryName, itemName, sep = "-")) +
    ylab('Total calories per person per day') + xlab('Year') +
    #labs(x="Year", y="Value") +
    theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
    scale_x_continuous(limits = c(1991, 2016), breaks = seq(1991, 2016, 1), labels = space) +
    # scale_y_continuous(limits = c(0, 4500), breaks = seq(0, 4500, 500)) +
    theme(legend.title = element_blank()) +
    # ggtitle(paste(country, sep=" - ")) +
    theme(axis.text=element_text(size=12, face="bold"),
          axis.title=element_text(size=14,face="bold"))

################################################################################

dlpath <- file.path("C:", "Users", "caetano", "Documents", "Github", "faoswsFood", "sandbox", "planB")
pdffile <- file.path(dlpath, "household_final_consumption", "calories-gdp-hh-final-consumption.pdf")

pdf(file = pdffile, height = 11, width = 16)

countries = unique(calorieCountryDayMelt$geographicAreaM49_description)
countries = countries[order(countries)]
dput(countries)

library(scales)
space <- function(x, ...) { 
    format(x, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

system.time(for (country in c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda", 
                              "Argentina", "Armenia", "Australia", "Azerbaijan", "Bahamas", 
                              "Bangladesh", "Barbados", "Bel-Lux(-1999)", "Belarus", "Belize", 
                              "Benin", "Bolivia (Plurinational State of)", "Bosnia and Herzegovina", 
                              "Botswana", "Brazil", "Brunei Darussalam", "Bulgaria", "Burkina Faso", 
                              "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Central African Republic", 
                              "Chad", "Chile", "China, Macao", "China, Main", "China,H.Kong", 
                              "Colombia", "Costa Rica", 
                              #"Côte d'Ivoire", 
                              "Croatia", "Cuba", 
                              "Cyprus", "Democratic Republic of the Congo", "Dominica", "Dominican Republic", 
                              "Ecuador", "El Salvador", "Estonia", "Ethiopia", "Fiji", "Finland", 
                              "France", "French Polynesia", "Gabon", "Gambia", "Ghana", "Greece", 
                              "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", 
                              "Honduras", "Hungary", "Iceland", "India", "Iran (Islamic Republic of)", 
                              "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", 
                              "Kenya", "Kiribati", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", 
                              "Latvia", "Lebanon", "Lesotho", "Libya", "Madagascar", "Malaysia", 
                              "Maldives", "Mali", "Malta", "Mauritania", "Mauritius", "Mexico", 
                              "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", 
                              "Nepal", "Netherlands", "New Caledonia", "Nicaragua", "Niger", 
                              "Nigeria", "Norway", "Oman", "Pakistan", "Panama", "Paraguay", 
                              "Peru", "Philippines", "Poland", "Portugal", "Republic of Korea", 
                              "Republic of Moldova", "Russian Federation", "Rwanda", "Saint Kitts and Nevis", 
                              "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "Sao Tome and Principe", 
                              "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", 
                              "Slovakia", "Solomon Islands", "Somalia", "South Africa", "Spain", 
                              "Sri Lanka", "Swaziland", "Sweden", "Switzerland", "Syrian Arab Republic", 
                              "Tajikistan", "The former Yugoslav Republic of Macedonia", 
                              #"Timor-Leste", 
                              "Togo", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", 
                              "Uganda", "Ukraine", "United Kingdom", "United States of America", 
                              "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela (Bolivarian Republic of)", 
                              "Viet Nam", "Zambia", "Zimbabwe")) {
    
    
    p = ggplot(calorieCountryDayMelt[geographicAreaM49_description == country],
           aes(x=timePointYears, y=value, group=variableAux, colour = variableAux)) +
        #geom_line(size = .8) +
        #geom_line(position=position_dodge(width=0.2), size = .4) +
        geom_line(size = 1.2, linetype = 2) +
        #facet_grid(country ~ variableNames) +
        #facet_wrap(measuredItemCPC_description ~ variableNames, scales = "free") +
        #geom_blank(data=meltOldNewFoodData[geographicAreaM49 == 356 & measuredItemCPC_description == item]) +
        #ggtitle(paste(countryName, itemName, sep = "-")) +
        ylab('Total calories per person per day') + xlab('Year') +
        #labs(x="Year", y="Value") +
        theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = .5)) +
        scale_x_continuous(limits = c(1991, 2016), breaks = seq(1991, 2016, 1), labels = space) +
        # scale_y_continuous(limits = c(0, 4500), breaks = seq(0, 4500, 500)) +
        theme(legend.title = element_blank()) +
        ggtitle(country)  +
        theme(axis.text=element_text(size=12, face="bold"),
              axis.title=element_text(size=14,face="bold"))
       
        
    
    print(p)
    
}
)

dev.off()
