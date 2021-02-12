suppressMessages({
    library(data.table)
    library(faosws)
    library(dplyr)
    # library(faoswsUtil)
    # library(faoswsFood)
    # library(faoswsFlag)
    library(countrycode)
    library(zoo)
    library(readxl)
})

options('repos' = c(CRAN = ifelse(getRversion() <= "3.3.3",
                                  
                                  "https://dev-sws-rcranrepo.s3-eu-west-1.amazonaws.com/", "https://cran.rstudio.com/")))


## Set up for the test environment and parameters
R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE <- Sys.getenv("R_DEBUG_MODE")

# This return FALSE if on the Statistical Working System
if (CheckDebug()) {
    
    message("Not on server, so setting up environment...")
    
    library(faoswsModules)
    SETTINGS <- ReadSettings("Modules/impute_food/sws.yml")
    
    # If you're not on the system, your settings will overwrite any others
    R_SWS_SHARE_PATH <- SETTINGS[["share"]]
    
    # Define where your certificates are stored
    SetClientFiles(SETTINGS[["certdir"]])
    
    # Get session information from SWS. Token must be obtained from web interface
    GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                       token = SETTINGS[["token"]])
    files <- dir("R", full.names = TRUE)
    sapply(files, source)
    
} else {
    R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
    options(error = function(){
        dump.frames()
        save(last.dump, file = "/work/SWS_R_Share/caetano/last.dump.RData")
    })
}


#read existing gdp data in SWS 

gdpSWS <- ReadDatatable("gdp_food_imputation")

gdpSWS[gdp == "NA", gdp := NA]

gdpSWS[, gdp := as.numeric(gdp)]

#check the latest year 
print(as.numeric(unique(gdpSWS$timepointyears)[length(unique(gdpSWS$timepointyears))]))

#download the latest gpd from 
#https://databank.worldbank.org/source/world-development-indicators#

latestGDP <- data.table(read_excel("modules/impute_food/Data/GDP_world_bank.xlsx"))

latestGDP[,c("Series Name","Series Code"):=NULL]

setnames(latestGDP,"2019 [YR2019]","2019")

latestGDP <- latestGDP[!is.na(`Country Code`)]

#mapping iso3 code to m49
latestGDP[,geographicaream49 := countrycode(`Country Code`,"iso3c","un")]
latestGDP[,geographicaream49 := as.character(geographicaream49)]

#it is noted that Kosovo does not have a m49 code in the package, but it does have a code in SWS (412).
#manual adjustment is executed to add the Kosovo code

latestGDP[`Country Name` == "Kosovo", geographicaream49 := "412"]

latestGDP[, c("Country Name","Country Code") := NULL]

# latestGDP <- latestGDP[!is.na(geographicaream49)]

latestGDP[,`2019` := as.numeric(`2019`)]

############

gdpSWS <- dcast.data.table(gdpSWS, geographicaream49 ~ timepointyears, value.var = c("gdp"))

#change china code to 1248

latestGDP[geographicaream49 == "156" , geographicaream49 := "1248"]

gdpSWS <- merge(gdpSWS,latestGDP, by="geographicaream49" , all.x = TRUE)


#missing data

missing_codes <- unique(gdpSWS[is.na(`2019`)]$geographicaream49)
codes <- data.table(countrycode_data)
codes <- codes[un %in% missing_codes]

#In order to fill the gdp data for missing countries, we utilize gdp data in UNCTAD

#https://unctadstat.unctad.org/EN/BulkDownload.html

# This site includes 2015 constant prices. A conversion to 2010 is needed. 

# Y_{t} = Y_{t-1} * X_{t}/X_{t-1} where Y is 2010 and X is 2015. 

gdp_2015 <- data.table(read_excel("modules/impute_food/Data/UNCTAD_gdp_data.xlsx"))

gdp_2015[, c("Series","Series Label","US dollars at current prices in millions","US dollars at current prices in millions Footnote"
             ,"US dollars at current prices per capita","US dollars at current prices per capita Footnote" ,
             "US dollars at constant prices (2015) in millions Footnote",
             "US dollars at constant prices (2015) per capita",
             "US dollars at constant prices (2015) per capita Footnote" ) := NULL]     

setnames(gdp_2015, "US dollars at constant prices (2015) in millions", "gdp[2015 constand $] in millions")

#convert to integer value
gdp_2015[,`gdp[2015 constand $] in millions` := `gdp[2015 constand $] in millions`*10^6]

setnames(gdp_2015, "gdp[2015 constand $] in millions", "gdp[2015 constand $]")


gdp_2015 <- subset(gdp_2015 , Year %in% c(2018:2019))


gdp_2015<- gdp_2015[Economy %in% missing_codes]
gdp_2015[, c("Economy Label") := NULL]

setnames(gdp_2015 ,"Economy", "geographicaream49" )


gdp_2015[,gdp_growth := `gdp[2015 constand $]`/shift(`gdp[2015 constand $]`) ,geographicaream49]

growth_data <- gdp_2015[!is.na(gdp_growth)]

growth_data <- growth_data[,c("geographicaream49","gdp_growth"), with = F]
growth_data[, geographicaream49 := as.character(geographicaream49)]

#extract 2018 gdp constant 2010 from gdpSWS for these countries 


gdp_2010 <- gdpSWS[geographicaream49 %in% missing_codes][,c("geographicaream49","2018"),with= FALSE]

#gdp for missing countries 
gdp_missing_Code <- merge(gdp_2010,growth_data , by= "geographicaream49",all.x = TRUE)

gdp_missing_Code[, gdp_2019 := `2018` * gdp_growth ]

gdp_missing_Code[, percent := round(((`2018`- gdp_2019)/`2018`)*100,0)]


#extract the computed 2019 values and merge with the final data

gdp_missing_Code <- gdp_missing_Code[,c("geographicaream49","gdp_2019"),with = F]


#merge with gdpSWS

gdpSWS <- merge(gdpSWS,gdp_missing_Code , by= "geographicaream49", all.x = TRUE)

gdpSWS[,`2019`:= ifelse(is.na(`2019`), gdp_2019,`2019`)]

gdpSWS[,gdp_2019 := NULL]


dataToSave= melt.data.table(gdpSWS, id.vars = c("geographicaream49"), value.name = "gdp")
setnames(dataToSave,"variable","timepointyears")
                              
dataToSave[, timepointyears := as.character(timepointyears)]


#gdpSWS[,percent := round(((`2018`- `2019`)/`2018`)*100,0)]

dataToSave[, gdp := round(gdp,0)]


write.csv(dataToSave,"modules/impute_food/Data/final_gdp.csv" ,row.names = F)    
    
# geographicaream49, timepointyears,gdp
