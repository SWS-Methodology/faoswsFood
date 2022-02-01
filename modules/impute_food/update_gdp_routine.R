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

#download the latest gpd from. The latest gdp is downlaoded in 2015 constant from 25/02/2022
#https://databank.worldbank.org/source/world-development-indicators#

latestGDP <- data.table(read_excel("modules/impute_food/Data/GDP_world_bank_2020.xlsx"))

latestGDP[,c("Series Name","Series Code"):=NULL]

setnames(latestGDP,c("2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), c("2018","2019","2020"))

latestGDP <- latestGDP[!is.na(`Country Code`)]

#mapping iso3 code to m49
latestGDP[,geographicaream49 := countrycode(`Country Code`,"iso3c","un")]
latestGDP[,geographicaream49 := as.character(geographicaream49)]

#it is noted that Kosovo does not have a m49 code in the package, but it does have a code in SWS (412).
#manual adjustment is executed to add the Kosovo code

latestGDP[`Country Name` == "Kosovo", geographicaream49 := "412"]

latestGDP[, c("Country Name","Country Code") := NULL]

# latestGDP <- latestGDP[!is.na(geographicaream49)]

gdp_new <- c("2018","2019","2020")

latestGDP[, (gdp_new) := lapply(.SD, as.numeric), .SDcols = gdp_new]

############

gdpSWS <- dcast.data.table(gdpSWS, geographicaream49 ~ timepointyears, value.var = c("gdp"))

#change china code to 1248

latestGDP[geographicaream49 == "156" , geographicaream49 := "1248"]


gdpSWS[,c("2018","2019") := NULL]

gdpSWS <- merge(gdpSWS,latestGDP, by="geographicaream49" , all.x = TRUE)


#missing data

missing_codes_2018 <- unique(gdpSWS[is.na(`2018`)]$geographicaream49)
missing_codes_2019 <- unique(gdpSWS[is.na(`2019`)]$geographicaream49)
missing_codes_2020 <- unique(gdpSWS[is.na(`2020`)]$geographicaream49)

missing_codes <- c(missing_codes_2018, missing_codes_2019, missing_codes_2020)

missing_codes <- missing_codes[!duplicated(missing_codes)]

codes <- data.table(countrycode_data)
codes <- codes[un %in% missing_codes]

#In order to fill the gdp data for missing countries, we utilize gdp data in UNCTAD

#https://unctadstat.unctad.org/EN/BulkDownload.html

# This site includes 2015 constant prices. A conversion to 2010 is needed. 

# Y_{t} = Y_{t-1} * X_{t}/X_{t-1} where Y is 2010 and X is 2015. 

#no need to convert to 2010 constant. We have decided to retain data in terms of constant 2015. 
#So all data sets are in 2015 constant starting from  2018. #### 25/01/2022

gdp_2015 <- data.table(read_excel("modules/impute_food/Data/UNCTAD_gdp_data_2020.xlsx"))

gdp_2015[, c("Series","Series Label","US dollars at current prices in millions","US dollars at current prices in millions Footnote"
             ,"US dollars at current prices per capita","US dollars at current prices per capita Footnote" ,
             "US dollars at constant prices (2015) in millions Footnote",
             "US dollars at constant prices (2015) per capita",
             "US dollars at constant prices (2015) per capita Footnote" ) := NULL]     

setnames(gdp_2015, "US dollars at constant prices (2015) in millions", "gdp[2015 constand $] in millions")

#convert to integer value
gdp_2015[,`gdp[2015 constand $] in millions` := `gdp[2015 constand $] in millions`*10^6]

setnames(gdp_2015, "gdp[2015 constand $] in millions", "gdp[2015 constand $]")


gdp_2015 <- subset(gdp_2015 , Year %in% c(2018:2020))


gdp_2015<- gdp_2015[Economy %in% missing_codes]
gdp_2015[, c("Economy Label") := NULL]

setnames(gdp_2015 ,c("Economy","Year"), c("geographicaream49","Year_UNC") )

gdp_2015 <- dcast.data.table(gdp_2015, geographicaream49 ~ Year_UNC, value.var = c("gdp[2015 constand $]"))

gdp_2015[, geographicaream49 := as.character(geographicaream49)]
# gdp_2015[,gdp_growth := `gdp[2015 constand $]`/shift(`gdp[2015 constand $]`) ,geographicaream49]

# growth_data <- gdp_2015[!is.na(gdp_growth)]

# growth_data <- growth_data[,c("geographicaream49","gdp_growth"), with = F]
# growth_data[, geographicaream49 := as.character(geographicaream49)]

#extract 2018 gdp constant 2010 from gdpSWS for these countries 


# gdp_2010 <- gdpSWS[geographicaream49 %in% missing_codes][,c("geographicaream49","2018"),with= FALSE]



# 
# gdp_missing_Code <- merge(gdp_2010,growth_data , by= "geographicaream49",all.x = TRUE)
# 
# gdp_missing_Code[, gdp_2019 := `2018` * gdp_growth ]
# 
# gdp_missing_Code[, percent := round(((`2018`- gdp_2019)/`2018`)*100,0)]


#extract the computed 2019 values and merge with the final data

# gdp_missing_Code <- gdp_missing_Code[,c("geographicaream49","gdp_2019"),with = F]


#merge with gdpSWS

gdpSWS_merge <- merge(gdpSWS,gdp_2015 , by= "geographicaream49", all.x = TRUE)


gdpSWS_merge[,`2018.x`:= ifelse(is.na(`2018.x`), `2018.y`,`2018.x`)]

gdpSWS_merge[,`2019.x`:= ifelse(is.na(`2019.x`), `2019.y`,`2019.x`)]

gdpSWS_merge[,`2020.x`:= ifelse(is.na(`2020.x`), `2020.y`,`2020.x`)]


gdpSWS_merge[,c ("2018.y","2019.y","2020.y") := NULL]


setnames(gdpSWS_merge,c("2018.x","2019.x","2020.x"),c("2018","2019","2020"))

# do this check to see if data is consistent since we are using two sources

# gdpSWS_merge[, growth_2019 := round(((`2019`-`2018`)/`2018`)*100,0)]
# 
# gdpSWS_merge[, growth_2020 := round(((`2020`-`2019`)/`2019`)*100,0)]
# 
# gdpSWS_merge[,differece := growth_2019 - growth_2020]


dataToSave= melt.data.table(gdpSWS_merge, id.vars = c("geographicaream49"), value.name = "gdp")
setnames(dataToSave,"variable","timepointyears")
                              
dataToSave[, timepointyears := as.character(timepointyears)]


#gdpSWS[,percent := round(((`2018`- `2019`)/`2018`)*100,0)]

dataToSave[, gdp := round(gdp,0)]


write.csv(dataToSave,"modules/impute_food/Data/final_gdp.csv" ,row.names = F)    
    
# geographicaream49, timepointyears,gdp
