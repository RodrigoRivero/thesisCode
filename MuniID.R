library(readxl)
library(stringi)
library(rgdal)
library(here)

### CREATING TABLE WITH COUNTRY, STATE, MUNICIPALITY AND ID ###
# Loading panel data (this is the master)
WD_paneldata <- read_excel(here("Nextcloud", "supplychaincommitments-data", "Dropbox", "SESYNC GIS Database", "DatafromWorldDevelopmentPaper", "WD_Panel analysis", "WD_paneldata.xlsx"))

# Getting year and only columns that are important (Country, Province, Identifier, and Name)
MuniIDTable <- WD_paneldata[WD_paneldata$year == 2012,c(1,2,3,4)]
# Putting the names to lower
MuniIDTable$Name <-stri_trans_general(MuniIDTable$Name, "lower")
MuniIDTable$Country <-stri_trans_general(MuniIDTable$Country, "lower")
MuniIDTable$Province <-stri_trans_general(MuniIDTable$Province, "lower")
# Removing Accents
MuniIDTable$Name <- stri_trans_general(MuniIDTable$Name, "Latin-ASCII")
MuniIDTable$Province <- stri_trans_general(MuniIDTable$Province, "Latin-ASCII")

### CREATING TABLE WITH nID AND CODE (nID is a unique id associated with each municipality)
adminRegions <- readOGR(dsn = here("Nextcloud", "supplychaincommitments-data", "Dropbox", "SESYNC GIS Database", "newYannAdminMaps", "madeByRodrigo", "newAdmin.shp") , layer = "newAdmin") ## the data source is the shapefile with municipalities
rasterTable <- data.frame(ID = adminRegions$ID, nID = adminRegions$nID ) # making frame with identifier and nID
adminTable <- merge(MuniIDTable,rasterTable, by.x = "Identifier", by.y = "ID", all.y = TRUE) # adding nID table, merging it by the identifier
rm(adminRegions,MuniIDTable,rasterTable,WD_paneldata) # removing unnecessary variables no longer used

### The following loop ads province info to the name of the municipality (if it is brazil or argentina because there are so many) and also changes the province of paraguay and bolivia to the country (cause there are very few)
for (i in 1:nrow(adminTable)){
 thisRow <- adminTable[i,]
 if (thisRow$Country == "brazil" | thisRow$Country == "argentina"  ){
  thisRow$Name <- paste(thisRow$Name," (" ,thisRow$Province, ")", sep = "")
 }
 if (thisRow$Country == "bolivia" | thisRow$Country == "paraguay"  ){
   thisRow$Province <- thisRow$Country
 }
 adminTable[i,] <- thisRow
}

### This part creates a frame with a unique number per province
idProvince <- data.frame(muni = unique(adminTable$Province), id = 1:length(unique(adminTable$Province)))

### This unique ID gets added to the data frame with muni info
adminTable$ProvinceID <- idProvince$id[match(adminTable$Province,idProvince$muni)]

### removing unused variables
rm(thisRow, i , idProvince)







