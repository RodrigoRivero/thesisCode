############################################################################
## This document is divided into three parts
## 1. Creating a table with info about the admin regions
## 2. Determining Neighboring States for Brazil
## 3. Calculating cost for ports we do have data
## 4. Calculating cost to neighboring munis



library(readxl)
library(stringi)
library(rgdal)
library(here)

### Part 1:CREATING TABLE WITH COUNTRY, STATE, MUNICIPALITY AND ID ###
# Loading panel data (this is the master)
WD_paneldata <- read_excel(here::here("Nextcloud", "supplychaincommitments-data", "Dropbox", "SESYNC GIS Database", "DatafromWorldDevelopmentPaper", "WD_Panel analysis", "WD_paneldata.xlsx"))

# Getting year and only columns that are important (Country, Province, Identifier, and Name)
MuniIDTable <- WD_paneldata[WD_paneldata$year == 2012,c(1,2,3,4)]
# Putting the names to lower
MuniIDTable$Name <-stri_trans_general(MuniIDTable$Name, "lower")
MuniIDTable$Country <-stri_trans_general(MuniIDTable$Country, "lower")
MuniIDTable$Province <-stri_trans_general(MuniIDTable$Province, "lower")
# Removing Accents
MuniIDTable$Name <- stri_trans_general(MuniIDTable$Name, "Latin-ASCII")
MuniIDTable$Province <- stri_trans_general(MuniIDTable$Province, "Latin-ASCII")

###  CREATING TABLE WITH nID AND CODE (nID is a unique id associated with each municipality)
adminRegions <- readOGR(dsn = here::here("Nextcloud", "supplychaincommitments-data", "Dropbox", "SESYNC GIS Database", "newYannAdminMaps", "madeByRodrigo", "newAdmin.shp") , layer = "newAdmin") ## the data source is the shapefile with municipalities
rasterTable <- data.frame(ID = adminRegions$ID, nID = adminRegions$nID ) # making frame with identifier and nID
adminTable <- merge(MuniIDTable,rasterTable, by.x = "Identifier", by.y = "ID", all.y = TRUE) # adding nID table, merging it by the identifier
rm(MuniIDTable,rasterTable,WD_paneldata) # removing unnecessary variables no longer used

#### The following loop ads province info to the name of the municipality (if it is brazil or argentina because there are so many) and also changes the province of paraguay and bolivia to the country (cause there are very few)
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


### Part 2:CREATING Neighboring States ###
require(sp)    
require(maptools)
require(rgeos)

brPolygons <- subset(adminRegions, stri_detect_regex(ID,"BR")) # just brazil

# creating table with list of neighbors
list.nb <- gTouches(brPolygons, byid = TRUE)
brTable <- data.frame(PolygonID = getSpPPolygonsIDSlots(brPolygons), Identifier = brPolygons$ID, nId =brPolygons$nID)
neighborTable <- data.frame()
for (i in 1:nrow(brPolygons)) {
  thisRowPolygonID <- brTable$PolygonID[i]
  thisRowIdentifier <- brTable$Identifier[i]
  thisRownID <- brTable$nId[i]
  muni <- data.frame(PolygonID = thisRowPolygonID, Identifier = thisRowIdentifier, nId = thisRownID)
  neighboringMuniPolygonIDs <- as.numeric(names(which(list.nb[toString(thisRowPolygonID),])))
  neighboringMunis <- brTable[brTable$PolygonID %in% neighboringMuniPolygonIDs,]
  muni$neighboringIdentifier <- toString(neighboringMunis$Identifier)
  muni$neighboringnIDs <- toString(neighboringMunis$nId)
  neighborTable <- rbind(neighborTable,muni)
}

rm(brPolygons, brTable, list.nb, muni, neighboringMunis, i , neighboringMuniPolygonIDs, thisRownID, thisRowPolygonID, adminRegions,thisRowIdentifier)

## PART 3. Calculating cost for ports we do have data
# Loading Needed Libraries
library(readxl)
library(stringr)
library(readr)
library(plyr)
library(dplyr)
library(stringdist)


# Loading needed tables
# Trase Data For Brazil
brazilTrase <- read.csv(here::here("Nextcloud", "supplychaincommitments-data", "Dropbox", "SESYNC GIS Database", "dinamica", "Model's Data", "Standard Model", "Trase", "BRAZIL_SOY_2.2.csv"))

# Port information
portInfo <- read.csv(here::here("Nextcloud", "supplychaincommitments-data", "Dropbox", "SESYNC GIS Database", "dinamica", "Model's Data", "Standard Model", "Trase", "portInfo.txt"))

# Getting year and removing unknowns
year <- brazilTrase[brazilTrase$YEAR == 2012,]

#### year <- year[year$PORT != "Domestic Consumption",]
year <- year[!str_detect(year$MUNICIPALITY, "Unknown"),]

### State table for brazil that links the name of the state with the abreviation, used for matching
stateTable <- data.frame( StateAB = tolower(c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")) , StateName = stri_trans_general(tolower(c("Acre","Alagoas","Amapá","Amazonas","Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás","Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí","Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo","Sergipe","Tocantins")), "Latin-ASCII") )

year$STATE <- tolower(year$STATE)
stateab <- vector()

year <- merge(year,stateTable, by.x = "STATE", by.y = "StateName", all.x = TRUE)

year$MUNICIPALITY <- paste(year$MUNICIPALITY," (" ,year$StateAB, ")", sep = "")
year$PORT <- gsub("Domestic Consumption", "Rio De Janeiro", year$PORT)

# Creating empty data frame
ma <- data.frame( Port = character(), Municipality = character(), Percentage = numeric(), PortIDs = numeric())
# Creating list of unique municipalities
municipalities <- unique(year$MUNICIPALITY)
# Loop for every municipality
for (i in 1:length(municipalities)){
  new <- year[year$MUNICIPALITY == municipalities[i],] # info for that municipality
  ports <- unique(new$PORT) # ports where soy went to 
  portIDs <- numeric() # empty vector which will eventually have a list of port id numbers
  muniTotal <- sum(new$SOY_EQUIVALENT_TONS) # total amount of soy in municipalities
  portPercentages <- vector() # empty vector which will have a list of percentages. soy in this port divided by total
  for (j in 1:length(ports)){ # loop per port in municipality
    port <- new[new$PORT == ports[j],] # info for this port
    portTotal <- sum(port$SOY_EQUIVALENT_TONS) # total soy that went to this port in tons
    portPercent <- portTotal/muniTotal # percentage of soy 
    portPercentages <- c(portPercentages, portPercent) # add percentage to empty vector
    thisPort <- portInfo[str_detect(portInfo$Trase_Name, as.character(ports[j])),] 
    portIDs <- c(portIDs, thisPort$FID)
  }
  muni <- data.frame(Port = ports, Municipality = tolower(municipalities[i]), Percentage = portPercentages, PortID = portIDs, stringsAsFactors = FALSE)
  ma <- rbind(ma,muni)
}

rm(i,j,muniTotal,portIDs,portPercent,portPercentages,portTotal,thisPort,ports,new,muni,port, thisMuni,unFilledTable, muniID, rasterID,stateab,municipalities)

# Exchanging muni name for muni ID for id in raster
muniID <- vector()
rasterID <- vector()
ma<- arrange(ma, Municipality)

# Changing TRASE Names to match Admin
ma$Municipality[3706] <- "faxinal dos guedes (sc)"
ma$Municipality[1378] <- "faxinal dos guedes (sc)"

# creating new column with code
for (i in 1:nrow(ma)){
  thisMuni <- adminTable[amatch(as.character(ma$Municipality[i]),(adminTable$Name), maxDist = 4),]
  muniID <- c(muniID,thisMuni$Identifier[1])
  rasterID <- c(rasterID,thisMuni$nID[1])
}

## Adding muni code and nID
ma$MuniCode <- muniID
ma$rasterID <- rasterID

## Getting all munis with port data
munisWithPorts <- ma[,c(4,6,3)]

## Getting brazil munis without data
allBrazilMunis <- adminTable[adminTable$Country == "brazil",]
munisWtihoutPorts <- allBrazilMunis[!(allBrazilMunis$nID %in%  munisWithPorts$rasterID ),]

## Removing variables
rm(allBrazilMunis, unfilledTable,i, municipalities, muniID, p, year,thisMuni,tempUnFilledTable, stateTable, portInfo,finalTable, rasterID, stateab, brazilTrase, ma)



backup1 <- munisWithPorts
backup2 <- munisWtihoutPorts
munisWithPorts <- backup1
munisWtihoutPorts <- backup2

### PART 4 filling in data for munis without ports
threshold <- 0.90

while (length(munisWtihoutPorts$nID) > 10 & threshold > -0.05){
  surroundedByPorts <- vector() # Vector with munis that will be first calculated 
  for (i in 1: length(munisWtihoutPorts$nID)){
    thisOne <- munisWtihoutPorts$nID[i]
    manyHave <- length(which(as.numeric(unlist(strsplit(neighborTable[neighborTable$nId == thisOne,5], split=", "))) %in% unique(munisWithPorts$rasterID) )) # how many surrounding ones have port data
    total <- length(as.numeric(unlist(strsplit(neighborTable[neighborTable$nId == thisOne,5], split=", ")))) # total surrounding ones
    if ((manyHave/total > threshold) & !is.na(manyHave/total)){
      surroundedByPorts <- c(surroundedByPorts,thisOne)
    }
    
  } ## this results in a vector with a list of municipalities suroounded by others with data
  rm(total, thisOne,manyHave,i)
  print(threshold)
  if (length(surroundedByPorts) < 20){
    threshold <- threshold - 0.05
    next
  }
  
  newMuniData <- data.frame(PortID = numeric(), rasterID = numeric(), Percentage = double())
  for (i in 1:length(surroundedByPorts)){
    neighboring  <- as.numeric(unlist(strsplit(neighborTable[neighborTable$nId == surroundedByPorts[i],5], split=", ")))
    surroundings <- munisWithPorts[munisWithPorts$rasterID %in% neighboring,]
    surroundings$Percentage <- surroundings$Percentage/length(unique(surroundings$rasterID))
    newPorts <- vector()
    newPercentage <- vector()
    for (j in 1:length(unique(surroundings$PortID))){
      newPercentage <- c(newPercentage ,sum(surroundings[surroundings$PortID == unique(surroundings$PortID)[j],3]))
      newPorts <- c(newPorts, unique(surroundings$PortID)[j])
    }
    rm(j)
    newMuniData <- rbind(newMuniData, data.frame(PortID = newPorts, rasterID = surroundedByPorts[i], Percentage = newPercentage))
    rm(newPercentage,newPorts,neighboring)
  } ## this results with a data frame with calculated info for the municiaplities that had it
  rm(i)
  munisWithPorts <- rbind(munisWithPorts,newMuniData)
  munisWtihoutPorts <- munisWtihoutPorts[!munisWtihoutPorts$nID %in% surroundedByPorts,]
}
rm(adminTable,neighborTable,newMuniData,surroundings,threshold,surroundedByPorts)

