library(rgdal)   #read geojson file
library(rgeos)   #simplify polygons so map loads faster
library(tigris)  #convert fips code
library("XML")   #parse the senate info
library(stringr) #pad numbers
library(readr)   #write to rds file

#-----------------------------------------------------------------------------------------------------------------------------------------------
# GET & SIMPLIFY SPATIALPOLYGONS DATAFRAME ----
#-----------------------------------------------------------------------------------------------------------------------------------------------

#read in json file into a SpatialPolygonsDataFrame
districtsDataFrame <- rgdal::readOGR(dsn = path.expand("tl_2018_us_cd116.json"), verbose = TRUE) #returns a SpatialPolygonsDataFrame

#notes about how to access things inside the dataframe
#see https://gist.github.com/mbacou/5880859
# slotNames(districtsDataFrame)               #[1] "data"        "polygons"    "plotOrder"   "bbox"        "proj4string"
# slotNames(districtsDataFrame@polygons[[1]]) #[1] "Polygons"  "plotOrder" "labpt"     "ID"        "area"  

#simplify the polygons from the original json file so they load much faster in the viewer
#--takes SpatialPolygonsDataframe object
#--tol of 0.01 reduces size of object to about 10% of original (higher tolerance reduces it further, but districts start to become too jagged)
districtsPolygonsSimple = rgeos::gSimplify(districtsDataFrame, 0.01, topologyPreserve = TRUE) 

#save the simplified polygons in a dataframe for use by leaflet
#will be in the same order, according to documentation, unless a whole polygon is dropped
districtsDataFrameSimpleRaw = SpatialPolygonsDataFrame(districtsPolygonsSimple, districtsDataFrame@data)

#-----------------------------------------------------------------------------------------------------------------------------------------------
# GET HOUSE INFO ----
#-----------------------------------------------------------------------------------------------------------------------------------------------
#get house district member info (state postal code, CA, and district number, 46)
districtsRepInfo <- read.csv("member_info_cd116.csv", header=TRUE)
#districtsRepInfo <- XML::xmlToDataFrame("member_info_cd116.xml")

#build state postal code and district number columns
districtsRepInfo$STATEPOSTAL <- sapply(districtsRepInfo$St.Dis, substring, 1, 2)  #state postal code (XX of XX00)
districtsRepInfo$CD116FP <- sapply(districtsRepInfo$St.Dis, substring, 3, 4)      #district number (00 of XX00)

#get postal code to fips mapping
dfFipsMap <- unique(tigris::fips_codes[1:2]) #'state' is postal code, 'state_code' is fips

#create geoid for representative info to allow for easy merging with polygon file
districtsRepInfo <- merge(districtsRepInfo, dfFipsMap, by.x='STATEPOSTAL', by.y='state') #merge on postal code to get fips
districtsRepInfo$GEOID <- paste(districtsRepInfo$state_code, districtsRepInfo$CD116FP, sep='')   #create geoid by concatenating postal & fips

#build name, party, and district columns
districtsRepInfo$NAME_HOUSE <- apply(districtsRepInfo, 1, FUN = function(x) if(x['MiddleName']=='') paste(x['FirstName'], x['LastName'], sep=' ') else paste(x['FirstName'], x['MiddleName'], x['LastName'], sep=' '))
districtsRepInfo$PARTY_HOUSE <- districtsRepInfo$Party #rename to all caps to fit convention of target dataframe
districtsRepInfo$DISTRICT <- paste(districtsRepInfo$STATEPOSTAL, districtsRepInfo$CD116FP, sep='-') #create readable district label 

#manually fix NC-03, NC-09, PA-12
#districtsRepInfo$NAME_HOUSE[districtsRepInfo$DISTRICT=='NC-03'] <- "Allen Thomas [Special Election]" #Sept 10th 2019 (lost race, so got rid of this adjustment)
#districtsRepInfo$PARTY_HOUSE[districtsRepInfo$DISTRICT=='NC-03'] <- "D"
#districtsRepInfo$NAME_HOUSE[districtsRepInfo$DISTRICT=='NC-09'] <- "Dan McCready [Special Election]" #Sept 10th 2019 (lost race, so got rid of this adjustment)
#districtsRepInfo$PARTY_HOUSE[districtsRepInfo$DISTRICT=='NC-09'] <- "D"
districtsRepInfo$NAME_HOUSE[districtsRepInfo$DISTRICT=='PA-12'] <- "Fred Keller" #recently elected, not yet listed in clerk's office file
districtsRepInfo$PARTY_HOUSE[districtsRepInfo$DISTRICT=='PA-12'] <- "R"
districtsRepInfo$NAME_HOUSE[districtsRepInfo$DISTRICT=='CA-25'] <- "the Democratic Candidate" #due to Katie Hill resignation 
districtsRepInfo$PARTY_HOUSE[districtsRepInfo$DISTRICT=='CA-25'] <- "D"


#create district map
districtGeoidMap = subset(districtsRepInfo, select = c('GEOID', 'STATEPOSTAL', 'CD116FP'))

#get rid of columns we don't want
districtsRepInfo <- subset(districtsRepInfo, select = c('GEOID', 'STATEPOSTAL', 'DISTRICT', 'PARTY_HOUSE', 'NAME_HOUSE'))


#-----------------------------------------------------------------------------------------------------------------------------------------------
# GET SENATE INFO ----
#-----------------------------------------------------------------------------------------------------------------------------------------------
#get senate member info to allow for easy merging with polygon file 
statesRepInfo <- XML::xmlToDataFrame("member_info_senate116.xml")
statesRepInfo$STATEPOSTAL <- statesRepInfo$state   #rename to all caps to fit convention of target dataframe
statesRepInfo$PARTY_SENATE <- statesRepInfo$party  #rename to all caps to fit convention of target dataframe
statesRepInfo$ELECTION <- statesRepInfo$class
statesRepInfo$ELECTION[statesRepInfo$ELECTION == "Class II"] <- "2020"  #Class II senators are up for re-election in 2020
statesRepInfo$ELECTION[statesRepInfo$ELECTION == "Class III"] <- "2022" #Class III senators are up for re-election in 2022
statesRepInfo$ELECTION[statesRepInfo$ELECTION == "Class I"] <- "2024"   #Class I senators are up for re-election in 2024
statesRepInfo$NAME_SENATE <- paste(statesRepInfo$first_name, statesRepInfo$last_name, sep=' ') #create full name 
statesRepInfo <- subset(statesRepInfo, is.na(STATEPOSTAL)==FALSE)

#convert dataframe from 100 1-senator rows into 50 2-senator rows to allow join with primary polygon dataframe
#NOTE: ordering by election at the beginning means senator0 will be re-elected sooner than senator1, so if the senate is in play, it will be senator0
statesRepInfo <- statesRepInfo[order( statesRepInfo['STATEPOSTAL'], statesRepInfo['ELECTION']) ,] #reorder before indexing
statesRepInfo <- mutate(statesRepInfo, STATE_UNIQUE = make.unique(as.character(STATEPOSTAL), sep='_')) #label duplicate states (e.g. AK, AK_1, AL, AL_1)
statesRepInfo$GROUPER <- sapply(statesRepInfo$STATE_UNIQUE, substring, 4) #add grouper value of 1 for second senator using AK_1
statesRepInfo$GROUPER[statesRepInfo$GROUPER == ''] <- 0 #add grouper value of 0 for first senator
statesRepInfoSet0 <- subset(statesRepInfo, GROUPER == 0) #break into first 50 row dataframe 
statesRepInfoSet1 <- subset(statesRepInfo, GROUPER == 1) #break into second 50 row dataframe
statesRepInfo <- merge(x = statesRepInfoSet0, y = statesRepInfoSet1, by = "STATEPOSTAL", all = TRUE) #merge the dataframes

#get rid of columns we don't want
statesRepInfo <- subset(statesRepInfo, select = c('STATEPOSTAL', 'NAME_SENATE.x', 'PARTY_SENATE.x', 'ELECTION.x', 'NAME_SENATE.y', 'PARTY_SENATE.y', 'ELECTION.y'))


#-----------------------------------------------------------------------------------------------------------------------------------------------
# SET FIELD TEAM 6 PRIORITIES AND TARGET RANKINGS ----
#-----------------------------------------------------------------------------------------------------------------------------------------------
#TODO: change this to google drive read at some point
districtsTargetClass <- read.csv("target_class.csv", header=TRUE)

#set NA fields to default values
districtsTargetClass$TARGETCLASS[is.na(districtsTargetClass$TARGETCLASS)] <- 99
districtsTargetClass$HOUSEINPLAY[is.na(districtsTargetClass$HOUSEINPLAY)] <- 0
districtsTargetClass$SENATEINPLAY[is.na(districtsTargetClass$SENATEINPLAY)] <- 0
districtsTargetClass$PRESIDENCYINPLAY[is.na(districtsTargetClass$PRESIDENCYINPLAY)] <- 0
districtsTargetClass$CD116FP <- stringr::str_pad(districtsTargetClass$CD116FP,2,pad='0')

#merge with geoid map to get geoid
districtsTargetClass = merge(districtsTargetClass, districtGeoidMap, by=c('CD116FP','STATEPOSTAL'))
#districtsTargetClass$GEOID <- paste(districtsTargetClass$STATEPOSTAL, stringr::str_pad(districtsTargetClass$CD116FP,2,pad='0'), sep='')   #create geoid by concatenating postal & fips

#add high/higher/highest priority level for friendly mode
districtsTargetClass$PRIORITY <- ''
districtsTargetClass$PRIORITY[is.element(districtsTargetClass$TARGETCLASS, c(1,2))] <- 'HIGHEST'        #triple word
districtsTargetClass$PRIORITY[is.element(districtsTargetClass$TARGETCLASS, c(3,4,5,7,8))] <- 'HIGHER'   #double word
districtsTargetClass$PRIORITY[is.element(districtsTargetClass$TARGETCLASS, c(6,9,10,11,12))] <- 'HIGH'  #single word

#merge target class into districtsRepInfo dataframe
districtsTargetClass <- subset(districtsTargetClass, select = c('GEOID', 'HOUSEINPLAY', 'SENATEINPLAY', 'PRESIDENCYINPLAY', 'TARGETCLASS', 'PRIORITY'))
districtsRepInfo <- merge(districtsRepInfo, districtsTargetClass, by='GEOID')

#merge target class into statesRepInfo dataframe
districtsTargetClass <- subset(districtsTargetClass, select = c('GEOID', 'HOUSEINPLAY', 'SENATEINPLAY', 'PRESIDENCYINPLAY', 'TARGETCLASS', 'PRIORITY'))
districtsTargetClass <- subset(districtsTargetClass, select = c('STATEPOSTAL', 'SENATEINPLAY'))

#combine states info with districts rep info
dfRepInfo <- merge(districtsRepInfo, statesRepInfo, by='STATEPOSTAL', all.x = TRUE) #NOTE THAT THIS MAKES THE NON-STATES HAVE NULL rows

#-----------------------------------------------------------------------------------------------------------------------------------------------
# SET FIELD TEAM 6 MISSION (color) ----
#-----------------------------------------------------------------------------------------------------------------------------------------------

#set house color to be used by sidebar and table
dfRepInfo$COLOR_HOUSE[dfRepInfo$HOUSEINPLAY==1 & dfRepInfo$PARTY_HOUSE=='D'] <- 'blue'
dfRepInfo$COLOR_HOUSE[dfRepInfo$HOUSEINPLAY==1 & dfRepInfo$PARTY_HOUSE=='R'] <- 'red'
dfRepInfo$COLOR_HOUSE[dfRepInfo$HOUSEINPLAY==0] <- 'grey' 

#set senate colors to be used by sidebar and table
dfRepInfo$COLOR_SENATE.x[dfRepInfo$SENATEINPLAY==1 & dfRepInfo$PARTY_SENATE.x=='D'] <- 'blue'
dfRepInfo$COLOR_SENATE.x[dfRepInfo$SENATEINPLAY==1 & dfRepInfo$PARTY_SENATE.x=='R'] <- 'red'
dfRepInfo$COLOR_SENATE.x[dfRepInfo$SENATEINPLAY==0] <- 'grey' 
dfRepInfo$COLOR_SENATE.y[dfRepInfo$SENATEINPLAY==1] <- 'grey' 
dfRepInfo$COLOR_SENATE.y[dfRepInfo$SENATEINPLAY==0] <- 'grey' 
#^if senate is in play, senator x will alway be the one up for election (not senator y) based on logic above

#set presidency colors to be used by sidebar and table
dfRepInfo$COLOR_PRESIDENCY[dfRepInfo$PRESIDENCYINPLAY==1] <- 'red'
dfRepInfo$COLOR_PRESIDENCY[dfRepInfo$PRESIDENCYINPLAY==0] <- 'grey'

#-------------------------------------s----------------------------------------------------------------------------------------------------------
# SET FIELD TEAM 6 MISSION (sidebar info) ----
#-----------------------------------------------------------------------------------------------------------------------------------------------

#NOTE: need to do this after the target class are set since it depends on those classifications
#HOUSE -- add Field Team 6 mission based on whether the house seat is in play
dfRepInfo$MISSION_HOUSE[dfRepInfo$HOUSEINPLAY==1 & dfRepInfo$PARTY_HOUSE=='R'] <- 'BOOT'
dfRepInfo$MISSION_HOUSE[dfRepInfo$HOUSEINPLAY==1 & dfRepInfo$PARTY_HOUSE=='D'] <- 'PROTECT'
dfRepInfo$MISSION_HOUSE[grepl('Candidate',dfRepInfo$NAME_HOUSE)] <- 'ELECT'

dfRepInfo$MISSION_HOUSE <- apply(dfRepInfo, 1, 
                                 FUN = function(x) if(x['HOUSEINPLAY']==0) '' else paste(x['MISSION_HOUSE'], ' ', 
                                                                                         x['NAME_HOUSE'], 
                                                                                         ' (', x['PARTY_HOUSE'], ')', sep=''))
#special treatment for special elections
dfRepInfo$MISSION_HOUSE <- apply(dfRepInfo, 1, 
                                 FUN = function(x) if(grepl('Special Election', x['NAME_HOUSE'])) paste('ELECT ', 
                                                                                 x['NAME_HOUSE'], 
                                                                                 ' (', x['PARTY_HOUSE'], ')', sep='') else x['MISSION_HOUSE'])

#SENATE -- add Field Team 6 mission based on whether the senate seat is in play
dfRepInfo$MISSION_SENATE <- ""
dfRepInfo$MISSION_SENATE[dfRepInfo$SENATEINPLAY==1 & dfRepInfo$PARTY_SENATE.x=='R'] <- 'BOOT'
dfRepInfo$MISSION_SENATE[dfRepInfo$SENATEINPLAY==1 & dfRepInfo$PARTY_SENATE.x=='D'] <- 'PROTECT'
dfRepInfo$MISSION_SENATE <- apply(dfRepInfo, 1, 
                                  FUN = function(x) if(x['MISSION_SENATE']=='') '' else paste(x['MISSION_SENATE'], ' ', 
                                                                                              x['NAME_SENATE.x'], 
                                                                                              ' (', x['PARTY_SENATE.x'], ')', sep=''))
dfRepInfo$MISSION_PRESIDENCY <- ""
dfRepInfo$MISSION_PRESIDENCY[dfRepInfo$PRESIDENCYINPLAY==1] <- 'BOOT Donald J. Trump (R)'

#-----------------------------------------------------------------------------------------------------------------------------------------------
# SET FIELD TEAM 6 MISSION (table info) ----
#-----------------------------------------------------------------------------------------------------------------------------------------------

if(FALSE) {
  #NOTE: need to do this after the target class are set since it depends on those classifications
  #HOUSE -- add Field Team 6 mission based on whether the house seat is in play
  dfRepInfo$HOUSE_DT[dfRepInfo$HOUSEINPLAY==1 & dfRepInfo$PARTY_HOUSE=='R'] <- 'BOOT'
  dfRepInfo$HOUSE_DT[dfRepInfo$HOUSEINPLAY==1 & dfRepInfo$PARTY_HOUSE=='D'] <- 'PROTECT'
  dfRepInfo$HOUSE_DT <- apply(dfRepInfo, 1, 
                              FUN = function(x) if(x['HOUSEINPLAY']==0) paste(x['NAME_HOUSE'], 
                                                                              ' (', x['PARTY_HOUSE'], ')', 
                                                                              ' -- not in play', sep='')
                              else paste(x['NAME_HOUSE'], 
                                         ' (', x['PARTY_HOUSE'], ')', 
                                         ' -- ',
                                         x['HOUSE_DT']
                                         , sep=''))
  
  #SENATE -- add Field Team 6 mission based on whether the senate seat is in play
  dfRepInfo$SENATE_DT.x <- ""
  dfRepInfo$SENATE_DT.x[dfRepInfo$SENATEINPLAY==1 & dfRepInfo$PARTY_SENATE.x=='R'] <- 'BOOT'
  dfRepInfo$SENATE_DT.x[dfRepInfo$SENATEINPLAY==1 & dfRepInfo$PARTY_SENATE.x=='D'] <- 'PROTECT'
  dfRepInfo$SENATE_DT.x <- apply(dfRepInfo, 1, 
                                 FUN = function(x) if(x['SENATE_DT.x']=='') paste(x['NAME_SENATE.x'], 
                                                                                  ' (', x['PARTY_SENATE.x'], ')', 
                                                                                  ' -- not in play', sep='')
                                 else paste(x['NAME_SENATE.x'], 
                                            ' (', x['PARTY_SENATE.x'], ')', 
                                            ' -- ',
                                            x['SENATE_DT.x'], sep=''))
  
  #add second senator who will always be 'not in play' because their election year is later
  dfRepInfo$SENATE_DT.y <- ""
  dfRepInfo$SENATE_DT.y <- apply(dfRepInfo, 1, 
                                 FUN = function(x)  paste(x['NAME_SENATE.y'], 
                                                          ' (', x['PARTY_SENATE.y'], ')', 
                                                          ' -- not in play', sep=''))
  
  dfRepInfo$PRESIDENCY_DT <- ""
  dfRepInfo$PRESIDENCY_DT[dfRepInfo$PRESIDENCYINPLAY==1] <- 'Donald J. Trump (R) -- BOOT'
  dfRepInfo$PRESIDENCY_DT[dfRepInfo$PRESIDENCYINPLAY==0] <- 'Donald J. Trump (R) -- not in play'
} else {
  #OR DO IT MORE SIMPLY
  dfRepInfo$HOUSE_DT  <- apply(dfRepInfo, 1, FUN = function(x)  paste(x['NAME_HOUSE'], ' (', x['PARTY_HOUSE'], ')',  sep=''))
  dfRepInfo$SENATE_DT.x  <- apply(dfRepInfo, 1, FUN = function(x)  paste(x['NAME_SENATE.x'], ' (', x['PARTY_SENATE.x'], ')',  sep=''))
  dfRepInfo$SENATE_DT.y  <- apply(dfRepInfo, 1, FUN = function(x)  paste(x['NAME_SENATE.y'], ' (', x['PARTY_SENATE.y'], ')',  sep=''))
  dfRepInfo$PRESIDENCY_DT <- 'Donald J. Trump (R)'
  
}




#-----------------------------------------------------------------------------------------------------------------------------------------------
# MERGE WITH MASTER DATAFRAME ----
#-----------------------------------------------------------------------------------------------------------------------------------------------

districtsDataFrameSimpleInfo <- merge(districtsDataFrameSimpleRaw, dfRepInfo, by='GEOID')
districtsDataFrameSimpleInfo[is.na(districtsRepInfo$STATEPOSTAL)] <- "N/A"


#-----------------------------------------------------------------------------------------------------------------------------------------------
# CLEANUP DATAFRAME & SAVE TO RDS FILE ----
#-----------------------------------------------------------------------------------------------------------------------------------------------
districtsDataFrameSimple = districtsDataFrameSimpleInfo

#remove the three N/A polygons from the dataframe & their associated information
#NOTE: this does not remove the values from each level, but it does remove the data rows (e.g. 441 data rows, but still 444 geoid levels)
districtsDataFrameSimple <- districtsDataFrameSimple[districtsDataFrameSimple@data$NAMELSAD!="Congressional Districts not defined", ]

#remove "Delegate Districts, same method as above
#NOTE: this does not remove the values from each level, but it does remove the data rows (e.g. 435 data rows, but still 444 geoid levels)
#(e.g. 435 data rows, but still 444 geoid levels)
districtsDataFrameSimple <- districtsDataFrameSimple[districtsDataFrameSimple@data$NAMELSAD!="Delegate District (at Large)", ]
districtsDataFrameSimple <- districtsDataFrameSimple[districtsDataFrameSimple@data$NAMELSAD!="Resident Commissioner District (at Large)", ]

#remove the levels that are no longer relevant after subsetting
#NOTE: this DOES remove the levels referenced in the notes above (e.g. 435 data rows & 435 geoid levels)
districtsDataFrameSimple@data <- droplevels(districtsDataFrameSimple@data)

#save SpatialPolygonsDataFrame as a rds (stores any R object) which is included in deployment & loaded directly in global.R
readr::write_rds(districtsDataFrameSimple, path = file.path(getwd(), "tl_2018_us_cd116_simplified.rds"))
districtsDataFrameSimpleReadTest <- readr::read_rds(file.path(getwd(), "tl_2018_us_cd116_simplified.rds"))

#check length of districtsPolygons vs districtsPolygonsSimple (make sure nothing has been simplified out)
numDistricts = length(districtsDataFrame@data[[1]])
numDistrictsSimple = length(districtsDataFrameSimple@data[[1]])
numDistrictsSimpleReadTest = length(districtsDataFrameSimpleReadTest@data[[1]])
if(TRUE) {
  print('original data: ')
  print(numDistricts)
  print('simplified data: ')
  print(numDistrictsSimple)
  print('simplified data (read test): ')
  print(numDistrictsSimpleReadTest)
}



