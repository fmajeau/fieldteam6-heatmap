#
# This is the global file of a Shiny Web Application. Run at the beginning of each app session.
#
# Written by Fiona Majeau for Field Team 6
#


library(shiny)          #backbone of interactive app
library(leaflet)        #builds map and layers on polygons
library(rgdal)          #to read geojson file
library(rgeos)          #to simplify polygons so map loads faster
library(RANN)           #for nearest neighbor search for lat/longs
library(zipcode)        #for zipcode to lat/long conversion
library(DT)             #for rendering datatable
library(shinyWidgets)   #for app background color
library(dplyr)          #didn't end up using this
library(httr)           #to interact with mobilize api
library(rlist)          #to perform list flattening
library(stringr)        #for string handling
library(sf)             #for state polylines (i think?)
library(readr)          #read rds file
library(USAboundaries)  #for state polylines 
#NOTE: USAboundaries is not on CRAN, you have to run the following command before re-deploying the app so shiny can find it
#   devtools::install_github('USAboundaries')
#   rsconnect::deployApp()

#access the zipcode database in the 'zipcode' package
data(zipcode)

#access the R build-in dataset to get rough state centroids (needed to display virtual Mobilize events)
data("state")

# ------------------------------------------------------------------------------------------------------------------------------------
# CONSTANTS & STRINGS 
# ------------------------------------------------------------------------------------------------------------------------------------

#constants
kmPerMile = 1.60934                       #[km] / [mi]
kmPerDegree = 111.1                       #[km] / [lat or long degree]
milesPerDegree = kmPerDegree / kmPerMile  #[mi] / [lat or long degree]
meterPerMile = kmPerMile * 1000           #[m] / [mi]
mileSliderMin = 50                        #[mi] lowest value on mileage slider
mileSliderMax = 1000                      #[mi] highest value on mileage slider

#colors
strColorRepublican  = '#8b0000'   #party - republican
strColorDemocrat = '#00008B'      #party - democrat
strColorHighest = '#800000'       #priority - highest
strColorHigher ='#cc6600'         #priority - higher
strColorHigh = '#cca300'          #priority - high
strColorNone = '#BEBEBE'          #priority - none
strFieldTeam6Webpage = '#02013C'  #ft6 webpage - dark blue background
strColorAqua = '#67DFFF'          #ft6 webpage - aqua accent
strColorSalmon = '#FA8072'        #ft6 webpage - salmon accent

#image location since addControl() used to make custom legend doesn't allow local files
strIconUrl = "https://images.squarespace-cdn.com/content/v1/5c592d60797f74738f73856a/1549365622348-3C1K2ND1Z4UFPYUDV652/ke17ZwdGBToddI8pDm48kKXbTL5U8xV7KgHrzqU-XYBZw-zPPgdn4jUwVcJE1ZvWEtT5uBSRWt4vQZAgTJucoTqqXjS3CfNDSuuf31e0tVHYShtqp9p317BEUKra4SA7joXG0jEu6ntXNgZ58T98lRur-lC0WofN0YB1wFg-ZW0/Field-Team-Six-01cXXBadg.png"

# #congressional district class descriptions
# strClassDescriptions = ''
# strClassDescriptions[1] = 'any TRIPLE WORD SCORE district – a House district where any Democrat you register will help us win a (FLIP) House seat, a Senate seat, AND the Presidency.'
# strClassDescriptions[2] = 'any TRIPLE WORD SCORE district – a House district where any Democrat you register will help us win a (HOLD) House seat, a Senate seat, AND the Presidency.'
# strClassDescriptions[3] = 'any DOUBLE WORD SCORE state that includes the Presidency AND the Senate. (ALSO, the rest of any states who have 1 or more districts that are Class 1 or Class 2 targets.)'
# strClassDescriptions[4] = 'any DOUBLE WORD SCORE district in play for the Presidency AND a House district (FLIP).'
# strClassDescriptions[5] = 'any DOUBLE WORD SCORE district in play for the Presidency AND a House district (HOLD).'
# strClassDescriptions[6] = 'any state that is ONLY in play for the Presidency. (ALSO, the rest of any states who have 1 or more districts that are Class 4 or Class 5 targets.)'
# strClassDescriptions[7] = 'any DOUBLE WORD SCORE district in play for the Senate and the House (FLIP).'
# strClassDescriptions[8] = 'any DOUBLE WORD SCORE district in play for the Senate and the House (HOLD).'
# strClassDescriptions[9] = 'any state that is ONLY in play for the Senate. (ALSO, the rest of any state who has 1 or more districts that are Class 7 or Class 8 targets.)'
# strClassDescriptions[10] = "any state legislative district in play for one of the statehouses, in a state where gerrymandering could be rolled back for the next decade... and NOT in a state otherwise targeted for anything. (If its state is also a target, we'll assign it to a target class on a case-by-case basis.)"
# strClassDescriptions[11] = 'any district in play for the House (FLIP) – in a state NOT in play for the Senate or Presidency.'
# strClassDescriptions[12] = 'any district in play for the House (HOLD) – in a state NOT in play for the Senate or Presidency.'

#friendly view class descriptions
strIntro <- 'Every Democrat you register in this district will help us ' 
strHouseHold <- 'hold our House majority'
strHouseFlip <- 'expand our House majority'
strSenateFlip <- 'flip the Senate' 
strWhiteHouseFlip <- 'take back the White House'
strClassDescriptionsFriendly <- ''
strClassDescriptionsFriendly[1] <- paste(strIntro, strHouseFlip, ', ',strSenateFlip, ', and ', strWhiteHouseFlip, '.', sep = '')
strClassDescriptionsFriendly[2] <- paste(strIntro, strHouseHold, ', ',strSenateFlip, ', and ', strWhiteHouseFlip,  '.', sep = '')
strClassDescriptionsFriendly[3] <- paste(strIntro, strSenateFlip, ' and ', strWhiteHouseFlip,  '.', sep = '')
strClassDescriptionsFriendly[4] <- paste(strIntro, strHouseFlip, ' and ', strWhiteHouseFlip,  '.', sep = '')
strClassDescriptionsFriendly[5] <- paste(strIntro, strHouseHold, ' and ', strWhiteHouseFlip,  '.', sep = '')
strClassDescriptionsFriendly[6] <- paste(strIntro, strWhiteHouseFlip,  '.', sep = '')
strClassDescriptionsFriendly[7] <- paste(strIntro, strHouseFlip, ' and ', strSenateFlip,  '.', sep = '')
strClassDescriptionsFriendly[8] <- paste(strIntro, strHouseHold, ' and ', strSenateFlip,  '.', sep = '')
strClassDescriptionsFriendly[9] <- paste(strIntro, strSenateFlip,  '.', sep = '')
strClassDescriptionsFriendly[10] <- ''
strClassDescriptionsFriendly[11] <- paste(strIntro, strHouseFlip,  '.', sep = '')
strClassDescriptionsFriendly[12] <- paste(strIntro, strHouseHold,  '.', sep = '')
strClassDescriptionsFriendly[99] <- 'Registering democrats is unlikely to affect elections in this area.'

# ------------------------------------------------------------------------------------------------------------------------------------
# PRE-PROCESS DISTRICT POLYGONS
# ------------------------------------------------------------------------------------------------------------------------------------

#read rds file created by simplify_shapefile.R
districtsDataFrameSimple <- readr::read_rds(file.path(getwd(), "tl_2018_us_cd116_simplified.rds")) #SpatialPolygonsDataFrame

#rename for clarity in ui and server scripts
dfDistricts <- districtsDataFrameSimple

#create matrix of polygon centroids for nearest neighbor search
dfDistrictCentroids <- data.frame(GEOID=dfDistricts$GEOID, 
                                INTPTLAT=as.numeric(levels(dfDistricts$INTPTLAT)[dfDistricts$INTPTLAT]),
                                INTPTLON=as.numeric(levels(dfDistricts$INTPTLON)[dfDistricts$INTPTLON]))

#create list of geoids (defines a district)
lsDistrictGeoids <- dfDistricts@data['GEOID']

#create html label for districts (e.g. CA-12)
dfDistricts$LABEL <- paste('<strong>', districtsDataFrameSimple$DISTRICT, '</strong>')

#get state polygons for the 10 battleground states
#"Arizona", "Colorado", "Florida", "Georgia", "Iowa", "Maine", "Michigan", "North Carolina", "Pennsylvania", "Wisconsin"
mpBattlegroundStates <- USAboundaries::us_states(states = c("Arizona", "Colorado", "Florida", "Georgia", "Iowa", "Maine", "Michigan", "North Carolina", "Pennsylvania", "Wisconsin"))

# ------------------------------------------------------------------------------------------------------------------------------------
# PRE-PROCESS MOBILIZE EVENTS
# ------------------------------------------------------------------------------------------------------------------------------------
#creates dfMobilizeEvents to be used by server.R
# -- The only fields used for the map are ID, LATITUDE, LONGITUDE, LABEL
# -- All other fields used for the table display

#pull in mobilize event data in a dataframe using mobilize API 
#https://github.com/mobilizeamerica/api/blob/master/README.md
# organization = 1255 <-- Field Team 6 org code
# timeslot_start = gte_new <-- all future events 
# per_page = 1000 <-- need large per_page number otherwise it will truncate your results
result <- GET("https://api.mobilize.us/v1/organizations/1255/events?timeslot_start=gte_now&per_page=1000")
lsMobilizeEvents <- httr::content(result)

#lsMobilizeEvents <- lapply(lsMobilizeEvents$data, function(x) { x$timeslot_count <- length(x$timeslots); return(x) })
#lsMobilizeEvents <- lapply(lsMobilizeEvents$data, function(x) { lapply(x$timeslots, function(y) {max(y$start_date)})})
#lsMobilizeEvents <- lapply(lsMobilizeEvents$data, function(x) { max(x$timeslots, key=lambda x: x$start_date) })

#events may have more than one timeslot, something that we want to display in the table
#this section loops through each event and builds an html string with the 4 soonest event date/times 
intEvents = length(lsMobilizeEvents$data)
for (i in 1:intEvents) {
  
  #set the total number of time slots available
  intTimeSlots <- length(lsMobilizeEvents$data[[i]]$timeslots)
  lsMobilizeEvents$data[[i]]$timeslot_count <- intTimeSlots
  
  #loop through every timeslot to find the min and max start date for that event
  intMinStartDate = Inf
  intMaxEndDate = 0
  strAllDates = ''
  lsAllDates <- list()
  if(intTimeSlots > 0) {
    for (j in 1:intTimeSlots) {
      intStartDate <- lsMobilizeEvents$data[[i]]$timeslots[[j]]$start_date
      intEndDate <- lsMobilizeEvents$data[[i]]$timeslots[[j]]$end_date
      if (intStartDate < intMinStartDate) {intMinStartDate = intStartDate}
      if (intEndDate > intMaxEndDate) {intMaxEndDate = intEndDate}
      strEventTimezone <- lsMobilizeEvents$data[[i]]$timezone 
      dtStartDate <- as.Date(as.POSIXct(as.numeric(as.character(intStartDate)),origin="1970-01-01",tz=strEventTimezone))
      strStartDate <- format(dtStartDate, "%b %d, %Y")
      lsAllDates[[j]] <- strStartDate #add to the list of all timeslot dates 
    }
  
    #only display the top 4 dates, since they are displaying vertically and it takes up too much space otherwise
    lsAllDates = unique(lsAllDates) # there can be multiple time slots per day
    if (length(lsAllDates) > 3) {
      lsDisplayDates <- lsAllDates[1:3]
      lsDisplayDates[[4]] <- '<i>(more)</i>'
    } else {
      lsDisplayDates <- lsAllDates
    }
    
    #turn the dates into a string that will display vertically in the table cell, set as variable
    strAllDates <- paste(lsDisplayDates, collapse = '<br>')
    lsMobilizeEvents$data[[i]]$date_list <- strAllDates
    
    #set the min start date and max end date for the event in the event's timezone
    strEventTimezone <- lsMobilizeEvents$data[[i]]$timezone 
    lsMobilizeEvents$data[[i]]$min_start_date <- as.character(as.Date(as.POSIXct(as.numeric(as.character(intMinStartDate)),origin="1970-01-01",tz=strEventTimezone)))
    lsMobilizeEvents$data[[i]]$max_end_date <- as.character(as.Date(as.POSIXct(as.numeric(as.character(intMaxEndDate)),origin="1970-01-01",tz=strEventTimezone)))
  }
  
} #end of loop through events


#flatten all inner lists into a primary list
lsMobilizeEventsFlat <- lapply(lsMobilizeEvents$data, rapply, f = c)

#select the columns that we care about (note that timeslots have a variable number...)
lsMobilizeEventsTrim <- lapply(lsMobilizeEventsFlat, function(x) { x[c('id',
                                                                       'title', 
                                                                       'event_type',
                                                                       'timeslot_count',
                                                                       'min_start_date',
                                                                       'max_end_date',
                                                                       'date_list',
                                                                       'location.location.latitude', 
                                                                       'location.location.longitude',
                                                                       'location.locality',
                                                                       'location.region',
                                                                       'browser_url')]})
#rename columns for cleanliness
lsMobilizeEventsTrim <- lapply(lsMobilizeEventsTrim, setNames, nm = c('ID', 'TITLE', 'EVENT_TYPE', 'TIMESLOT_COUNT', 'MIN_START_DATE', 'MAX_END_DATE', 'DATE_LIST', 'LATITUDE', 'LONGITUDE', 'CITY', 'STATE', 'URL'))

#convert list of lists into a matrix
matMobilizeEvents <- do.call("rbind", lsMobilizeEventsTrim)

#convert matrix into a dataframe
dfMobilizeEvents <- as.data.frame(matMobilizeEvents, stringsAsFactors=FALSE)

#add lat/longs for all virtual events using the center of the state 
dfMobilizeEventsVirtual <- dfMobilizeEvents[is.na(dfMobilizeEvents$LATITUDE) & is.na(dfMobilizeEvents$LONGITUDE) & is.na(dfMobilizeEvents$CITY), ]

#loop through each virtual event and assign lat/longs and city/state so it will show up on the map
# state only      -- lat/long of center of state
# city+state only -- TODO: NOT IMPLEMENTED YET
for (i in 1:length(dfMobilizeEventsVirtual)) {
  
  #use regex on the title to find the state that the virtual event is affiliated with
  strTitle <- dfMobilizeEventsVirtual[i,]$TITLE[1]
  m <- regexec("FLIPPIN'\\s((?:[A-Z]+\\s)+)-\\s", strTitle) #looks for and captures "FLIPPIN' {STATE NAME} - "
  regmatches(strTitle, m)
  strState <- regmatches(strTitle, m)[[1]][2] #grab the state name capture group
  strState <- str_trim(str_to_title(strState)) #convert from "CALIFORNIA " to "California"
  
  #set the coordinates to be the center of the state
  if(!is.na(strState)) {
    dfMobilizeEvents$LATITUDE[dfMobilizeEvents$ID == dfMobilizeEventsVirtual[i,]$ID[1]] <- as.numeric(state.center$y[state.name==strState])
    dfMobilizeEvents$LONGITUDE[dfMobilizeEvents$ID == dfMobilizeEventsVirtual[i,]$ID[1]] <- as.numeric(state.center$x[state.name==strState])
  }
  
  #set the city and state to be (REMOTE), CA
  dfMobilizeEvents$CITY[dfMobilizeEvents$ID == dfMobilizeEventsVirtual[i,]$ID[1]] <- '(REMOTE)'
  dfMobilizeEvents$STATE[dfMobilizeEvents$ID == dfMobilizeEventsVirtual[i,]$ID[1]] <- state.abb[state.name==strState]
  
}

#get rid of any events that still do not have lat/longs, because we can't display them on the map
#includes the generic training call (no state affiliation) and events with only city,state (TODO)
dfMobilizeEvents <- dfMobilizeEvents[!is.na(dfMobilizeEvents$LATITUDE) & !is.na(dfMobilizeEvents$LONGITUDE), ]

#make sure the coords are numeric
dfMobilizeEvents$LONGITUDE <- as.numeric(dfMobilizeEvents$LONGITUDE)
dfMobilizeEvents$LATITUDE <- as.numeric(dfMobilizeEvents$LATITUDE)

#format event type and use as map label
dfMobilizeEvents$EVENT_TYPE <- stringr::str_replace(dfMobilizeEvents$EVENT_TYPE, '_', ' ')
dfMobilizeEvents$LABEL <- paste('<strong>', dfMobilizeEvents$EVENT_TYPE, '</strong>')

#create coordinates object to build SpatialPointsDataFrame
xy <- dfMobilizeEvents[,c('LONGITUDE', 'LATITUDE')]

#build SpatialPointsDataFrame, the data structure required to apply markers to the leaflet map
dfMobilizeEvents <- SpatialPointsDataFrame(coords = xy, 
                                             data = dfMobilizeEvents,
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


#create list of event ids (defines an event)
lsMobilizeEventIds <- dfMobilizeEvents@data['ID']



# ---------------------------
# NOTES
# ---------------------------

#### how to access things inside a SpatialPolygonsDataFrame
#see https://gist.github.com/mbacou/5880859
# slotNames(districtsDataFrame)               #[1] "data"        "polygons"    "plotOrder"   "bbox"        "proj4string"
# slotNames(districtsDataFrame@polygons[[1]]) #[1] "Polygons"  "plotOrder" "labpt"     "ID"        "area"  

#### how to read the mobilize api GET response
#json string will have some number of data elements, each one representing an event
#to get all data elements: length(json$data[])
#to access values in one event:
#json$data[[24]]$browser_url
#json$data[[24]]$title
#json$data[[24]]$location$locality
#json$data[[24]]$location$region
#json$data[[24]]$location$location$latitude
#json$data[[24]]$location$location$longitude
#json$data[[24]]$location$congressional_district

#### final version of ~/.R/Makevars after edits
# VER=-8
# CC=gcc$(VER)
# CXX=g++$(VER)
# CXX11=g++$(VER)
# CXX14=g++$(VER)
# CXX17=g++$(VER)
# CFLAGS=-mtune=native -g -O2 -Wall -pedantic -Wconversion
# CXXFLAGS=-mtune=native -g -O2 -Wall -pedantic -Wconversion
# FLIBS=-L/usr/local/Cellar/gcc/8.3.0_2/lib/gcc/8