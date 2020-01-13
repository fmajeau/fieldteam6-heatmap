#
# This is the server logic of a Shiny web application. You can run the application by clicking 'Run App' above.
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/
#
# Written by Fiona Majeau for Field Team 6
#

# Overview of dataframes
#   dfDistricts --> all non-user specific information about each polygon/distric (full set of districts) *defined in global.R*
#   dfDistrictsUser --> includes miles between user zipcode and each polygon (full set of districts)
#   dfDistrictsDatatable --> certain columns of dfDistrictsUser, renamed for readability (full set of districts)
#   v$dfDistrictsDatatableFiltered --> dfDistrictsDatatable filtered by input$miles (subset of districts)
#   dfMobilizeEvents --> all non-user specific information about each polygon/distric (full set of events) *defined in global.R*
#   dfMobilizeEventsUser --> includes miles between user zipcode and each polygon (full set of events)
#   dfMobilizeEventsDatatable --> certain columns of dfMobilizeEventsUser, renamed for readability (full set of events)
#   v$dfMobilizeEventsDatatableFiltered --> dfMobilizeEventsDatatable filtered by input$miles (subset of events)

# Define server logic for creating map & datatables
shinyServer(
  
  function(input, output, session) {
    
    #---- RESPOND TO RESET BUTTON ----
    
    observeEvent(input$reset, {
        updateTextInput(session, "zipcode", value = "")
        updateSliderInput(session, "miles", value = mileSliderMin)
        leafletProxy("usmap") %>% fitBounds(-120, 25, -75, 50) #contiguous 48
        
        v$clickedGeoIdPrev <- v$clickedGeoIdNew     #set v$clickedGeoIdPrev so it is removed from map
        v$clickedGeoIdNew <- vector()               #clear v$clickedGeoIdNew so no new highlighting is added
        renderDistrictSelectionOnMap()              #allow function to render the new clickedGeoIdPrev/clickedGeoIdNew values
        
        v$clickedEventIdPrev <- v$clickedEventIdNew #set v$clickedEventIdPrev so it is removed from map
        v$clickedEventIdNew <- vector()             #clear v$clickedEventIdNew so no new highlighting is added
        renderEventSelectionOnMap()                 #allow function to render the new clickedGeoIdPrev/clickedGeoIdNew values

    })

    #---- DEFINE REACTIVE VALUES ----
    
    #reactive values that help with handling district selection & the datatable range
    v <- shiny::reactiveValues(clickedGeoIdNew = vector(),  #store the geoid of the district currently selected by the user 
                               clickedGeoIdPrev = vector(), #store the geoid of the district previously selected by the user in order to reset it to the original map formatting
                               clickedEventIdNew = vector(),  #store the event id of the event currently selected by the user 
                               clickedEventIdPrev = vector(), #store the event id of the event previously selected by the user in order to reset it to the original map formatting
                               
                               dfDistrictsDatatableFiltered = data.frame(), #store the districts datatable data as filtered by the current userMiles 
                               dfMobilizeEventsDatatableFiltered = data.frame(), #store the districts datatable data as filtered by the current userMiles 
                               userMiles = NULL, #store the current value of userMiles
                               validZipcode = 0 #keep track of whether the current zipcode is valid
    )
    
    #check validity of user's zipcode
    observeEvent(input$zipcode,{
        zipcodeInfo <- subset(zipcode,zip==input$zipcode)
        if(nrow(zipcodeInfo) != 0)
          v$validZipcode <- 1
        else
          v$validZipcode <- 0
      
    })
    
    #---- HANDLE MAP+TABLE DISTRICT SELECTION ----
    
    #assign shape_click output to variable (tells us which polygon was clicked on, used below to create clickedDistrictInfo)
    observeEvent(input$usmap_shape_click, {
        v$clickedGeoIdPrev <- v$clickedGeoIdNew #save currently clicked district as the previously clicked district
        click <- input$usmap_shape_click  #grab new click info
        geoid <- click$id                 #grab geoid of new clicked district
        v$clickedGeoIdNew <- geoid        #redefine currently clicked district 
        renderDistrictSelectionOnMap()    #since the map was clicked, update the map
        renderDistrictSelectionInTable()  #since the map was clicked, update the table
    })
    
    #listen to a table row click, update map to render the selected district
    observeEvent(input$districts_datatable_rows_selected,{
        rowNumber <- input$districts_datatable_rows_selected 
        geoid <- v$dfDistrictsDatatableFiltered[rowNumber,]$GEOID

        #if the new geoid originated from a table click, update the map
        #NOTE: if map is clicked, renderDistrictSelectionInTable will be called which alters input$districts_datatable_rows_selected, 
        #      thereby triggering this function, in which case we do not want to redefine Prev/New
        if(!is.na(geoid)) {
            v$clickedGeoIdPrev <- v$clickedGeoIdNew
            v$clickedGeoIdNew <- geoid
            renderDistrictSelectionOnMap()
        }
        renderDistrictSelectionInTable() #since the table was clicked, update the table
        
    })
    
    #---- HANDLE MAP+TABLE EVENT SELECTION ----
    
    #assign marker_click output to variable (tells us which marker was clicked on, used below to create the event datatable)
    #NOTE: this is almost exactly the same as the district click functions/logic
    observeEvent(input$usmap_marker_click, { 
      v$clickedEventIdPrev <- v$clickedEventIdNew
      click <- input$usmap_marker_click   #grab new click info
      eventId <- click$id                 #grab eventId of new clicked marker
      v$clickedEventIdNew <- eventId
      
      #not going to change the scope of the map depending on which event you click on ...
      #if the table is being displayed, check whether we need to adjust the data bounds 
      if(v$validZipcode == 1) {
          g <- count(subset(v$dfMobilizeEventsDatatableFiltered, ID==eventId)) #identify whether clicked event id is in the current datatable display
          if(g == 0) {
              #districtDistances holds GEOID and miles from zip
              #use that to find new miles input range so the clicked district shows up in the table
              userMilesNew <- subset(dfMobilizeEventsUser(),ID==eventId)$USERDIST
              
              #only rebuild the table if you're expanding the scope (if not already at max & if clicked district increases the range)
              if(v$userMiles < mileSliderMax & userMilesNew > v$userMiles) {
                  if(userMilesNew >= mileSliderMax) 
                      v$userMiles <- mileSliderMax
                  else
                      v$userMiles <- userMilesNew
                  updateSliderInput(session, "miles", value = v$userMiles)
                  #update the filter of the datatable based on the expanded zoom 
                  #-- need to do this here because the normal observeEvent will not update the datatable before we need to select the
                  #   row using renderDistrictSelectionInTable() -- this is the reason the datatable data needs to be a reactive value
                  v$dfMobilizeEventsDatatableFiltered <- subset(dfMobilizeEventsDatatable(), as.numeric(MilesFromZip) <= v$userMiles)
              }
              
          }
          
      } #else, table should not be displaying yet
      
      renderEventSelectionOnMap()   #since the map was clicked, update the map
      renderEventSelectionInTable() #since the map was clicked, update the table
      
    })
    
    #listen to a table row click, update map to render the selected district
    observeEvent(input$events_datatable_rows_selected,{
        rowNumber <- input$events_datatable_rows_selected 
        eventId <- v$dfMobilizeEventsDatatableFiltered[rowNumber,]$ID
        #print(eventId)
        #print(v$dfMobilizeEventsDatatableFiltered)
        #if the new geoid originated from a table click, update the map
        #NOTE: if map is clicked, renderDistrictSelectionInTable will be called which alters input$events_datatable_rows_selected, 
        #      thereby triggering this function, in which case we do not want to redefine Prev/New
        if(!is.na(eventId)) {
            v$clickedEventIdPrev <- v$clickedEventIdNew
            v$clickedEventIdNew <- eventId
            renderEventSelectionOnMap()
        }
        renderEventSelectionInTable() #since the table was clicked, update the table #<----------- NOT DEFINED YET
        
    })
    
    
    #---- BUILD DATATABLE DATAFRAME BASED ON ZIPCODE ----
    
    #update userMiles reactive value
    observeEvent({input$miles
                  input$zipcode},{
        #if (1) it is the beginning of a session i.e. null OR (2) everything is still below mileSliderMax OR (3) input miles is under max value
        if (is.null(v$userMiles) || (v$userMiles <= mileSliderMax & input$miles == mileSliderMax) || input$miles < mileSliderMax)
            v$userMiles <- input$miles
        #else let v$userMiles remain the true value 
        
    })
  
    #calculate distance between user's zipcode and each event
    dfMobilizeEventsUser <- reactive({
      
      shiny::validate(
        need(v$validZipcode == 1, "")
      )
      
      zipcodeInfo <- subset(zipcode,zip==input$zipcode) #get info from database
      shiny::validate(
        need(nrow(zipcodeInfo) != 0,'')
        
      ) 
      userLatLong <- cbind(as.numeric(zipcodeInfo['latitude']), as.numeric(zipcodeInfo['longitude']))
      
      #use nearest neighbor search to find distance between lat/long of user and lat/long of each district centroid
      nearest <- RANN::nn2(dfMobilizeEvents@data[,c('LATITUDE','LONGITUDE')], userLatLong, k=nrow(dfMobilizeEvents@data)) 
      
      #create dataframe of districtDistances which will eventually be merged with the districts dataframe
      distances = round(nearest$nn.dists[1,] * milesPerDegree, 0) #convert from degrees to miles
      eventIds = list(as.character(lsMobilizeEventIds[nearest$nn.idx,]))[[1]]
      eventDistances <- data.frame(ID=eventIds, USERDIST=as.numeric(distances))

      #create return matrix of districts ordered by proximity, store distance from user in miles
      dfMobilizeEventsUser <- merge(dfMobilizeEvents, eventDistances, by='ID')
    })
    
    #calculate distance between user's zipcode and each district
    dfDistrictsUser <- reactive({
        
        shiny::validate(
          need(v$validZipcode == 1, "")
        )
      
        zipcodeInfo <- subset(zipcode,zip==input$zipcode) #get info from database
        
        shiny::validate(
          need(nrow(zipcodeInfo) != 0,'')
        )
        
        #set user's lat long based on zipcode
        userLatLong <- cbind(as.numeric(zipcodeInfo['latitude']), as.numeric(zipcodeInfo['longitude']))
      
        #use nearest neighbor search to find distance between lat/long of user and lat/long of each district centroid
        nearest <- RANN::nn2(dfDistrictCentroids[,c('INTPTLAT','INTPTLON')], userLatLong, k=nrow(dfDistrictCentroids)) 
        
        #create dataframe of districtDistances which will eventually be merged with the districts dataframe
        distances = round(nearest$nn.dists[1,] * milesPerDegree, 0) #convert from degrees to miles
        geoids = list(as.character(lsDistrictGeoids[nearest$nn.idx,]))[[1]]
        districtDistances <- data.frame(GEOID=geoids, USERDIST=as.numeric(distances))

        #print(districtDistances[districtDistances$GEOID=="0608"])
        
        #-----------------------------------------------------------------------------------------------------------------------------
        #STILL TESTING -- another way to do this using dist2Line
        #-----------------------------------------------------------------------------------------------------------------------------
        if(FALSE) {
          
          #https://gis.stackexchange.com/questions/169599/extract-all-the-polygon-coordinates-from-a-spatialpolygonsdataframe
          #(1) convert SpatialPolygonsDataFrame to SpatialLinesDataFrame
          dfDistrictsLines <- sp::coordinates(as(dfDistricts, "SpatialLinesDataFrame"))
          #geosphere::dist2Line(c(-97.75013, 34.58659), temp[[440]][[1]])
          
          #(2) generate list of distance between the user's lat long and the closest point in each SpatialLinesDataFrame
          #lsDistrictDistancesRaw <- lapply(dfDistrictsLines, function(x) {geosphere::dist2Line(userLatLong, x[[1]])})
          x <- dfDistrictsLines
          userLongLat <- cbind(as.numeric(zipcodeInfo['longitude']), as.numeric(zipcodeInfo['latitude']))
          lsDistrictDistancesRaw <- lapply(seq_along(x), function(i) cbind(GEOID=dfDistricts@data$GEOID[[i]],
                                                                           USERDIST=as.double(geosphere::dist2Line(userLongLat, x[[i]][[1]])[1,'distance'])
                                                                           )
                                           )
          ##test with real lat/long
          #lsDistrictDistancesRaw <- lapply(seq_along(x), function(i) cbind(GEOID=dfDistricts@data$GEOID[[i]],
          #                                                                 USERDIST=as.double(geosphere::dist2Line(c(-97.75013, 34.58659), x[[i]][[1]])[1,'distance'])
          #                                                                 )
          #                                 )
          
          #put the geoid and userdist into a dataframe
          lsDistrictDistances <-  do.call(rbind, lsDistrictDistancesRaw)
          districtDistances <- data.frame(GEOID=lsDistrictDistances[,'GEOID'], 
                                          USERDIST=lsDistrictDistances[,'USERDIST']/meterPerMile)
        }
        #-----------------------------------------------------------------------------------------------------------------------------
        
        #if a user's district is large, it is possible the centroid of their district is too far from their zipcode for the district 
        #to be included in even the smallest specified range. set the user's district to be 0 miles away to ensure it is always included. 
        userLocation <- data.frame(Longitude = c(userLatLong[1,2]), Latitude =c(userLatLong[1,1]) )
        coordinates(userLocation) <- ~ Longitude + Latitude
        dfDistrictsTemp <- dfDistricts
        
        #use `over` function to find the district that includes the user's zipcode lat/long
        # -- function requires both inputs to have the same coordinate frame (dfDistricts already set as ESPG:4326 i.e. WGS 84)
        # -- returns row of NAs if no district is found
        proj4string(userLocation) <- proj4string(dfDistrictsTemp) 
        userDistrict <- sp::over(userLocation, dfDistrictsTemp) #SpatialPoints, SpatialPolygonsDataFrame
        
        #set USERDIST to be 0 for the district in which the user lives (if a district is found)
        if(!is.na(userDistrict$GEOID)) 
            districtDistances$USERDIST[districtDistances$GEOID==userDistrict$GEOID] <- 0 

        #create return matrix of districts ordered by proximity, store distance from user in miles
        dfDistrictsUser <- merge(dfDistricts, districtDistances, by='GEOID')
    })
    
    #select subset of columns to be shown in datatable, rename some for readability
    #NOTE: restriction based on user's mile range happens in an observeEvent and is saves in a reactive value
    dfDistrictsDatatable <- reactive( {
        shiny::validate(
            need(v$validZipcode == 1, "")
        )
        dfDistrictsDatatable <- data.frame(District=dfDistrictsUser()$DISTRICT,
                                        Priority=dfDistrictsUser()$PRIORITY,
                                        Class=dfDistrictsUser()$TARGETCLASS,
                                        HouseRep=dfDistrictsUser()$HOUSE_DT,
                                        Senator1=dfDistrictsUser()$SENATE_DT.x,
                                        Senator2=dfDistrictsUser()$SENATE_DT.y,
                                        President=dfDistrictsUser()$PRESIDENCY_DT,
                                        MilesFromZip=as.numeric(as.character(dfDistrictsUser()$USERDIST)),
                                        HouseColor=dfDistrictsUser()$COLOR_HOUSE,           #needed for text coloring
                                        SenateColorX=dfDistrictsUser()$COLOR_SENATE.x,      #needed for text coloring
                                        SenateColorY=dfDistrictsUser()$COLOR_SENATE.y,      #needed for text coloring
                                        PresidencyColor=dfDistrictsUser()$COLOR_PRESIDENCY, #needed for text coloring
                                        GEOID=dfDistrictsUser()$GEOID) #needed to allow user to click table row to select district
        
        #only display target districts (priority classes 1-12)
        dfDistrictsDatatable <- subset(dfDistrictsDatatable, Class < 13)
        
    })
    
    #select subset of columns to be shown in datatable, rename some for readability
    #NOTE: restriction based on user's mile range happens in an observeEvent and is saves in a reactive value
    dfMobilizeEventsDatatable <- reactive( {
      shiny::validate(
        need(v$validZipcode == 1, "")
      )
      dfMobilizeEventsDatatable <- data.frame(Type=dfMobilizeEventsUser()$EVENT_TYPE,
                                              Event=dfMobilizeEventsUser()$TITLE,
                                              #Times=paste(dfMobilizeEventsUser()$TIMESLOT_COUNT, ' timeslots between ', dfMobilizeEventsUser()$MIN_START_DATE, ' and ', dfMobilizeEventsUser()$MAX_END_DATE, sep='' ),
                                              Dates=dfMobilizeEventsUser()$DATE_LIST,
                                              Location=paste(dfMobilizeEventsUser()$CITY, ', ', dfMobilizeEventsUser()$STATE, sep=''),
                                              MilesFromZip=dfMobilizeEventsUser()$USERDIST,
                                              SignupLink=paste('<a href=\"',dfMobilizeEventsUser()$URL, '\">VOLUNTEER!</a>'),
                                              ID=dfMobilizeEventsUser()$ID) #needed to allow user to click table row to select district
      
    })

    #restrict district & event datatables based on the user's selected mile range whenever new user inputs are set
    observeEvent({input$zipcode
                    input$miles} , {
            
            if(v$validZipcode == 1) {
              #filter district table
              v$dfDistrictsDatatableFiltered <- subset(dfDistrictsDatatable(), as.numeric(MilesFromZip) <= v$userMiles)
              #table will redraw itself to display the correct entries so we need to re-select the district in the table
              renderDistrictSelectionInTable() 
              #filter events table
              v$dfMobilizeEventsDatatableFiltered <- subset(dfMobilizeEventsDatatable(), as.numeric(MilesFromZip) <= v$userMiles) # ADD THIS BACK
              #table will redraw itself to display the correct entries so we need to re-select the event in the table
              renderEventSelectionInTable()
            }
              
    })
    
    #---- BUILD DATATABLE OUTPUTS ----
     
    output$events_datatable <- DT::renderDataTable({
      shiny::validate(
        need(length(v$dfMobilizeEventsDatatableFiltered) != 0, ""),
        need(v$validZipcode == 1, "")
      )
      df <- v$dfMobilizeEventsDatatableFiltered 
      d <- DT::datatable(df,   
                         selection = 'single', #only allow one row selection at a time
                         options = list( #lengthMenu = c(10,25,50,100,500),
                           dom = 't', #show: table (t); exclude: page length control (l) , search box/filter (f), info summary (i), page control (p), processing display (r))
                           pageLength = 500, 
                           order = list(list(4, 'asc')) , #3 = MilesFromZip
                           columnDefs = list(list(targets = c(6), visible = FALSE)), #turn off visibility for subset of columns
                           highlightOptions(fillColor = 'blue', opacity=1, bringToFront = TRUE)
                         ),
                         rownames = FALSE,
                         colnames = c("<span style='color:#67DFFF'>Event Type</span>" = 1,
                                      "<span style='color:#67DFFF'>Event Name</span>" = 2,
                                      "<span style='color:#67DFFF'>Dates</span>" = 3,
                                      "<span style='color:#67DFFF'>Location</span>" = 4,
                                      "<span style='color:#67DFFF'>Miles From Zip</span>" = 5,
                                      "<span style='color:#67DFFF'>Signup Link</span>" = 6), #color the column title text & add spaces
                         escape = FALSE,
                         caption = tags$caption(
                            tags$span(style = paste('color:',strColorSalmon, ';font-size:18px; font-weight:bold'), "Volunteer events "),
                            tags$span(style = paste('color:',strColorNone, ';font-size:16px'), " within "),
                            tags$span(style = paste('color:',strColorSalmon, ';font-size:18px; font-weight:bold'), as.character(v$userMiles)),
                            tags$span(style = paste('color:',strColorNone, ';font-size:16px'), "miles of you: ")
                            ),
      ) %>% 
        formatStyle(
          "<span style='color:#67DFFF'>Event Type</span>", fontWeight = 'bold', width = '12%'
        ) %>%
        formatStyle(
          "<span style='color:#67DFFF'>Event Name</span>", fontSize = '80%', width = '39%'
        ) %>%
        formatStyle(
          "<span style='color:#67DFFF'>Dates</span>", fontSize = '80%', width = '10%', textAlign = 'center'
        ) %>%
        formatStyle(
          "<span style='color:#67DFFF'>Location</span>", fontSize = '90%', fontWeight = 'bold', width = '18%'
        )  %>%
        formatStyle(
          "<span style='color:#67DFFF'>Miles From Zip</span>", fontWeight = 'bold', width = '9%',
          backgroundColor = 'lightgrey'
        )  %>%
        formatStyle(
          "<span style='color:#67DFFF'>Signup Link</span>", fontSize = '90%', width = '11%', textAlign = 'center'
        )  
    })
    
    #use datatable dataframe to build the datatable output 
    output$districts_datatable <- DT::renderDataTable({
        shiny::validate(
            need(length(v$dfDistrictsDatatableFiltered) != 0, ""),
            need(v$validZipcode == 1, "")
        )
        df <- v$dfDistrictsDatatableFiltered 
        d <- DT::datatable(df,   
                           selection = 'single', #only allow one row selection at a time
                           options = list(
                             dom = 't', #show: table (t); exclude: page length control (l) , search box/filter (f), info summary (i), page control (p), processing display (r))
                             pageLength = 500, 
                             order = list(list(1, 'desc'), list(7, 'asc')), #2 = Priority, 7 = MilesFromZip
                             columnDefs = list(list(targets = c(2,3,4,5,6,8,9,10,11,12), visible = FALSE)), #turn off visibility for subset of columns
                             highlightOptions(fillColor = 'blue', opacity=1, bringToFront = TRUE)
                           ),
                           rownames = FALSE,
                           colnames = c("<span style='color:#67DFFF'>District</span>" = 1,
                                        "<span style='color:#67DFFF'>Priority</span>" = 2,
                                        "<span style='color:#67DFFF'>Miles From Zip</span>" = 8), #color the column title text & add spaces
                           escape = FALSE,
                           caption = tags$caption(
                             tags$span(style = paste('color:',strColorSalmon, ';font-size:18px; font-weight:bold'), " Priority districts "),
                             tags$span(style = paste('color:',strColorNone, ';font-size:16px'), " within "),
                             tags$span(style = paste('color:',strColorSalmon, ';font-size:18px; font-weight:bold'), as.character(v$userMiles)),
                             tags$span(style = paste('color:',strColorNone, ';font-size:16px'), "miles of you: ")
                           )
        ) %>% 
          formatStyle(
            "<span style='color:#67DFFF'>District</span>", fontWeight = 'bold', width = '10%'
          ) %>% 
          formatStyle(
            "<span style='color:#67DFFF'>Priority</span>", fontWeight = 'bold', width = '10%', color = 'white',
            backgroundColor =  styleEqual(c('HIGH', 'HIGHER', 'HIGHEST'), c(strColorHigh, strColorHigher, strColorHighest))
          ) %>%
          formatStyle(
            "<span style='color:#67DFFF'>Miles From Zip</span>", fontWeight = 'bold', width = '10%',
            backgroundColor = 'lightgrey'
          ) 

    })
    
    #---- BUILD MAP OUTPUT ----
    
    #color districts based on target class
    pal <- colorFactor(c(strColorHighest, strColorHigher,strColorHigh), domain=c('HIGH','HIGHER', 'HIGHEST'), na.color = "#FFFFFF00", reverse=TRUE)
    
    output$usmap <- renderLeaflet({
        
        df <- dfDistricts #nothing in map object itself should be dependent on user input to speed things up

        #create the map using openstreet map and set the initial view
        m <- leaflet(data = df) %>%
                        addTiles() %>% #includes the default attributions
                        addTiles(attribution = paste("| <a href=\"https://catalog.data.gov/harvest/116th-congressional-district\">District Map Data</a>",
                                                     "| <a href=\"http://clerk.house.gov/member_info/\">House Data</a>",
                                                     "| <a href=\"https://www.senate.gov/general/contact_information/senators_cfm.cfm\">Senate Data</a>",
                                                     "| <a href=\"https://www.mobilize.us\">MobilizeAmerica</a> Events",
                                                     "| Priority analysis by Jason Berlin")
                        ) %>%
                        fitBounds(-120, 25, -75, 50) %>% #contiguous 48
                        addPolygons(layerId=~GEOID, #layerId is returned during a click event
                                    
                                    #set style of polygons
                                    fillColor = ~pal(PRIORITY),#~pal(TARGETCLASS),
                                    fillOpacity = 0.3,
                                    stroke = TRUE, 
                                    weight=2,
        
                                    #highlight districts upon mouseover
                                    highlight = highlightOptions(
                                        weight = 4
                                    ),  
                                    
                                    #add HTML formatted label info upon mouseover
                                    label = lapply(df$LABEL, HTML),
                                    labelOptions = labelOptions(
                                        style = list("font-weight" = "normal", padding = "3px 8px"),
                                        textsize = "15px",
                                        direction = "auto")
                                    ) %>%
          
                        #add battleground states
                        addPolylines(data = mpBattlegroundStates,
                                    stroke = TRUE, 
                                    weight = 5,        #heavy (rest of map has lighter lines)
                                    color = 'black',   #black (rest of map is blue)
                                    opacity = 1.0      #opaque (rest of map is <1.0)
                                    ) %>%
          
                        #add event markers (load all of them at once)
                        addMarkers(layerId = dfMobilizeEvents$ID, #layerId is returned during a click event
                                   lng = dfMobilizeEvents$LONGITUDE, 
                                   lat = dfMobilizeEvents$LATITUDE, 
                                   group = 'event', 
                                   icon = fieldTeam6Icon,
                                   label = lapply(dfMobilizeEvents$LABEL, HTML) #add HTML formatted label info upon mouseover
                                   ) %>% 
                        addLegendCustom(colors = c("white", "white"), 
                                        labels = c("Congressional District", "FT6 Battleground State"), 
                                        sizes = c(15, 15), 
                                        shapes = c("square", "square"), 
                                        borders = c("blue", "black"),
                                        fontsize = '11px',
                                        position = "bottomright",
                                        opacity = 1.0)  %>%
                        addControl(html = paste("<img src='" , strIconUrl, "'; width=20px;style='border:10px solid black'>  FT6 Event"), 
                                   position = "bottomright")  %>%
                        addLegend(pal = pal, 
                                  values = c('HIGHEST','HIGHER','HIGH' ), 
                                  title = "PRIORITY", 
                                  opacity = 0.3, 
                                  position = "bottomright")
       
    })
    
    #field team 6 icon -- normal
    fieldTeam6Icon <- makeIcon(
      'fieldteam6_icon.png',
      iconWidth = 30, iconHeight = 30
    )
    
    #field team 6 icon -- selected
    fieldTeam6IconBright <- makeIcon(
      'fieldteam6_icon_bright.png',
      iconWidth = 30, iconHeight = 30
    )
    
    #add a marker to represent the user's location when they specify a zipcode
    observeEvent({input$zipcode}, {
        leafletProxy("usmap") %>%
            clearGroup(group='zipcode')
        
        if (v$validZipcode == 1) {
            zipcodeInfo <- subset(zipcode,zip==input$zipcode) #get info from database
            userLatLong = cbind(as.numeric(zipcodeInfo['latitude']), as.numeric(zipcodeInfo['longitude']))
            leafletProxy("usmap") %>% 
                addMarkers(lng = userLatLong[2], lat = userLatLong[1], 
                           label = paste('zipcode ',input$zipcode),
                           group = 'zipcode',
                           labelOptions = labelOptions(
                               style = list("font-weight" = "bold", padding = "3px 8px", color = strColorDemocrat),
                               textsize = "13px",
                               direction = "auto"))
        } 
        
    })
    
    
    #change map zoom based on zipcode and miles specified without reloading the whole map
    observeEvent({input$miles
                  input$zipcode}, {
        #if user has hit the rest button or if it is the beginning of a session      
        if (v$validZipcode == 0) {
            leafletProxy("usmap") %>% 
                fitBounds(-120, 25, -75, 50) #contiguous 48
        #if user has chosen a zipcode and a radius    
        } else {
            #redefine map bounds
            districtPolygonsInRadius <- subset(dfDistrictsUser(), as.numeric(USERDIST) <= v$userMiles)
            longitudes = as.numeric(levels(districtPolygonsInRadius$INTPTLON)[districtPolygonsInRadius$INTPTLON])
            latitudes = as.numeric(levels(districtPolygonsInRadius$INTPTLAT)[districtPolygonsInRadius$INTPTLAT])
            minLng <- min(longitudes[longitudes<0]) # there are some rogue weird ones in there possible from type conversion or non-states
            maxLng <- max(longitudes[longitudes<0])
            minLat <- min(latitudes[latitudes>0])
            maxLat <- max(latitudes[latitudes>0])
            leafletProxy("usmap") %>% fitBounds(minLng, minLat, maxLng, maxLat)
        }
             
    })
    
    #---- BUILD SIDEBAR DISTRICT INFO ----
    
    #create district+priority to be displayed on sidebar when user clicks on a district
    output$clickedDistrictInfoHeader <- renderPrint({ 
      shiny::validate(
        need(length(v$clickedGeoIdNew) > 0, '')
      )
      
      #grab geoid of current polygon selection
      geoid <- v$clickedGeoIdNew
      #save district info in temp dataframe based on user click
      df <- dfDistricts[dfDistricts@data$GEOID==geoid, ]
      
      #set color of candidate based on party (i.e. do we want to boot or protect)
      strHouseColor <- if(grepl('BOOT', df$MISSION_HOUSE)) { strColorRepublican } else { strColorDemocrat }
      strSenateColor <- if(grepl('BOOT', df$MISSION_SENATE)) { strColorRepublican } else { strColorDemocrat }
      strPresidencyColor <- strColorRepublican
      
      #we want the democratic candidates to win the special elections
      strHouseColor <- if(grepl('Special Election', df$MISSION_HOUSE)) { strColorDemocrat } else { strHouseColor }
      
      
      #set priority color based on priority class of district
      strPriorityColor <- switch(as.character((df$PRIORITY)),
                                 'HIGHEST' = strColorHighest,
                                 'HIGHER' = strColorHigher,
                                 'HIGH' = strColorHigh)
      strPriorityColor <- if(df$PRIORITY=='') { strColorNone } else { strPriorityColor }
      
      #set target class description (text displayed under mission if detailed view)
      strFriendlyClassDesc = strClassDescriptionsFriendly[as.numeric(df$TARGETCLASS)]
      
      #set background color
      strBackgroundStyle = paste(';background-color:', strPriorityColor)
      
      #display boxed district name and priority tag in the appropriate priority color
      tags$div(class="header", checked=NA,
               list(
                 tags$p(style='font-size: 10px', ''),
                 tags$table(style = "padding: 25%; width: 100%",
                            #1st and only row in this "table"
                            tags$tr(style='align:center',
                                    #column 1 (district)
                                    tags$td(style=paste('width:65%; align:center; padding:0.2em; border:4px; border-style:solid; border-color:', strPriorityColor),
                                            tags$strong(style='font-size: 30px; padding:0.5em', as.character(df$DISTRICT))),
                                    #column 2 (list of priorities)
                                    tags$td(style='width:35%;align:center',
                                            tags$table(style='padding:5%',
                                                       #row 1 of 4 (highest)
                                                       tags$tr(
                                                         tags$td(
                                                           style=paste('width:50%;align:center;padding:0.2em', if(strColorHighest==strPriorityColor) {strBackgroundStyle}), 
                                                           tags$strong(style=paste('font-size:12px;color:', if(strColorHighest==strPriorityColor)  {'white'} else {strColorHighest}), 'HIGHEST PRIORITY')
                                                         )
                                                       ),
                                                       #row 2 of 4 (higher)
                                                       tags$tr(
                                                         tags$td(
                                                           style=paste('width:50%;align:center;padding:0.2em', if(strColorHigher==strPriorityColor) {strBackgroundStyle}), 
                                                           tags$strong(style=paste('font-size:12px;color:', if(strColorHigher==strPriorityColor)  {'white'} else {strColorHigher}), 'HIGHER PRIORITY')
                                                         )
                                                       ),
                                                       #row 3 of 4 (high)
                                                       tags$tr(
                                                         tags$td(
                                                           style=paste('width:50%;align:center;padding:0.2em', if(strColorHigh==strPriorityColor) {strBackgroundStyle}), 
                                                           tags$strong(style=paste('font-size:12px;color:', if(strColorHigh==strPriorityColor)  {'white'} else {strColorHigh}), 'HIGH PRIORITY')
                                                         )
                                                       ),
                                                       #row 4 of 4 (not prioritized)
                                                       tags$tr(
                                                         tags$td(
                                                           style=paste('width:50%;align:center;padding:0.2em', if(strColorNone==strPriorityColor) {strBackgroundStyle}), 
                                                           tags$strong(style=paste('font-size:12px;color:', if(strColorNone==strPriorityColor)  {'white'} else {strColorNone}), 'NOT PRIORITIZED')
                                                         )
                                                       )
                                            )
                                    )
                                    
                            )
                            
                 )
                 
               ) #end of list in div tag
               
      ) #end of sidebar html
      
    })
    
    #create district mission table to be displayed on sidebar when user clicks on a district
    output$clickedDistrictInfoMission <- renderPrint({ #renderPrint
        shiny::validate(
            need(length(v$clickedGeoIdNew) > 0, '')
        )

        #grab geoid of current polygon selection
        geoid <- v$clickedGeoIdNew
        
        #save district info in temp dataframe based on user click
        df <- dfDistricts[dfDistricts@data$GEOID==geoid, ]
        
        #set color of candidate based on party (i.e. do we want to boot or protect)
        strHouseColor <- if(grepl('BOOT', df$MISSION_HOUSE)) { strColorRepublican } else { strColorDemocrat }
        strSenateColor <- if(grepl('BOOT', df$MISSION_SENATE)) { strColorRepublican } else { strColorDemocrat }
        strPresidencyColor <- strColorRepublican
        
        #we want the democratic candidates to win the special elections
        strHouseColor <- if(grepl('Special Election', df$MISSION_HOUSE)) { strColorDemocrat } else { strHouseColor }
        
        #set priority color based on priority class of district
        strPriorityColor <- switch(as.character((df$PRIORITY)),
                                   'HIGHEST' = strColorHighest,
                                   'HIGHER' = strColorHigher,
                                   'HIGH' = strColorHigh)
        strPriorityColor <- if(df$PRIORITY=='') { strColorNone } else { strPriorityColor }

        #set target class description (text displayed under mission if detailed view)
        strFriendlyClassDesc = strClassDescriptionsFriendly[as.numeric(df$TARGETCLASS)]
        
        #strBorderStyle = paste('; border:2px; border-style:solid; border-color:' , strPriorityColor, '; padding: 0.3em; background:white')
        strBackgroundStyle = paste(';background-color:', strPriorityColor)
        
        #define action words based on whether 
        tags$div(class="header", checked=NA,
                 
                 list(
                     tags$p(style='font-size: 10px', ''),
                     
                     #description of mission
                     tags$strong(style='font-size: 14px','MISSION'),
                     tags$table(style = "padding: 25%; width: 100%; border:1px; border-style:solid; border-color:grey",
                                #row 1 of 3 (house)
                                tags$tr(
                                    tags$td(style='width: 30%; padding-left:3%; padding-right:2%; padding-top:0.3em; border:1px; border-style:solid; border-color:grey',tags$i(style='color:grey; font-style:italic', 'HOUSE ')),
                                    tags$td(style='padding-left:2%; padding-right:2%; padding-top:0.3em; border:1px; border-style:solid; border-color:grey',tags$strong(style=paste('color:', strHouseColor), df$MISSION_HOUSE))
                                ),
                                #row 2 of 3 (senate)
                                tags$tr(
                                    tags$td(style='padding-left:3%; padding-right:2%; padding-top:0.3em; border:1px; border-style:solid; border-color:grey',tags$i(style='color:grey; font-style:italic', 'SENATE ')),
                                    tags$td(style='padding-left:2%; padding-right:2%; padding-top:0.3em; border:1px; border-style:solid; border-color:grey', tags$strong(style=paste('color:', strSenateColor), df$MISSION_SENATE))
                                ),
                                #row 3 of 3 (presidency)
                                tags$tr(
                                    tags$td(style='padding-left:3%; padding-right:2%; padding-top:0.3em; border:1px; border-style:solid; border-color:grey',tags$i(style='color:grey; font-style:italic', 'PRESIDENT ')),
                                    tags$td(style='padding-left:2%; padding-right:2%; padding-top:0.3em; border:1px; border-style:solid; border-color:grey', tags$strong(style=paste('color:', strPresidencyColor), df$MISSION_PRESIDENCY))
                                )
                     )
                 ) #end of list in div tag
                 
        ) #end of sidebar html
        
    })
    
    
    #create district mission text to be displayed on sidebar when user clicks on a district
    output$clickedDistrictInfoDescription <- renderPrint({ #renderPrint
      shiny::validate(
        need(length(v$clickedGeoIdNew) > 0, '')
      )
      
      #grab geoid of current polygon selection
      geoid <- v$clickedGeoIdNew
      
      #save district info in temp dataframe based on user click
      df <- dfDistricts[dfDistricts@data$GEOID==geoid, ]
      
      #set color of candidate based on party (i.e. do we want to boot or protect)
      strHouseColor <- if(grepl('BOOT', df$MISSION_HOUSE)) { strColorRepublican } else { strColorDemocrat }
      strSenateColor <- if(grepl('BOOT', df$MISSION_SENATE)) { strColorRepublican } else { strColorDemocrat }
      strPresidencyColor <- strColorRepublican
      
      #we want the democratic candidates to win the special elections
      strHouseColor <- if(grepl('Special Election', df$MISSION_HOUSE)) { strColorDemocrat } else { strHouseColor }
      
      #set priority color based on priority class of district
      strPriorityColor <- switch(as.character((df$PRIORITY)),
                                 'HIGHEST' = strColorHighest,
                                 'HIGHER' = strColorHigher,
                                 'HIGH' = strColorHigh)
      strPriorityColor <- if(df$PRIORITY=='') { strColorNone } else { strPriorityColor }
      
      #set background color
      strBackgroundStyle = paste(';background-color:', strPriorityColor)
      
      #set target class description (text displayed under mission if detailed view)
      strFriendlyClassDesc = strClassDescriptionsFriendly[as.numeric(df$TARGETCLASS)]
      
      #define action words based on whether 
      tags$div(class="header", checked=NA,
               list(
                 #description of priority or target class
                 tags$br() %>%
                   tags$strong(strFriendlyClassDesc) 
               ) #end of list in div tag
      ) #end of sidebar html
      
    })
    
    #---- RENDER DISTRICT SELECTION FUNCTIONS ----
    
    #highlight selected district on map (when clicking a district for the first time, switching districts, or reseting the map)
    renderDistrictSelectionOnMap <- function(){

        #if a polygon had already been selected, reset it to the normal state
        if(length(v$clickedGeoIdPrev)>0) {
            removeGeoid <- v$clickedGeoIdPrev #previously selected geoid
            df <- dfDistricts[dfDistricts@data$GEOID==removeGeoid, ] #dataframe for the previously selected geoid
            
            #reset polygon and polylines to have same formatting as the rest of the map
            #NOTE: there can only ever be one polygon for a given layerId, so this technically redraws/overwrites the polygon 
            leafletProxy( mapId = "usmap" ) %>%
                addPolygons(data = df, 
                            layerId = removeGeoid,
                            
                            #set style of polygons
                            fillColor = ~pal(PRIORITY),
                            fillOpacity = 0.3,
                            stroke = TRUE, 
                            weight=2,
                            
                            #highlight districts upon mouseover
                            highlight = highlightOptions(
                                weight = 4
                            ),  
                             
                            #add HTML formatted label info when mouseover
                            label = lapply(list(df$LABEL), HTML),
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto"
                            )
                            #let it use the default style just like original map
                            ) %>%
              #add the battleground states back onto the map, otherwise the blue district lines will redraw over the black state lines if they share a border
              addPolylines(data = mpBattlegroundStates,
                           stroke = TRUE, 
                           weight = 5,        #heavy (rest of map has lighter lines)
                           color = 'black',   #black (rest of map is blue)
                           opacity = 1.0      #opaque (rest of map is <1.0)
              )
        }
        #highlight the newly selected polygon
        if( is.null( v$clickedGeoIdNew ) ){ 
            req( v$clickedGeoIdNew )
        } else {
            addGeoid <- v$clickedGeoIdNew
            df <- dfDistricts[dfDistricts@data$GEOID==addGeoid, ] #clicked district

            #draw the polygon with a thicker black border
            #NOTE: there can only ever be one polygon for a given layerId, so this technically redraws/overwrites the entire polygon 
            leafletProxy( mapId = "usmap" ) %>%
                addPolygons(data = df, 
                            layerId = addGeoid,
                            
                            #set style of polygons
                            fillColor = ~pal(PRIORITY), #~pal(TARGETCLASS),
                            fillOpacity = 0.3, 
                            stroke = TRUE, 
                            weight = 4,        #heavy (rest of map has lighter lines)
                            opacity = 1.0,     #opaque but keep as blue default (rest of map is <1.0)
                            
                            #no need to highlight upon mouseover while selected
                            
                            #add HTML formatted label info when mouseover
                            label = lapply(list(df@data$LABEL), HTML),
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto"
                            )
                )
              
        }
    } #end of renderDistrictSelectionOnMap
    
    #create proxy datatable so we can update without reloading the datatable
    districtTableProxy <- DT::dataTableProxy('districts_datatable')

    #highlight selected district, if possible, in table
    renderDistrictSelectionInTable <- function(){
        
        #table will only be displayed if user has entered a zipcode
        shiny::validate(
            need(v$validZipcode == 1, "")
        )
        
        #if a new geoid has been selected on the map, check if it is in the current display of table rows
        if(length(v$clickedGeoIdNew)>0) {
            #get geoid that is being displayed on the map
            geoid <- v$clickedGeoIdNew
            
            #in order to select a row in the datatable display, you need the row number specific to the displayed data
            #indexing into the dataframe to return the matching row will redefine the row number, thus discarding the value required for selection
            #to get around this, make a copy of the dataframe, save the row number as a new column, then index
            labeledTableInfo <- v$dfDistrictsDatatableFiltered
            labeledTableInfo$ROWNUMBER <- row(v$dfDistrictsDatatableFiltered)[,1]
            clickedRow <- labeledTableInfo[labeledTableInfo$GEOID==geoid, ]
            rowNumber <- clickedRow$ROWNUMBER
            #if (length(rowNumber) == 0) print('That district is not within the range you specified ') #TESTING

            districtTableProxy %>% DT::selectRows(as.numeric(rowNumber))
            
            }
        
    } #end of renderDistrictSelectionInTable
    
    #---- RENDER EVENT SELECTION FUNCTIONS ----
    
    #highlight selected district on map (when clicking a event for the first time, switching events, or reseting the map)
    renderEventSelectionOnMap <- function(){

        #if a polygon had already been selected, reset it to the normal state
        if(length(v$clickedEventIdPrev)>0) {
            removeEventId <- v$clickedEventIdPrev #previously selected event id
            df <- dfMobilizeEvents[dfMobilizeEvents@data$ID==removeEventId, ] #dataframe for the previously selected event id
            
            #reset polygon to have same formatting as the rest of the map
            #NOTE: there can only ever be one polygon for a given layerId, so this technically redraws/overwrites the polygon 
            leafletProxy( mapId = "usmap" ) %>%
                
                addMarkers(data = df,
                           layerId = removeEventId, #layerId is returned during a click event
                           lng = df$LONGITUDE, 
                           lat = df$LATITUDE, 
                           group='event', 
                           icon=fieldTeam6Icon,
                           label = lapply(df$LABEL, HTML) #add HTML formatted label info upon mouseover
                           #let it use the default style just like original map
                           ) 
        }
        #highlight the newly selected polygon
        if( is.null( v$clickedEventIdNew ) || length(v$clickedEventIdNew) == 0 ){ 
            req( v$clickedEventIdNew )
        } else {
            addEventId <- v$clickedEventIdNew
            df <- dfMobilizeEvents[dfMobilizeEvents@data$ID==addEventId, ] #clicked district

            #add a bright highlight around the icon
            #NOTE: there can only ever be one polygon for a given layerId, so this technically redraws/overwrites the entire polygon 
            leafletProxy( mapId = "usmap" ) %>%
              addMarkers(data = df,
                           layerId = addEventId, #layerId is returned during a click event
                           lng = df$LONGITUDE, 
                           lat = df$LATITUDE,  
                           group='event', 
                           icon=fieldTeam6IconBright,
                           label = lapply(df$LABEL, HTML) #add HTML formatted label info upon mouseover
                           #let it use the default style just like original map
                           )   
              
              
        }
    } #end of renderEventSelectionOnMap
    
    #create proxy datatable so we can update without reloading the datatable
    eventTableProxy <- DT::dataTableProxy('events_datatable')

    #highlight selected district, if possible, in table
    renderEventSelectionInTable <- function(){
        
        #table will only be displayed if user has entered a zipcode
        shiny::validate(
            need(v$validZipcode == 1, "")
        )
        
        #if a new geoid has been selected on the map, check if it is in the current display of table rows
        if(length(v$clickedEventIdNew)>0) {
            #get eventid that is being displayed on the map
            eventId <- v$clickedEventIdNew
            
            #in order to select a row in the datatable display, you need the row number specific to the displayed data
            #indexing into the dataframe to return the matching row will redefine the row number, thus discarding the value required for selection
            #to get around this, make a copy of the dataframe, save the row number as a new column, then index
            labeledTableInfo <- v$dfMobilizeEventsDatatableFiltered
            labeledTableInfo$ROWNUMBER <- row(v$dfMobilizeEventsDatatableFiltered)[,1]
            clickedRow <- labeledTableInfo[labeledTableInfo$ID==eventId, ]
            rowNumber <- clickedRow$ROWNUMBER
            #if (length(rowNumber) == 0) print('That district is not within the range you specified ') #TESTING

            eventTableProxy %>% DT::selectRows(as.numeric(rowNumber)) #%>%
            
            }
        
    } #end of renderEventSelectionInTable
    
    
    #function to build a custom legend
    #added location but otherwise copied directly from
    #https://stackoverflow.com/questions/52812238/custom-legend-with-r-leaflet-circles-and-squares-in-same-plot-legends
    addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, fontsize = '14px', position = 'bottomright', opacity = 0.5){
      
      make_shapes <- function(colors, sizes, borders, shapes) {
        shapes <- gsub("circle", "50%", shapes)
        shapes <- gsub("square", "0%", shapes)
        paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
      }
      make_labels <- function(sizes, labels) {
        paste0("<div style='display: inline-block;height: ", 
               sizes, "px;margin-top: 4px;line-height: ", 
               sizes, "px;font-size: ",
               fontsize, "'>", labels, "</div>")
      }
      
      legend_colors <- make_shapes(colors, sizes, borders, shapes)
      legend_labels <- make_labels(sizes, labels)
      
      return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity, position = position))
    }



}) #end of shinyServer


