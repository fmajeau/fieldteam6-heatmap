#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#SUMMARY OF DATAFRAMES
#dfDistricts --> all non-user specific information about each polygon/distric (full set of districts) *defined in global.R*
#dfDistrictsUser --> includes miles between user zipcode and each polygon (full set of districts)
#dfDistrictsDatatable --> certain columns of dfDistrictsUser, renamed for readability (full set of districts)
#v$dfDistrictsDatatableFiltered --> dfDistrictsDatatable filtered by input$miles (subset of districts)

# Define server logic required to create map
shinyServer(function(input, output, session) {
    
    #---- RESPOND TO RESET BUTTON ----
    
    observeEvent(input$reset, {
        updateTextInput(session, "zipcode", value = "")
        updateSliderInput(session, "miles", value = mileSliderMin)
        leafletProxy("usmap") %>% fitBounds(-120, 25, -75, 50) #contiguous 48
        v$clickedIdPrev <- v$clickedIdNew #set v$clickedIdPrev so it is removed from map
        v$clickedIdNew <- vector()        #clear v$clickedIdNew so no new highlighting is added
        renderSelectionOnMap()            #allow function to render the new clickedIdPrev/clickedIdNew values
        v$strEventSignupUrl <- 'https://www.mobilize.us/ft6/'
    })

    #---- DEFINE REACTIVE VALUES ----
    
    #reactive values that help with handling district selection & the datatable range
    v <- shiny::reactiveValues(clickedIdNew = vector(),  #store the geoid of the district currently selected by the user 
                               clickedIdPrev = vector(), #store the geoid of the district previously selected by the user in order to reset it to the original map formatting
                               dfDistrictsDatatableFiltered = data.frame(), #store the datatable data as filtered by the current userMiles 
                               userMiles = NULL, #store the current value of userMiles
                               strEventSignupUrl = 'https://www.mobilize.us/ft6/', #store the event signup url based on user zipcode
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
        v$clickedIdPrev <- v$clickedIdNew #save currently clicked district as the previously clicked district
        click <- input$usmap_shape_click  #grab new click info
        geoid <- click$id                 #grab geoid of new clicked district
        v$clickedIdNew <- geoid           #redefine currently clicked district 
        #print('map click:')
        #print(geoid)

        #if the table is being displayed, check whether we need to adjust the data bounds 
        if(v$validZipcode == 1) {
            g <- count(subset(v$dfDistrictsDatatableFiltered, GEOID==geoid)) #identify whether clicked geoid is in the current datatable display
            if(g == 0) {
                #districtDistances holds GEOID and miles from zip
                #use that to find new miles input range so the clicked district shows up in the table
                userMilesNew <- subset(dfDistrictsUser(),GEOID==geoid)$USERDIST
                
                #only rebuild the table if you're expanding the scope (if not already at max & if clicked district increases the range)
                if(v$userMiles < mileSliderMax & userMilesNew > v$userMiles) {
                    if(userMilesNew >= mileSliderMax) 
                        #userMilesSlider <- mileSliderMax
                        v$userMiles <- mileSliderMax
                    else
                        #userMilesSlider <- userMilesNew
                        v$userMiles <- userMilesNew
                    #v$userMiles <- userMilesNew
                    updateSliderInput(session, "miles", value = v$userMiles)
                    #update the filter of the datatable based on the expanded zoom 
                    #-- need to do this here because the normal observeEvent will not update the datatable before we need to select the
                    #   row using renderSelectionInTable() -- this is the reason the datatable data needs to be a reactive value
                    v$dfDistrictsDatatableFiltered <- subset(dfDistrictsDatatable(), as.numeric(MilesFromZip) <= v$userMiles)
                }
                
            }
            
        } #else, table should not be displaying yet
        renderSelectionOnMap()   #since the map was clicked, update the map
        renderSelectionInTable() #since the map was clicked, update the table
    })
    
    #listen to a table row click, update map to render the selected district
    observeEvent(input$datatable_rows_selected,{
        rowNumber <- input$datatable_rows_selected 
        geoid <- v$dfDistrictsDatatableFiltered[rowNumber,]$GEOID

        #if the new geoid originated from a table click, update the map
        #NOTE: if map is clicked, renderSelectionInTable will be called which alters input$datatable_rows_selected, 
        #      thereby triggering this function, in which case we do not want to redefine Prev/New
        if(!is.na(geoid)) {
            v$clickedIdPrev <- v$clickedIdNew
            v$clickedIdNew <- geoid
            renderSelectionOnMap()
        }
        renderSelectionInTable() #since the table was clicked, update the table
        
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
    
    
    #convert user zipcode to lat long
    #userLatLong <- reactive({
    #    if(v$validZipcode == 1)
    #      zipcodeInfo <- subset(zipcode,zip==input$zipcode) #get info from database
    #      userLatLong = cbind(as.numeric(zipcodeInfo['latitude']), as.numeric(zipcodeInfo['longitude']))
    #})
    
    #calculate distance between user's zipcode and each district
    dfDistrictsUser <- reactive({
      
        validate(
          need(v$validZipcode == 1, "")
        )
      
        zipcodeInfo <- subset(zipcode,zip==input$zipcode) #get info from database
        
        validate(
          need(nrow(zipcodeInfo) != 0,'')
            
        )
        userLatLong <- cbind(as.numeric(zipcodeInfo['latitude']), as.numeric(zipcodeInfo['longitude']))
      
        #use nearest neighbor search to find distance between lat/long of user and lat/long of each district centroid
        nearest <- RANN::nn2(dfDistrictCentroids[,c('INTPTLAT','INTPTLON')], userLatLong, k=nrow(dfDistrictCentroids)) 
        
        #create dataframe of districtDistrances which will eventually be merged with the districts dataframe
        distances = round(nearest$nn.dists[1,] * milesPerDegree, 0) #convert from degrees to miles
        geoids = list(as.character(lsDistrictGeoids[nearest$nn.idx,]))[[1]]
        districtDistances <- data.frame(GEOID=geoids, USERDIST=as.numeric(distances))

        #print(districtDistances[districtDistances$GEOID=="0608"])
        
        #if a user's district is large, it is possible the centroid of their district is too far from their zipcode for the district 
        #to be included in even the smallest specified range. set the user's district to be 0 miles away to ensure it is always included. 
        userLocation <- data.frame(Longitude = c(userLatLong[1,2]), Latitude =c(userLatLong[1,1]) )
        #userLocation <- data.frame(Longitude = c(2.220551), Latitude =c(48.809509) ) #TESTING
        coordinates(userLocation) <- ~ Longitude + Latitude
        dfDistrictsTemp <- dfDistricts
        
        #use `over` function to find the district that includes the user's zipcode lat/long
        # -- function requires both inputs to have the same coordinate frame (dfDistricts already set as ESPG:4326 i.e. WGS 84)
        # -- returns row of NAs if no district is found
        #proj4string(userLocation) <- CRS("+init=EPSG:4326")    #DOESN'T WORK
        #proj4string(dfDistrictsTemp) <- CRS("+init=EPSG:4326") #DOESN'T WORK
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
        validate(
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
        
        #only display target districts
        dfDistrictsDatatable <- subset(dfDistrictsDatatable, Class < 13)
        
    })

    #restrict datatable based on the user's selected mile range whenever new user inputs are set
    observeEvent({input$zipcode
                    input$miles} , {
            
            if(v$validZipcode == 1) {
              v$dfDistrictsDatatableFiltered <- subset(dfDistrictsDatatable(), as.numeric(MilesFromZip) <= v$userMiles)
              renderSelectionInTable() 
              #if the user adjusts the mileage range, the table will redraw itself to display the correct entries
              #so we need to select the district in the table
            }
              
    })
    
    #---- BUILD DATATABLE OUTPUT ----
    
    #use datatable dataframe to build the datatable output 
    #NOTE: friendly/detailed views originate from the same data, only difference in rendering is the number of columns displayed
    output$datatable <- DT::renderDataTable({
        validate(
            need(length(v$dfDistrictsDatatableFiltered) != 0, ""),
            need(v$validZipcode == 1, "")
        )
        df <- v$dfDistrictsDatatableFiltered 
        if(strViewingMode == 'friendly') {
            #FRIENDLY VIEW (District, Priority, MilesFromZip)
            d <- DT::datatable(df,   
                               selection = 'single', #only allow one row selection at a time
                               options = list(#lengthMenu = c(10,25,50,100,500),
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
                               caption = tags$caption(tags$h4(style = paste('color:',strColorSalmon), paste('Priority districts within ', as.character(v$userMiles), ' miles of you:')))
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
        } else {
            #DETAILED VIEW (District, Priority, Class, House, Senate1, Senate2, President, MilesFromZip)
            d <- DT::datatable(df,
                               selection = 'single', #only allow one row selection at a time
                               options = list(
                                   dom = 't', #show: table (t); exclude: page length control (l) , search box/filter (f), info summary (i), page control (p), processing display (r))
                                   pageLength = 500, 
                                   order = list(list(2, 'asc'), list(7, 'asc')), #2 = Class, 7 = MilesFromZip
                                   columnDefs = list(list(targets = c(8,9,10,11,12), visible = FALSE)),  #turn off visibility for subset of columns
                                   highlightOptions(fillColor = 'blue', opacity=1, bringToFront = TRUE)
                               ),
                               rownames = FALSE,
                               #very unideal and inelegant solution but could not find a better way 
                               colnames = c("<span style='color:#67DFFF'>District</span>" = 1,
                                            "<span style='color:#67DFFF'>Priority</span>" = 2,
                                            "<span style='color:#67DFFF'>Class</span>" = 3,
                                            "<span style='color:#67DFFF'>House Rep</span>" = 4, 
                                            "<span style='color:#67DFFF'>Senator 1</span>" = 5, 
                                            "<span style='color:#67DFFF'>Senator 2</span>" = 6,
                                            "<span style='color:#67DFFF'>President</span>" = 7,
                                            "<span style='color:#67DFFF'>Miles From Zip</span>" = 8), #add spaces to the column names
                               escape = FALSE,
                               caption = tags$caption(
                                   tags$h4(style = paste('color:',strColorSalmon), paste('Priority districts within ', as.character(v$userMiles), ' miles of you:')), 
                                   tags$i(style = 'color:silver', 'Democrats we need to protect are in '), 
                                   tags$strong(style = paste('color:', strColorDemocrat, ';background:white;padding:0.2em'), 'BLUE'), 
                                   tags$i(style = 'color:silver', ', Republicans we need to boot are in '),
                                   tags$strong(style = paste('color:', strColorRepublican, ';background:white;padding:0.2em'), 'RED'), 
                                   tags$i(style = 'color:silver', ', all others are either not at risk, not currently flippable, or not up for re-election. ')
                               )
            ) %>% 
                #bold the district 
                formatStyle(
                    "<span style='color:#67DFFF'>District</span>", fontWeight = 'bold'
                ) %>%  
                #color the priority
                formatStyle(
                    "<span style='color:#67DFFF'>Priority</span>", fontWeight = 'bold', color = 'white',
                    backgroundColor =  styleEqual(c('HIGH', 'HIGHER', 'HIGHEST'), c(strColorHigh, strColorHigher, strColorHighest))
                ) %>%
                #color the miles background
                formatStyle(
                    "<span style='color:#67DFFF'>Miles From Zip</span>", fontWeight = 'bold',
                    backgroundColor = 'lightgrey'
                ) %>%
                #color the house rep name
                formatStyle(  
                    "<span style='color:#67DFFF'>House Rep</span>", valueColumns = 'HouseColor', fontWeight = styleEqual(c('blue','red'), c('bold', 'bold')),
                    color = styleEqual(c('blue','red','grey'), c(strColorDemocrat, strColorRepublican, 'grey'))
                ) %>%
                #color the senate x rep name
                formatStyle(  
                    "<span style='color:#67DFFF'>Senator 1</span>", valueColumns = 'SenateColorX', fontWeight = styleEqual(c('blue','red'), c('bold', 'bold')),
                    color = styleEqual(c('blue','red','grey'), c(strColorDemocrat, strColorRepublican, 'grey'))
                ) %>%
                #color the senate y rep name
                formatStyle(  
                    "<span style='color:#67DFFF'>Senator 2</span>", valueColumns = 'SenateColorY', fontWeight = styleEqual(c('blue','red'), c('bold', 'bold')),
                    color = styleEqual(c('blue','red','grey'), c(strColorDemocrat, strColorRepublican, 'grey'))
                ) %>%
                #color the president name
                formatStyle(  
                    "<span style='color:#67DFFF'>President</span>", valueColumns = 'PresidencyColor', fontWeight = styleEqual(c('blue','red'), c('bold', 'bold')),
                    color = styleEqual(c('blue','red','grey'), c(strColorDemocrat, strColorRepublican, 'grey'))
                ) 
        }

        
    })
    
    #---- BUILD MAP OUTPUT ----
    
    #color districts based on target class
    #pal <- colorNumeric('YlOrRd', domain=c(1,2,3,4,5,6,10,11,12), na.color = "#FFFFFF00", reverse=TRUE)
    pal <- colorFactor(c(strColorHighest, strColorHigher,strColorHigh), domain=c('HIGH','HIGHER', 'HIGHEST'), na.color = "#FFFFFF00", reverse=TRUE)
    
    output$usmap <- renderLeaflet({
        
        #df <- dfDistrictsUser()
        df <- dfDistricts #nothing in map object itself should be dependent on user input to speed things up

        #create the map using openstreet map and set the initial view
        m <- leaflet(data = df) %>%
                        addTiles() %>% #includes the default attributions
                        addTiles(attribution = paste("| <a href=\"https://catalog.data.gov/harvest/116th-congressional-district\">District Map Data</a>",
                                                     "| <a href=\"http://clerk.house.gov/member_info/\">House Data</a>",
                                                     "| <a href=\"https://www.senate.gov/general/contact_information/senators_cfm.cfm\">Senate Data</a>",
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
                                        weight = 4,
                                        color = "black"
                                    ),  
                                    
                                    #add HTML formatted label info upon mouseover
                                    label = lapply(df$LABEL, HTML),
                                    labelOptions = labelOptions(
                                        style = list("font-weight" = "normal", padding = "3px 8px"),
                                        textsize = "15px",
                                        direction = "auto")
                                    ) %>%
                        
                        #addLegend("bottomright", pal = pal, values = c(1,2,3,4,5,6,7,8,9,10,11,12), title = "Target Class", opacity = 0.3)   
                        addLegend("bottomright", pal = pal, values = c('HIGHEST','HIGHER','HIGH' ), title = "PRIORITY", opacity = 0.3)
                        
    })
    
    #add a marker to represent the user's location when they specify a zipcode
    observeEvent({input$zipcode}, {
        leafletProxy("usmap") %>%
            clearMarkers()
        
        if (v$validZipcode == 1) {
            zipcodeInfo <- subset(zipcode,zip==input$zipcode) #get info from database
            userLatLong = cbind(as.numeric(zipcodeInfo['latitude']), as.numeric(zipcodeInfo['longitude']))
            leafletProxy("usmap") %>% 
                addMarkers(lng = userLatLong[2], lat = userLatLong[1], 
                           label = paste('zipcode ',input$zipcode),
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
    
    #adjust the event url based on zipcode, if available, otherwise maintain general link
    observeEvent({input$usmap_shape_click
                  input$zipcode}, {
        if(v$validZipcode == 0)
          v$strEventSignupUrl <- 'https://www.mobilize.us/ft6/' #general link
        else
          v$strEventSignupUrl <- paste('https://www.mobilize.us/ft6/', '?address=', input$zipcode, sep='') #user location specific link
    })
    
    #create info to be displayed on sidebar when user clicks on a district
    output$clickedDistrictInfo <- renderPrint({ #renderPrint
        #tags$p(style='font-size: 10px', '')
        validate(
            need(length(v$clickedIdNew) > 0, 'Click on any district for more info!')
        )

        #grab geoid of current polygon selection
        geoid <- v$clickedIdNew
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
        
        #set priority description (text displayed under mission if friendly view)
        #REPLACED WITH strClassDescFriendly
        ##strPriorityDesc = if (df$PRIORITY=='') strPriorityDescriptions['NOT PRIORITIZED'] else strPriorityDescriptions[as.character(df$PRIORITY)]
        
        #set target class description (text displayed under mission if detailed view)
        strClassDesc = if(df$TARGETCLASS==99) '' else paste('This class includes ', strClassDescriptions[as.numeric(df$TARGETCLASS)])
        
        #set target class description (text displayed under mission if detailed view)
        strFriendlyClassDesc = strClassDescriptionsFriendly[as.numeric(df$TARGETCLASS)]
        
        #strBorderStyle = paste('; border:2px; border-style:solid; border-color:' , strPriorityColor, '; padding: 0.3em; background:white')
        strBackgroundStyle = paste(';background-color:', strPriorityColor)
        
        #define action words based on whether 
        tags$div(class="header", checked=NA,
                 
                 list(
                     #tags$hr(),
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
                                
                     ),
                     tags$p(style='font-size: 10px', ''),
                     
                     #description of mission
                     tags$strong(style='font-size: 14px','MISSION'),
                     tags$table(style = "padding: 25%; width: 100%;",
                                #row 1 of 3 (house)
                                tags$tr(
                                    tags$td(style='padding-left:3%; padding-right:2%; padding-top:0.3em',tags$i(style='color:grey; font-style:italic', 'HOUSE ')),
                                    tags$td(style='padding-left:2%; padding-right:2%; padding-top:0.3em',tags$strong(style=paste('color:', strHouseColor), df$MISSION_HOUSE))
                                ),
                                #row 2 of 3 (senate)
                                tags$tr(
                                    tags$td(style='padding-left:3%; padding-right:2%; padding-top:0.3em',tags$i(style='color:grey; font-style:italic', 'SENATE ')),
                                    tags$td(style='padding-left:2%; padding-right:2%; padding-top:0.3em', tags$strong(style=paste('color:', strSenateColor), df$MISSION_SENATE))
                                ),
                                #row 3 of 3 (presidency)
                                tags$tr(
                                    tags$td(style='padding-left:3%; padding-right:2%; padding-top:0.3em',tags$i(style='color:grey; font-style:italic', 'PRESIDENT ')),
                                    tags$td(style='padding-left:2%; padding-right:2%; padding-top:0.3em', tags$strong(style=paste('color:', strPresidencyColor), df$MISSION_PRESIDENCY))
                                )
                     ),
                     
                     #description of priority or target class
                     if(strViewingMode == 'friendly') {
                         tags$br() %>%
                             #tags$strong(strPriorityDesc) 
                             tags$strong(strFriendlyClassDesc) 
                         
                         
                     } else {
                         #DETAILED VERSION
                         tags$hr() %>%
                             tags$table(style = "padding: 25%; width: 100%",
                                        #1st and only row
                                        tags$tr(style='align:center',
                                                #column 1 (classification)
                                                tags$td(style='width:65%; align:center; padding:0.2em; border:2px; border-style:solid; border-color:grey',
                                                        tags$strong(style='font-size: 13px; padding:0.5em', 'FIELD TEAM 6 RANKING')),
                                                #column 2 (target class)
                                                tags$td(style='width:35%;align:center',
                                                        tags$table(style='padding:5%',
                                                                   #row 1 of 1
                                                                   tags$tr(
                                                                       tags$td(style = 'width:50%;background-color:grey;padding:0.2em',
                                                                               tags$strong(style='font-size: 12px; color:white', if(df$TARGETCLASS==99) {'NONE'} else paste('CLASS', df$TARGETCLASS))
                                                                       )
                                                                   )
                                                        )
                                                )
                                        )    
                             ) %>%
                             #tags$br()  %>%
                             tags$p(style='font-size: 10px', '')  %>%
                             tags$p(strClassDesc)
                     },
                     
                     #link to event signup
                     tags$hr() ,
                     tags$a(style='font-size: 17px; font-weight:bold', href=v$strEventSignupUrl, target="_blank" , "Click here to find or host an event!")
                     
                 ) #end of list in div tag
                 
        ) #end of sidebar html
        
    })
    
    
    #---- RENDER SELECTION FUNCTIONS ----
    
    #highlight selected district on map (when clicking a district for the first time, switching districts, or reseting the map)
    renderSelectionOnMap <- function(){

        #if a polygon had already been selected, reset it to the normal state
        if(length(v$clickedIdPrev)>0) {
            removeGeoid <- v$clickedIdPrev #previously selected geoid
            df <- dfDistricts[dfDistricts@data$GEOID==removeGeoid, ] #dataframe for the previously selected geoid
            
            #reset polygon to have same formatting as the rest of the map
            #NOTE: there can only ever be one polygon for a given layerId, so this technically redraws/overwrites the polygon 
            leafletProxy( mapId = "usmap" ) %>%
                addPolygons(data = df, 
                            layerId = removeGeoid,
                            
                            #set style of polygons
                            fillColor = ~pal(PRIORITY),
                            #fillColor = ~pal(TARGETCLASS),
                            fillOpacity = 0.3,
                            stroke = TRUE, 
                            weight=2,
                            
                            #highlight districts upon mouseover
                            highlight = highlightOptions(
                                weight = 4,
                                color = "black"
                            ),  
                             
                            #add HTML formatted label info when mouseover
                            label = lapply(list(df$LABEL), HTML),
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto"
                            )
                            #let it use the default style just like original map
                            )
        }
        #highlight the newly selected polygon
        if( is.null( v$clickedIdNew ) ){ 
            req( v$clickedIdNew )
        } else {
            addGeoid <- v$clickedIdNew
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
                            color = 'black',   #black (rest of map is blue)
                            opacity = 1.0,     #opaque (rest of map is <1.0)
                            
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
    } #end of renderSelectionOnMap
    
    #create proxy datatable so we can update without reloading the datatable
    tableProxy <- DT::dataTableProxy('datatable')

    #highlight selected district, if possible, in table
    renderSelectionInTable <- function(){
        
        #table will only be displayed if user has entered a zipcode
        validate(
            need(v$validZipcode == 1, "")
        )
        
        #if a new geoid has been selected on the map, check if it is in the current display of table rows
        if(length(v$clickedIdNew)>0) {
            #get geoid that is being displayed on the map
            geoid <- v$clickedIdNew
            
            #in order to select a row in the datatable display, you need the row number specific to the displayed data
            #indexing into the dataframe to return the matching row will redefine the row number, thus discarding the value required for selection
            #to get around this, make a copy of the dataframe, save the row number as a new column, then index
            labeledTableInfo <- v$dfDistrictsDatatableFiltered
            labeledTableInfo$ROWNUMBER <- row(v$dfDistrictsDatatableFiltered)[,1]
            clickedRow <- labeledTableInfo[labeledTableInfo$GEOID==geoid, ]
            rowNumber <- clickedRow$ROWNUMBER

            if (length(rowNumber) == 0) print('That district is not within the range you specified ')

            tableProxy %>% DT::selectRows(as.numeric(rowNumber)) #%>%
            
            }
        
    } #end of renderSelectionInTable


})


