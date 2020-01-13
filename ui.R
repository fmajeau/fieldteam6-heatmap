#
# This is the user-interface definition of a Shiny web application. You can run the application by clicking 'Run App' above.
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/
#
# Written by Fiona Majeau for Field Team 6
#


# Define UI for application
shinyUI(
    fluidPage(
        
        #display title & subtitle
        titlePanel(
            tags$div(class="p", checked=NA,
                     tags$strong(style=paste('color:',strColorAqua),"Field Team 6 "),
                     tags$strong(style=paste('color:',strColorSalmon),"HEAT MAP"),
                     tags$span(lapply('&nbsp;',HTML)),
                     tags$i(style=paste('color:','silver', ';font-size: 16px'),"Where America Needs Democrats Most")
                     
            ) ,
            windowTitle = 'Field Team 6 HEAT MAP' #text in browser tab
        ),
        
        #set background color of entire page
        setBackgroundColor(strFieldTeam6Webpage),
        
        #plot header 
        sidebarPanel(
            width = 12,
            #display user input fields
            tags$div(style="display:inline-block; width:100%; color:grey",
                     fluidRow(
                         #zipcode user input
                         column(3,
                                textInput("zipcode", 
                                   lapply(paste("<span style='color:black'>1. </span>",
                                                "<span style='font-weight:normal'>Enter your </span>",
                                                "<span style='color:", strFieldTeam6Webpage, ";font-weight:bold'>zipcode</span>"
                                                ), 
                                          HTML)
                                   )
                                
                                ),
                         #travel range user input
                         column(4,
                                sliderInput("miles",
                                     lapply(paste("<span style='color:black'>2. </span>",
                                                  "<span style='font-weight:normal'>Choose your </span>",
                                                  "<span style='color:", strFieldTeam6Webpage, ";font-weight:bold'>travel range</span> (miles):"
                                                  ),
                                            HTML),
                                     min = mileSliderMin,
                                     max = mileSliderMax,
                                     value = mileSliderMin,
                                     ticks = FALSE
                                     )
                                ),
                         #user instructions
                         column(3,
                                lapply(paste("<span style='color:black;font-weight:bold'>3. </span>",
                                             "Scroll down to find a list of <span style='color:",
                                             strFieldTeam6Webpage, 
                                             ";font-weight:bold'>nearby events</span>, or click on any district to <span style='color:",
                                             strFieldTeam6Webpage, 
                                             ";font-weight:bold'>learn more about the elections</span> FT6 is targeting."
                                             ), HTML
                                       )
                                
                                ),
                         #user reset button
                         column(2,
                                tags$br(),
                                actionButton("reset", tags$strong(style='color:silver',"Reset"), style='width:100%')
                                )
                         
                     )
            ),
            #display district info header subsection
            fluidRow(
                column(6, 
                       htmlOutput('clickedDistrictInfoHeader')
                ),
                column(6,
                       htmlOutput('clickedDistrictInfoMission')
                )
            ),
            htmlOutput('clickedDistrictInfoDescription')
        ),
    
        #plot map
        mainPanel(
            width = 12,
            leafletOutput("usmap", height = 450)
        ),
        
        #display events table
        fluidRow(
            column(12, #table width (full width = 12)
                   dataTableOutput('events_datatable')
            )
        ),
        
        #display districts data table
        fluidRow(
            column(12, #table width (full width = 12)
                   dataTableOutput('districts_datatable')
            )
        )
    
    ) #end of fluidPage
    
    
            
) #end of shinyUI
