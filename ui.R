#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application
shinyUI(
    fluidPage(
        
        #display title & subtitle
        titlePanel(
            tags$div(class="p", checked=NA,
                     tags$strong(style=paste('color:',strColorAqua),"Field Team 6 "),
                     tags$strong(style=paste('color:',strColorSalmon),"HEAT MAP"),
                     tags$br(),
                     tags$i(style=paste('color:','silver', ';font-size: 16px'),"Where America Needs Democrats Most")
                     
            ) ,
            windowTitle = 'Field Team 6 HEAT MAP' #text in browser tab
        ),
        
        #set background color of entire page
        setBackgroundColor(strFieldTeam6Webpage),
        
        #plot sidebar 
        sidebarPanel(
            width = 4,
            textInput("zipcode",
                       "Enter your zipcode:"
            ),
            sliderInput("miles",
                        "Choose your travel range (miles):",
                        min = mileSliderMin,
                        max = mileSliderMax,
                        value = mileSliderMin
            ),
            tags$div(style="display:inline-block; width:100%",
                    fluidRow(
                        #column(6, actionButton("go", tags$strong(style=paste('color:', strColorDemocrat ),"Go"), style='width:100%')), #not build
                        column(12, actionButton("reset", tags$strong(style='color:silver',"Reset"), style='width:100%'))
                    )
            ),
            tags$p(""),
            htmlOutput('clickedDistrictInfo')
        ),
    
        #plot map and friendly/detailed view toggle
        mainPanel(
            width = 8,
            #prettyRadioButtons("radio", label=NULL, inline=TRUE,
            #                   choiceNames = list(tags$strong(style = 'color:silver','FRIENDLY VIEW'), tags$strong(style = 'color:silver','DETAILED VIEW')  ), 
            #                   choiceValues = list( 'friendly', 'detailed'),
            #                   selected = 'friendly',
            #                   shape = 'curve'),
            leafletOutput("usmap", height = 500) #,
            
            # #display links to data sources below map (currently display in the map attributions instead)
            # tags$div(class="p", style='font-size:9px', checked=NA,
            #          tags$a(href="https://catalog.data.gov/harvest/116th-congressional-district", "Congressional District Map Data"),
            #          tags$span(style='color:silver',"|"),
            #          tags$a(href="http://clerk.house.gov/member_info/", "House Member Data"),
            #          tags$span(style='color:silver',"|"),
            #          tags$a(href="https://www.senate.gov/general/contact_information/senators_cfm.cfm", "Senate Member Data"),
            #          tags$span(style='color:silver',"|"),
            #          tags$span(style='color:grey', "Priority analysis by Jason Berlin"),
            #          #tags$p(style='color:lightgrey', 'We apologize for bugs and appreciate your patience while this tool is still in development mode... improvements will occur regularly!')
            #          tags$br(),
            #          tags$br()
            # )
        ),
        
        #plot data table
        fluidRow(
            column(12, #table width (full width = 12)
                   dataTableOutput('datatable')
            )
        )
        # #retain connection with websocket
        # #from https://github.com/virtualstaticvoid/heroku-buildpack-r/issues/97
        # tags$head(
        #     HTML(
        #         "<script>
        #         var socket_timeout_interval
        #         var n = 0
        #         $(document).on('shiny:connected', function(event) {
        #             socket_timeout_interval = setInterval(function(){
        #                 Shiny.onInputChange('count', n++)
        #                 }, 15000)
        #             alert(event.socket.readyState)
        #         });
        #         $(document).on('shiny:disconnected', function(event) {
        #             clearInterval(socket_timeout_interval)
        #         });
        #     </script>"
        #     )
        # )
    
    ) #end of fluidPage
    
    
            
) #end of shinyUI


#HTML table display describing classes 
#(feature of very first version, not using anymore)

# tags$div(class="table", checked=NA,
#          tags$table(style = "padding: 10%; width: 100%; rules: all",    
#                     tags$tr(
#                         tags$th("Target Class"),
#                         tags$th(""),
#                         tags$th("House"),
#                         tags$th("Senate"),
#                         tags$th("President")
#                     ),
#                     tags$tr(
#                         tags$td(1),
#                         tags$td(""),
#                         tags$td('X (flip)'),
#                         tags$td('X'),
#                         tags$td('X')
#                     ),
#                     tags$tr(
#                         tags$td(2),
#                         tags$td(""),
#                         tags$td('X (hold)'),
#                         tags$td('X'),
#                         tags$td('X')
#                     ),
#                     tags$tr(
#                         tags$td(3),
#                         tags$td(""),
#                         tags$td('--'),
#                         tags$td('X'),
#                         tags$td('X')
#                     ),
#                     tags$tr(
#                         tags$td(4),
#                         tags$td(""),
#                         tags$td('X (flip)'),
#                         tags$td('--'),
#                         tags$td('X')
#                     ),
#                     tags$tr(
#                         tags$td(5),
#                         tags$td(""),
#                         tags$td('X (hold)'),
#                         tags$td('--'),
#                         tags$td('X')
#                     ),
#                     tags$tr(
#                         tags$td(6),
#                         tags$td(""),
#                         tags$td('--'),
#                         tags$td('--'),
#                         tags$td('X')
#                     ),
#                     tags$tr(
#                         tags$td(7),
#                         tags$td(""),
#                         tags$td('X (flip)'),
#                         tags$td('X'),
#                         tags$td('--')
#                     ),
#                     tags$tr(
#                         tags$td(8),
#                         tags$td(""),
#                         tags$td('X (hold)'),
#                         tags$td('X'),
#                         tags$td('--')
#                     ),
#                     tags$tr(
#                         tags$td(9),
#                         tags$td(""),
#                         tags$td('--'),
#                         tags$td('X'),
#                         tags$td('--')
#                     ),
#                     tags$tr(
#                         tags$td(10),
#                         tags$td(""),
#                         tags$td('[state leg]'),
#                         tags$td(''),
#                         tags$td('')
#                     ),
#                     tags$tr(
#                         tags$td(11),
#                         tags$td(""),
#                         tags$td('X (flip)'),
#                         tags$td('--'),
#                         tags$td('--')
#                     ),
#                     tags$tr(
#                         tags$td(12),
#                         tags$td(""),
#                         tags$td('X (hold)'),
#                         tags$td('--'),
#                         tags$td('--')
#                     ),
#                     tags$tr(
#                         tags$td(99),
#                         tags$td(""),
#                         tags$td('--'),
#                         tags$td('--'),
#                         tags$td('--')
#                     )
#          ),
#          tags$p(""),
#          tags$i("* X = seat is in play")
# )
