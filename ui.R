library(shiny)
library(leaflet)
library(RColorBrewer)

# ui <- bootstrapPage(
#   
#   tags$style(type="text/css", "html, body {width:100%; height:100%}"),
#   leafletOutput("map", width = "100%", height = "100%")
#   
# )

ui <- navbarPage("Crime Mapping in Liverpool", id="nav", 
                 
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                        
                        tags$head(
                          includeCSS("https://fonts.googleapis.com/css?family=Montserrat"),
                          includeCSS("styles.css")
                        ),

                        leafletOutput("map", width = "100%"),
                        
                        absolutePanel(id="controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 330, height = "auto",
                          
                          h2("Data controller"),
                          # sliderInput("daterange", "Date range: ", min(as.numeric(document$month)), max(as.numeric(document$month)), value = range(as.numeric(document$month))),
                          plotOutput("crimescatter", height = 400)
                        )
                    )
            ),
           
           tabPanel("Data explorer", 
                    p("The data explorer tab displays the tabulation of crime occurences in Liverpool. Search functionality allows
                      a user to filter a location by road name to count the number of crimes."),
                    DT::dataTableOutput("crimetable"),
                    hr()
                    )
           
)

