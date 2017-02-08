library(shiny)
library(leaflet)
library(RColorBrewer)

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
                          sliderInput("time", "Month Slider",
                                      min(document$month.int),
                                      max(document$month.int),
                                      pre = '2016-',
                                      value = c(min(document$month.int), max(document$month.int)),
                                      step = 1),
                          plotOutput("crimeline", height = 200),
                          p(class="text-muted", "Note: please wait for data to be pulled from the Police API")
                          )

                        ),
                    
                    fluidRow(
                      
                      column(6,
                        h4("KNN-distance plot for optimal epsilon value - k = 10"),
                        plotOutput("dbopt")
                             
                      ),
                      
                      column(6,
                        h4("DBSCAN - epsilson = 0.002, min points = 10"),
                        plotOutput("dbscan", height=400, width=400)
          
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