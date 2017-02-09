library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinythemes)
library(htmltools)
library(htmlwidgets)
library(metricsgraphics)
library(RColorBrewer)

# use shinythemes
ui <- navbarPage("Crime Mapping in Liverpool", theme = shinytheme("flatly"), id="nav", 
                 
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                        tags$head(
                          includeCSS("https://fonts.googleapis.com/css?family=Montserrat"),
                          includeCSS("www/styles.css")
                        ),

                        leafletOutput("map", width = "100%"),
                        
                        absolutePanel(id="controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 330, height = "auto",
                          
                          h2("Data controller"),
                          sliderInput("time", "Month Slider",
                                      min(1),
                                      max(12),
                                      pre = '2016-',
                                      value = 1,
                                      step = 1),
                          plotOutput("crimeline", height = 200),
                          sliderInput("samplesize", "Sample Size", min = 1000, max=10000, value = 10000, step = 1000),
                          p(class="text-muted", "Note: pulling data from the Police API may take a few minutes...")
                          )

                        ),
                    hr(),
                    fluidRow(class="graphs",
                      
                      column(4,
                        h4("KNN-distance plot for optimal epsilon value - k = 10"),
                        plotOutput("dbopt")
                      ),
                      
                      column(4,
                        h4("DBSCAN - epsilson = 0.002, min points = 10, n = 68793"),
                        plotOutput("dbscan")
                      ),
                      column(4,
                        h4("Crime category frequencies (n = 68793)"),
                        plotOutput("crimebar")
                      ),
                      hr()
                    )
                    
                    
            ),
           
           tabPanel("Data explorer", 
                    p("The data explorer tab displays the tabulation of crime occurences in Liverpool. Search functionality allows
                      a user to filter a location by road name to count the number of crimes."),
                    DT::dataTableOutput("crimetable"),
                    hr()
                    )
           
)