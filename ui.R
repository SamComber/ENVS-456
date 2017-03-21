# load dependencies
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
                 
                 # create tab panel for single-page application design
                 tabPanel("Interactive map",
                          div(class="outer",
                              # expand map dimensions to fit screen  
                              tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                              tags$style(HTML("#map {width: 100% !important;}")),
                              
                              tags$head(
                                # include custom google font
                                includeCSS("https://fonts.googleapis.com/css?family=Montserrat"),
                                # include custom css script
                                includeCSS("www/styles.css")
                              ),
                              
                              # create div element for map
                              leafletOutput("map", width = "100%"),
                              
                              # create movable 'data controller', allow it to be draggable
                              absolutePanel(id="controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            h2("Data controller"),
                                            # define month slider for exploring temporal variation in crime
                                            sliderInput("time", "Month Slider", min = 1, max = 12, value = c(1,12),
                                                        step=1, pre = "Month: "),
                                            # plot time-series of crime incidents
                                            plotOutput("crimeline", height = 200),
                                            # sample size slider
                                            sliderInput("samplesize", "Sample Size", min = 5000, max=20000,
                                                        value = 10000, step = 10000),
                                            p(class="text-muted", "Note: pulling data from the Police API may take a few minutes...")
                              )
                          ),
                          hr(),
                          # bootstrap uses a 12 column grid so we use 4 columns per plot 
                          # as 12 is divisible by 4
                          fluidRow(class="graphs",
                                   
                                   column(4,
                                          h4("KNN-distance plot for optimal epsilon value - k = 10"),
                                          plotOutput("dbopt")
                                   ),
                                   
                                   column(4,
                                          h4("DBSCAN - epsilson = 0.002, min points = 10"),
                                          plotOutput("dbscan")
                                   ),
                                   column(4,
                                          h4("Crime category frequencies"),
                                          plotOutput("crimebar")
                                   ),
                                   hr()
                          )
                 ),
                 
                 # define data explorer tab for exploring tidied data
                 tabPanel("Data explorer", 
                          p("The data explorer tab displays the tabulation of crime occurences in Liverpool.
                            Search functionality allows a user to filter a location by road name to count the
                            number of crimes."),
                          # render data table in ui
                          DT::dataTableOutput("crimetable"),
                          hr()
                          ),
                 # define about section to explain purpose of project
                 tabPanel("About",
                          p("This shiny app is the product of a class project for ENVS456 - Web Mapping and Analysis
                            designed by Sam Comber. Cluster markers provided by the Leaflet library are used to lower
                            the overhead of plotting 45,000+ individual crime points. By zooming into or clicking
                            each cluster, the user is able to visualise the geography of crime density across
                            Liverpool in their viewport. Crime clusters can be expanded until their constituent
                            crimes are shown individually (clicking individual crimes shows a popup detailing
                            descriptive information). As there is no server-side backend for storing crimes, each
                            page refresh requires a new HTTP request to the Police API which significantly enhances
                            the client-side loadtime of the app. For this reason, we use a single-page application (SPA)
                            design that uses AJAX to eliminate the need for page refreshes. The data explorer tab
                            tabulates the tidied dataset, allowing the user to search by location, outcome
                            status and more."),
                          p("Moreover, by using the interactive sliders, the user is able to observe the spatio-temporal
                            variation in crime density. Note: the sample size slider does not change the number of
                            crime points visualised on the map. The Github repository for the project can be found
                            ", a("here", href="https://github.com/SamComber/ENVS-456"), " and the R markdown file
                            that explains the design process ",
                            a("here.", href="https://github.com/SamComber/ENVS-456/blob/master/webmap-md.html"))
                          )
                 )