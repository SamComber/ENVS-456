library(shiny)
library(leaflet)
library(RColorBrewer)
library(httr)
library(jsonlite)
library(DT)

# dates = c("2015-12", "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06", "2016-07", "2016-08", "2016-09", "2016-10", "2016-11")
dates = c("2016-11")

for (month in dates) {
  url = sprintf("https://data.police.uk/api/crimes-street/all-crime?poly=53.5803,-2.6882:53.5803,-2.6882:53.2307,-3.2389:53.2307,-3.2389&date=%s", month)
  r <- GET(url)
  json <- content(r, "text")
  
  document <- fromJSON(txt=json)
}

r <- GET("https://data.police.uk/api/crimes-street/all-crime?poly=53.5803,-2.6882:53.5803,-3.2389:53.2307,-2.6882:53.2307,-3.2389&date=2016-11")
json <- content(r, "text")
document <- fromJSON(txt=json)

# intersection for liverpool geojson

server <- function(input, output, session) {
  
  # ---------- DATA CLEANING -----------
  
  # remove hyphenation for cleaner legend + popup
  document$category <- gsub('-', ' ', document$category)
  
  # Capitalise each word in string for aesthetical plot
  document$category <- paste(toupper(substring(document[,c("category")], 1, 1)), substring(document[,c("category")], 2), sep="")
  
  # ---------- DATA PROCESSING ----------
  
  pal <- colorFactor(
    palette = "YlGnBu",
    domain = document$category
  )
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% addCircles(lng = as.numeric(document$location$longitude), lat = as.numeric(document$location$latitude), color = pal(document$category), weight =1, 
                                            popup =   paste("<b>Crime:</b> ", document$category, "<br>",
                                                            "<b>Date:</b> <i>", document$month, "</i><br>",
                                                            "<b>Location:</b> ", document$location$street$name, "<br>", 
                                                            "<b>Outcome:</b> ", document$outcome_status$category, "<br>")) %>% addLegend("bottomleft", values = document$category, pal = pal)
  })
  


  output$crimescatter <- renderPlot({
    counts <- table(document$category)
    barplot(counts, horiz=TRUE, las=1)
  })
  
  output$crimetable <- DT::renderDataTable({
    
    # subset crimes table here
    
    # df <- document %>% filter()
    action <- DT::dataTableAjax(session, document)
    
    DT::datatable(document, options = list(ajax = list(url = action)), escape=FALSE)
  })
  
}