library(shiny)
library(leaflet)
library(RColorBrewer)
library(httr)
library(jsonlite)
library(DT)
library(rgdal)
library(RJSONIO)
library(downloader)
library(RCurl)
devtools::install_github("njtierney/ukpolice")
library(ukpolice)

u <- "http://statistics.data.gov.uk/boundaries/E08000012.json"
downloader::download(url = u, destfile = "/tmp/lpool.geojson")
lpool <- readOGR(dsn = "/tmp/lpool.geojson", layer = "OGRGeoJSON")
lpool <- lpool@polygons[[1]]@Polygons[[1]]@coords
lpool.co <- data.frame(lat = lpool[,2], long = lpool[,1] )

# lpool_poly <- ukp_crime_poly(lpool.co)
# 
# 
# 
# poly_df_3 <- data.frame(lat = c(53.45862, 53.40135, 53.34943,53.34461),
#                         long = c(-2.89155,-3.10132, -2.91164,-2.71371))
# 
# d1 <- ukp_crime_poly(poly_df_3, date = "2016-09")
# d2 <- ukp_crime_poly(poly_df_3, date = "2016-10")
# 
# str(d)

# get polygon bound

# sample 1000 inside lappy

curl.string <- paste0(paste0(sprintf('%s,%s',lpool[,2], lpool[,1]), collapse = ':'),'&date=2013-01')

json <- '{"poly":"52.268,0.543:52.794,0.238:52.130,0.478","date":"2013-01"}'

json <- paste0('{"poly":"',curl.string,'","date":"2016-10"}')
r <- POST("https://data.police.uk/api/crimes-street/all-crime", body = json, encode = "json")
r <- httr::POST("https://data.police.uk/api/crimes-street/all-crime", body = "{'poly':'52.268,0.543:52.794,0.238:52.130,0.478','date':'2013-01'}", encode = "json", verbose()) # the default



js <- toJSON(tmp)
isValidJSON(json, TRUE)

# dates = c("2015-12", "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06", "2016-07", "2016-08", "2016-09", "2016-10", "2016-11")
# dates = c("2016-11", "2016-10")
# 
# document <- lapply(dates, function(month) {
#   url = sprintf("https://data.police.uk/api/crimes-street/all-crime?poly=53.5803,-2.6882:53.5803,-3.2389:53.2307,-2.6882:53.2307,-3.2389&date=%s", month)
#   r <- GET(url)
#   json <- content(r, "text")
#   jsonlite::fromJSON(txt=json)
# })
# 
# 
# document[[1]][sample(nrow(document[[1]]), 1000), ]
# 
# document[sample(nrow(document), 3), ]
# 
# do.call(rbind, document)



server <- function(input, output, session) {
  
  
  url = "https://data.police.uk/api/crimes-street/all-crime?poly=53.5803,-2.6882:53.5803,-3.2389:53.2307,-2.6882:53.2307,-3.2389&date=2016-10"
  r <- GET(url)
  json <- content(r, "text")
  document <- jsonlite::fromJSON(txt=json)
  
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
  
  data <- reactive({
    filter(crimefilter %in% input$crimefilter)
  })
  
  output$map <- renderLeaflet({
    
    withProgress(message = 'Rendering Map Data',
                 detail = 'this may take a few seconds...', value = 0, {
                   for (i in 1:60) {
                     incProgress(1/60)
                     Sys.sleep(0.25)
                   }
                   
                   leaflet(data) %>% addTiles() %>% addCircles(lng = as.numeric(document$location$longitude), lat = as.numeric(document$location$latitude), color = pal(document$category), weight =1, 
                                                           popup =   paste("<b>Crime:</b> ", document$category, "<br>",
                                                                           "<b>Date:</b> <i>", document$month, "</i><br>",
                                                                           "<b>Location:</b> ", document$location$street$name, "<br>", 
                                                                           "<b>Outcome:</b> ", document$outcome_status$category, "<br>")) %>% addLegend("bottomleft", values = document$category, pal = pal)
                 })
  })
  
  output$crimescatter <- renderPlot({
    counts <- table(document$category)
    barplot(counts, horiz=TRUE, las=1)
  })
  
  
  output$crimetable <- DT::renderDataTable({
    # subset crimes table 
    new.document <- data.frame(document$category, document$location$latitude, document$location$longitude, document$location$street$name ,document$month)
    # rename columns
    names(new.document) <- c("Category", "Latitude", "Longitude", "Location", "Month")
    
    # df <- document %>% filter()
    action <- DT::dataTableAjax(session, new.document)
    
    DT::datatable(new.document, options = list(ajax = list(url = action)), escape=FALSE)
  })
  
}