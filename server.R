library(shiny)
library(leaflet)
library(RColorBrewer)
library(httr)
library(jsonlite)
library(DT)
library(rgdal)
library(RJSONIO)
library(downloader)
library(plyr)
library(ggplot2)
library(scales)
library(dbscan)
library(fpc)

server <- function(input, output, session) {

  # download geojson
  u <- "http://statistics.data.gov.uk/boundaries/E08000012.json"
  downloader::download(url = u, destfile = "/tmp/lpool.geojson")
  lpool <- readOGR(dsn = "/tmp/lpool.geojson", layer = "OGRGeoJSON")
  # access coords slot
  lpool <- lpool@polygons[[1]]@Polygons[[1]]@coords
  # build lat/lon + date string to send with postrequest
  curl.string <- paste0('poly=',paste0(sprintf('%s,%s',lpool[,2], lpool[,1]), collapse = ':'))

  dates = c("2015-12", "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06", "2016-07", "2016-08", "2016-09", "2016-10", "2016-11")
  # dates = c("2015-12", "2016-01")

  document <- lapply(dates, function(month) {
    # format acceptable packet for http request
    curl.string <- list(poly=c(curl.string), date=c(month))
    # post custom polygon to police api (note: post needed as appending curl string to url is too long)
    r <- httr::POST("https://data.police.uk/api/crimes-street/all-crime", body = curl.string, encode="multipart", verbose())
    json <- content(r, "text")
    jsonlite::fromJSON(txt=json)
  })
  
  master <- data.frame(id=numeric(0), category=character(0), lat=character(0), lon=character(0), month=character(0), outcome_status=character(0))
  d1 <- data.frame(category=document[[1]]$category,lat=document[[1]]$location$latitude, lon=document[[1]]$location$longitude, id=document[[1]]$id, name=document[[1]]$location$street$name, month=document[[1]]$month, outcome_status=document[[1]]$outcome_status$category)
  d2 <- data.frame(category=document[[2]]$category,lat=document[[2]]$location$latitude, lon=document[[2]]$location$longitude, id=document[[2]]$id, name=document[[2]]$location$street$name, month=document[[2]]$month, outcome_status=document[[2]]$outcome_status$category)
  d3 <- data.frame(category=document[[3]]$category,lat=document[[3]]$location$latitude, lon=document[[3]]$location$longitude, id=document[[3]]$id, name=document[[3]]$location$street$name, month=document[[3]]$month, outcome_status=document[[3]]$outcome_status$category)
  d4 <- data.frame(category=document[[4]]$category,lat=document[[4]]$location$latitude, lon=document[[4]]$location$longitude, id=document[[4]]$id, name=document[[4]]$location$street$name, month=document[[4]]$month, outcome_status=document[[4]]$outcome_status$category)
  d5 <- data.frame(category=document[[5]]$category,lat=document[[5]]$location$latitude, lon=document[[5]]$location$longitude, id=document[[5]]$id, name=document[[5]]$location$street$name, month=document[[5]]$month, outcome_status=document[[5]]$outcome_status$category)
  d6 <- data.frame(category=document[[6]]$category,lat=document[[6]]$location$latitude, lon=document[[6]]$location$longitude, id=document[[6]]$id, name=document[[6]]$location$street$name, month=document[[6]]$month, outcome_status=document[[6]]$outcome_status$category)
  d7 <- data.frame(category=document[[7]]$category,lat=document[[7]]$location$latitude, lon=document[[7]]$location$longitude, id=document[[7]]$id, name=document[[7]]$location$street$name, month=document[[7]]$month, outcome_status=document[[7]]$outcome_status$category)
  d8 <- data.frame(category=document[[8]]$category,lat=document[[8]]$location$latitude, lon=document[[8]]$location$longitude, id=document[[8]]$id, name=document[[8]]$location$street$name, month=document[[8]]$month, outcome_status=document[[8]]$outcome_status$category)
  d9 <- data.frame(category=document[[9]]$category,lat=document[[9]]$location$latitude, lon=document[[9]]$location$longitude, id=document[[9]]$id, name=document[[9]]$location$street$name, month=document[[9]]$month, outcome_status=document[[9]]$outcome_status$category)
  d10 <- data.frame(category=document[[10]]$category,lat=document[[10]]$location$latitude, lon=document[[10]]$location$longitude, id=document[[10]]$id, name=document[[10]]$location$street$name, month=document[[10]]$month, outcome_status=document[[10]]$outcome_status$category)
  d11 <- data.frame(category=document[[11]]$category,lat=document[[11]]$location$latitude, lon=document[[11]]$location$longitude, id=document[[11]]$id, name=document[[11]]$location$street$name, month=document[[11]]$month, outcome_status=document[[11]]$outcome_status$category)
  d12 <- data.frame(category=document[[12]]$category,lat=document[[12]]$location$latitude, lon=document[[12]]$location$longitude, id=document[[12]]$id, name=document[[12]]$location$street$name, month=document[[12]]$month, outcome_status=document[[12]]$outcome_status$category)
  # document <- rbind(master, d1, d2)
  document <- rbind(master, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12)
  
  document <- document[sample(nrow(document), 10000), ]

  # fix bug where columns are converted to factors. Set as chr class
  document <- rapply(document, as.character, classes="factor", how="replace")
  # recode month to integer value for sliderInput functionality
  document$month.int <- ifelse(document['month'] == "2015-12", 12,
                                  ifelse(document['month'] == "2016-01", 1, 2))


  # ---------- DATA CLEANING -----------

  # remove hyphenation for cleaner legend + popup
  document$category <- gsub('-', ' ', document$category)

  # Capitalise each word in string for aesthetical plot
  document$category <- paste(toupper(substring(document[,c("category")], 1, 1)), substring(document[,c("category")], 2), sep="")

  # # ---------- DATA PROCESSING ----------

  pal <- colorFactor(
    palette = "YlGnBu",
    domain = document$category
  )

  output$map <- renderLeaflet({

    withProgress(message = 'Rendering Map Data',
                 detail = 'this may take a few seconds...', value = 0, {
                   for (i in 1:60) {
                     incProgress(1/60)
                     Sys.sleep(0.25)
                   }

                   leaflet() %>% addProviderTiles("CartoDB.Positron") %>% addCircles(lng = as.numeric(document$lon), lat = as.numeric(document$lat), color = pal(document$category), opacity = .5, weight = 2,
                                                           popup =   paste("<b>Crime:</b> ", document$category, "<br>",
                                                                           "<b>Date:</b> <i>", document$month, "</i><br>",
                                                                           "<b>Location:</b> ", document$name, "<br>",
                                                                           "<b>Outcome:</b> ", document$outcome_status, "<br>")) %>% addLegend("bottomleft", values = document$category, pal = pal)
                 })
  })

  output$crimeline <- renderPlot({
    # summate crimes per month
    t <- count(document, "month")
    ggplot(t, aes(month, freq, group = 1)) +
      geom_point(colour = '#496D64') +
      geom_line(colour = '#496D64') + theme_bw() +
      geom_smooth(method="loess") + labs(x="Month", y="Frequency") +
      scale_y_continuous(labels = comma) +
      theme(axis.text.x=element_text(angle=90, hjust=1))
  })

  output$dbopt <- renderPlot({
    x <- data.frame(lat=as.numeric(document$lat), lon=as.numeric(document$lon))
    dbscan::kNNdistplot(x, k = 10)
  })

  output$dbscan <- renderPlot({
    x <- data.frame(lat=as.numeric(document$lat), lon=as.numeric(document$lon))
    res.db <- dbscan::dbscan(x, 0.0015, 5)
    plot(res.db, x,
         frame = FALSE,
         main="",
         xlab="Latitude",
         ylab="Longitude")
  })
  # 
  # # ---------- REACTIVE DATA -----------
  # 
  # # reactive expression to subset crimes chosen in date slider
  # filteredCrime <- reactive({
  #   document[document$month.int >= input$time[1] & document$month.int <= input$time[2], ]
  # })
  
  # observe({
  #   leafletProxy("map", data = filteredCrime()) %>% clearShapes()
  # })
  
  output$crimescatter <- renderPlot({
    counts <- data.frame(table(document$category))
    row.names(counts) <- counts$Var1
    par(mar = c(7, 4, 2, 2) + 0.2) #add room for the rotated labels
    
    barplot(counts$Freq, xlab="", ylab="Frequency", space=1)
    end_point = 0.5 + nrow(counts) + nrow(counts)-1 #this is the line which does the trick (together with barplot "space = 1" parameter)
    
    text(seq(1.5,end_point,by=2), par("usr")[3]-0.25, 
         srt = 60, adj= 1, xpd = TRUE,
         labels = paste(rownames(counts)), cex=1)
  })

  output$crimetable <- DT::renderDataTable({
    # subset crimes table
    new.document <- data.frame(document$category, document$lat, document$lon, document$name, document$outcome_status ,document$month)
    # rename columns
    names(new.document) <- c("Category", "Latitude", "Longitude", "Location", "Outcome", "Month")

    # df <- document %>% filter()
    action <- DT::dataTableAjax(session, new.document)

    DT::datatable(new.document, options = list(ajax = list(url = action)), escape=FALSE)
  })
  
}