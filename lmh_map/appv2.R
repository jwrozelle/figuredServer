
# Libraries ====================================================================
library(RMySQL)
library(shiny)
library(sp)
library(ggmap)
library(ggplot2)
library(GISTools)
library(sf)
library(dplyr)
library(rgdal)
library(leaflet)
library(htmltools)

# SQL connection & DL ==========================================================

killDbConnections <- function () {
  all_cons <- dbListConnections(MySQL())
  print(all_cons)
  for(con in all_cons)
    +  dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}
killDbConnections()

cha.con <- DBI::dbConnect(
  MySQL(),
  user = 'lastmile_admin',
  host = "166.62.33.107",
  port = 3306,
  password = "LastMile14",
  dbname = "lastmile_cha"
)

upload.con <-DBI::dbConnect(
  MySQL(),
  user = 'lastmile_admin',
  host = "166.62.33.107",
  port = 3306,
  password = "LastMile14",
  dbname = "lastmile_upload"
)

# Upload
cha_msr_cur.sqlr <- dbSendQuery(upload.con,
                                'SELECT *, SUM(3_1_b_muac_red) AS redsum, SUM(3_1_c_muac_yellow) AS yelsum
                            FROM de_cha_monthly_service_report
                            GROUP BY community_id, month_reported, year_reported'
)
cha_msr_cur.df <- fetch(cha_msr_cur.sqlr, n=-1)
dbClearResult(dbListResults(upload.con)[[1]])
rm(cha_msr_cur.sqlr)

# community GPS data
community.sqlr <- dbSendQuery(cha.con, ' SELECT * FROM community')
community.df <- fetch(community.sqlr, n=-1)
dbClearResult(dbListResults(cha.con)[[1]])
rm(community.sqlr)

rm(upload.con, cha.con)

killDbConnections()

# workingdir <- "/srv/shiny-server/lmh_map"

# source(paste(workingdir, "/sql_pull.R", sep = ""))
# community.df <- readRDS(paste(workingdir, "/community.rds", sep = ""))
# cha_msr_cur.df <- readRDS(paste(workingdir, "/cha_msr_cur.rds", sep = ""))

# Shapefiles
geoDirectory <- "/srv/shiny-server/lmh_map/shapefiles/admin"

# lbr_adm0.ply <- st_read(paste(geoDirectory, "/LBR_adm0.shp", sep =""))
# lbr_adm1.ply <- st_read(paste(geoDirectory, "/LBR_adm1.shp", sep =""))
# lbr_adm2.ply <- st_read(paste(geoDirectory, "/LBR_adm2.shp", sep =""))
lbrogr_adm1.ply <- readOGR(paste(geoDirectory, "/LBR_adm1.shp", sep =""))
h_dist.ply <- readOGR(paste(geoDirectory, "/Health district boundaries.shp", sep =""))

# MUAC NUMBERS ###########################################################
merged.df <- merge(cha_msr_cur.df, community.df, by="community_id", all.x=TRUE)
rm(cha_msr_cur.df, community.df)

# malnutrition calculator
merged.df$malnut <- (as.numeric(merged.df$redsum)*2) + as.numeric(merged.df$yelsum) +1

comm.df <- as.data.frame(filter(merged.df, !is.na(x) & !is.na(y)))
comm.pnt <- st_as_sf(comm.df, coords = c("x", "y"), crs = "+proj=longlat +ellps=WGS84")




# UI ========================================
ui <- fluidPage(
  tags$head(HTML(
    "<!-- Global site tag (gtag.js) - Google Analytics -->
    <script async src='https://www.googletagmanager.com/gtag/js?id=UA-129297024-1'></script>
    <script>
    window.dataLayer = window.dataLayer || [];
    function gtag(){dataLayer.push(arguments);}
    gtag('js', new Date());
    
    gtag('config', 'UA-129297024-1');
    </script>")),
  tags$a(href = "https://figured.io", tags$img(src = "https://figured.io/aux/logo1.png", width = 137, height = 50, padding = "20px")),
  tags$hr(),
  tags$h1("LMH MUAC data"),
  sidebarLayout(sidebarPanel(
    sliderInput(inputId = "setm", "Select Month", min = 1, max = 12, value = 10, animate = TRUE),
    selectInput(inputId = "setyr", label = "Select Year",
                choices = c("2018" = 2018,
                            "2017" = 2017,
                            "2016" = 2016,
                            "2015" = 2015
                            )
                ),
    selectInput(inputId = "tiles", label = "Select background map",
                choices = c("World Shaded Relief" = "Esri.WorldShadedRelief",
                            "Toner Lite" = "Stamen.TonerLite",
                            "Esri World Terrain" = "Esri.WorldTerrain",
                            "NASA Earth at Night" = "NASAGIBS.ViirsEarthAtNight2012"
                            
                )
    ),
    checkboxInput(inputId = "rmZeros", label = "Remove communities with values of 0.", FALSE)
    ),
  mainPanel(#tags$h2("Map"),
            #plotOutput("simple_map"),
            #tags$hr(),
            #tags$h2("Leaflet Map"),
            #leafletOutput("leaflet_map"),
            #tags$hr(),
            tags$h2("Second leaflet map"),
            leafletOutput("leaflet_map2")
            ))
)


# Output  #######################################################################

server <- function(input, output) {
  
  
  # output$simple_map <- renderPlot({
  #   comm.pnt_f <- filter(comm.pnt, month_reported == input$setm & year_reported == input$setyr)
  #   comm.pnt_f <- filter(comm.pnt_f, !is.na(malnut))
  #   lbr_plot <- ggplot() + geom_sf(data = lbr_adm0.ply, size = 1, color = "black", fill = "white")
  #   lbr_plot <- lbr_plot + geom_sf(data = lbr_adm2.ply, size = 0.75)
  #   lbr_plot <- lbr_plot + geom_sf(data = lbr_adm1.ply, size = 1, alpha = 0)
  #   lbr_plot <- lbr_plot + geom_sf(data = comm.pnt_f, size = 0.5, aes(color = comm.pnt_f$malnut))
  #   lbr_plot <- lbr_plot +  scale_color_gradient(low = "white", high = "red") 
  #   lbr_plot
  # })
  # output$leaflet_map <- renderLeaflet({
  #   #first get and filter data needed
  #   comm.pnt_f2 <- filter(comm.df, month_reported == input$setm & year_reported == input$setyr)
  #   comm.pnt_f2 <- filter(comm.pnt_f2, !is.na(malnut) & !is.na(x) & !is.na(y))
  #   if (input$rmZeros == TRUE) {comm.pnt_f2 <- filter(comm.pnt_f2, malnut > 1)}
  #   comm.pnt_f2$label <- paste("<p> Community:", comm.pnt_f2$community.y, "</p>",
  #                              "<p> Red MUAC:", comm.pnt_f2$redsum, "</p>",
  #                              "<p> Yellow MUAC:", comm.pnt_f2$yelsum, "</p>")
  #   # Assign color palette
  #   pal <- colorNumeric(
  #     palette = c("white", "red"),
  #     domain = comm.pnt_f2[, "malnut"])
  #   # Create leaflet map
  #   examp.lflt <- leaflet() %>%
  #     addProviderTiles(get(input$tiles, leaflet::providers)) %>%
  #     setView(lng = -9.4218, lat = 6.4337, zoom = 7) %>%
  #     addPolygons(data = lbrogr_adm1.ply,
  #                 color = "#082c38",
  #                 weight = 1,
  #                 smoothFactor = 1) %>% # higher than 1 helps the map to load faster
  #     addCircleMarkers(lng=comm.pnt_f2$x,
  #                      lat = comm.pnt_f2$y,
  #                      radius = 1,
  #                      label = lapply(comm.pnt_f2$label, HTML),
  #                      color = pal(comm.pnt_f2[, "malnut"])
  #                      )
  #   
  #   examp.lflt
  # })
  
  output$leaflet_map2 <- renderLeaflet({
    bydist <- merged.df %>% filter(year_reported == input$setyr, month_reported == input$setm) %>% group_by(district) %>% summarise(malnut = sum(malnut), redmuac = sum(redsum), yellowmuac = sum(yelsum))
    #choropleth <- sp::merge(h_dist.ply, bydist, by.x = "name", by.y = "district")
    polynames <- as_data_frame(h_dist.ply$name)
    polynames$name <- polynames$value
    bydist <- base::merge(polynames, bydist, by.x = "name", by.y = "district", all.x = TRUE)
    rm(polynames)
    # reorder
    bydist <- bydist[order(match(bydist$district, h_dist.ply$name)),]

    # Assign color palette
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = bydist[, "malnut"])


    m <- leaflet() %>%
      addProviderTiles(get(input$tiles, leaflet::providers)) %>%
      setView(lng = -9.4218, lat = 6.4337, zoom = 7) %>%
      addPolygons(data = h_dist.ply,
                  weight = 1,
                  smoothFactor = 1#,
                  #fillColor = pal(bydist[, "malnut"])
      )

    m
  })
}


# shinyApp ==================================
shinyApp(ui = ui, server = server)





