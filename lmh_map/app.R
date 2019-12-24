
# Libraries ====================================================================
library(RMySQL)
library(shiny)
library(sp)
library(sf)
library(dplyr)
library(rgdal)
library(leaflet)
library(htmltools)
library(shinythemes)
# library(rmapshaper)




# Read in RDSs
# background data

background.folder <- paste("/srv/shiny-server/lmh_map")


merged.df <- readRDS("RObjects/mergeddf.rds")
comm.df <- readRDS("RObjects/commdf.rds")
comm.pnt <- readRDS("RObjects/commpnt.rds")
lbrogr_adm1.ply <- readRDS("RObjects/lbrogr_adm1ply.rds")
h_dist.ply <- readRDS("RObjects/h_distply.rds")


# UI ========================================
ui <- fluidPage(
  theme = shinytheme("yeti"),
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
  # textInput("pword", "Enter Password"),
  # conditionalPanel(
  #   "input.pword == 'lmh@2019'",
    sidebarLayout(sidebarPanel(
      sliderInput(
        inputId = "setm",
        "Select Month",
        min = 1,
        max = 12,
        value = 1,
        animate = animationOptions(interval = 3000),
        ticks = FALSE,
        step = 1
      ), 
      selectInput(inputId = "setyr", label = "Select Year",
                  choices = c("2019" = 2019,
                              "2018" = 2018,
                              "2017" = 2017
                  )
      ),
      selectInput(inputId = "tiles", label = "Select background map",
                  choices = c("World Shaded Relief" = "Esri.WorldShadedRelief",
                              "Toner Lite" = "Stamen.TonerLite",
                              "Esri World Terrain" = "Esri.WorldTerrain",
                              "NASA Earth at Night" = "NASAGIBS.ViirsEarthAtNight2012"
                              
                  )
      ),
      selectInput(inputId = "level",
                  label = "Select Level",
                  choices = c("Community" = 0, "District" = 1)
      ),
      conditionalPanel(
        condition = "input.level == 0",
        checkboxInput(inputId = "rmZeros", label = "Remove communities with values of 0.", FALSE)
      ),
      conditionalPanel("input.dataRefresh < 1",
                       actionButton("dataRefresh", "Reload Background Data")
      )
    ),
    
    mainPanel(
      tags$h2("Leaflet Map"),
      tags$h3(textOutput("loadText")),
      leafletOutput("leaflet_map", height = 500)
    ))
  #)# Parentheses for password panel
)


# Output  #######################################################################

server <- function(input, output) {
  
  output$loadText <- renderText({
    if(input$dataRefresh >= 1)
    
    {source("maploadr.R")
    return(paste("Refresh page to use reloaded data"))}
  })
  
  

  output$leaflet_map <- renderLeaflet({
    
    if (input$level == 0) {
      #first get and filter data needed
      comm.pnt_f2 <- filter(comm.df, month_reported == input$setm & year_reported == input$setyr)
      comm.pnt_f2 <- filter(comm.pnt_f2, !is.na(malnut) & !is.na(x) & !is.na(y))
      if (input$rmZeros == TRUE) {comm.pnt_f2 <- filter(comm.pnt_f2, malnut > 1)}
      comm.pnt_f2$label <- paste("<p> Community:", comm.pnt_f2$community.y, "</p>",
                                 "<p> Red MUAC:", comm.pnt_f2$redsum, "</p>",
                                 "<p> Yellow MUAC:", comm.pnt_f2$yelsum, "</p>")
      # Assign color palette
      pal <- colorNumeric(
        palette = c("white", "red"),
        domain = comm.pnt_f2[, "malnut"])
      # Create leaflet map
      indicator.lflt <- leaflet() %>%
        addProviderTiles(get(input$tiles, leaflet::providers)) %>%
        setView(lng = -9.4218, lat = 6.4337, zoom = 7) %>%
        addPolygons(data = lbrogr_adm1.ply,
                    color = "#082c38",
                    weight = 1,
                    smoothFactor = 1) %>% # higher than 1 helps the map to load faster
        addCircleMarkers(lng=comm.pnt_f2$x,
                         lat = comm.pnt_f2$y,
                         radius = 1,
                         label = lapply(comm.pnt_f2$label, HTML),
                         color = pal(comm.pnt_f2[, "malnut"])
        )} else if (input$level == 1) {
          bydist <- merged.df %>% filter(year_reported == input$setyr, month_reported == input$setm) %>% group_by(district) %>% summarise(malnut = sum(malnut), redmuac = sum(redsum), yellowmuac = sum(yelsum))
          bydist$district <- ifelse(bydist$district == "District 3 (C)", "District #3 (C)", bydist$district)
          choropleth <- sp::merge(h_dist.ply, bydist, by.x = "name", by.y = "district", all.x = TRUE, all.y = TRUE)
          choropleth[["label2"]] <- paste("<p> County:", choropleth[["name_cnty"]], "</p>",
                                          "<p> District:", choropleth[["name"]], "</p>",
                                          "<p> Red MUAC:", choropleth[["redmuac"]], "</p>",
                                          "<p> Yellow MUAC:", choropleth[["yellowmuac"]], "</p>")

          # Assign color palette
          pal <- colorNumeric(
            palette = c("white", "red"),
            domain = choropleth[["malnut"]],
            na.color = "#808080",
            reverse = FALSE)
          
          
          indicator.lflt <- leaflet() %>%
            addProviderTiles(get(input$tiles, leaflet::providers)) %>%
            setView(lng = -9.4218, lat = 6.4337, zoom = 7) %>%
            addPolygons(data = h_dist.ply,
                        color = "#082c38",
                        weight = 1,
                        fillColor = ~pal(choropleth[["malnut"]]),
                        label = lapply(choropleth[["label2"]], HTML),
                        highlight = highlightOptions(
                          weight = 3,
                          color = "#082c38",
                          # dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE)
                        )
        }
    
    indicator.lflt
  })
  
}


# shinyApp ==================================
shinyApp(ui = ui, server = server)

