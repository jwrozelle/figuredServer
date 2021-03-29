
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
library(ggplot2) # temporary
library(classInt)
# library(rmapshaper)

analysis_variables <- c("Confirmed" = "Confirmed",
                        "Deaths" = "Deaths",
                        "Confirmed per 100,000" = "Confirmed_pop",
                        "Deaths per 100,000" = "Deaths_pop"
                        )

# Read in RDSs
# background data

# background.folder <- paste("/srv/shiny-server/lmh_map")

# temporary covid data
choropleth_county <- readRDS("RObjects/choropleth_county.rds")

choropleth_state <- readRDS("RObjects/choropleth_state.rds")

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
  tags$h1("COVID-19 US map"),
  # textInput("pword", "Enter Password"),
  # conditionalPanel(
  #   "input.pword == 'lmh@2019'",
    sidebarLayout(sidebarPanel(
      # sliderInput(
      #   inputId = "setm",
      #   "Select Month",
      #   min = 1,
      #   max = 12,
      #   value = 1,
      #   animate = animationOptions(interval = 3000),
      #   ticks = FALSE,
      #   step = 1
      # ), 
      
      selectInput(inputId = "variable", label = "Select Variable for colorization",
                  choices = analysis_variables
      ),
      
      
      selectInput(inputId = "color_style", label = "Calculation for color",
                  choices = c("Jenks" = "Jenk",
                              "Quantiles" = "Quantile",
                              "Standard Deviation" = "Sd"
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
                  choices = c("County" = 0, "State" = 1)
      )#,
      # conditionalPanel(
      #   condition = "input.level == 0",
      #   checkboxInput(inputId = "rmZeros", label = "Remove communities with values of 0.", FALSE)
      # ),
      # conditionalPanel("input.colorscaleRefresh < 1",
      #                  actionButton("colorscaleRefresh", "Reload Background Data")
      # )
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
  
  # output$loadText <- renderText({
  #   if(input$dataRefresh >= 1)
  #   
  #   {source("maploadr.R")
  #   return(paste("Refresh page to use reloaded data"))}
  # })
  
  

  output$leaflet_map <- renderLeaflet({
    
    if (input$level == 0) {
      # # first get and filter data needed
      # comm.pnt_f2 <- filter(comm.df, month_reported == input$setm & year_reported == input$setyr)
      # comm.pnt_f2 <- filter(comm.pnt_f2, !is.na(malnut) & !is.na(x) & !is.na(y))
      # if (input$rmZeros == TRUE) {comm.pnt_f2 <- filter(comm.pnt_f2, malnut > 1)}
      # comm.pnt_f2$label <- paste("<p> Community:", comm.pnt_f2$community.y, "</p>",
      #                            "<p> Red MUAC:", comm.pnt_f2$redsum, "</p>",
      #                            "<p> Yellow MUAC:", comm.pnt_f2$yelsum, "</p>")
      
        # pull break file from RObjects
      
        breaks <- readRDS(paste0("./RObjects/colorbreaks/county/brk", input$color_style, "_", input$variable, ".rds"))
        # breaks <- readRDS(paste0("RObjects/brk", "Jenk", "_", "Confirmed", ".rds"))
        
        breaks <- unique(breaks$brks)
        
        #cut the variable of interest into breaks
        choropleth_county[["color_classes"]] <- cut(choropleth_county[[input$variable]], breaks, include.lowest = T)
        
        choropleth_county$color_classes <- as.numeric(choropleth_county$color_classes)
        
        # Create Label
        choropleth_county[["county_label"]] <- paste("<p> State:", choropleth_county[["STATE_NAME"]], "</p>",
                                        "<p><b>County:</b>", choropleth_county[["NAME"]], "</p>",
                                        "<p><b>Confirmed:</b>", choropleth_county[["Confirmed"]], "</p>",
                                        "<p><b>Confirmed per 100000:</b>", choropleth_county[["Confirmed_pop"]], "</p>",
                                        "<p><b>Deaths:</b>", choropleth_county[["Deaths"]], "</p>",
                                        "<p><b>Deaths per 100000:</b>", choropleth_county[["Deaths_pop"]], "</p>",
                                        "<p><b>Recovered:</b>", choropleth_county[["Recovered"]], "</p>",
                                        "<p><b>Active:</b>", choropleth_county[["Active"]], "</p>")
        
      
        
        
      # Assign color palette
      pal <- colorNumeric(
        palette = c("white", "yellow", "red"),
        domain = choropleth_county[["color_classes"]],
        na.color = "#808080",
        reverse = FALSE)
        
        
        
        
      
      # Create leaflet map
      indicator.lflt <- leaflet() %>%
        addProviderTiles(get(input$tiles, leaflet::providers)) %>% 
        setView(lng = -98.583333, lat = 39.833333, zoom = 4) %>%
        addPolygons(data = choropleth_county,
                    color = "#082c38",
                    weight = 1,
                    fillOpacity = 0.5,
                    fillColor = ~pal(choropleth_county[["color_classes"]]),
                    smoothFactor = 1,
                    highlight = highlightOptions(weight = 3, 
                                                 fillOpacity = 0.7, 
                                                 bringToFront = TRUE),
                    label = lapply(choropleth_county[["county_label"]], HTML)
                    ) # %>% # higher than 1 helps the map to load faster
        # addCircleMarkers(lng=comm.pnt_f2$x,
        #                  lat = comm.pnt_f2$y,
        #                  radius = 1,
        #                  label = lapply(comm.pnt_f2$label, HTML),
        #                  color = pal(comm.pnt_f2[, "malnut"])
        } else if (input$level == 1) {
          
          
          # pull break file from RObjects
          
          breaks <- readRDS(paste0("./RObjects/colorbreaks/state/brk", input$color_style, "_", input$variable, ".rds"))
          # breaks <- readRDS(paste0("RObjects/brk", "Jenk", "_", "Confirmed", ".rds"))
          
          breaks <- unique(breaks$brks)
          
          #cut the variable of interest into breaks
          choropleth_state[["color_classes"]] <- cut(choropleth_state[[input$variable]], breaks, include.lowest = T)
          
          choropleth_state$color_classes <- as.numeric(choropleth_state$color_classes)
          
          
          # Assign color palette
          pal <- colorNumeric(
            palette = c("white", "yellow", "red"),
            domain = choropleth_state[["color_classes"]],
            na.color = "#808080",
            reverse = FALSE)
          
          
          # Create Label
          choropleth_state[["state_label"]] <- paste("<p><b>State:</b>", choropleth_state[["NAME"]], "</p>",
                                                     "<p><b>Confirmed:</b>", choropleth_state[["Confirmed"]], "</p>",
                                                     "<p><b>Confirmed per 100000:</b>", choropleth_state[["Confirmed_pop"]], "</p>",
                                                     "<p><b>Deaths:</b>", choropleth_state[["Deaths"]], "</p>",
                                                     "<p><b>Deaths per 100000:</b>", choropleth_state[["Deaths_pop"]], "</p>",
                                                     "<p><b>Recovered:</b>", choropleth_state[["Recovered"]], "</p>",
                                                     "<p><b>Active:</b>", choropleth_state[["Active"]], "</p>")
          
          
          indicator.lflt <- leaflet() %>%
            addProviderTiles(get(input$tiles, leaflet::providers)) %>% 
            setView(lng = -98.583333, lat = 39.833333, zoom = 4) %>%
            addPolygons(data = choropleth_state,
                        color = "#082c38",
                        weight = 1,
                        fillOpacity = 0.5,
                        fillColor = ~pal(choropleth_state[["color_classes"]]),
                        smoothFactor = 1,
                        highlight = highlightOptions(weight = 3, 
                                                     fillOpacity = 0.7, 
                                                     bringToFront = TRUE),
                        label = lapply(choropleth_state[["state_label"]], HTML)
            )
          
        }
    
    indicator.lflt
  })
  
}


# shinyApp ==================================
shinyApp(ui = ui, server = server)

