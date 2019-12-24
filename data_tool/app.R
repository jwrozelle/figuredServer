
library(dplyr)
library(ggplot2)
library(ggthemes)
library(shiny)
library(shinythemes)
library(scales)

plat.df <- read.csv("/srv/shiny-server/data_tool/www/platform.csv")

# Rework data
plat.df2 <- plat.df
plat.df2$year0 <- ifelse(plat.df$year == min(plat.df$year), plat.df$number, 0)

# Calculate comparison to 2008 and total weighting factor
(weighstart.df <- plat.df2 %>% group_by(tool) %>% summarise(total_tool = sum(number), year0 = sum(year0)))

(yrly.df <- plat.df2 %>% group_by(year) %>% summarise(total_year = sum(number)))


# merge back into dataframe
plat.df3 <- merge(plat.df, weighstart.df, by = "tool", all.x = TRUE)
(plat.df3 <- merge(plat.df3, yrly.df, by = "year", all.x = TRUE))

plat.df3$relative_yr <- plat.df3$number / plat.df3$total_year
plat.df3$diff_yr0_n <- plat.df3$number - plat.df3$year0
plat.df3$diff_yr0_p <- (plat.df3$number - plat.df3$year0)/plat.df3$year0
plat.df3$diff_yr0_pw <- ((plat.df3$number - plat.df3$year0)/plat.df3$year0) * (plat.df3$total_tool / sum(plat.df3$number))

# ggplot


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
  
  # App title ----
  titlePanel("Results on Google Scholar by Statistics Platform"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # select analysis
      selectInput("analysis", "Select Y Axis variable",
                  c("# per year" = "number",
                    "% of yearly total" = "relative_yr",
                    "# difference compared to 2008" = "diff_yr0_n",
                    "% difference compared to 2008" = "diff_yr0_p",
                    "Change weighted by number published" = "diff_yr0_pw"))
      
    ),
    
    # Main panel for displaying outputs ===================================================
    mainPanel(
      
      #Explanation
      tags$h3("Explanation"),
      HTML("<p>
           Using search terms defined by Robert A. Muenchen <a href = 'http://r4stats.com/articles/how-to-search-for-data-science-articles/'> here</a>,
           I created a simple dataset. I simply limited the publication to one year,
           searched using the defined terms. Each manual search (as of 13-Jan-2019) had a row
           with tool, year and number of results recorded. The options for patents and
           citations were de-selected.
           </p>
           <p> Of course there are limitations to the quality of the data retrieved using this
           simplistic approach. A better solution would be Harzing's Publish or Perish. Unfortunately,
           that tool limits a query to 1000 citations. As you can see, the current project far
           excedes that limit. However, I feel it is sufficient to get a general sense of the trends
           over the past 10 years.
           </p>"),
      
      # Output: chart ----
      tags$hr(),
      tags$h3("Y Axis calculation"),
      tags$p(textOutput("yvar_desc")),
      tags$p(textOutput("factor_ex")),
      plotOutput("chart")
    )
    
  )
)


# Percent difference compared to 2008 weighted by total number of articles
server <- function(input, output) {
  output$chart <- renderPlot({
    display.plot <-
      ggplot(plat.df3, aes_string(
        y = input$analysis,
        color = plat.df3$tool
      ))
    if (input$analysis == "relative_yr" | input$analysis == "diff_yr0_p") {
      display.plot <- display.plot + scale_y_continuous(name = "Percent", labels = percent)
    } else if (input$analysis != "diff_yr0_pw") {
      display.plot <- display.plot + scale_y_continuous(labels = number)
    }
    display.plot <-
      display.plot + theme_economist()
    display.plot <-
        display.plot + geom_line(aes(x = year), size = 1.5) + scale_x_continuous(name = "Year", breaks=seq(2008,2018,1))
    return(display.plot)
    }
  )
  
  # Y variable explanation
  output$yvar_desc <- renderText({
    if (input$analysis == "number") {
      paste0("simple number of results per year")
    } else if (input$analysis == "relative_yr") {
      paste0("(number of results) / (number of results that year)")
    } else if (input$analysis == "diff_yr0_n") {
      paste0("(number of results in a given year) - (number of results in 2015)")
    } else if (input$analysis == "diff_yr0_p") {
      paste0("((number of results in a given year) - (number of results in 2015)) / (number of results in 2015)")
    } else if (input$analysis == "diff_yr0_pw") {
      paste0("(((number of results in a given year) - (number of results in 2015)) / (number of results in 2015)) * (total results for a tool across years / total results across years)")
    }
  })
  
  output$factor_ex <- renderText({
    if (input$analysis == "diff_yr0_pw") {
      paste0("This is a weighted trend line. It's intended to show relative change, weighted by overall articles published. The 400+% increase in articles mentioning R is not directly comparible to the decrease in SPSS because SPSS started so much higher. By weighting the trend lines by the relative number of articles published, trends are meant to be comparible.")
    } # else {paste0(" ")}
  })
}

# run application
shinyApp(ui, server)


