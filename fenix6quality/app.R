
library(shiny)
library(shinythemes)

# Load data ###############################################################

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
  tags$h1("Garmin Fenix 6 Series Quality Report"),
  tags$p("This Google Data Studio dashboard updates every 15 minutes. If you have purchased a Garmin Fenix 6 Series watch, and haven't yet filled out a survey, it only takes a minute or two. You can do so", tags$a(href = "https://forms.gle/9cnDvDJbSagWFaBf8", "here"),"."),
# CONTENT HERE

  tags$iframe(src="https://datastudio.google.com/embed/reporting/1pmkup9fZiH7zINvu-5--6_MNTy6sPr_k/page/Dof3",
              width = 1200,
              height = 1050,
              scrolling = "auto",
              seamless = TRUE),
  
  tags$hr()
  
  )

# server ====================================
server <- function(input, output) {

}



# shinyApp ==================================
shinyApp(ui = ui, server = server)






