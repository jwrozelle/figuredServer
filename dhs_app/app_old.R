

library(shiny)
library(ggplot2)
library(haven)

# Load data ###############################################################

child.df <- readRDS("/srv/shiny-server/dhs_app/childdf.rds")


child.df$whz1 <- child.df$hc72
child.df$whz1 <- ifelse(child.df$hc70 == 9999, NA, child.df$whz1)
child.df$whz1 <- ifelse(child.df$hc70 == 99999, NA, child.df$whz1)
child.df$whz1 <- ifelse(child.df$hc70 == 9998, NA, child.df$whz1)
child.df$whz1 <- ifelse(child.df$hc70 == 99998, NA, child.df$whz1)
child.df$whz1 <- child.df$whz1/100

child.df$whz <- NA
child.df$whz <- ifelse(child.df$whz1 <= -2, 1, child.df$whz)
child.df$whz <- ifelse(child.df$whz1 > -2, 0, child.df$whz)



child.df$hv270 <- as.factor(child.df$hv270)
child.df$hv025 <- as.factor(child.df$hv025)
child.df$hv104 <- as.factor(child.df$hv104)
child.df$hv219 <- as.factor(child.df$hv219)
child.df$hc61 <- as.factor(child.df$hc61)
child.df$hv113 <- as.factor(child.df$hv113)
child.df$hv111 <- as.factor(child.df$hv111)





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
  tags$h1("DHS Anthro app"),
  tags$p("*Unweighted analysis of DHS data from Tanzania"),
  sidebarLayout(sidebarPanel(
  selectInput(inputId = "disagg", label = "Select disaggregation variable",
              choices = c("None" = "1",
                          "Wealth Index" = "hv270",
                          "Urban vs. Rural" = "hv025",
                          "Sex of child" = "hv104",
                          "Head of HH sex" = "hv219",
                          "Mothers education level" = "hc61",
                          "Father alive" = "hv113",
                          "Mother alive" = "hv111"
                          )
              ),
  selectInput(inputId = "set100", label = "Y axis type",
              choices = c("Auto" = "0",
                          "100%" = "1"
                          )
              )
  ),
  mainPanel(tags$h2("Stunting"),
            plotOutput("stunt_bar"),
            tags$hr(),
            tags$h2("Wasting"),
            plotOutput("whz_bar"))
  ),
  tags$hr()
  
)

# server ====================================
server <- function(input, output) {

  output$stunt_bar <- renderPlot({
    histog <- ggplot(child.df, aes_string(x = input$disagg))
    histog <- histog + aes(y = stunt)
    histog <- histog + stat_summary(fun.y = mean, geom='bar', fill = "#546f42")
    histog <- histog + labs(title = paste("Stunting by", input$disagg))
    histog <- histog + theme_classic()
    if (input$set100 == "1") {histog <- histog + ylim(c(0,1))}
    histog
  })
  output$whz_bar <- renderPlot({
    histog <- ggplot(child.df, aes_string(x = input$disagg))
    histog <- histog + aes(y = whz)
    histog <- histog + stat_summary(fun.y = mean, geom='bar', fill = "#546f42")
    histog <- histog + theme_classic()
    if (input$set100 == "1") {histog <- histog + ylim(c(0,1))}
    histog
  })
}



# shinyApp ==================================
shinyApp(ui = ui, server = server)






