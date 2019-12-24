library(shiny)
library(ggplot2)
source("Example 1.R")

# UI ========================================
ui <- fluidPage(
  tags$a(href = "https://figured.io", tags$img(src = "https://figured.io/aux/logo1.png", width = 137, height = 50, padding = "20px")),
  tags$hr(),
  tags$h2("Example App"),
  wellPanel(sliderInput(inputId = "num",
              label = "Choose a number of bins",
              value = 1, min = 0, max = 100)),
  plotOutput("hist"),
  tags$hr()
  
  )

# server ====================================
server <- function(input, output) {
  
  
    output$hist <- renderPlot({
    hist.rCSI <- ggplot(edrought.df, aes(rCSI_score, group=province, fill=province)) + geom_histogram(position="identity", aes(y=(..density..)*100), breaks = seq(0,input$num,by=1), alpha=0.5)
    hist.rCSI <- hist.rCSI + labs(x = "rCSI Score (Lower is better)", y = "Frequency (%)")
    hist.rCSI <- hist.rCSI + theme_bw() + theme(text=element_text(family="Arial", size = 11, color = "#000000")) + xlim(0, input$num)
    hist.rCSI <- hist.rCSI + expand_limits(x = 0)
    hist.rCSI <- hist.rCSI + geom_vline(aes(xintercept = 3), colour="black")
    hist.rCSI <- hist.rCSI + geom_vline(aes(xintercept = 10), colour="black")
    hist.rCSI
    
    
  })
  
  
}



# shinyApp ==================================
shinyApp(ui = ui, server = server)










