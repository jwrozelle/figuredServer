#
# SAMPLE SIZE CALCULATOR
#

# Libraries ================================================================================

library(shiny)
library(pwr)
library(effsize)

# UI =======================================================================================

# Define UI for application that draws a histogram
ui <- fluidPage(
  # background data
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
  
  # Application title
  titlePanel("Sample Size Calculator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "test_type", label = "Type of test",
                  choices = c("T-Test" = "ttest",
                              "Two Proportions" = "twoProp",
                              "Population Estimate" = "prop"
                  )
      ),
      
      ## T-Test Conditional Panels
      # Sub type of t-test
      conditionalPanel(condition = "input.test_type == 'ttest'",
                       selectInput(inputId = "ttest_sub", label = "T-Test sub type",
                                   choices = c("Two Sample" = "two.sample",
                                               "One Sample" = "one.sample",
                                               "Paired" = "paired")
                       )
      ),
      # Sub type of t-test
      conditionalPanel(condition = "input.test_type == 'ttest' | input.test_type == 'twoProp'",
                       selectInput(inputId = "dir_alternative", label = "Test direction",
                                   choices = c("Two-sided" = "two.sided",
                                               "Greater" = "greater",
                                               "Less" = "less")
                       )
      ),
      # Group 1 estimate
      conditionalPanel(condition = "input.test_type == 'ttest' & input.ttest_sub == 'two.sample'",
                       numericInput(inputId = "ttest_grp1", label = "Intervention / Group of Interest estimated mean", value = 0.5
                       )
      ),
      # Group 2 / population estimate
      conditionalPanel(condition = "input.test_type == 'ttest'",
                       numericInput(inputId = "ttest_grp2", label = "Control / Population group estimated mean", value = 0.5
                       )
      ),
      
      ## Conditional panels for two proportions
      # Proportion (group 1)
      conditionalPanel(condition = "input.test_type == 'twoProp' | input.test_type == 'prop'",
                       numericInput(inputId = "prop_grp1",
                                    label = "Group 1 estimated proportion",
                                    value = 0.5)
                       ),
      # Proportion Among Group 2
      conditionalPanel(condition = "input.test_type == 'twoProp'",
                       numericInput(inputId = "prop_grp2",
                                    label = "Group 2 estimated proportion",
                                    value = 0.5)
      ),
      
      ## Conditional panel for population estimate
      # Margin of Error
      conditionalPanel(condition = "input.test_type == 'prop'",
                       numericInput(inputId = "moe",
                                    label = "Margin of Error (Default is 0.05)",
                                    value = 0.05,
                                    min = 0,
                                    max = 1)
      ),
      
      # Permanent Panels
      numericInput(inputId = "fpc", label = "Finite Population Correction", value = NULL),
      
      numericInput(inputId = "power", label = "Power", min = 0, max = 1, value = 0.8),
      numericInput(inputId = "sig_level", label = "Level of Significance (defaults at 0.05)", min = 0.00000000001, max = 0.999999999, value = 0.05),
      numericInput(inputId = "deff", label = "Design Effect (Default of 1 for no design effect)", value = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("sample_size")
    )
  )
  )

# Server ===================================================================================

# Define server logic required to compute the sample sizes
server <- function(input, output) {
  
  output$sample_size <- renderPrint({
    # Beginning of if block for the type of test
    
    ## For T-Test sample sizes
    if (input$test_type == "ttest") {
      d <- cohen.ES(test = "t", size = "medium")$effect.size
      result <- pwr.t.test(d = d,
                 power = input$power,
                 sig.level = as.numeric(input$sig_level),
                 type = input$ttest_sub,
                 alternative = input$dir_alternative
                 )
      s_size <- result$n
    # For comparison of two proportions
    } else if (input$test_type == "twoProp") {
      result <- stats::power.prop.test(
        p1 = input$prop_grp1,
        p2 = input$prop_grp2,
        sig.level = input$sig_level,
        power = input$power,
        alternative = input$dir_alternative
      )
      s_size <- result$n
    # For a single proportion
    } else if (input$test_type == "prop") {
      s_size <- (qnorm((input$sig_level / 2), lower.tail = FALSE)^2) * input$prop_grp1 * (1 - input$prop_grp1) / ((input$moe)^2)
      if (is.na(input$fpc)) {
      }
    }
    if (input$deff != 1) {
      s_size <- input$deff * s_size
    }
    if (isTruthy(input$fpc)) {
      s_size <- ((input$fpc * s_size) / (s_size + input$fpc - 1))
    }
    return(ceiling(s_size))
  })
}

# Run the application ======================================================================
shinyApp(ui = ui, server = server)












