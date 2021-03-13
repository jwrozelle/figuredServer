
library(shiny)
library(xlsx)
library(pwr)
library(effsize)
library(shinythemes)

# Define UI for data upload app ----
ui <- fluidPage(
  theme = shinytheme("yeti"),
  tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    HTML(
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
  tags$h1("Complex Sampl.R"),
  
  tabsetPanel(type="tabs",
              
              # CLUSTER SELECTION UI TAB  ======================================================
              tabPanel("Cluster Selection",
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           
                           # Input: Select a file ----
                           tags$h3("Import options for .csv"),
                           fileInput("file1", "Choose CSV File",
                                     multiple = TRUE,
                                     accept = c("text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv")),
                           
                           # Horizontal line ----
                           # tags$hr(),
                           
                           # Input: Checkbox if file has header ----
                           # checkboxInput("header", "Header", TRUE),
                           
                           # Input: Select separator ----
                           radioButtons("sep", "Separator",
                                        choices = c(Comma = ",",
                                                    Semicolon = ";",
                                                    Tab = "\t"),
                                        selected = ","),
                           
                           # Input: Select quotes ----
                           radioButtons("quote", "Quote",
                                        choices = c(None = "",
                                                    "Double Quote" = '"',
                                                    "Single Quote" = "'"),
                                        selected = '"'),
                           tags$hr(), # horizontal line
                           tags$h3("Sampling Design options"), # text
                           
                           # 
                           numericInput("num_clusters", "Number of Primary Sampling Units", value = NULL, min = 0),
                           
                           # Numeric input for number of USUs in each cluster
                           numericInput("cluster_size", "Number of Ultimate Sampling Units in each cluster", value = NULL, min = 0),
                           
                           # Conditional panel that shows "generate random start" button when previous inputs are added
                           conditionalPanel("input.num_clusters > 0", # & input.tot_sample > 0",
                                            actionButton("gen_random", "Generate Random Start")),
                           conditionalPanel("input.num_clusters > 0 & input.gen_random >= 1", #& input.tot_sample > 0",
                                            numericInput("randomStart", "Random Start Number", value = NULL, min = 0)
                           )
                         ),
                         
                         # Main panel for displaying outputs ===================================================
                         mainPanel(
                           
                           # Explanation of the app.
                           tags$h3("About Complex Sampl.R"),
                           tags$p("This app is designed to do the sampling of the clusters for a two-stage Probability Proportional to Size (PPS) cluster sampling design. All that is needed is a .csv file (xlsx is not yet supported) with at least one column labeled 'population'. This column should contain the population or number of ultimate sampling units within each cluster or primary sampling unit."),
                           tags$p("If the number of USUs in the PSU is smaller than the cluster size, the app will automatically sample from the next row(s) until there are sufficient USUs for a cluster."),
                           tags$h4("Known limitations"),
                           tags$ul(
                             tags$li("First, the code currently only samples subsequent rows in the dataset when the population of a PSU is smaller than the cluster sample size. So, if the loop gets to the end of the csv file and there are no following rows - the loop will stop, even if there are insufficient USUs available for the cluster size."),
                             tags$li("The other limitation is that the loop is currently calculated only by counting the sum of the population from a group of PSU's until there are as many or more USUs than the cluster size. In rare cases, this may mean that more USU's may be assigned to a PSU than it has. For example, if the cluster size was 100, and one PSU had 80 and the following PSU had 30 USUs the loop would recognize that there are more than 100 USUs between the two of them, and assign half (50) of the cluster to be taken from the first (with a 80 USUs) and 50 to be taken from the next (with only 30).")
                             ),
                           tags$p("Both of these issues should be infrequent, but will be addressed in future versions."),
                           tags$h3("Instructions"),
                           tags$ol(
                             tags$li("Upload a csv file using the 'Browse...' button. Your csv file must (at least) contain a column called 'population', containing the number of USUs in that PSU. Other columns may be included as desired. If you wish, you may use the Example data (downloadable below) to test the app, or as a template."),
                             tags$li("Input the number of PSUs in your sample, and the number of USUs in each PSU"),
                             tags$li("Click 'Generate Random Start'."),
                             tags$li("(Optional) After inputting random start, you may select your own random start. This may be useful when trying to reproduce a previous selection."),
                             tags$li("(Optional) At this time, if you wish to stratify your sample, each strata can be uploaded as a separate csv. You may then follow steps 1-4 for each strata.")
                           ),
                           
                           # download tempalte
                           tags$h3("Example data"),
                           downloadButton("downloadTemplate", "Download Template"),
                           
                           # Output: Data file ----
                           tags$hr(),
                           tags$h2(textOutput("startWarn")),
                           conditionalPanel("input.num_clusters > 0",
                           tags$h3("Meta Data about Sample Result")),
                           tags$h4(textOutput("sampIntRendered")),
                           tags$h4(textOutput("totalSample")),
                           
                           #Button to download resulting data
                           conditionalPanel("input.gen_random >= 1 & input.cluster_size >= 0",
                                            tags$hr(),
                                            downloadButton("downloadData", "Download Sample Result")),
                           
                           #Show table of resulting data
                           conditionalPanel("input.gen_random >= 1",
                                            # tags$hr(),
                                            dataTableOutput("contents")
                           )
                         )
                       )
                       
              ),
              
              # SAMPLE SIZE UI TAB =====================================================================
              tabPanel("Sample Sizes",
                       tags$h2("Currently Under Development")#,
                       # sidebarLayout(
                       #   sidebarPanel(
                       #     selectInput(inputId = "test_type", label = "Type of test",
                       #                 choices = c("Two Proportions" = "twoProp",
                       #                             "Population Estimate" = "prop",
                       #                             "T-Test" = "ttest"
                       #                 )
                       #     ),
                       #     
                       #     ## T-Test Conditional Panels
                       #     # Sub type of t-test
                       #     conditionalPanel(condition = "input.test_type == 'ttest'",
                       #                      selectInput(inputId = "ttest_sub", label = "T-Test sub type",
                       #                                  choices = c("Two Sample" = "two.sample",
                       #                                              "One Sample" = "one.sample",
                       #                                              "Paired" = "paired")
                       #                      )
                       #     ),
                       #     # Sub type of t-test
                       #     conditionalPanel(condition = "input.test_type == 'ttest' | input.test_type == 'twoProp'",
                       #                      selectInput(inputId = "dir_alternative", label = "Test direction",
                       #                                  choices = c("Two-sided" = "two.sided",
                       #                                              "Greater" = "greater",
                       #                                              "Less" = "less")
                       #                      )
                       #     ),
                       #     # Group 1 estimate
                       #     conditionalPanel(condition = "input.test_type == 'ttest' & input.ttest_sub == 'two.sample'",
                       #                      numericInput(inputId = "ttest_grp1", label = "Intervention / Group of Interest mean", value = NULL
                       #                      )
                       #     ),
                       #     # Group 2 / population estimate
                       #     conditionalPanel(condition = "input.test_type == 'ttest'",
                       #                      numericInput(inputId = "ttest_grp2", label = "Control / Population group mean", value = NULL
                       #                      )
                       #     ),
                       #     
                       #     ## Conditional panels for two proportions
                       #     # Proportion (group 1)
                       #     conditionalPanel(condition = "input.test_type == 'twoProp' | input.test_type == 'prop'",
                       #                      numericInput(inputId = "prop_grp1",
                       #                                   label = "Group 1 estimated proportion",
                       #                                   value = 0.5)
                       #     ),
                       #     # Proportion Among Group 2
                       #     conditionalPanel(condition = "input.test_type == 'twoProp'",
                       #                      numericInput(inputId = "prop_grp2",
                       #                                   label = "Group 2 estimated proportion",
                       #                                   value = 0.5)
                       #     ),
                       #     
                       #     ## Conditional panel for population estimate
                       #     # Margin of Error
                       #     conditionalPanel(condition = "input.test_type == 'prop'",
                       #                      numericInput(inputId = "moe",
                       #                                   label = "Margin of Error (Default is 0.05)",
                       #                                   value = 0.05,
                       #                                   min = 0,
                       #                                   max = 1)
                       #     ),
                       #     
                       #     # Permanent Panels
                       #     ## Power input
                       #     numericInput(inputId = "power", label = "Power", min = 0, max = 1, value = 0.8),
                       #     ## Significance Level Input
                       #     numericInput(inputId = "sig_level", label = "Level of Significance (defaults at 0.05)", min = 0.00000000001, max = 0.999999999, value = 0.05),
                       #     ## Design Effect Input
                       #     numericInput(inputId = "deff", label = "Design Effect (Default of 1 for no design effect)", value = 1),
                       #     ## Eligibility Adjustment
                       #     numericInput(inputId = "adjustment", label = "Other sample size consideration (i.e. USUs that meet selection criteria)", value = 1, min = 0),
                       #     ## Finite Population Correction
                       #     numericInput(inputId = "fpc", label = "Finite Population Size", value = NULL),
                       #     ## Response rate input
                       #     numericInput(inputId = "respRate", label = "Response Rate (1 is no adjustment)", value = 1, min = 0, max = 1)
                       #   ),
                       #   
                       #   # Show a plot of the generated distribution
                       #   mainPanel(
                       #     conditionalPanel("input.test_type == 'ttest' | input.test_type == 'prop'",
                       #       tags$h3("Warning! Do not use values for this test."),
                       #       tags$p("This particular test has not been fully developed yet, and the values it produces should not be trusted. In time, this test will be fully developed and made available.")
                       #       ),
                       #     textOutput("sample_size")
                         # )
                       # )
              )
  )
)

# Define server logic to read selected file ===============================================
server <- function(input, output, session) {
  
  # CLUSTER SAMPLING SERVER ===============================================================
  # Create downloadable data
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste("SamplingFrameTemplate.csv")
    },
    content = function(file) {
      file.copy("/srv/shiny-server/sampling_app/test.csv", file)
    }
  )
  
  
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
  
  # Data table to render
  df <- reactive({
    
    req(input$file1)
    # req(input$num_clusters)
    # req(input$tot_sample)
    # req(input$randomStart)
    
    df <- read.csv(input$file1$datapath,
                   # header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    df$cumulative <- NA
    df$cumulative[1] <- df$population[1]
    df$cumulative0 <- NA
    df$cumulative0 <- 0
    
    df$selected <- 0
    
    
    
    for (row in 2:nrow(df)){
      df$cumulative[row] <- (df$population[row] + df$cumulative[row - 1]);
      df$cumulative0[row] <- (df$cumulative[row - 1])
    }
    
    return(df)
  })
  
  # Generate the sampling interval
  sampInt <- reactive({
    req(input$num_clusters)
    req(input$file1)
    data2 <- df()
    x <- (sum(data2$population) / input$num_clusters)
    return(x)
  })
  
  # Render the sampling interval
  output$sampIntRendered <- renderText({
    req(input$file1)
    req(input$num_clusters)
    paste0("Sample Interval: ", sampInt())
  })
  output$totalSample <- renderText({
    req(input$file1)
    req(input$num_clusters)
    req(input$cluster_size)
    paste0("Total sample size: ", (input$num_clusters * input$cluster_size))
  })
  
  observeEvent(input$gen_random, {
    req(input$file1)
    req(input$num_clusters)
    # req(input$tot_sample)
    sampling_interval <- as.numeric(sampInt())
    randStartValue <- runif(1, min = 0, max = runif(1, min = 0, max = sampling_interval))
    updateNumericInput(
      session,
      "randomStart",
      label = paste0("Random Start (Suggested ", randStartValue, ")"),
      value = randStartValue,
      min = 0,
      max = sampling_interval
    )
  })
  
  # If the 'random start' entered is higher than the sampling interval, show a warning
  output$startWarn <- renderText({
    req(input$randomStart)
    if (input$randomStart > sampInt()) {paste0("Warning! The random start value you entered (", input$randomStart, ") is higher than the sampling interval (", sampInt(), "). The application will not work properly!")}
  })
  
  # Use the sampling interval to select the clusters
  
  df2 <- reactive({
    # To prevent errors, require that objects are not missing
    req(input$randomStart)
    req(input$file1)
    
    # Create a starting value for interval status
    intervalStatus <- input$randomStart
    sampling_Interval <- as.numeric(sampInt())
    
    data2 <- df()
    
    # number selected variable
    data2$num_selected
    
    # loop through each row and check if the interval status falls within the population
    for (row in 1:nrow(data2)) {
      # Selection based on random start & sampling interval
      if (data2$cumulative[row] >= intervalStatus & data2$cumulative0[row] < intervalStatus) {
        # Create a while loop to for sampling with replacement of clusters
        while (data2$cumulative[row] >= intervalStatus & data2$cumulative0[row] < intervalStatus) {
          data2$selected[row] <- 1 + data2$selected[row]
          intervalStatus <- (intervalStatus + sampInt())
        }
        
        # for primary sampling units that selected 1 or more times, this will count the
        # number of USU's in the cluster
        data2$num_selected <- input$cluster_size * data2$selected
        
        # Primary sampling units with a population that is smaller than the cluster size will
        # need to have neighbors sampled.
        
        grp_size <- data2$population[row] # keeps track of the total group size
        grp_clusters <- 1 # keeps track of the number of clusters in the group
        
        # This while loop will keep adding primary sampling units to the cluster until the 
        # group of primary sampling units has a population as large or larger than the Cluster
        # size
        while(grp_size < input$cluster_size & ((row + grp_clusters) <= nrow(data2))) {
          grp_clusters <- grp_clusters + 1
          grp_size <- sum(data2$population[row:(row + grp_clusters - 1)])
          data2$num_selected[row:(row + grp_clusters - 1)] <- (input$cluster_size / grp_clusters)
          data2$selected[row:(row + grp_clusters - 1)] <- (1 / grp_clusters)
        }
        rm(grp_size, grp_clusters)
        
      }
    }
    # drop the cumulative0 variable
    drops <- c("cumulative0")
    data2 <- data2[, !(names(data2) %in% drops)]
    
    # return final data
    return(data2)
  })
  
  # Render the completed data table
  output$contents <- renderDataTable({df2()})
  
  meta_data <- reactive({
    meta.df <- read.csv("/srv/shiny-server/sampling_app/data/dataframe.csv")
    meta.df$num_clusters[1] <- input$num_clusters
    meta.df$cluster_size[1] <- input$cluster_size
    meta.df$sample_size[1] <- input$num_clusters * input$cluster_size
    meta.df$sampling_interval[1] <- sampInt()
    meta.df$random_start[1] <- input$randomStart
    
    # drop the cumulative0 variable
    drops <- c("row")
    meta.df <- meta.df[, !(names(meta.df) %in% drops)]
    
    return(meta.df)
  })
  
  # Create downloadable data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$num_clusters, "x", input$cluster_size, "ClusterSample_", Sys.Date(),".xlsx", sep ="")
    },
    content = function(file) {
      write.xlsx(df2(), file, sheetName = "Sample", row.names = FALSE);
      write.xlsx(meta_data(), file, sheetName = "Sample Info", row.names = FALSE, append = TRUE)
    }
  )
  
  # SAMPLE SIZE SERVER =====================================================================
  output$sample_size <- renderPrint({
    # Beginning of if block for the type of test
    
    ## For T-Test sample sizes
    if (input$test_type == "ttest") {
      # d <- cohen.ES(test = "t", size = "medium")$effect.size
      result <- power.t.test(power = input$power,
                             delta = (input$ttest_grp2 - input$ttest_grp1),
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
    
    # Calculate design effect
    if (input$deff != 1) {
      s_size <- input$deff * s_size
    }
    
    # Other adjustment
    s_size <- (s_size / input$adjustment)
    
    # Finite Population Correction
    if (isTruthy(input$fpc)) {
      s_size <- ((input$fpc * s_size) / (s_size + input$fpc - 1))
    }
    
    # Add response rate
    if (input$respRate < 1) {
      s_size <- ((s_size / input$respRate))
    }
    
    return(ceiling(s_size))
  })
  
}

# Run the app ----
shinyApp(ui, server)







