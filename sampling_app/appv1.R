
library(shiny)
library(xlsx)

# Define UI for data upload app ----
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
  
  # App title ----
  tags$h1("Cluster Sampling with data frame"),
  
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
      tags$hr(),
      
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
      tags$hr(),
      tags$h3("Sampling Design options"),
      numericInput("num_clusters", "Number of Primary Sampling Units", value = NULL, min = 0),
      numericInput("cluster_size", "Number of Ultimate Sampling Units in each cluster", value = NULL, min = 0),
      conditionalPanel("input.num_clusters > 0", # & input.tot_sample > 0",
        actionButton("gen_random", "Generate Random Start")),
      conditionalPanel("input.num_clusters > 0 & input.gen_random >= 1", #& input.tot_sample > 0",
                       numericInput("randomStart", "Random Start Number", value = NULL, min = 0)
      ),
      conditionalPanel("input.gen_random >= 1 & input.cluster_size >= 0",
      tags$hr(),
      downloadButton("downloadData", "Download Sample"))
    ),
    
    # Main panel for displaying outputs ===================================================
    mainPanel(
      
      # Explanation of the app.
      tags$p("This app is designed to do the sampling of the clusters for a two stage cluster sampling design. All that is needed is a .csv file (xlsx is not yet supported) with at least one column labeled 'population'. This column should contain the population or number of ultimate sampling units within each cluster or primary sampling unit."),
      tags$p("If the number of USUs in the PSU is smaller than the cluster size, the app will automatically sample from the next row(s) until there are sufficient USUs for a cluster."),
      tags$p("There are currently two limitations to this."),
      tags$li("First, the code currently only samples the next row in the dataset. So, if the loop gets to the end of the csv file and there are no following rows - the loop will stop, even if there are insufficient USUs available for the cluster size."),
      tags$li("The other limitation is that the loop is currently calculated only by counting the sum of the population from a group of PSU's until there are as many or more USUs than the cluster size. In rare cases, this may mean that more USU's may be assigned to a PSU than it has. For example, if the cluster size was 100, and one PSU had 80 and the following PSU had 30 USUs the loop would recognize that there are more than 100 USUs between the two of them, and assign half (50) of the cluster to be taken from the first (with a 80 USUs) and 50 to be taken from the next (with only 30)."),
      
      # download tempalte
      tags$h3("Example data"),
      downloadButton("downloadTemplate", "Download Template"),

      # Output: Data file ----
      tags$hr(),
      tags$h2(textOutput("startWarn")),
      textOutput("sampIntRendered"),
      textOutput("totalSample"),
      conditionalPanel("input.gen_random >= 1",
                       tags$hr(),
                       dataTableOutput("contents")
                       )
    )
    
  )
)

# Define server logic to read selected file ===============================================
server <- function(input, output, session) {
  
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
}

# Run the app ----
shinyApp(ui, server)







