
library(shiny)
library(openxlsx)
library(dplyr)
library(shinythemes)


# Define UI for data upload app ----
ui <- fluidPage(
  theme = shinytheme("yeti"),
  tags$head(
    
    HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
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
  tags$h1("Codebook.R"),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      tags$h3("Import options for ODK xls"),
      fileInput("file1", "Choose .xls or .xlsx File",
                multiple = TRUE,
                accept = c(".xls",
                           ".xlsx")),
      #Button to download resulting data
      tags$hr(),
      downloadButton("downloadData", "Download Codebook")
    ),
    
    # Main panel for displaying outputs ===================================================
    mainPanel(
      
      # Explanation of the app.
      tags$h3("About Codebook.R"),
      tags$p("I create a lot of surveys in ODK, and usually do so by creating the form first in excel, and then uploading to a kobo server or ONA. I often, then, later have a need to create a single document with a codebook. There were often times I used the Nafundi and Task Force created ", tags$a(href = "https://opendatakit.org/2013/06/task-force-and-nafundi-release-codebook-generator-for-odk/", "LINKS Codebook Generator"), ". However, I found that was often insufficient for my needs. This was for several reasons. More sophisticated calculations, like `pulldata()`, broke it. Additionally, I couldn't seek skip patterns or calculations. Finally - it output into pdf format, which made edits or rearranging more challenging."),
      tags$p("Since I always start my XFORMS in Excel, it made sense to simply create an app that would take an xls form and turn it into a single sheet codebook. So, I wrote a few lines of R code and made this!"),
      tags$p("This is a simple project, but open source. You can find the code for this and other apps ", tags$a(href="https://github.com/jwilliamrozelle/figuredServer", "here"), " . Currently, this only takes xlsx forms. If you develop your forms in Excel, you can upload it directly here. If you work in kobotoolbox or ONA, you should be able to download your work as an xlsx file and do the same. At some point, I may add support for XForms in XML, but the parser for that will add a considerable level of sophistication that I just don't need or use. If that is of any interest to you, comment on my github or send me a message there."),
      tags$h4("Known limitations"),
      tags$ul(
        tags$li("You cannot use a file with multiple languages. Currently, only the `label`column will be recognized."),
        tags$li("There may be versions of ODK that the current codebook maker will not work for.")
      ),
      tags$p("Both of these issues should be infrequent, but will be addressed in future versions. Find the source code for this app under a GNU license at https://github.com/jwilliamrozelle/figuredio."),
      tags$h3("Instructions"),
      tags$ol(
        tags$li("Upload your .xls or .xlsx odk form. For now, the survey worksheet must have columns called `label`, `type`, `name`, `relevant` and `calculation`. The choices tab must have columns labelled `list_name`, `name`, and `label`."),
        tags$li("Download the generated sample in xlsx format by clicking 'Download Codebook'.")
      )
    ),
    position = "left"
  )
)


# Define server logic to read selected file ===============================================
server <- function(input, output, session) {
  
  # CLUSTER SAMPLING SERVER ===============================================================
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
  
  # Data table to render
  df <- reactive({
    
    req(input$file1)
    
    survey   <- read.xlsx(input$file1$datapath, "survey")
    choices  <- read.xlsx(input$file1$datapath, "choices")
    
    survey$qType <- strsplit(survey$type, "\\s+")
    
    survey$qType <- sapply(survey$type, function(x) strsplit(x, "\\s+")[1])
    survey$qType <- sapply(survey$qType, function(x) x[1])
    
    survey$choiceName <- sapply(survey$type, function(x) strsplit(x, "\\s+"))
    survey$choiceName <- sapply(survey$choiceName, function(x) x[2])
    
    survey$indexNum[1] <- 1
    for(row in 2:nrow(survey)) {
      survey$indexNum[row] <- survey$indexNum[row-1]+1
    }
    
    # Remove unused choices
    choices$utilized <- NA
    choices$utilized <- ifelse(choices$list_name %in% survey$choiceName, 1, 0)
    choices <- filter(choices, utilized == 1)
    
    
    # MERGE #####
    codebook <- merge(survey, choices, by.x = "choiceName", by.y = "list_name", all = TRUE, suffixes = c(".survey",".choice"))
    
    rm(survey, choices)
    
    # Order the codebook by index then choice code
    codebook <- codebook[with(codebook, order(indexNum, name.choice)), ]
    codebook$duplicated <- ifelse(duplicated(codebook$name.survey), 1, 0)
    
    # remove duplicated information from the codebook
    codebook$qType <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$qType)
    codebook$calculation <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$calculation)
    codebook$label.survey <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$label.survey)
    codebook$relevant <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$relevant)
    codebook$name.survey <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$name.survey)
    
    
    # NARROW DOWN CODEBOOK #####
    codebook$Variable                   <- codebook$name.survey
    codebook$`Question Type`            <- codebook$qType
    codebook$`Text / Calculation`       <- NA
    codebook$`Text / Calculation`       <- ifelse(codebook$qType == "calculate", codebook$calculation, codebook$label.survey)
    codebook$Code                       <- codebook$name.choice
    codebook$Choice                     <- codebook$label.choice
    codebook$`Skip Pattern`             <- codebook$relevant
    
    keepvars <- c("Question Type", "Variable", "Text / Calculation", "Code", "Choice", "Skip Pattern")
    codebook <- codebook[,keepvars]
    
    return(codebook)
  })
  
  # Create downloadable data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("odkCodebook_", Sys.Date(),".xlsx", sep ="")
    },
    content = function(file) {
      outputwb <- createWorkbook()
      
      addWorksheet(outputwb, "codebook")
      
      writeData(outputwb, "codebook", df())
      freezePane(outputwb, "codebook", firstRow = TRUE)
      
      saveWorkbook(outputwb, file, TRUE)
    }
  )
  
}

# Run the app ----
shinyApp(ui, server)







