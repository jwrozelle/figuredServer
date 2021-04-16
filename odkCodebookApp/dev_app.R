
library(shiny)
library(openxlsx)
library(dplyr)
library(shinythemes)
library(stringr)


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
      # include notes
      checkboxInput("include_notes", "Include 'note' type rows in codebook", value = TRUE, width = NULL),
      checkboxInput("include_type", "Include 'Question Type' column in codebook", value = TRUE, width = NULL),
      #Button to download resulting data
      tags$hr(),
      #
      uiOutput('select_lang'),
      
      # Conditional formatting based 
      conditionalPanel("output.fileUploaded", # & input.tot_sample > 0",
                       downloadButton("downloadData", "Download Codebook")
                       )
    ),
    
    # Main panel for displaying outputs ===================================================
    mainPanel(
      
      # Explanation of the app.
      tags$h3("About Codebook.R"),
      
      # tags$p(textOutput("what")),
      
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
        tags$li("Upload your .xls or .xlsx odk form. For now, the survey worksheet must have columns called `type`, `name`, `relevant` and `calculation`. The choices tab must have columns labelled `list_name` and `name`. Both the survey and choices tab must have at least one column that includes `label` text. Named languages should have the `label::` prefix."),
        tags$li("Select options. If you have multiple languages, select one or more languages you would like to include in the codebook. If you do not select any language - all languages with `label` in the survey tab will be included in the codebook"),
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
  
  output$select_lang <- renderUI({
    
    #require that the file is uploaded
    req(input$file1)
    
    # read in survey
    survey <- read.xlsx(input$file1$datapath, "survey")
    
    # get survey names
    variables <- names(survey)
    rm(survey)
    
    # check to see if there are multiple languages
    labels_vector <- variables[grepl("^label*", variables)]
    
    languages <- str_remove(labels_vector, "label::")
    languages <- str_replace(languages, "label", "Unspecified")
    
    if (length(labels_vector) > 1) {
      
      labels_vector <- setNames(labels_vector, languages)
      # actual selection
      selectInput("language", 
                  "Select Language", 
                  labels_vector, 
                  selected = NULL, 
                  multiple = TRUE)
    }
    
    # 
    
  })
  
  
  # Data table to render
  df <- reactive({
    
    req(input$file1)
    
    if(is.null(input$file1)) 
      {return(NULL)}
    
    survey   <- read.xlsx(input$file1$datapath, "survey")
    choices  <- read.xlsx(input$file1$datapath, "choices")
    
    # get survey variable names
    variables <- names(survey)
    
    if (!is.null(input$language)) {
      selected_languages <- input$language
    } else {
      # make a vector of language labels
      selected_languages <- variables[grepl("^label*", variables)]
    }
    
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
    
    # Create a vector of selected languages
    
    languages.v <- str_remove(selected_languages, "label::")
    languages.v <- str_replace(languages.v, "label", "Unspecified")
    
    rm(survey, choices)
    
    # Order the codebook by index then choice code
    codebook <- codebook[with(codebook, order(indexNum, name.choice)), ]
    codebook$duplicated <- ifelse(duplicated(codebook$name.survey), 1, 0)
    
    # define variables to keep
    keepvars <- c("Question Type", "Variable", "Calculation", "Code", "Skip Pattern")
    
    # NARROW DOWN CODEBOOK #####
    # MULTIPLE LANGUAGES
    
    # create vectors that will be added in to the codebook variables to keep later
    choice_labels_format <- c()
    survey_labels_format <- c()
    
    for (lang in 1:length(selected_languages)) {
      # CHOICES
      # create choice label column name that matches merged codebook dataset, from selected language
      choice_labels_codebook <- paste0(selected_languages[lang], ".choice")
      # create a formatted choice label stored in a variable
      choiceLabel.format <- paste0("Choice Text (", languages.v[lang], ")")
      # Copy merged codebook variable to the formatted column name
      codebook[, choiceLabel.format] <- codebook[, choice_labels_codebook]
      # Create a vector of the formatted choice name, to be placed in keepvars later
      choice_labels_format <- append(choice_labels_format, choiceLabel.format)
      
      # SURVEY
      # create survey label column name that matches merged codebook dataset, from selected language
      survey_labels_codebook <- paste0(selected_languages[lang], ".survey")
      # create a formatted survey label stored in a variable
      surveyLabel.format <- paste0("survey Text (", languages.v[lang], ")")
      # Copy merged codebook variable to the formatted column name
      codebook[, surveyLabel.format] <- codebook[, survey_labels_codebook]
      # Remove multiple copies of survey language label
      codebook[, surveyLabel.format] <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook[, surveyLabel.format])
      # Create a vector of the formatted survey name, to be placed in keepvars later
      survey_labels_format <- append(survey_labels_format, surveyLabel.format)
      
      # REMOVE FROM MEMORY
      rm(choice_labels_codebook, 
         survey_labels_codebook, 
         choiceLabel.format,
         surveyLabel.format)
      
    }
    
    # remove duplicated information from the codebook
    codebook$qType <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$qType)
    codebook$calculation <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$calculation)
    # codebook$label.survey <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$label.survey)
    codebook$relevant <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$relevant)
    codebook$name.survey <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$name.survey)
    
    # EDIT THE KEEPVARS VECTOR BASED ON LANGUAGES
    # add selected language choice labels to the variables that are kept
    keepvars <- append(keepvars, choice_labels_format, after = 4)
    # add selected language survey labels to the variables that are kept
    keepvars <- append(keepvars, survey_labels_format, after = 2)
    
    # Remove objects no longer used
    rm(languages.v, choice_labels_format, survey_labels_format)
    # else {
    #   codebook$`Survey Text` <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$label.survey)
    #   codebook$Choice        <- codebook$label.choice
    #   
    #   keepvars <- c("Question Type", "Variable", "Survey Text", "Calculation", "Code", "Choice", "Skip Pattern")
    # }
    
    # remove question type if option selected
    if (input$include_type == FALSE) {
      keepvars <- keepvars[!keepvars %in% c("Question Type")]
    }
    
    # Remove notes if option is selected
    if (input$include_notes == FALSE) {
      codebook <- dplyr::filter(codebook, qType != "note")
    }
    
    # Readable names
    codebook$Variable                   <- codebook$name.survey
    codebook$`Question Type`            <- codebook$qType
    codebook$Calculation                <- codebook$calculation
    codebook$Code                       <- codebook$name.choice
    codebook$`Skip Pattern`             <- codebook$relevant
    
    # add in text that is selected
    
    append(keepvars, input$language, after = 2)
    
    codebook <- codebook[,keepvars]
    
    return(codebook)
  })
  
  # create conditional output based on whether the file has been uploaded
  output$fileUploaded <- reactive({
    return(!is.null(df()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  # output$what <- renderPrint({input$language})
  
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







