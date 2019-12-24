

library(RMySQL)
library(dplyr)
library(lubridate)
library(scales)
library(shiny)
library(shinythemes)


# FUNCTIONS ==================================================================================================

# killing databases

killDbConnections <- function () {
  all_cons <- dbListConnections(MySQL())
  print(all_cons)
  for(con in all_cons)
    +  dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}

# proc functions
proc.freq = function(data, vari_col, indic, group_col) {
  require(dplyr)
  require(lazyeval)
  require(scales)
  the.percent <- interp(~ percent(mean(var, na.rm = TRUE)), var = as.name(vari_col))
  the.mean <- interp(~ mean(var, na.rm = TRUE), var = as.name(vari_col))
  the.sum <- interp(~ sum(var, na.rm = TRUE), var = as.name(vari_col))
  the.total <- interp(~ sum(!is.na(var)), var = as.name(vari_col))
  the.missing <- interp(~ sum(is.na(var)), var = as.name(vari_col))
  the.name  <- interp(~ var, var = as.name("indic"))
  if (!missing(indic)){  
    if (!missing(group_col)) {
      data %>% 
        group_by_(lazyeval::interp(~ grp, grp = as.name(group_col))) %>%
        summarise_(indicator = the.name,
                   percent = the.percent,
                   proportion = the.mean, 
                   n = the.sum, 
                   Total = the.total, 
                   Missing = the.missing) -> result
    } else {
      data %>%
        summarise_(indicator = the.name,
                   percent = the.percent,
                   Proportion = the.mean, 
                   n = the.sum, 
                   Total = the.total, 
                   Missing = the.missing) -> result
    }} else {
      if (!missing(group_col)) {
        data %>% 
          group_by_(lazyeval::interp(~ grp, grp = as.name(group_col))) %>%
          summarise_(percent = the.percent,
                     Proportion = the.mean, 
                     n = the.sum, 
                     Total = the.total, 
                     Missing = the.missing) -> result
      } else {
        data %>%
          summarise_(percent = the.percent,
                     Proportion = the.mean, 
                     n = the.sum, 
                     Total = the.total, 
                     Missing = the.missing) -> result
      }
    }
  
  return(result)
}

proc.means = function(data, vari_col, indic, group_col) {
    require(dplyr)
    require(lazyeval)
    require(scales)
    the.mean <- interp(~ mean(var, na.rm = TRUE), var = as.name(vari_col))
    the.median <- interp(~ median(var, na.rm = TRUE), var = as.name(vari_col))
    the.min <- interp(~ min(var, na.rm = TRUE), var = as.name(vari_col))
    the.max <- interp(~ max(var, na.rm = TRUE), var = as.name(vari_col))
    the.total <- interp(~ sum(!is.na(var)), var = as.name(vari_col))
    the.missing <- interp(~ sum(is.na(var)), var = as.name(vari_col))
    the.name  <- interp(~ var, var = as.name("indic"))
    if (!missing(indic)){  
      if (!missing(group_col)) {
        data %>% 
          group_by_(lazyeval::interp(~ grp, grp = as.name(group_col))) %>%
          summarise_(indicator = the.name,
                     mean = the.mean,
                     median = the.median,
                     min = the.min,
                     max = the.max,
                     Total = the.total, 
                     Missing = the.missing) -> result
      } else {
        data %>%
          summarise_(indicator = the.name,
                     mean = the.mean,
                     median = the.median,
                     min = the.min,
                     max = the.max,
                     Total = the.total, 
                     Missing = the.missing) -> result
      }} else {
        if (!missing(group_col)) {
          data %>% 
            group_by_(lazyeval::interp(~ grp, grp = as.name(group_col))) %>%
            summarise_(mean = the.mean,
                       median = the.median,
                       min = the.min,
                       max = the.max,
                       Total = the.total, 
                       Missing = the.missing) -> result
        } else {
          data %>%
            summarise_(mean = the.mean,
                       median = the.median,
                       min = the.min,
                       max = the.max,
                       Total = the.total, 
                       Missing = the.missing) -> result
        }
      }
    
    return(result)
  }

# month subset
monthSubset <- function (data, date_column, yearoi, monthoi) {
  require(dplyr)
  yyyymm <- as.Date(paste(yearoi, "-", monthoi, "-", 1, sep = ""))
  data_subset <- dplyr::filter(data, data[, paste(date_column)] >= yyyymm &
                                 data[, paste(date_column)] < (seq(
                                   yyyymm, by = "1 month", length = 2
                                 )[2]))
  return(data_subset)
}

# loop that checks to see if a value of a variable in one set is in another set

inloop <- function (xset, yset, xvar, yvar) {
  loop_counter <- 1
  xset[, "placeholder"] <- NA
  for (row in 1:nrow(xset)){
    if(xset[row, paste(xvar)] %in% yset[, paste(yvar)]){
      xset[loop_counter, "placeholder"] <- 1
    } else {
      xset[loop_counter, "placeholder"] <- 0
    }
    loop_counter <- loop_counter + 1
  }
  rm(loop_counter)
  return(xset[, "placeholder"])
}

# SCHEMA CONNECTIONS =========================================================================================

lmh_user <- Sys.getenv("lmh_user")
lmh_host <- Sys.getenv("lmh_host")
lmh_port <- as.integer(Sys.getenv("lmh_port"))
lmh_pwd  <- Sys.getenv("lmh_pwd")

cha.con <- DBI::dbConnect(
  MySQL(),
  user = lmh_user,
  host = lmh_host,
  port = lmh_port,
  password = lmh_pwd,
  dbname = "lastmile_cha"
)

upload.con <-DBI::dbConnect(
  MySQL(),
  user = lmh_user,
  host = lmh_host,
  port = lmh_port,
  password = lmh_pwd,
  dbname = "lastmile_upload"
)

# TABLE DOWNLOADS ============================================================================================

# view_position_cha_supervisor
pos_sup.sqlr <- dbSendQuery(cha.con, 'SELECT * FROM view_position_cha_supervisor')
pos_sup.df <- fetch(pos_sup.sqlr, n=-1)
dbClearResult(dbListResults(cha.con)[[1]])
rm(pos_sup.sqlr)

# view_position_chss_person_geo
chss_pos.sqlr <- dbSendQuery(cha.con, 'SELECT * FROM view_position_chss_person_geo')
chss_pos.df <- fetch(chss_pos.sqlr, n=-1)
dbClearResult(dbListResults(cha.con)[[1]])
rm(chss_pos.sqlr)

# de_chss_monthly_service_report
chss_up.sqlr <- dbSendQuery(upload.con, 'SELECT * FROM de_chss_monthly_service_report')
chss_up.df <- fetch(chss_up.sqlr, n=-1)
dbClearResult(dbListResults(upload.con)[[1]])
rm(chss_up.sqlr)

# de_chss_monthly_service_report
cha_up.sqlr <- dbSendQuery(upload.con, 'SELECT * FROM de_cha_monthly_service_report')
cha_up.df <- fetch(cha_up.sqlr, n=-1)
dbClearResult(dbListResults(upload.con)[[1]])
rm(cha_up.sqlr)

# lastmile_upload.odk_chaRestock
chaRestock.sqlr <- dbSendQuery(upload.con, 'SELECT * FROM odk_chaRestock')
chaRestock.df <- fetch(chaRestock.sqlr, n=-1)
dbClearResult(dbListResults(upload.con)[[1]])
rm(chaRestock.sqlr)

# de_chss_commodity_distribution
chssRestock.sqlr <- dbSendQuery(upload.con, 'SELECT * FROM de_chss_commodity_distribution')
chssRestock.df <- fetch(chssRestock.sqlr, n=-1)
dbClearResult(dbListResults(upload.con)[[1]])
rm(chssRestock.sqlr)

# ODK Supervision visit log
odk_sup.sqlr <- dbSendQuery(upload.con, 'SELECT * FROM odk_supervisionVisitLog')
odk_sup.df <- fetch(odk_sup.sqlr, n=-1)
dbClearResult(dbListResults(upload.con)[[1]])
rm(odk_sup.sqlr)


killDbConnections()
rm(cha.con, upload.con)

# Recode data  ===============================================================================================

## Formatting to Dates
chaRestock.df$sub_date <- as.Date(chaRestock.df$meta_autoDate, "%Y-%m-%d")
chaRestock.df$res_date <- as.Date(chaRestock.df$manualDate, "%Y-%m-%d")
cha_up.df$sub_date <- as.Date(cha_up.df$meta_de_date, "%Y-%m-%d")
chssRestock.df$res_date <- as.Date(chssRestock.df$restock_date, "%Y-%m-%d")
odk_sup.df$manualDate <- as.Date(odk_sup.df$manualDate, "%Y-%m-%d")

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
  tags$h1("CHSS Scorecard"),
  sidebarLayout(sidebarPanel(
    sliderInput(inputId = "month_oi", "Select Month", min = 1, max = 12, value = 10, animate = TRUE),
    selectInput(inputId = "year_oi", label = "Select Year",
                choices = c("2019" = 2019,
                            "2018" = 2018,
                            "2017" = 2017,
                            "2016" = 2016,
                            "2015" = 2015
                )
    ),
    selectInput(inputId = "chss_id", label = "Select the CHSS id of interest",
                choices = c("4CB0-001" = "4CB0-001",
                            "4CB0-002" = "4CB0-002",
                            "4CB0-003" = "4CB0-003",
                            "3FZ7-001" = "3FZ7-001",
                            "3K07-001" = "3K07-001",
                            "3K07-002" = "3K07-002",
                            "4PB0-001" = "4PB0-001",
                            "4SB0-001" = "4SB0-001",
                            "4SB0-002" = "4SB0-002",
                            "4WK0-001" = "4WK0-001",
                            "4X40-001" = "4X40-001",
                            "8832-001" = "8832-001",
                            "88Y2-001" = "88Y2-001",
                            "88Y2-002" = "88Y2-002",
                            "8GK2-001" = "8GK2-001",
                            "92B5-001" = "92B5-001",
                            "92B5-002" = "92B5-002",
                            "92B5-003" = "92B5-003",
                            "92B5-004" = "92B5-004",
                            "92B5-005" = "92B5-005",
                            "9M25-001" = "9M25-001",
                            "A8G8-001" = "A8G8-001",
                            "ALJ8-001" = "ALJ8-001",
                            "AX38-001" = "AX38-001",
                            "BB01-001" = "BB01-001",
                            "BYQ1-001" = "BYQ1-001",
                            "BYQ1-002" = "BYQ1-002",
                            "BYQ1-003" = "BYQ1-003",
                            "DR27-001" = "DR27-001",
                            "DWR7-001" = "DWR7-001",
                            "DZD7-001" = "DZD7-001",
                            "DZD7-002" = "DZD7-002",
                            "FNV3-001" = "FNV3-001",
                            "GER6-001" = "GER6-001",
                            "JTP5-001" = "JTP5-001",
                            "K3C8-001" = "K3C8-001",
                            "LTM1-001" = "LTM1-001",
                            "LTM1-002" = "LTM1-002",
                            "MZ14-001" = "MZ14-001",
                            "NE37-001" = "NE37-001",
                            "NW07-001" = "NW07-001",
                            "P1E3-001" = "P1E3-001",
                            "P1E3-002" = "P1E3-002",
                            "Q7P6-001" = "Q7P6-001",
                            "QU46-001" = "QU46-001",
                            "S0T2-001" = "S0T2-001",
                            "SQB2-001" = "SQB2-001",
                            "TC25-001" = "TC25-001",
                            "UTY8-001" = "UTY8-001",
                            "VBU1-001" = "VBU1-001",
                            "X6F7-001" = "X6F7-001",
                            "YEK0-001" = "YEK0-001",
                            "YX80-001" = "YX80-001",
                            "ZKV3-001" = "ZKV3-001",
                            "ZZZZ-001" = "ZZZZ-001",
                            "ZZZZ-002" = "ZZZZ-002",
                            "ZZZZ-003" = "ZZZZ-003",
                            "ZZZZ-004" = "ZZZZ-004",
                            "ZZZZ-005" = "ZZZZ-005"
                            
                )
    )),
    mainPanel(
      tags$h2(textOutput("name")),
      tags$h2(textOutput("county")),
      tags$h2(textOutput("healthfacility")),
      tags$h2(textOutput("super_p")),
      tags$h2(textOutput("odk_super_p")),
      tags$h2(textOutput("report_p")),
      tags$h2(textOutput("stock_p")),
      tags$h2(textOutput("rec_restock"))
      
    ))
  )


# Server ===================================================================================================

server <- function(input, output) {
  
  msr_due_date <- reactive({  
    # set the msr due date
    if(input$month_oi != 12) {
      msr_due_date <- (as.Date(paste(input$year_oi, "-", input$month_oi+1, "-", 18, sep = ""), "%Y-%m-%d"))
    } else {
      msr_due_date <- (as.Date(paste(input$year_oi+1, "-", 1, "-", 18, sep = ""), "%Y-%m-%d"))
    }
    msr_due_date
  })
  
  # mmyyy_oi_fix <- reactive({
  #   if(input$month_oi != 12) {
  #     mmyyy_oi_fix <- (as.Date(paste(input$year_oi, "-", input$month_oi+1, "-", 1, sep = ""), "%Y-%m-%d"))
  #   } else {
  #     mmyyy_oi_fix <- (as.Date(paste(input$year_oi+1, "-", 1, "-", 1, sep = ""), "%Y-%m-%d"))
  #   }
  #   mmyyy_oi_fix
  # })
  
  dataInput <- reactive({
    # create a copy of pos_sup (from SQL) to dump new information into
    cha_level.df <- pos_sup.df
    
    
    
    # Filter restock records from only the last month, asign it to new dataset cha_restock.df
    cha_restock.df <- monthSubset(chaRestock.df, "res_date", input$year_oi, input$month_oi)
    
    # Run my inloop function over the restock form
    cha_level.df$curr_restock <- inloop(cha_level.df, cha_restock.df, "position_id", "chaID")
    # loop_counter = 1
    # for (row in 1:nrow(cha_level.df)){
    #   if((cha_level.df$'position_id'[row] %in% cha_restock.df$'chaID')){
    #     cha_level.df$curr_restock[loop_counter] <- 1
    #   } else {
    #     cha_level.df$curr_restock[loop_counter] <- 0
    #   }
    #   loop_counter <- loop_counter + 1
    # }
    # rm(loop_counter)
    
    # remove out of use datasets
    rm(cha_restock.df)
    
    # filter CHA MSR records from only the last month
    cha_msr.df <- dplyr::filter(cha_up.df, month_reported == input$month_oi & year_reported == input$year_oi)
    
    
    # For each cha, check if their ID shows up in the last cha_msr.df (created above) AND if the data was
    # entered on or before the msr due date of the current month
    
    cha_level.df$curr_report <- inloop(cha_level.df, cha_msr.df, "position_id","cha_id")
    
    # loop_counter = 1
    # for (row in 1:nrow(cha_level.df)){
    #   if((cha_level.df$'position_id'[row] %in% cha_msr.df$cha_id) && cha_msr.df$sub_date <= msr_due_date()){
    #     cha_level.df$curr_report[loop_counter] <- 1
    #   } else {
    #     cha_level.df$curr_report[loop_counter] <- 0
    #   }
    #   loop_counter <- loop_counter + 1
    # }
    # rm(loop_counter)
    
    # remove out of use datasets
    rm(cha_msr.df)
    
    # Subset odk supervision forms by month of interest
    odk_sup_sub.df <- monthSubset(odk_sup.df, "manualDate", input$year_oi, input$month_oi)
    
    # inloop to check if cha's show up in the supervision form
    cha_level.df$odk_sup <- inloop(
      xset = cha_level.df,
      yset = odk_sup_sub.df,
      xvar = "position_id",
      yvar = "supervisedCHAID"
    )
    
    
    ## CHSS LEVEL =================================================================================================
    
    # Summarize to CHSS level
    chss_level.df <- cha_level.df %>% group_by(position_supervisor_id) %>% summarise(num_chas = n(), sum_cha_report = sum(curr_report), sum_cha_restock = sum(curr_restock), sum_odk_sup = sum(odk_sup))
    
    # Join newly calculated chss level indicators to CHSS position information
    chss_report.df <- merge(chss_pos.df, chss_level.df, by.x = "position_id", by.y = "position_supervisor_id")
    
    # Drop out of use datasets
    rm(cha_level.df, chss_level.df)
    
    # Filter out chssRestock.df to only the month of interest
    chss_res.df <- monthSubset(chssRestock.df, "res_date", input$year_oi, input$month_oi)
    # chss_res.df <-
    #   dplyr::filter(chssRestock.df, res_date < mmyyy_oi_fix() &
    #            res_date >= (seq(
    #              mmyyy_oi_fix(), by = "-1 month", length = 2
    #            )[2]))
    
    
    # For each CHSS, check if their ID shows up in the last chss_res.df (created above), assuming that means they
    # recieved a restock in the last month
    
    chss_report.df$curr_restock <- inloop(chss_report.df, chss_res.df, "position_id", "chss_id")
    
    # loop_counter = 1
    # for (row in 1:nrow(chss_report.df)){
    #   if((chss_report.df$'position_id'[row] %in% chss_res.df$'chss_id')){
    #     chss_report.df$curr_restock[loop_counter] <- 1
    #   } else {
    #     chss_report.df$curr_restock[loop_counter] <- 0
    #   }
    #   loop_counter <- loop_counter + 1
    # }
    # rm(loop_counter)
    
    # Drop out of use datasets
    rm(chss_res.df)
    
    
    # filter CHA MSR records from only the last month
    chss_msr.df <- dplyr::filter(chss_up.df, month_reported == input$month_oi & year_reported == input$year_oi)
    
    # create a numeric vector of the column numbers with CHA ID's
    chaIDs.v <- c(23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53, 56, 59, 62)
    
    loop_counter = 1
    for (row in 1:nrow(chss_report.df)) {
      if((chss_report.df$'position_id'[row] %in% chss_msr.df$'chss_id')){
        chss_up_f.df <- dplyr::filter(chss_msr.df, chss_id == chss_report.df$'position_id'[row])
        chss_report.df$num_suped[loop_counter] <- sum(!is.na(chss_up_f.df[, chaIDs.v]))
        rm(chss_up_f.df)
      } else {
        chss_report.df$num_suped[loop_counter] <- NA
      }
      loop_counter <- loop_counter + 1
    }
    rm(loop_counter)
    
    rm(chss_msr.df, chss_up_f.df)
    
    chss_report.df$super <- (chss_report.df$num_suped / chss_report.df$num_chas)
    chss_report.df$report <- (chss_report.df$sum_cha_report / chss_report.df$num_chas)
    chss_report.df$stock <- (chss_report.df$sum_cha_restock / chss_report.df$num_chas)
    chss_report.df$odk_super <- (chss_report.df$sum_odk_sup / chss_report.df$num_chas)
    chss_report.df$rec_restock <- NA
    chss_report.df$rec_restock <- ifelse(chss_report.df$curr_restock == 1, "Yes", chss_report.df$rec_restock)
    chss_report.df$rec_restock <- ifelse(chss_report.df$curr_restock == 0, "No", chss_report.df$rec_restock)
    
    chss_report.df$super_p <- scales::percent(chss_report.df$super)
    chss_report.df$report_p <- scales::percent(chss_report.df$report)
    chss_report.df$stock_p <- scales::percent(chss_report.df$stock)
    chss_report.df$odk_super_p <- scales::percent(chss_report.df$odk_super)
    
    
    return(chss_report.df)
  })
  
  
  
  # finalInput <- reactive({
  #   if (!input$adjust) return(dataInput())
  #   adjust(dataInput())
  # })

  output$name <- renderText({
    dataset <- dataInput()
    dataset <- dplyr::filter(dataset, position_id == as.character(input$chss_id))
    #rownumber <- which(dataset$position_id == input$chss_id)
    paste("Name: [currently redacted] ", input$chss_id, " "#,
          #dataset$last_name[1], sep = ""
          )
  })
  
  output$county <- renderText({
    dataset <- dataInput()
    dataset <- dplyr::filter(dataset, position_id == as.character(input$chss_id))
    #rownumber <- which(dataset$position_id == input$chss_id)
    paste("County: ", dataset$county[1], sep = "")
  })

  output$healthfacility <- renderText({
    dataset <- dataInput()
    dataset <- dplyr::filter(dataset, position_id == as.character(input$chss_id))
    #rownumber <- which(dataset$position_id == input$chss_id)
    paste("Health Facility: ", dataset$health_facility[1], sep = "")
  })
  
  output$super_p <- renderText({
    dataset <- dataInput()
    dataset <- dplyr::filter(dataset, position_id == as.character(input$chss_id))
    #rownumber <- which(dataset$position_id == input$chss_id)
    paste("Supervision Percent (Paper): ", dataset$super_p[1], "  (", dataset$num_suped, " / ", dataset$num_chas, ")", sep = "")
  })
  
  output$odk_super_p <- renderText({
    dataset <- dataInput()
    dataset <- dplyr::filter(dataset, position_id == as.character(input$chss_id))
    #rownumber <- which(dataset$position_id == input$chss_id)
    paste("Supervision Percent (ODK): ", dataset$odk_super_p[1], "    (", dataset$sum_odk_sup, " / ", dataset$num_chas, ")", sep = "")
  })
  
  output$report_p <- renderText({
    dataset <- dataInput()
    dataset <- dplyr::filter(dataset, position_id == as.character(input$chss_id))
    #rownumber <- which(dataset$position_id == input$chss_id)
    paste("Report Percent: ", dataset$report_p[1], "    (", dataset$sum_cha_report, " / ", dataset$num_chas, ")", sep = "")
  })
  
  output$stock_p <- renderText({
    dataset <- dataInput()
    dataset <- dplyr::filter(dataset, position_id == as.character(input$chss_id))
    #rownumber <- which(dataset$position_id == input$chss_id)
    paste("CHA Restock Percent (ODK): ", dataset$stock_p[1], "    (", dataset$sum_cha_restock, " / ", dataset$num_chas, ")", sep = "")
  })
  
  output$rec_restock <- renderText({
    dataset <- dataInput()
    dataset <- dplyr::filter(dataset, position_id == as.character(input$chss_id))
    #rownumber <- which(dataset$position_id == input$chss_id)
    paste("Recieved Restock: ", dataset$rec_restock[1], sep = "")
  })  
  
}


# shinyApp ==================================
shinyApp(ui = ui, server = server)








