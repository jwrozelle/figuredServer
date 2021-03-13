
library(shiny)
library(ggplot2)
library(tidyverse)
#library(srvyr)
library(survey)
library(haven)
library(DT)

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

# calculate weights
#child.df$weights <- child.df$hv005/1000000

# Survey Design
## child.design <- as_survey_design(child.df, ids = hv021, strata = hv022, weights = weights)
child.design <- svydesign(id = child.df$hv021, strata= child.df$hv022, weights = child.df$hv005/1000000, data=child.df)



# out <- child.design %>%
#   group_by(hv270) %>%
#   summarize(weighted_v = survey_mean(na.rm=TRUE, stunt, vartype = "ci"),
#             n = unweighted(n()))
# 
# weighted_p <- ggplot(data = out, aes(x = hv270, y = weighted_v)) +
#   geom_bar(stat = "identity", position = "dodge", fill = "#546f42") +
#   geom_errorbar(ymax = out$weighted_v_upp, ymin = out$weighted_v_low, width = 0.2) +
#   geom_text(aes(y = 0, label = scales::percent(weighted_v)), position = position_dodge(width = 0.9), vjust = -1)
# weighted_p <- weighted_p + ylim(c(0,1))
# weighted_p <- weighted_p + theme_classic()
# weighted_p





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
  tags$p("*Weighted analysis of DHS data from Tanzania, "),
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
            plotOutput("whz_bar"),
            tags$hr(),
            tags$h2("Table"),
            DT::dataTableOutput("table")
            )
  ),
  tags$hr()
  
)

# server ====================================
server <- function(input, output) {
  output$stunt_bar <- renderPlot({
    if (input$disagg != "1") {
      disagg.f <- formula(paste0("~", input$disagg))
    out <-
      svyby( ~stunt,
             disagg.f,
             child.design,
             svyciprop,
             vartype = "ci")
    } else {
      out.ci <- svyciprop(~stunt, child.design, level = 0.95)
      out.mean <- mean(out.ci, na.rm=TRUE)
      out.confint <- confint(out.ci)
      out <- cbind(out.mean, out.confint)
      rm(out.ci, out.mean, out.confint)
      out <- as.data.frame(out)
      out$ci_u <- out$`97.5%`
      out$ci_l <- out$`2.5%`
      out$stunt <- out$out.mean
    }
    
    weighted.bar <- ggplot(data = out, aes_string(x = input$disagg))
    weighted.bar <- weighted.bar + aes(y = stunt)
    weighted.bar <- weighted.bar + geom_bar(stat = "identity", position = "dodge", fill = "#546f42")
    weighted.bar <- weighted.bar + geom_errorbar(ymax = out$ci_u, ymin = out$ci_l, width = 0.2, size = 1.5)
    weighted.bar <- weighted.bar + geom_text(aes(y = 0, label = scales::percent(stunt)), position = position_dodge(width = 0.9), vjust = -1)
    if (input$set100 == "1") {weighted.bar <- weighted.bar + ylim(c(0,1))}
    weighted.bar <- weighted.bar + theme_classic()
    weighted.bar
    })    
    
  output$whz_bar <- renderPlot({
    if (input$disagg != "1") {
      disagg.f <- formula(paste0("~", input$disagg))
      out <-
        svyby( ~whz,
               disagg.f,
               child.design,
               svyciprop,
               vartype = "ci")
    } else {
      out.ci <- svyciprop(~whz, child.design, level = 0.95)
      out.mean <- mean(out.ci, na.rm=TRUE)
      out.confint <- confint(out.ci)
      out <- cbind(out.mean, out.confint)
      rm(out.ci, out.mean, out.confint)
      out <- as.data.frame(out)
      out$ci_u <- out$`97.5%`
      out$ci_l <- out$`2.5%`
      out$whz <- out$out.mean
    }
    
    weighted.bar <- ggplot(data = out, aes_string(x = input$disagg))
    weighted.bar <- weighted.bar + aes(y = whz)
    weighted.bar <- weighted.bar + geom_bar(stat = "identity", position = "dodge", fill = "#546f42")
    weighted.bar <- weighted.bar + geom_errorbar(ymax = out$ci_u, ymin = out$ci_l, width = 0.2, size = 1.5)
    weighted.bar <- weighted.bar + geom_text(aes(y = 0, label = scales::percent(whz)), position = position_dodge(width = 0.9), vjust = -1)
    if (input$set100 == "1") {weighted.bar <- weighted.bar + ylim(c(0,1))}
    weighted.bar <- weighted.bar + theme_classic()
    weighted.bar
  })
  
  output$table = DT::renderDataTable({
    if (input$disagg != "1") {
      disagg.f <- formula(paste0("~", input$disagg))
      whz.dt <-
        svyby( ~whz,
               disagg.f,
               child.design,
               svyciprop,
               vartype = "ci")
      stunt.dt <-
        as.data.frame(svyby( ~stunt,
               disagg.f,
               child.design,
               svyciprop,
               vartype = "ci"))
      (table <- base::merge(stunt.dt, whz.dt, by = input$disagg, all = TRUE))
      
    } else {
      whzdt.ci <- svyciprop(~whz, child.design, level = 0.95)
      whzdt.mean <- mean(whzdt.ci, na.rm=TRUE)
      whzdt.confint <- confint(whzdt.ci)
      whzdt <- cbind(whzdt.mean, whzdt.confint)
      rm(whzdt.ci, whzdt.mean, whzdt.confint)
      whzdt <- as.data.frame(whzdt)
      whzdt$indicator <- "WHZ"
      
      stuntdt.ci <- svyciprop(~stunt, child.design, level = 0.95)
      stuntdt.mean <- mean(stuntdt.ci, na.rm=TRUE)
      stuntdt.confint <- confint(stuntdt.ci)
      stuntdt <- cbind(stuntdt.mean, stuntdt.confint)
      rm(stuntdt.ci, stuntdt.mean, stuntdt.confint)
      stuntdt <- as.data.frame(stuntdt)
      stuntdt$indicator <- "HAZ"
      
      # Merge the two dataframes into a single data table for display
      (table <- base::merge(stuntdt, whzdt, all = TRUE))
    }
  })
  
      
}



# shinyApp ==================================
shinyApp(ui = ui, server = server)






