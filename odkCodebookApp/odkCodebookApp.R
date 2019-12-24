
library(openxlsx)
library(dplyr)

survey <- read.xlsx("C:\\Users\\jwill\\Downloads\\LMSGeneralSurvey.xlsx", "survey")
choices <- read.xlsx("C:\\Users\\jwill\\Downloads\\LMSGeneralSurvey.xlsx", "choices")

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
codebook$`label::English.survey` <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$`label::English.survey`)
codebook$relevant <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$relevant)
codebook$name.survey <- ifelse(codebook$duplicated == 1 & !is.na(codebook$name.survey), NA, codebook$name.survey)


# NARROW DOWN CODEBOOK #####
codebook$Variable                   <- codebook$name.survey
codebook$`Question Type`            <- codebook$qType
codebook$`Text / Calculation`       <- NA
codebook$`Text / Calculation`       <- ifelse(codebook$qType == "calculate", codebook$calculation, codebook$`label::English.survey`)
codebook$Code                       <- codebook$name.choice
codebook$Choice                     <- codebook$`label::English.choice`
codebook$`Skip Pattern`             <- codebook$relevant

keepvars <- c("Question Type", "Variable", "Text / Calculation", "Code", "Choice", "Skip Pattern")
codebook <- codebook[,keepvars]

outputwb <- createWorkbook()

addWorksheet(outputwb, "codebook")

writeData(outputwb, "codebook", codebook)
freezePane(outputwb, "codebook", firstRow = TRUE)

saveWorkbook(outputwb, "C:\\Users\\jwill\\Documents\\LMH Transfer\\ODK Codebooks\\General.xlsx", TRUE)


# variance covariance estimator "huber-white sandwich estimator"

# to tell stata you're using binomial glm link(idenity) fam(bin)














