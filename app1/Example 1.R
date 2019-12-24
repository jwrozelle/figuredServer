# DATA LOCATIONS #################################################################################

baseFolder <- "."
outputFolder <- "."

edrought.df <- read.csv(paste(baseFolder, "/ECHO_Drought2.csv", sep = ""))

# RECODE #########################################################################################

# Recode "Don't know" to missing
edrought.df$wat_unavail <- ifelse(edrought.df$wat_unavail == 88, NA, edrought.df$wat_unavail)
edrought.df$wat_dist_change <- ifelse(edrought.df$wat_dist_change == 88, NA, edrought.df$wat_dist_change)
edrought.df$wat_quant_change <- ifelse(edrought.df$wat_quant_change == 88, NA, edrought.df$wat_quant_change)
edrought.df$treat_water <- ifelse(edrought.df$treat_water == 88, NA, edrought.df$treat_water)




# Improved water source
edrought.df$imp_wat <- NA
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 11, 1, edrought.df$imp_wat)
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 12, 1, edrought.df$imp_wat)
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 13, 1, edrought.df$imp_wat)
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 14, 1, edrought.df$imp_wat)
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 15, 1, edrought.df$imp_wat)
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 16, 0, edrought.df$imp_wat)
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 17, 1, edrought.df$imp_wat)
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 18, 0, edrought.df$imp_wat)
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 19, 1, edrought.df$imp_wat)
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 20, 0, edrought.df$imp_wat)
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 21, 0, edrought.df$imp_wat)
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 22, 0, edrought.df$imp_wat)
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 23, 0, edrought.df$imp_wat)
edrought.df$imp_wat <- ifelse(edrought.df$water_source == 96, 0, edrought.df$imp_wat)


# Source of water within 30 minutes
edrought.df$win_30 <- NA
edrought.df$win_30 <- ifelse(edrought.df$collect_mins <= 30, 1, 0)

# Recode "Don't know" to missing
edrought.df$wat_unavail <- ifelse(edrought.df$wat_unavail == 88, NA, edrought.df$wat_unavail)

# Access to improved water source
edrought.df$imp_wat_acc <- NA
edrought.df$imp_wat_acc <- ifelse(edrought.df$imp_wat == 1 & edrought.df$win_30 == 1 & edrought.df$wat_unavail == 0, 1, 0)
edrought.df$imp_wat_acc <- ifelse(is.na(edrought.df$imp_wat) | is.na(edrought.df$win_30) | is.na(edrought.df$wat_unavail), NA, edrought.df$imp_wat_ac)

## Fix rCSI items
edrought.df$rCSI_1 <- ifelse(edrought.df$rCSI_1 == 22, 2, edrought.df$rCSI_1)
edrought.df$rCSI_1 <- ifelse(edrought.df$rCSI_1 > 7, 7, edrought.df$rCSI_1)
edrought.df$rCSI_2 <- ifelse(edrought.df$rCSI_2 > 7, 7, edrought.df$rCSI_2)
edrought.df$rCSI_3 <- ifelse(edrought.df$rCSI_3 > 7, 7, edrought.df$rCSI_3)
edrought.df$rCSI_4 <- ifelse(edrought.df$rCSI_4 > 7, 7, edrought.df$rCSI_4)
edrought.df$rCSI_5 <- ifelse(edrought.df$rCSI_5 > 7, 7, edrought.df$rCSI_5)

## Re-calculate rCSI score
edrought.df$rCSI_score <- NA
edrought.df$rCSI_score <- (edrought.df$rCSI_1 * 1) + (edrought.df$rCSI_2 * 2) + (edrought.df$rCSI_3 * 1) + (edrought.df$rCSI_4 * 3) + (edrought.df$rCSI_5 * 1)

## rCSI index
edrought.df$rCSI_index <- NA
edrought.df$rCSI_index <- ifelse(edrought.df$rCSI_score <= 3, 1, edrought.df$rCSI_index)
edrought.df$rCSI_index <- ifelse(edrought.df$rCSI_score >= 4 & edrought.df$rCSI_score <= 10, 2, edrought.df$rCSI_index)
edrought.df$rCSI_index <- ifelse(edrought.df$rCSI_score > 10, 3, edrought.df$rCSI_index)



