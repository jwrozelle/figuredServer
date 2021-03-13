


# for loop to calculate jenks for each variable
countvar <- 0

while (countvar <= length(analysis_variables)) {
  countvar = countvar + 1
  
  colvar = analysis_variables[[countvar]]
  
  # calculate color breaks
  brkQuantile <- classIntervals(choropleth_county[[colvar]], n = 20, style = "quantile", unique = TRUE)
  saveRDS(brkQuantile, file = paste0("RObjects/brkQuantile_", colvar, ".rds"), ascii = FALSE)
  
  
  brkJenk <- classIntervals(choropleth_county[[colvar]], n = 20, style = "jenks", unique = TRUE)
  saveRDS(brkQuantile, paste0("RObjects/brkJenk_", colvar, ".rds"))
  
  brkSd <- classIntervals(choropleth_county[[colvar]], n = 10, style = "sd", unique = TRUE)
  saveRDS(brkSd, paste0("RObjects/brkSd_", colvar, ".rds"))
  
}

countvar