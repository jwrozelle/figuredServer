library(stringr)


# 

# get basic data from the folder
covid_data = read.csv("./www/COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports/04-06-2020.csv", colClasses = c("FIPS"="character"))

# add leading 0's
covid_data$FIPS_5 <- str_pad(covid_data$FIPS, width = 5, side = "left", pad = "0")

# filter to only data that has a FIPS
covid_data_fips = filter(covid_data, !is.na(FIPS))

# get the county shapfiles
UScounty.ply <- st_read("./www/UScounties/UScounties.shp")

# Merge the data into the an sp object
choropleth_county <- sp::merge(UScounty.ply, covid_data, by.x = "FIPS", by.y = "FIPS_5", all.x = TRUE, all.y = FALSE)




# Save the data as an rds object
saveRDS(choropleth_county, "RObjects/choropleth_county.rds")


# load the state shapefiles
# USstate.ply <- st_read("C:\\Users\\jwill\\Downloads\\cb_2018_us_state_500k\\cb_2018_us_state_500k.shp")
# 
# saveRDS(USstate.ply, "RObjects/choropleth_state.rds")


source("./colorBreaks.R")






