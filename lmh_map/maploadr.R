
# Libraries ====================================================================
library(RMySQL)
library(sp)
library(sf)
library(dplyr)
library(rgdal)
# library(rmapshaper)

# SQL connection & DL ==========================================================

killDbConnections <- function () {
  all_cons <- dbListConnections(MySQL())
  print(all_cons)
  for(con in all_cons)
    +  dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}
killDbConnections()

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

# Upload
cha_msr_cur.sqlr <- dbSendQuery(upload.con,
                                'SELECT *, SUM(3_1_b_muac_red) AS redsum, SUM(3_1_c_muac_yellow) AS yelsum
                                FROM de_cha_monthly_service_report
                                GROUP BY community_id, month_reported, year_reported'
)
cha_msr_cur.df <- fetch(cha_msr_cur.sqlr, n=-1)
dbClearResult(dbListResults(upload.con)[[1]])
rm(cha_msr_cur.sqlr)

# community GPS data
community.sqlr <- dbSendQuery(cha.con, ' SELECT * FROM community')
community.df <- fetch(community.sqlr, n=-1)
dbClearResult(dbListResults(cha.con)[[1]])
rm(community.sqlr)

rm(upload.con, cha.con)

killDbConnections()

# Shapefiles
geoDirectory <- "/srv/shiny-server/lmh_map/shapefiles/admin"

lbrogr_adm1.ply <- readOGR(paste(geoDirectory, "/LBR_adm1.shp", sep ="")) # County layer
h_dist.ply <- readOGR(paste(geoDirectory, "/hdist.shp", sep =""), layer = "hdist") # Health District Layer
# h_dist.ply <- rmapshaper::ms_simplify(h_dist.ply)
h_dist.ply <- spTransform(h_dist.ply, CRS("+proj=longlat +datum=WGS84 +no_defs")) #Transform, this is why it wasn't rendering before

# MUAC NUMBERS ###########################################################
merged.df <- merge(cha_msr_cur.df, community.df, by="community_id", all.x=TRUE)
rm(cha_msr_cur.df, community.df)

# malnutrition calculator
merged.df$malnut <- (as.numeric(merged.df$redsum)*2) + as.numeric(merged.df$yelsum) +1

comm.df <- as.data.frame(filter(merged.df, !is.na(x) & !is.na(y)))
comm.pnt <- st_as_sf(comm.df, coords = c("x", "y"), crs = "+proj=longlat +ellps=WGS84")

background.folder <- paste("/srv/shiny-server/lmh_map")

saveRDS(merged.df, "RObjects/mergeddf.rds")
saveRDS(comm.df, "RObjects/commdf.rds")
saveRDS(comm.pnt, "RObjects/commpnt.rds")
saveRDS(lbrogr_adm1.ply, "RObjects/lbrogr_adm1ply.rds")
saveRDS(h_dist.ply, "RObjects/h_distply.rds")




