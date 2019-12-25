
library(RMySQL)

# variables
workingdir <- "/srv/shiny-server/lmh_map" # will be used later to save data

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
saveRDS(cha_msr_cur.df, paste(workingdir, "/cha_msr_cur.rds", sep = ""))


# community GPS data
community.sqlr <- dbSendQuery(cha.con, ' SELECT * FROM community')
community.df <- fetch(community.sqlr, n=-1)
dbClearResult(dbListResults(cha.con)[[1]])
rm(community.sqlr)
rm(upload.con, cha.con)

saveRDS(community.df, paste(workingdir, "/community.rds", sep = ""))

rm(cha_msr_cur.df, community.df)


killDbConnections()










