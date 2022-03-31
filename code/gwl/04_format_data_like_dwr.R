library(here)
library(tidyverse)
library(data.table)

# relational tables in periodic groundwater level measurement database
# m_t    <- read_csv(here("in", "periodic_gwl_bulkdatadownload", "measurements.csv"))
# perf <- read_csv(here("in", "periodic_gwl_bulkdatadownload", "perforations.csv"))
# sta  <- read_csv(here("in", "periodic_gwl_bulkdatadownload", "stations.csv"))
# 
# # join measurement data to perforation and station data
# m_t <- left_join(m_t, perf, by = "SITE_CODE") %>% 
#   left_join(sta, by = "SITE_CODE")
# 
# # make spatial
# m <- st_as_sf(m, coords = c("LONGITUDE","LATITUDE"), crs = 4269)
# m <- as(m, "Spatial")



# Clean domestic well data ------------------------------------------------


m <- readxl::read_xlsx(here("in",'GW_Monitoring_Wells_2021_2025.xlsx'),skip=1)
# clean dataframe so it is in long format if needed
m_cols <- m %>% colnames()
dtw_cols <- m_cols[m_cols %>% str_detect('_q')]
id_cols <- m_cols[!(m_cols %>% str_detect('_q'))]

m <- m %>% reshape2::melt( id.vars = id_cols, measure.vars=dtw_cols, 
                           variable.name='year_quarter', value.name = 'rpe_gwe')
yq <- str_split_fixed(m$year_quarter,'_q',n=2)
m <- m %>% mutate(gwe = well_head_elevation - rpe_gwe,
             dtw = well_ground_elevation - gwe,
             year = as.numeric(yq[,1]),
             quarter = as.numeric(yq[,2]),
             date = make_date(year = year, month = quarter*3-1, day=1))
if('date' %in% m_cols){
m <- m %>% mutate(year = as.numeric(yq[,1]),
                  quarter = as.numeric(yq[,2]),
                  date = make_datetime(year = year, month = quarter*3-1, day=1,hour=12,min=0))
}

# make spatial
m <- st_as_sf(m, coords = c("long","lat"), crs = 4269)
m <- as(m, "Spatial")




# option 1 rename data to match CASGEM
# this web dashboard data will never be quite the same
# better to re-write code

# option 2 rewrite code

