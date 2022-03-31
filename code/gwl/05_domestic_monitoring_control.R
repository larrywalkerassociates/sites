library(tidyverse)
library(sp)
library(sf)
library(leaflet)
library(leafpop)
library(plotly)
library(htmltools)
library(htmlwidgets)
library(leafem)
library(here)
library(lubridate)

sites_dir <- here::here()
while (basename(sites_dir)!='sites'){
  sites_dir <- dirname(sites_dir)
}
data_dir <- file.path(sites_dir,'data')

# find water year given year
calculate_water_year <- function(date){
  yr = lubridate::year(date)
  mh = lubridate::month(date)
  yr = ifelse(mh %in% 1:9, yr, yr + 1)
  return(yr)
}

# relational tables in periodic groundwater level measurement database
m    <- read_csv(here("in", "periodic_gwl_bulkdatadownload", "measurements.csv"))
perf <- read_csv(here("in", "periodic_gwl_bulkdatadownload", "perforations.csv"))
sta  <- read_csv(here("in", "periodic_gwl_bulkdatadownload", "stations.csv"))

# join measurement data to perforation and station data
m <- left_join(m, perf, by = "SITE_CODE") %>% 
  left_join(sta, by = "SITE_CODE")

# make spatial
m <- st_as_sf(m, coords = c("LONGITUDE","LATITUDE"), crs = 4269)
m <- as(m, "Spatial")

# Consider joining periodic database with domestic well later
# Load domestic well data
m <- readxl::read_xlsx(here("in",'GW_Monitoring_Wells_2021_2025.xlsx'),skip=1)

# make spatial
m <- st_as_sf(m, coords = c("long","lat"), crs = 4269)
m <- as(m, "Spatial")  

# write dashboards
write_dashboard <- function(x) {
  rmarkdown::render(input       = here("02_index.Rmd"), 
                    output_file = sprintf(paste0(data_dir,"/%s/index.html"), x),
                    params      = list(AOI = x)
  )
}

# three aois to write
aois <- paste0(c("sasb"), "_gwl")

for(j in seq_along(aois)){
  # j <- 1 # testing
  aoi_out_path <- aois[j]
  
  
  if(aois[j] == "sasb_gwl") wyt <- read_csv(here("in", "sac_water_year_types.csv"))

  source(here("04_domestic_monitoring_gwl.R"))
  
  write_dashboard(paste0(aois[j]),'dw')
}




