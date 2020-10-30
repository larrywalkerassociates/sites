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

# find water year given year
calculate_water_year <- function(date){
  yr = lubridate::year(date)
  mh = lubridate::month(date)
  yr = ifelse(mh %in% 1:9, yr - 1, yr)
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

# water year type data
wyt <- read_csv(here("in", "sac_water_year_types.csv"))

# write dashboards
write_dashboard <- function(x) {
  rmarkdown::render(input       = here("02_index.Rmd"), 
                    output_file = sprintf("/Users/richpauloo/Documents/GitHub/sites/data/%s/index.html", x),
                    params      = list(AOI = x)
  )
}

# three aois to write
aois <- paste0(c("sasb", "shasta", "ukiah"), "_gwl")

for(j in seq_along(aois)){
  aoi_out_path <- aois[j]
  source(here("01_gwl.R"))
  write_dashboard(aois[j])
}
