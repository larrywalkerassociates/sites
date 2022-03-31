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

# write dashboards
write_dashboard <- function(x) {
  rmarkdown::render(input       = here("02_index.Rmd"), 
                    output_file = sprintf(paste0(data_dir,"/%s/index.html"), x),
                    params      = list(AOI = x)
  )
}

# three aois to write
aois <- paste0(c("sasb", "shasta", "ukiah", "butte", "sierra"), "_gwl")

for(j in seq_along(aois)){
   # j <- 1 # testing
  aoi_out_path <- aois[j]
  
  # water year type data
  if(aois[j] == "ukiah_gwl") wyt <- read_csv(here("in", "ukiah_water_year_types.csv")) %>% 
      mutate(
        water_year_type = case_when(
          water_year_type == "Wet" ~ "W",
          water_year_type == "Above normal" ~ "AN",
          water_year_type == "Below normal" ~ "BN",
          water_year_type == "Dry" ~ "D",
          water_year_type == "Critical" ~ "C")
      )
  if(aois[j] == "butte_gwl") wyt <- read_csv(here("in", "sgma_wyt_dataset.csv")) %>% 
      filter(HUC8 == 18010205) %>% 
      mutate(
        water_year_type = case_when(
          WYT == "Wet" ~ "W",
          WYT == "Above Normal" ~ "AN",
          WYT == "Below Normal" ~ "BN",
          WYT == "Dry" ~ "D",
          WYT == "Critical" ~ "C")
      )
  if(aois[j] == "shasta_gwl") wyt <- read_csv(here("in", "sgma_wyt_dataset.csv")) %>% 
      filter(HUC8 == 18010207) %>% 
      mutate(
        water_year_type = case_when(
          WYT == "Wet" ~ "W",
          WYT == "Above Normal" ~ "AN",
          WYT == "Below Normal" ~ "BN",
          WYT == "Dry" ~ "D",
          WYT == "Critical" ~ "C")
      )
  if(aois[j] == "sasb_gwl") wyt <- read_csv(here("in", "sac_water_year_types.csv"))
  if(aois[j] == "sierra_gwl") wyt <- read_delim(here("in", "wyt_gus.txt"), "\t") %>% 
      mutate(
        water_year_type = case_when(
          Type == "Wet" ~ "W",
          Type == "Above Normal" ~ "AN",
          Type == "Below Normal" ~ "BN",
          Type == "Dry" ~ "D",
          Type == "Critical" ~ "C")
        ) %>% 
      select(WY = WY_Start, water_year_type)
  
  source(here("01_gwl.R"))
  
  write_dashboard(aois[j])
}




