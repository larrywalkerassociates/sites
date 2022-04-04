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
library(data.table)

sites_dir <- here::here()
while (basename(sites_dir)!='sites'){
  sites_dir <- dirname(sites_dir)
}
data_dir <- file.path(sites_dir,'data/')

# find water year given year
calculate_water_year <- function(date){
  yr = lubridate::year(date)
  mh = lubridate::month(date)
  yr = ifelse(mh %in% 1:9, yr, yr + 1)
  return(yr)
}

# Clean domestic well data ------------------------------------------------

m <- readxl::read_xlsx(here("in",'GW_Monitoring_Wells_2021_2025.xlsx'),skip=1)
# remove any well owner identification information
volunteer_info <- c('email_volunteer','volunteer_name','phone_number','well_apn',
              'address_num','address_str','address_zip','address_city','address')
m <- m %>% select(-volunteer_info)

# clean dataframe so it is in long format if needed
m_cols <- m %>% colnames()
dtw_cols <- m_cols[m_cols %>% str_detect('_q')]
id_cols <- m_cols[!(m_cols %>% str_detect('_q'))]

m <- m %>% reshape2::melt( id.vars = id_cols, measure.vars=dtw_cols, 
                           variable.name='year_quarter', value.name = 'rpe_gwe')
yq <- str_split_fixed(m$year_quarter,'_q',n=2)
m <- m %>% mutate(gwe = well_head_elevation - rpe_gwe,
                  dtw = well_ground_elevation - gwe)
if(!('date' %in% m_cols)){
  m <- m %>% mutate(year = as.numeric(yq[,1]),
                    quarter = as.numeric(yq[,2]),
                    date = make_datetime(year = year, month = quarter*3-1, day=1,hour=12,min=0))
}

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
  aoi_out_path <- file.path(data_dir,paste0(aois[j],'_dw'))
  
  
  if(aois[j] == "sasb_gwl") wyt <- read_csv(here("in", "sac_water_year_types.csv"))

  source(here("04_domestic_monitoring_gwl.R"))
  
  write_dashboard(paste0(aois[j],'_dw'))
}




