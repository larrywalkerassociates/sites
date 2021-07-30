# estimate ag res parcel count

library(tidyverse)
library(sf)
library(here)

# load parcel data
# https://data.saccounty.net/datasets/8d011b99aebc46d48060b2e5bdd7f4a0_0/explore?location=38.375406%2C-121.437555%2C10.00
# p <- st_read(here("data", "Parcels", "Parcels.shp"))

# load parcel centroid data
# https://data.saccounty.net/datasets/7da6753c252b42af9f85751dc5b27bbb_0/explore?location=38.378241%2C-121.437720%2C10.54
pc <- st_read(here("data", "Parcel_Centroids", "Parcel_Centroids.shp"))

# load zoning data
# https://data.saccounty.net/datasets/c93dfff02b3241f6aea0783a4ad2ee46_0/explore?location=38.375707%2C-121.439956%2C10.45&showTable=true
z <- st_read(here("data", "Zoning-shp", "Zoning.shp"))

# south american subbasin
sasb <- st_read(here("data","sasb","SAS Subbasin_B118.shp")) %>% 
  st_transform(2226)

# ag res zones
arz <- z %>% 
  filter(str_detect(DESCRIPTIO, "AGRICULTURAL-RESIDENTIAL")) %>% 
  st_union()

# parcels in ag res zones
arpc <- st_intersection(pc, arz)

# sasb parcels in agres zones
arpc <- st_intersection(arpc, sasb)
write_rds(arpc, here("results", "ag_res_parcel_centroids.rds"))

# simplify and save parcel
# arps <- rmapshaper::ms_simplify(arp, keep_shapes = TRUE)
zs <- rmapshaper::ms_simplify(z, keep_shapes = TRUE)

# write_rds(ps, here("results", "parcel_simple.rds"))
write_rds(zs, here("results", "zoning_simple.rds"))
