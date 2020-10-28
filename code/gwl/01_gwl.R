# AOI determination
# aoi_out_path <- "sasb_gwl"
# aoi_out_path <- "ukiah_gwl"
# aoi_out_path <- "shasta_gwl"

# spatial data for AOI: SASb, Ukiah, Shasta
if(aoi_out_path == "sasb_gwl")   {aoi_shp <- "shp_sas/SAS Subbasin_B118.shp"}
if(aoi_out_path == "ukiah_gwl")  {aoi_shp <- "shp_ukiah/GSA_limit.shp"}
if(aoi_out_path == "shasta_gwl") {aoi_shp <- "shp_shasta/Shasta_GWBasin_New.shp"}
aoi <- st_read(here("in", aoi_shp)) %>% 
  st_transform(crs = 4269) %>% 
  as("Spatial")

# plotly plugin for leaflet
add_deps <- function(dtbl, name, pkg = name) {
  tagList(
    dtbl,
    htmlwidgets::getDependency(name, pkg)
  )
}

# plotly buttons to remove
buttons_to_remove <- 
  list("zoom2d", "select2d", "lasso2d", "autoscale",
       "hoverClosestCartesian", "hoverCompareCartesian",
       "zoom3d", "pan3d", "resetCameraDefault3d",
       "resetCameraLastSave3d", "hoverClosest3d",
       "orbitRotation", "tableRotation","zoomInGeo", 
       "zoomOutGeo", "resetGeo", "hoverClosestGeo",
       "sendDataToCloud", "pan2d","hoverClosestGl2d",
       "hoverClosestPie","toggleHover","toggleSpikelines",
       "autoScale2d","zoomIn2d","zoomOut2d")

# subset measurement points to AOI polygon and back to sf
identical(proj4string(aoi), proj4string(m))
maoi <- m[aoi, ] %>% st_as_sf()

# add observation count and time range. site code has no NA vals
maoi <- maoi %>% 
  group_by(SITE_CODE) %>% 
  mutate(t_range   = paste(range(MSMT_DATE), collapse = " - "),
         n_samples = n()) %>% 
  ungroup()

# WSE and DBGS in units FEET
# omit erroneous value
if(aoi_shp == "shp_sas/SAS Subbasin_B118.shp"){
  maoi <- maoi %>% filter(WSE > -200)
}

# sanity check: water levels
ggplot(maoi, aes(MSMT_DATE, WSE, color = SWN)) + geom_line() + guides(color = FALSE)
ggplot(maoi, aes(MSMT_DATE, -GSE_WSE, color = SWN)) + geom_line() + guides(color = FALSE)

# sanity check: mapview of wells
mapview::mapview(maoi, zcol = "WELL_DEPTH") + 
  mapview::mapview(aoi, alpha.regions = 0, lwd = 2, color = "red")

# ggplots of DBGS - IMPT TO SORT!!!
sc <- unique(maoi$SITE_CODE) %>% sort() 
ns <- group_by(maoi, SITE_CODE) %>% slice(1) %>% arrange(SITE_CODE)
ns$lab <- paste0("<p><b>state well num:</b> ", ns$SWN, "</p>",
                 "<p><b>coords x:</b> ", as_tibble(st_coordinates(ns))$X, "</p>",
                 "<p><b>coords y:</b> ", as_tibble(st_coordinates(ns))$Y, "</p>",
                 "<p><b>n samp:</b> ", ns$n_samples, "</p>",
                 "<p><b>t range:</b> ", ns$t_range, "</p>",
                 "<p><b>depth (ft):</b> ", ns$WELL_DEPTH, "</p>",
                 "<p><b>perf int (ft):</b> ", ns$TOP_PRF, " to ", ns$BOT_PRF, "</p>",
                 "<p><b>use:</b> ", ns$WELL_USE, "</p>",
                 "<p><b>type:</b> ", ns$WELL_TYPE, "</p>",
                 "<p><b>agency:</b> ", ns$WLM_ORG_NAME, "</p>")

# build plotly objects
p <- vector("list", length(sc))
for(i in seq_along(p)){
  d <- maoi %>% 
    filter(SITE_CODE == sc[i]) 
  
  p[[i]] <- d %>% 
    ggplot(aes(MSMT_DATE, -GSE_WSE)) +
    geom_point() +
    geom_line() +
    labs(y = "DBGS (FT)", 
         x = "Measurement date",
         title = sc[i])
  p[[i]] <- ggplotly(p[[i]]) %>% 
    as.tags() %>%
    {tags$div(style="width:270px; height:220px;", .)} %>%
    as.character() %>% 
    stringr::str_replace("height:400px","height:100%")
}

# leaflet
pal <- colorNumeric(colormap::colormap(colormap::colormaps$viridis, nshades = 10), 
                    domain = ns$WELL_DEPTH)

l <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "World") 

l <- l %>%
  addPolygons(data = st_transform(st_as_sf(aoi), 4326),
              fillOpacity = 0, 
              color = "red") %>% 
  addCircleMarkers(data = st_transform(ns, 4326), 
                   color = ~pal(ns$WELL_DEPTH), 
                   stroke = FALSE,
                   radius = 4, 
                   fillOpacity = .8,
                   popup = p, 
                   label = lapply(ns$lab, htmltools::HTML)) %>% 
  addLegend(pal = pal, values = ns$WELL_DEPTH,
            title    = "Well depth (ft)",
            position = "bottomright") %>% 
  addLayersControl(
    baseGroups    = c("Light", "World"),
    options       = layersControlOptions(collapsed = FALSE,
                                         position = "bottomleft")) %>% 
  # JS for polygon popups
  onRender(
    "function(el,x) {
    this.on('popupopen', function() {HTMLWidgets.staticRender(); remove()})
    }"
  ) %>%
  add_deps("plotly") %>%
  htmltools::attachDependencies(plotly:::plotlyMainBundle(), append = TRUE) %>%
  htmltools::attachDependencies(crosstalk::crosstalkLibs(),  append = TRUE) %>%
  browsable()
l

# write_rds(l, here("out", "leaf.rds"))

# data table
dt <- ns %>% 
  as("Spatial") %>% 
  .@data %>% 
  select(SITE_CODE, SWN, WLM_ORG_NAME, TOP_PRF:n_samples) #%>% 
  # write_rds(here("out", "dt.rds"))

# zip data
# subset to the site codes
n <- unique(maoi$SITE_CODE)

# write csvs - first arrange data and add geometry
df <- as(maoi, "Spatial")@data %>% 
  select(-c("STN_ID.x", "STN_ID.y")) %>% 
  bind_cols(as_tibble(st_coordinates(maoi)))

for(i in 1:length(n)){
  df %>% 
    filter(SITE_CODE == n[i]) %>% 
    write_csv(paste0("/Users/richpauloo/Documents/GitHub/sites/data/", aoi_out_path, "/", n[i], ".csv"))
}

write_csv(df, 
          paste0("/Users/richpauloo/Documents/GitHub/sites/data/", aoi_out_path, "/all_gwl_data.csv"))

zip_dir    <- paste0("/Users/richpauloo/Documents/GitHub/sites/data/", aoi_out_path, "/", aoi_out_path,"_data.zip")
file_paths <- list.files(paste0("/Users/richpauloo/Documents/GitHub/sites/data/", aoi_out_path),  full.names = TRUE)
zip(zip_dir, file_paths, extras = "-j")
file.remove(file_paths)
