# AOI determination
# aoi_out_path <- "sasb_gwl"
# aoi_out_path <- "ukiah_gwl"
# aoi_out_path <- "shasta_gwl"

# spatial data for AOI: SASb, Ukiah, Shasta
if(aoi_out_path == "sasb_gwl")   {aoi_shp <- "shp_sas/SAS Subbasin_B118.shp"}

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

# remove samples with NA values
maoi <- maoi %>% filter(!is.na(dtw))

# add observation count and time range. site code has no NA vals
maoi <- maoi %>% 
  group_by(well_id) %>% 
  mutate(t_range   = paste(range(date), collapse = " - "),
         n_samples = n()) %>% 
  ungroup()

# gwe and DBGS in units FEET
# omit erroneous value
# if(aoi_shp == "shp_sas/SAS Subbasin_B118.shp"){
#   maoi <- maoi %>% filter(gwe > -200)}

# No crosswalk between well_id or SWN and WCRNumber -> I think this obscures use
# periodic database works with well_id, date and gwe, SWN
# includes X, Y, well_depth, TOP_RF, BOT_PRF, WELL_USE, WELL_TYPE, WLM_ORG_NAME
# as pop ups in map (color for dtw)

# sanity check: water levels
#ggplot(maoi, aes(date, gwe, color = SWN)) + geom_line() + guides(color = FALSE)
#ggplot(maoi, aes(date, -dtw, color = SWN)) + geom_line() + guides(color = FALSE)

# sanity check: mapview of wells
#mapview::mapview(maoi, zcol = "well_depth") + 
#  mapview::mapview(aoi, alpha.regions = 0, lwd = 2, color = "red")

# ggplots of DBGS - IMPT TO SORT!!!
sc <- unique(maoi$well_id) %>% sort() 
ns <- group_by(maoi, well_id) %>% slice(1) %>% ungroup() %>% arrange(well_id) 
ns$lab <- paste0("<p><b>coords x:</b> ", as_tibble(st_coordinates(ns))$X, "</p>",
                 "<p><b>coords y:</b> ", as_tibble(st_coordinates(ns))$Y, "</p>",
                 "<p><b>Date range:</b> ", ns$t_range, "</p>",
                 "<p><b>Well head elevation (ft):</b> ", ns$well_head_elevation, "</p>",
                 "<p><b>depth (ft):</b> ", ns$well_depth, "</p>",
                 "<p><b>pump depth (ft):</b> ", ns$pump_depth ,"</p>",
                 "<p><b>agency:</b> ", 'DWG', "</p>")

# ------------------------------------------------------------------------
# add seasons
maoi <- maoi %>% 
  mutate(
    season = case_when(
      month(date) %in% 8:11 ~ "fall",
      month(date) %in% 3:5  ~ "spring",
      TRUE ~ "other"
    )
  ) %>% 
  # add seasonal range
  group_by(well_id, season) %>% 
  mutate(fall_high   = ifelse(season == "fall",   min(dtw, na.rm = TRUE), NA),
         fall_low    = ifelse(season == "fall",   max(dtw, na.rm = TRUE), NA),
         spring_high = ifelse(season == "spring", min(dtw, na.rm = TRUE), NA),
         spring_low  = ifelse(season == "spring", max(dtw, na.rm = TRUE), NA)) %>% 
  ungroup() %>% 
  mutate(water_year = calculate_water_year(date)) %>% 
  left_join(wyt, by = c("water_year" = "WY")) %>% 
  mutate(water_year_type = 
           factor(water_year_type,
                  levels = c("W","AN","BN","D","C"), 
                  labels = c("Wet","Above Normal","Below Normal","Dry","Critical")),
         water_year_start = ymd_hms(paste0(water_year-1, "-10-01 00:00:00")), 
         water_year_end   = ymd_hms(paste0((water_year), "-09-30 24:00:00")))

# ------------------------------------------------------------------------
# build plotly objects
p <- vector("list", length(sc))
for(i in seq_along(p)){
  d <- maoi %>% 
    filter(well_id == sc[i])
  
  # closest to 2015-01-01 
  # close_2015 <- 
  #   tibble(date = d[which.min(abs(d$date - ymd_hms("2015-01-01 00:00:00"))), ]$date,
  #          close_Jan_2015 = d[which.min(abs(d$date - ymd_hms("2015-01-01 00:00:00"))), ]$dtw)
  
  # water year range rectangles - join to WY type data
  wy_rng <- c(min(d$water_year_start, na.rm = TRUE), 
              max(d$water_year_start, na.rm = TRUE),
              min(d$water_year_end, na.rm = TRUE), 
              max(d$water_year_end, na.rm = TRUE))
  wy_rect <- tibble(t0 = seq(wy_rng[1], wy_rng[2], "1 year"),
                    t1 = seq(wy_rng[3], wy_rng[4], "1 year")) %>% 
    mutate(water_year = year(t1)) %>% 
    left_join(wyt, by = c("water_year" = "WY")) %>% 
    mutate(water_year_type = 
             factor(water_year_type,
                    levels = c("W","AN","BN","D","C"), 
                    labels = c("Wet","Above Normal","Below Normal","Dry","Critical")))
  
  # dumb hack to make ggplotly carry all factor levels
  dummy_vals <- setdiff(levels(wy_rect$water_year_type), unique(wy_rect$water_year_type))
  for(diff in seq_along(dummy_vals)) {
    new_row_i <- nrow(wy_rect) + 1
    wy_rect[new_row_i,] <- wy_rect[1,]
    for (n in names(wy_rect[1,])) {
      wy_rect[new_row_i,][n] = NA
    }
    wy_rect[new_row_i,]$water_year_type <- dummy_vals[diff]
  }
  # remove NA values since dealing with just 1 year
  wy_rect <- wy_rect %>% filter(!is.na(water_year))
  # build ggplots and plotly objects
  p[[i]] <- ggplot() +
    geom_rect(data = wy_rect, 
              mapping = aes(
                xmin  = t0, 
                xmax  = t1, 
                ymin  = min(-d$dtw, na.rm=TRUE), 
                ymax  = 0,
                fill  = water_year_type,
                WY    = water_year), alpha = 0.5) +
    geom_point(data = d, mapping = aes(date, -dtw)) +
    geom_line(data = d, mapping = aes(date, -dtw)) +
    # geom_hline(data = close_2015, mapping = aes(yintercept = -close_Jan_2015), lwd = 1, color = "grey50") +
    geom_hline(data = d, mapping = aes(yintercept = -spring_high), lwd = 1, linetype = "dotted", color = "blue") +
    geom_hline(data = d, mapping = aes(yintercept = -spring_low),  lwd = 1, linetype = "dotted", color = "cyan") +
    geom_hline(data = d, mapping = aes(yintercept = -fall_high),   lwd = 1, linetype = "dotted", color = "orange") +
    geom_hline(data = d, mapping = aes(yintercept = -fall_low),    lwd = 1, linetype = "dotted", color = "red") +
    scale_fill_brewer(palette = "RdYlBu", direction = -1, drop = FALSE) +
    coord_cartesian(ylim = c(min(-d$dtw, na.rm=TRUE), 0)) +
    labs(y = "DBGS (FT)", x = "Measurement date",
         fill = "Water Year\nType", title = sc[i])
  p[[i]] <- ggplotly(p[[i]]) %>% 
    as.tags() %>%
    {tags$div(style="width:800px; height:400px;", .)} %>%
    as.character() %>% 
    stringr::str_replace("height:400px","height:100%")
}

# ------------------------------------------------------------------------
# leaflet
pal <- colorNumeric(colormap::colormap(colormap::colormaps$viridis, nshades = 10), 
                    domain = ns$dtw)

l <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "World") 

l <- l %>%
  addPolygons(data = st_transform(st_as_sf(aoi), 4326),
              fillOpacity = 0, 
              color = "red") %>% 
  addCircleMarkers(data = st_transform(ns, 4326), 
                   color = ~pal(ns$dtw), 
                   stroke = FALSE,
                   radius = 4, # original is 4, zooms in with depth
                   fillOpacity = .8,
                   popup = p,
                   label = lapply(ns$lab, htmltools::HTML)) %>% 
  addLegend(pal = pal, values = ns$dtw,
            title    = "Depth to water (ft)",
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
# l

# write_rds(l, here("out", "leaf.rds"))

# data table
dt <- ns %>% 
  as("Spatial") %>% 
  .@data %>% 
  select(well_id,  well_depth:n_samples) #%>% 
# write_rds(here("out", "dt.rds"))

# zip data
# subset to the site codes
n <- unique(maoi$well_id)

# ------------------------------------------------------------------------
# write csvs - first arrange data and add geometry
df <- as(maoi, "Spatial")@data %>% 
  bind_cols(as_tibble(st_coordinates(maoi)))

for(i in 1:length(n)){
  df %>% 
    filter(well_id == n[i]) %>% 
    write_csv(paste0(aoi_out_path, "/", n[i], ".csv"))
}

write_csv(df, 
          paste0(aoi_out_path, "/all_gwl_data.csv"))

zip_dir    <- paste0(aoi_out_path, "/", basename(aoi_out_path),"_data.zip")
file_paths <- list.files(paste0(aoi_out_path),  full.names = TRUE)
zip(zip_dir, file_paths, extras = "-j") # doesn't work right now
file.remove(file_paths)
