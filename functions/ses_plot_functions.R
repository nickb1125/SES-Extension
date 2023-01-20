# plotting functions

# evaluates string (useful for later functions)
eval_string <- function(string) {
  return(eval(parse(text = string)))
}

# filters shape file to only wanted areas for plotting

filter_shape <- function(dat_all = dat_all, years, states, geo) {
  if (states == 'All') {
    dat_filter <- dat_all %>% filter( year %in% years,  !(StateAb %in% c('HI', 'AK')   ))
  }
  else {
    dat_filter <- dat_all %>% filter(year %in% years, StateAb %in% states )
  }
  
  if (geo == "county") {
    shape_filter <- sp::merge(shape, dat_filter, by = "fips", duplicateGeoms = TRUE)
    shape_filter <- subset(shape_filter, fips %in% dat_filter$fips)
  }
  if (geo == "zcta") {
    shape_filter <- sp::merge(shape, dat_filter, by = "zcta", duplicateGeoms = TRUE)
    shape_filter <- subset(shape_filter, zcta %in% dat_filter$zcta)
  }
  
  return(shape_filter)
}

# plots leaflet of desired areas and desired ses variable, and year selection

get_leaflet <- function(dat_all = dat_all, years, states, attribute, geo) {
  all_years <- unique(dat_all$year)
  if (states != "All") {
    all_state <- dat_all %>% filter(StateAb %in% states )
  }
  if (states == "All"){
    all_state <- dat_all %>% filter(!(StateAb %in% c('HI', 'AK') ))
  }
  min <- min(all_state[,attribute])
  max <- max(all_state[,attribute])
  
  shape_filter <- filter_shape(dat_all = dat_all, years = years, states = states, geo = geo)
  
  pal1 <- colorNumeric(
    palette = c("#a6cee3", "#1f78b4", '#7570b3'),
    domain = all_state[,attribute])
  
  plot <- leaflet(shape_filter) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~colorQuantile("YlOrRd", shape_filter@data[,attribute])(shape_filter@data[,attribute]),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)) %>%
    addProviderTiles(providers$Stamen.TonerLite)
  
  if (states == 'NC') {
    return(plot %>% setView(lng = -79.0193, lat = 35.7596, zoom = 7))
  }
  
  else {
    return(plot %>% setView(lng = -98.5795, lat = 39.8293, zoom = 3.75))
  }
  
  setView(lng = -79.0193, lat = 35.7596, zoom = 7)
  
}

# plots ggplot of desired areas and desired ses variable, and year selection

get_geom_poly <- function(dat_all = dat_all, states, years, attribute, geo) {
  all_years <- unique(dat_all$year)
  all_state <- dat_all %>% filter(StateAb %in% states )
  min <- min(all_state[,attribute])
  max <- max(all_state[,attribute])
  
  shape_filter <- filter_shape(dat_all = dat_all, years = years, states = states, geo = geo)
  shp_df <- broom::tidy(shape_filter, region = "id")
  shp_df <- shp_df %>% left_join(shape_filter@data, by = c("id"="id"))
  ggplot() + geom_polygon(data = shp_df, aes_string(x = 'long', y = 'lat', group = 'group', fill =  attribute), colour = "black") + theme_void() + 
    ggtitle(paste(attribute, 'in', states, years)) + 
    scale_fill_gradientn(colors = viridis_pal()(9), limits=c(min, max), 
                         na.value = "#FDE725FF")
}

# plots ggplot year progression of desired areas and desired ses variable, and years selection

see_year_progression <- function(dat_all = dat_all, states, attribute, geo, years) {
  shape_filter <- filter_shape(dat_all = dat_all, years = years, states = states, geo = geo)
  all_years <- as.integer(unique(dat_all$year))
  
  all_plots <- lapply(as.character(all_years), 
                      get_geom_poly, dat_all = dat_all, states = states, attribute = attribute, geo = geo)
  
  n <- length(all_plots)
  nCol <- floor(sqrt(n) + 1)
  
  do.call("grid.arrange", c(all_plots, ncol=nCol))
}