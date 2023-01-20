library(rgdal)

########## ZCTA


download.file("https://www2.census.gov/geo/tiger/TIGER2019/ZCTA5/tl_2019_us_zcta510.zip", here::here("shape_pull/zcta_shapefile_uncomp_zip.zip"))
dir.create(file.path(here::here("shape_pull"), "zcta_shape"))
unzip(here::here("shape_pull/zcta_shapefile_uncomp_zip.zip"), exdir = here::here("shape_pull/zcta_shape"))
shape <- readOGR(here::here("shape_pull/zcta_shape"), layer = 'tl_2019_us_zcta510')
shape@data <- shape@data %>% dplyr::rename('zcta' = 'ZCTA5CE10')
shape@data <- shape@data %>% mutate(id = row.names(.))
if ( !(file.exists(here::here("shape_pull/zcta_shape/zcta.dbf"))) ) {
  writeOGR(shape, here::here("shape_pull/zcta_shape"), "zcta", 
           driver = "ESRI Shapefile")
}






########## COUNTY


download.file("https://www2.census.gov/geo/tiger/TIGER2019/COUNTY/tl_2019_us_county.zip", here::here("shape_pull/county_shapefile_uncomp_zip.zip"))
dir.create(file.path(here::here("shape_pull"), "county_shape"))
unzip(here::here("shape_pull/county_shapefile_uncomp_zip.zip"), exdir = here::here("shape_pull/county_shape"))
shape <- readOGR(here::here("shape_pull/county_shape"), layer = 'tl_2019_us_county')
shape@data <- shape@data %>% mutate(fips = paste0(STATEFP, COUNTYFP))
shape@data <- shape@data %>% mutate(id = row.names(.))
if (!(file.exists(here::here("shape_pull/county_shape/county.dbf")))) {
writeOGR(shape, here::here("shape_pull/county_shape"), "county", 
           driver = "ESRI Shapefile")
}




