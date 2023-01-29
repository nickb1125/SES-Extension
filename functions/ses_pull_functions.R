
# get SES codes that are valid in all years requested 

get_existing_codes <- function(years, file_path) {
  
  # read in SES codes from file (NOTE: Currently only contains codes applicable to Terry's previous projects)
  
  my_codes <- read_csv(file = file_path, show_col_types = FALSE)
  
  # Loop through years to check that all codes requested from file exist from years requested
  
  all_omit_names <- c()
  for (year in years) {
    vars_exist <- load_variables(year, "acs5", cache = TRUE)$name
    omit_vars <- (my_codes %>% filter(!(acs_code %in% vars_exist)))$my_vrb
    my_codes <- my_codes %>% filter(acs_code %in% vars_exist)
    all_omit_names <- c(all_omit_names, omit_vars)
  }
  
  # print all valid variables from all these years
  
  print(paste('Removed vars:', all_omit_names))
  
  # return codes
  
  return(my_codes)
}




# Pull codes via get_acs function at the level specified (ie. zip, etc) 

my_get_acs <- function(year,codes, geo){
  my_vars <- my_codes$acs_code
  
  County_ACS <- get_acs(
    geography = geo,
    variables = na.omit(my_vars),
    survey = "acs5",
    year = year,
    output = "tidy",
  ) 
  
  County_ACS <- County_ACS %>%
    select(-moe) %>%
    spread(variable, estimate)
  
  # change from codes to labels
  
  x <- names(County_ACS)
  for (i in 1:length(names(County_ACS))) {
    if (x[i] %in% c("GEOID", "NAME", "geometry")) {
      x[i] <- x[i]
    } else {
      x[i] <- my_codes$my_vrb[my_codes$acs_code == x[i]]
    }
  }
  names(County_ACS) <- x
  
  County_ACS$County <- sub("\\,.*", "", County_ACS$NAME)
  County_ACS$State <- sub('.*, ', '', County_ACS$NAME)
  
  # combine fips codes
  
  data("fips_codes")
  fips <- fips_codes
  fips$fipscode <- as.numeric(paste(fips$state_code, fips$county_code, sep=""))
  fips <- fips %>%
    mutate(StateAb = state,
           County = county)
  County_ACS <- County_ACS %>%
    select(-NAME)
  if (geo == "county") {
    County_ACS$State <- state.abb[match(County_ACS$State, state.name)]
    County_ACS <- County_ACS %>%
      rename(StateAb = State)
    County_ACS <- left_join(County_ACS, fips)
  }
  if (geo == "zcta") {
    County_ACS$state_code <- substr(County_ACS$GEOID, 1, 2)
    fips <- fips %>% dplyr::select(state_code, StateAb) %>% unique()
    County_ACS <- left_join(County_ACS, fips)
  }
  

  return(County_ACS)
}

# change SES to long version

get_ACS_long <- function(year, codes, geo) {
  ACS <- my_get_acs(year, codes, geo)
  if (geo == "county") {
    ACS_long <- ACS %>% pivot_longer(cols = !c("GEOID", "County", "StateAb", "fipscode"))
  }
  if (geo == "zcta") {
    ACS_long <- ACS %>%
      mutate(zcta = as.numeric(substr(State, 7, 12))) %>%
      dplyr::select(-County, -State, -state_code) %>% pivot_longer(cols = !c("zcta", "GEOID", "StateAb"))
  }
  return(ACS_long)
}

# get multiple years of SES in long version

get_multiple_ACS <- function(years, codes, geo) {
  years_all <- lapply(years, function(x) { return(     get_ACS_long(x, codes, geo) %>% mutate(year = x)   )    })
  years_all_merge <- do.call(rbind, years_all)
  if (geo == "county") {
    years_all_wide <- years_all_merge %>% pivot_wider(id_cols = c("GEOID", "County", "StateAb", "fipscode", "year")) %>% 
      mutate(year = as.character(year)) %>% dplyr::rename('fips' = 'fipscode')
  }
  if (geo == "zcta") {
    years_all_wide <- years_all_merge %>% pivot_wider(id_cols = c("zcta", "GEOID", "StateAb", "year")) %>% 
      mutate(year = as.character(year))
  }
  return(years_all_wide)
}





