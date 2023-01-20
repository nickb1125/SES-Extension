
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
  
  # some data cleaning (NOTE: This is specific to Terry's previously used variables, further cleaning will likely needed to be added if new variables are selected)
  
  num_occupants_1.01_up_all <- County_ACS %>%
    select(num_occupants_1.01_1.5_owned, num_occupants_1.51_2_owned,
           num_occupants_2.1_up_owned, num_occupants_1.01_1.5_rented,
           num_occupants_1.51_2_rented, num_occupants_2.1_up_rented) %>%
    rowSums()
  County_ACS <- County_ACS %>%
    mutate(num_occupants_crowded = num_occupants_1.01_up_all)
  
  phone_service_rented_owned <- County_ACS %>%
    select(phone_service_owned, phone_service_rented) %>%
    rowSums()
  
  County_ACS <- County_ACS %>%
    mutate(phone_service = phone_service_rented_owned)
  
  County_ACS <- data.frame(County_ACS) %>% 
    mutate(population = male_total,
           male_prop = male/male_total, # sex covariate
           # race covariates
           white_prop = white / white_total,
           pop_non_hispanic_black_prop = pop_non_hispanic_black/pop_total,
           hispanic_prop = hispanic / hispanic_total,
           # age covariates
           # 
           age_under_5_prop = (male_under_5 + 
                                 female_under_5) / (female_total + male_total),
           age_5_17_prop = (male_5_9 + 
                              male_10_14 + 
                              male_15_17 +
                              female_5_9 + 
                              female_10_14 + 
                              female_15_17) / (male_total + female_total),
           age_18_39_prop = (male_18_19 +
                               male_20 +
                               male_21 +
                               male_22_24 +
                               male_25_29 +
                               male_30_34 +
                               male_35_39 +
                               female_18_19 +
                               female_20 +
                               female_21 +
                               female_22_24 +
                               female_25_29 +
                               female_30_34 +
                               female_35_39) / (male_total + female_total),
           age_40_64_prop = (male_40_44 + 
                               male_45_49 + 
                               male_50_54 + 
                               male_55_59 + 
                               male_60_61 + 
                               male_62_64 + 
                               female_40_44 + 
                               female_45_49 + 
                               female_50_54 + 
                               female_55_59 + 
                               female_60_61 + 
                               female_62_64) / (male_total + female_total),
           age_over_65_prop = (male_65_66 + 
                                 male_67_69 + 
                                 male_70_74 + 
                                 male_75_79 + 
                                 male_80_84 + 
                                 male_85_over + 
                                 female_65_66 + 
                                 female_67_69 + 
                                 female_70_74 + 
                                 female_75_79 + 
                                 female_80_84 + 
                                 female_85_over) / (male_total + female_total),
           # SES variables
           poverty_prop = below_poverty_line/poverty_total,
           family_type_female_householder_prop = family_type_female_householder/family_type_total,
           family_type_single_prop = family_type_single/family_type_total,
           housing_rental_prop = housing_rental/housing_total,
           crowded_prop = num_occupants_crowded/num_occupants_total,
           phone_service_prop = phone_service/phone_service_total,
           public_assistance_prop = public_assistance/public_assistance_total) %>%
    select(GEOID, NAME, population, ends_with("prop")) %>%
    ungroup %>%
    filter(!is.na(NAME))
  
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





