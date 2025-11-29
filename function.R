# R/functions.R -----------------------------------------------------------
# All reusable, documented functions for the healthcare desert analysis
# -------------------------------------------------------------------------

#' Get NYC census tracts with population (2020)
get_nyc_tracts <- function(year = 2020, cache_dir = "data/mp02") {
  cache_file <- file.path(cache_dir, sprintf("nyc_tracts_%d.rds", year))
  if (file.exists(cache_file)) return(readRDS(cache_file))
  
  message("Downloading NYC tract boundaries...")
  tracts <- tidycensus::get_acs(
    geography = "tract",
    variables = "B01003_001",  # Total population
    state = "NY",
    county = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
    year = year,
    geometry = TRUE,
    cb = FALSE
  ) %>%
    dplyr::mutate(
      borough = dplyr::case_when(
        substr(GEOID, 6, 8) %in% c("005") ~ "Bronx",
        substr(GEOID, 6, 8) %in% c("047") ~ "Brooklyn",
        substr(GEOID, 6, 8) %in% c("061") ~ "Manhattan",
        substr(GEOID, 6, 8) %in% c("081") ~ "Queens",
        substr(GEOID, 6, 8) %in% c("085") ~ "Staten Island"
      ),
      population = estimate
    ) %>%
    dplyr::select(GEOID, NAME, borough, population, geometry) %>%
    sf::st_transform(2263)  # NY State Plane (ft)
  
  saveRDS(tracts, cache_file)
  return(tracts)
}

#' Get healthcare facilities from NYC Open Data (FacDB)
get_healthcare_facilities <- function(cache_dir = "data/mp02") {
  cache_file <- file.path(cache_dir, "facilities.rds")
  if (file.exists(cache_file)) return(readRDS(cache_file))
  
  message("Downloading healthcare facilities via Socrata API...")
  app_token <- Sys.getenv("NYC_OPEN_DATA_TOKEN")
  if (app_token == "") {
    warning("NYC_OPEN_DATA_TOKEN not set. Using public access (rate-limited).")
  }
  
  fac <- RSocrata::read.socrata(
    "https://data.cityofnewyork.us/resource/ji82-xba5.json",
    app_token = app_token
  ) %>%
    dplyr::filter(
      facdomain == "HEALTH AND HUMAN SERVICES",
      !is.na(latitude), !is.na(longitude)
    ) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        stringr::str_detect(tolower(facgroup), "hospital") ~ "Hospital",
        stringr::str_detect(tolower(facgroup), "fqhc|clinic|primary|health center") ~ "Primary Care",
        stringr::str_detect(tolower(facgroup), "urgent") ~ "Urgent Care",
        TRUE ~ "Other"
      ),
      borough = dplyr::case_when(
        boro == "BRONX" ~ "Bronx",
        boro == "BROOKLYN" ~ "Brooklyn",
        boro == "MANHATTAN" ~ "Manhattan",
        boro == "QUEENS" ~ "Queens",
        boro == "STATEN ISLAND" ~ "Staten Island",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(borough)) %>%
    dplyr::select(name = facname, type, borough, latitude, longitude) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    sf::st_transform(2263)
  
  saveRDS(fac, cache_file)
  return(fac)
}

#' Get socioeconomic data (ACS 2020)
get_socioeconomic_data <- function(year = 2020, cache_dir = "data/mp02") {
  cache_file <- file.path(cache_dir, sprintf("ses_%d.csv", year))
  if (file.exists(cache_file)) return(readr::read_csv(cache_file, show_col_types = FALSE))
  
  message("Downloading ACS socioeconomic variables...")
  vars <- c(
    "B19013_001", # Median household income
    "B17001_002", # Poverty count
    "B17001_001", # Total for poverty rate
    "B27001_005", "B27001_008", "B27001_011", # Uninsured adults
    "B27001_033", "B27001_036", "B27001_039"  # Uninsured children
  )
  
  ses <- tidycensus::get_acs(
    geography = "tract",
    variables = vars,
    state = "NY",
    county = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
    year = year,
    survey = "acs5"
  ) %>%
    dplyr::select(-moe) %>%
    tidyr::pivot_wider(names_from = variable, values_from = estimate) %>%
    dplyr::mutate(
      GEOID = as.character(GEOID),
      median_income = B19013_001,
      poverty_rate = (B17001_002 / B17001_001) * 100,
      uninsured_rate = (
        (B27001_005 + B27001_008 + B27001_011 +
           B27001_033 + B27001_036 + B27001_039) / B17001_001
      ) * 100
    ) %>%
    dplyr::select(GEOID, median_income, poverty_rate, uninsured_rate)
  
  readr::write_csv(ses, cache_file)
  return(ses)
}

#' Compute minimum distance from each tract to facilities
compute_access_distances <- function(tracts, facilities) {
  message("Computing distances to facilities...")
  dist_matrix <- sf::st_distance(tracts, facilities)
  
  tracts$dist_any <- apply(dist_matrix, 1, min) / 5280  # ft → miles
  
  types <- c("Hospital", "Primary Care", "Urgent Care", "Other")
  for (t in types) {
    col_name <- paste0("dist_", tolower(gsub(" ", "_", t)))
    sub_fac <- facilities %>% dplyr::filter(type == t)
    if (nrow(sub_fac) > 0) {
      tracts[[col_name]] <- apply(sf::st_distance(tracts, sub_fac), 1, min) / 5280
    } else {
      tracts[[col_name]] <- NA_real_
    }
  }
  return(tracts)
}

#' Classify healthcare deserts and vulnerability
classify_deserts <- function(tracts) {
  message("Classifying healthcare deserts...")
  tracts <- tracts %>%
    dplyr::mutate(
      is_desert = dist_any > 1.0,
      is_desert_primary_care = dist_primary_care > 1.0,
      is_desert_urgent_care = dist_urgent_care > 0.75,
      is_desert_hospital = dist_hospital > 3.0,
      vulnerability = (poverty_rate > 20) + (uninsured_rate > 15) + (median_income < 50000),
      priority_index = 0.6 * scales::rescale(dist_any, to = c(100, 0)) +
        0.4 * scales::rescale(vulnerability, to = c(0, 100))
    )
  return(tracts)
}

# Optional: Full pipeline
run_full_analysis <- function() {
  tracts <- get_nyc_tracts()
  facilities <- get_healthcare_facilities()
  ses <- get_socioeconomic_data()
  tracts <- tracts %>%
    dplyr::left_join(ses, by = "GEOID") %>%
    compute_access_distances(facilities) %>%
    classify_deserts()
  return(tracts)
}