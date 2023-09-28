library(tigris)
library(sf)
library(tidycensus)
library(tidyverse)
library(rvest)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

api_key <- "3bcdcf966c06f679fe1abb540eac2f5c33694350"
census_api_key(api_key)
# Check your API key
Sys.getenv("CENSUS_API_KEY")

url <- "https://en.wikipedia.org/wiki/Delaware_Valley"
counties <- read_html(url) %>%
    html_nodes("td") %>%
    html_text %>%
    str_trim %>% 
    .[17:160] %>% 
    matrix(ncol = 9, byrow = TRUE) %>%
    as_tibble() %>% 
    select(1:2) %>%
    rename(county = "V1", state = "V2")
saveRDS(counties, "counties.rds")

my_counties <- readRDS("counties.rds")

# filter it
counties <- my_counties %>% 
    filter(state %in% c("PA", "NJ"),
           county %in% c("Philadelphia","Burlington",
                         "Bucks","Montgomery", "Gloucester", 
                         "Delaware", "Camden")) 
   
# query
raw_tracts <- map2(counties$state, counties$county, function(x, y) {
    get_acs(geography = "tract", state = x, county = y,
            variables = c(hhincome = "B19013_001"), 
            geometry = TRUE)
}) %>% 
    do.call(rbind, .)

# confirm crs
# st_crs(raw_tracts)
# st_is_longlat(raw_tracts)

# reproject
proj_crs <- 2272
proj_tracts <- raw_tracts %>% 
    st_transform(crs = proj_crs)
st_crs(proj_tracts)

# choose a central lng lat point and radius to define circle
city_hall_lng <- -75.1657936
city_hall_lat <- 39.952383
geo_crs <- 4326
buffer <- 55000

circle <- st_sfc(st_point(c(city_hall_lng, city_hall_lat)), 
                 crs = geo_crs) %>% 
    st_transform(crs = proj_crs) %>% 
    st_buffer(dist = buffer)

st_crs(circle)

# perform spatial subset
sgbp <- st_contains(circle, proj_tracts)
subset_tracts <- proj_tracts[unlist(sgbp),]

# final manipulation and save
geo_subset <- subset_tracts %>% 
    separate(col = NAME, into = c("tract", "county", "state"), sep = ", ") %>% 
    mutate(
        tract = word(tract, start = -1),
        county = word(county, sep = " County"),
        state = ifelse(state == "Pennsylvania", "PA", "NJ")
    ) %>% 
    rename(hhincome = estimate) %>% 
    select(-variable) %>% 
    st_transform(crs = geo_crs)

saveRDS(geo_subset, "shiny/geo_sub.rds")

# confirm
plot(st_geometry(proj_tracts))
plot(circle, add = TRUE, color = "red")
plot(st_geometry(subset_tracts))

