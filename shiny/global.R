library(sf)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(dplyr)


# Set global variables ----------------------------------------------------

clng <- -75.1
clat <- 39.95
my_geo_crs <- 4326
my_proj_crs <- 2272
plot_height <- 300
axis_text_size <- 12
axis_title_size <- 14
plot_title_size <- 15
city_hall_lng <- -75.1657936
city_hall_lat <- 39.952383

# Choices for topo relations dropdown
topo_relations <- c(
    "Intersects",
    "Disjoint",
    "Touches",
    "Crosses",
    "Within",
    "Contains",
    "Overlaps",
    "Covered By"
)


# Load/transform data -----------------------------------------------------

# data loads with a geographic crs
tracts_geo <- readRDS("geo_sub.rds") # WGS84

# needed in place of transforming sf object with 0 observations
empty_geo <- tracts_geo %>% filter(is.null(GEOID))

# drop geometry for base histogram which doesn't change
tracts_df <- tracts_geo %>% 
    st_set_geometry(NULL)

# sometimes need a projected crs
tracts_proj <- tracts_geo %>% 
    st_transform(crs = my_proj_crs)

# need union data for outer shape
tracts_geo_union <- tracts_geo %>%
    st_union()

# union in projected crs needed to check if point is inside
tracts_proj_union <- tracts_geo_union %>% 
    st_transform(crs = my_proj_crs)

# transform lng, lat numbers into a projected sfc POINT
make_proj_point <- function(lng, lat) {
    
    st_sfc(st_point(c(lng, lat)), crs = my_geo_crs) %>% 
    st_transform(crs = my_proj_crs)    
    
}

# Spatial subset-related functions ----------------------------------------

# returns logical whether projected point lies within projected boundary polygon
# inverse: st_contains(proj_boundary, proj_point) etc
is_within_boundary <- function(proj_point, proj_boundary) {
    st_within(proj_point, proj_boundary,
              sparse = FALSE) %>%
        .[1,1]
}

# returns an sfc polygon with a projected crs
calculate_circle <- function(lng, lat, buffer) {
    st_sfc(st_point(c(lng, lat)), crs = my_geo_crs) %>%
        st_transform(crs = my_proj_crs) %>% 
        st_buffer(dist = buffer) 
}

# performs chosen spatial subset; returns filtered sf
subset_data <- function(circle_proj, topo_operation) {
    
    topo <- switch(topo_operation,
           "Intersects" = st_intersects,
           "Disjoint" = st_disjoint,
           "Touches" = st_touches,
           "Crosses" = st_crosses, 
           "Within" = st_within,
           "Contains" = st_contains,
           "Overlaps" = st_overlaps,
           "Covered By" = st_covered_by
           )
    
    tracts_proj %>%
        filter(
            topo(x = ., y = circle_proj,
                 sparse = FALSE)
        )
}

# checks if returned sf has 0 observations; 
#if so, can't be transformed so return the empty sf with a geo crs
is_zero_geo_transform <- function(proj_data) {
    if (nrow(proj_data) != 0) {
        st_transform(proj_data, crs = my_geo_crs)
    } else {
        empty_geo
    }
}

# prints sf subset call text
get_subset_call <- function(topo_operation) {
    
    topo <- switch(topo_operation,
                   "Intersects" = "st_intersects",
                   "Disjoint" = "st_disjoint",
                   "Touches" = "st_touches", 
                   "Crosses" = "st_crosses",
                   "Within" = "st_within",
                   "Contains" = "st_contains",
                   "Overlaps" = "st_overlaps",
                   "Covered By" = "st_covered_by"
    )
    paste0(
"tracts[circle, , op = ", topo, "] # OR\n\n",
"tracts %>%
    filter(", topo, "(x = ., y = circle,
    sparse = FALSE))"
    )
    
}

# Draw the plot -----------------------------------------------------------

draw_histogram <- function(df, point) {
    
    p <- ggplot(tracts_df) +
        geom_histogram(aes(x = hhincome / 1000)) +
        scale_x_continuous("Median Household Income ($k)",
                           labels = scales::dollar) +
        scale_y_continuous("Tract Count") +
        labs(title = "Census Tracts by Median Household Income") +
        theme(
            axis.text = element_text(size = axis_text_size),
            axis.title = element_text(size = axis_title_size, 
                                      face = "bold"),
            plot.title = element_text(size = plot_title_size)
        )
    
    if (is.null(point)) {
        return(p)
        
    } else {
        p <- p +
            labs(subtitle = paste0( "Selected ", nrow(df), 
                                    " of ", nrow(tracts_df), 
                                    " tracts in red")) +
            scale_color_discrete(guide = FALSE)
        
        if (!nrow(df) == 0) {
            p +
                geom_histogram(data = df, 
                               aes(x = hhincome / 1000, 
                                   color = "Subset"))
        } else {
            p
        }
            
    }
}


# Leaflet functions -------------------------------------------------------

draw_base_map <- function() {
    
    leaflet(
        options = leafletOptions(minZoom = 10, maxZoom = 15)
    ) %>%
        addTiles(
            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        addResetMapButton() %>% 
        setView(lng = clng, lat = clat, zoom = 11) %>% 
        addMarkers(lng = city_hall_lng, lat = city_hall_lat, 
                   label = "City Hall") %>% 
        addPolylines(data = tracts_geo_union)
}

draw_circle <- function(map, my_lng, my_lat, buffer) {

    leafletProxy(map) %>%
        addCircles(lng = my_lng, lat = my_lat,
                   radius = buffer * 0.3048, # crs is in ft; leaflet radius in meters
                   layerId = "selection", # adding layerId makes it plot only most recent
                   fillOpacity = 0.1) 
}

pal <- colorNumeric(palette = "viridis", domain = NULL)

draw_map_colors <- function(map, sf) {
    
    leafletProxy(map, data = sf) %>% 
        clearShapes() %>% 
        addPolygons(
            stroke = TRUE,
            weight = 1,
            opacity = 1,
            color = "white",
            fillOpacity = 0.6, 
            fillColor = ~ pal(hhincome)
        ) %>% 
        addPolylines(data = tracts_geo_union)
}

draw_map_legend <- function(map, df) {
    leafletProxy(map, data = df) %>%
        clearControls() %>%
        addLegend(
            "bottomleft",
            pal = pal, 
            values = ~ hhincome / 1000,
            title = ~ "Median</br>HHI ($k)",
            opacity = 1
        )
}
