library(sf)
library(tidyverse)

# read data ---------------------------------------------------------------

d11 <- readRDS("shiny/data/district/district11.rds")
d01 <- readRDS("shiny/data/district/district01.rds")
d91 <- readRDS("shiny/data/district/district91.rds")

d01 <- d01 %>% 
    mutate(
        num_ea_la = NA, num_ea_ln = NA,
        num_en_la = NA, num_en_ln = NA
    )

d91 <- st_set_crs(d91, NA)

# combine years into one sf -----------------------------------------------

list_of_sf <- list(
    d91, d01, d11
)

sf <- list_of_sf %>% 
    map(
        ~ .x %>% 
            filter(
                water_source == "All Sources",
                water_avail == "Total"
            ) %>% 
            select(
                district_abb, societal_section, demo_section, 
                year, total_hh, num_ea_la, num_ea_ln, 
                num_en_la, num_en_ln, num_ea, num_la,
                ea, la, ea_la, ea_ln, en_la, en_ln
            )
    ) %>% 
    do.call(rbind,.)


decile_labels <- c("0-10%","10-20%","20-30%","30-40%","40-50%",
                   "50-60%","60-70%","70-80%","80-90%","90-100%")

final_sf <- sf %>% 
    #st_crs() %>% 
    # need centroids for grad symbols
    mutate(
        CENTROID = st_as_sfc(purrr::map(geometry, st_centroid))
        #COORDS = purrr::map(CENTROID, st_coordinates),
        #COORDS_X = purrr::map_dbl(COORDS, 1),
        #COORDS_Y = purrr::map_dbl(COORDS, 2)
    ) %>% 
    #st_transform(4326) %>% 
    # bin percentages for choropleth colors
    mutate_at(
        c("ea","la","ea_la","ea_ln","en_la","en_ln"),
        cut, breaks = 10, labels = decile_labels 
    ) %>% 
    # character for legend
    mutate_at(
        c("ea","la","ea_la","ea_ln","en_la","en_ln"),
        as.character 
    ) %>%
    # scale to meters for symbols
    mutate_at(
        vars(starts_with("num_")),
        .funs = funs(sy = . / 10)
    ) %>% 
    # rename for legend
    rename(
        Electricity = ea,
        Latrine = la,
        `Electricity</br>& Latrine` = ea_la,
        `Electricity</br>but no</br>Latrine` = ea_ln,
        `No Electricity</br>but Latrine` = en_la,
        `Neither</br>Electricity</br>nor Latrine` = en_ln
    ) %>% 
    st_set_crs(4326)

# see add_missing.R

saveRDS(final_sf, "shiny_all/final_sf.rds")
