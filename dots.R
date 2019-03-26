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
                num_en_la, num_en_ln, num_ea, num_la
            )
    ) %>% 
    do.call(rbind,.)


# extract geometry as gather seems to struggle with it --------------------

geometry <- sf %>% select(district_abb) %>% unique()

# gather data and rejoin geometry -----------------------------------------

gathered_sf <- sf %>% 
    st_set_geometry(NULL) %>% 
    gather(key = "category", value = "hh", 
           -district_abb, -societal_section, 
           -demo_section, -year, -total_hh) %>%
    arrange(district_abb) %>%
    left_join(geometry) %>% 
    st_as_sf()

# functions for sampling points -------------------------------------------
# source: https://www.andybeger.com/2018/05/11/us-2016-dot-density/

stochastic_round <- function(x){
    ## extract the decimal portion
    q = abs(x - trunc(x))
    
    ## draw a value 0 or 1 with probability
    ## based on how close we already are
    adj = rbinom(n = length(x), size = 1, prob = q)
    
    ## make it negative if x is
    adj <- ifelse(x < 0, adj * -1, adj)
    
    ## return our new value
    trunc(x) + adj
}

# Modified version of sf:::st_poly_sample that always returns correct size
# when sampling a polygon
# one modification: return empty for when size is NA
st_poly_sample_n <- function(x, size) {
    stopifnot(length(x) == 1)
    stopifnot(length(size) == 1)
    if (is.na(size)) {
        return(st_as_sfc("POINT EMPTY"))
    }
    x <- st_geometry(x)
    size <- stochastic_round(size)
    if (size == 0) {
        return(st_as_sfc("POINT EMPTY"))
    } else {
        pts <- st_sample(x, size)
        max_iter <- 10
        iter <- 1
        while (length(pts) < size & !(iter > max_iter)) {
            need <- size - length(pts)
            pts <- c(pts, st_sample(x, need))
            iter <- iter + 1
        }
        if (length(pts) == size) {
            return(pts)
        } else if (length(pts) > size) {
            return(pts[1:size])
        }
    }
}

# Modified version of sf:::st_sample that combines points by sampled polyon
st_sample_by_poly <- function(x, size) {
    x <- st_geometry(x)
    res <- lapply(1:length(x), function(i) {
        y <- st_poly_sample_n(x[i], size[i]) %>%
            st_combine()
        if (st_is_empty(y)) {
            y <- st_sfc(st_multipoint()) #st_as_sfc("MULTIPOINT EMPTY")
        }
        y
    })
    do.call(c, res)
}

# sample points -----------------------------------------------------------

denom <- 25000 # 1 dot is this many people

multipoint_sf <- gathered_sf %>% 
    mutate(
        multipoint = st_sample_by_poly(., size = hh / denom)
    )

# group by category -------------------------------------------------------

final_sf <- multipoint_sf %>% 
    st_set_geometry(.$multipoint) %>% 
    select(-multipoint) %>%
    group_by(
        societal_section, demo_section, year, category
    ) %>% 
    summarise(
        n = n(),
        sum_hh = sum(hh, na.rm = TRUE)
    ) %>% 
    st_set_crs(4326)

# empty geometries need to be empty multipoints ---------------------------

empty <- final_sf %>% 
    filter(sum_hh == 0) %>% 
    mutate(
        geometry = st_sfc(st_multipoint()) %>% 
            st_set_crs(4326)
    )

final_sf <- final_sf %>% 
    filter(!sum_hh == 0) %>% 
    rbind(empty)

# if sum_hh is 0, should be NA --------------------------------------------

final_sf <- final_sf %>% 
    mutate(
        sum_hh = ifelse(sum_hh == 0, NA, sum_hh)
    )

saveRDS(final_sf, "shiny_dots/final_sf.rds")
saveRDS(final_sf, "shiny_all/final_dots.rds")
saveRDS(final_sf, "shiny_all/final_dots1.rds")

# test plot ---------------------------------------------------------------

final_sf %>% 
    arrange(desc(year)) %>% View()

final_sf %>% 
    filter(
        societal_section == "ALL",
        demo_section == "Total",
        year == 2011,
        category == "num_ea_la"
    ) %>% 
    ggplot() +
    geom_sf(size = 0.1)

rural <- final_sf %>% 
    filter(
        societal_section == "ALL",
        demo_section == "Rural",
        year == 2011,
        category == "num_ea_la"
    )
urban <- final_sf %>% 
    filter(
        societal_section == "ALL",
        demo_section == "Urban",
        year == 2011,
        category == "num_ea_la"
    )
ggplot() +
    geom_sf(data = rural, size = 0.1, color = "red") +
    geom_sf(data = urban, size = 0.1, color = "blue")
