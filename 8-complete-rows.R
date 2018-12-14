### filter districts from final data and expand to make implicit NAs in SC, ST rows explicit

library(tidyverse)
library(sf)

f11 <- readRDS("9-final-abb-data/2011.rds") %>%
    select(-c(region, country, subdistrict, state01, state91))

f01 <- readRDS("9-final-abb-data/2001.rds") %>% 
    select(-c(region, country, subdistrict, state11, state91))

f91 <- readRDS("9-final-abb-data/1991.rds") %>% 
    select(-c(region, country, subdistrict, state11, state01))

# use complete() to fill out districts with implicitly missing sc or st rows; 
# otherwise will be completely missing from map
d11 <- f11 %>% 
    filter(geo_section == "district") %>% 
    complete(district_abb, societal_section, demo_section,
             water_source, water_avail, year)

d01 <- f01 %>% 
    filter(geo_section == "district") %>% 
    complete(district_abb, societal_section, demo_section,
             water_source, water_avail, year)

d91 <- f91 %>% 
    filter(geo_section == "district") %>% 
    complete(district_abb, societal_section, demo_section,
             water_source, water_avail, year)

# JK in 91 is entirely missing; need to add these implicitly missing rows; 
# simp91 needed for list of districts in jk
abb_region_df <- readRDS("8-abb-region/abb_region.rds") %>% 
    select(-region)

simp91 <- readRDS("10-simple-shapes/simp91.rds")

simp91 <- simp91 %>% 
    mutate(state = ifelse(state == "Pondicherry", "Puducherry", state)) %>% 
    left_join(abb_region_df, by = c("state" = "state91")) %>% 
    select(-c(state11, state01)) %>% 
    mutate(district_abb = str_c(district, " (", abb, ")"))

jk <- simp91 %>% 
    st_set_geometry(NULL) %>% 
    filter(abb == "JK")

kashmir <- data.frame(matrix(NA, nrow = 14, ncol = 41))

names(kashmir) <- names(d91)

kashmir <- kashmir %>% 
    mutate(
        district_abb = jk$district_abb,
        district = jk$district,
        state = jk$state,
        abb = jk$abb,
        year = 1991,
        geo_section = "district"
    )

# take rows of correct dimension and then replace with correct district names
jk_rows <- d91 %>% 
    slice(1:2646) %>% #14*189
    mutate(
        district_abb = rep(jk$district_abb, each = 189),
        district = rep(jk$district, each = 189),
        state = "Jammu & Kashmir",
        abb = "JK"
    )

jk_rows[10:40] <- NA

d91 <- bind_rows(d91, jk_rows)

# newly expanded rows are missing geo_section, state, district, abb so add them
d11 <- d11 %>%
    mutate(
        geo_section = "district", 
        abb2 = word(district_abb, -1),
        abb2 = str_extract(abb2, "[A-Z]+"),
        district2 = str_sub(district_abb, start = 1, end = -6)
    ) %>% 
    select(-c(abb, district)) %>% 
    rename(abb = abb2, district = district2) %>%
    left_join(abb_region_df, by = "abb") %>%
    select(-c(state, state01, state91)) %>% 
    rename(state = state11)

d01 <- d01 %>% 
    mutate(
        geo_section = "district",
        abb2 = word(district_abb, -1),
        abb2 = str_extract(abb2, "[A-Z]+"),
        district2 = str_sub(district_abb, start = 1, end = -6)
    ) %>% 
    select(-c(abb, district)) %>%
    rename(abb = abb2, district = district2) %>%
    left_join(abb_region_df, by = "abb") %>%
    select(-c(state, state11, state91)) %>% 
    rename(state = state01)

d91 <- d91 %>% 
    mutate(
        geo_section = "district",
        abb2 = word(district_abb, -1),
        abb2 = str_extract(abb2, "[A-Z]+"),
        district2 = str_sub(district_abb, start = 1, end = -6)
    ) %>%
    select(-c(abb, district)) %>%
    rename(abb = abb2, district = district2) %>% 
    left_join(abb_region_df, by = "abb") %>%
    select(-c(state, state11, state01)) %>% 
    rename(state = state91)

# confirm implicitly missing ST rows added:
#d11 %>% filter(district_abb == "Muktsar (PB)") %>% View

if (!dir.exists("11-completed-data")) {
    dir.create("11-completed-data")
}

saveRDS(d11, "11-completed-data/2011.rds")
saveRDS(d01, "11-completed-data/2001.rds")
saveRDS(d91, "11-completed-data/1991.rds")

rm(list = ls())
