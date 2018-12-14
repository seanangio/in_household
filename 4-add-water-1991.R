##### Spread out water_source and water_avail variables from tidy data and join back in
##### output final dataset for 1991

library(tidyverse)

tidy91 <- readRDS("6-tidy-data/1991.rds")

# spread water_avail into a new df
water_avail91 <- tidy91 %>%   
    select(-c(12:ncol(.))) %>%
    group_by(year, geo_section, country, state, district, subdistrict,
             societal_section, demo_section, water_source) %>%
    spread(water_avail, total_hh) %>%
    mutate(
        per_within = `Within Premises` / `Total`,
        per_outside = `Outside Premises` / `Total`
    ) %>%
    ungroup()

# join water_avail df into main tidy df
tidy91_wa <- tidy91 %>% 
    left_join(water_avail91) %>%
    mutate(
        num_total = ifelse(water_avail == "Total", `Total`, NA),
        num_within = ifelse(water_avail == "Total", `Within Premises`, NA),
        num_outside = ifelse(water_avail == "Total", `Outside Premises`, NA),
        within = ifelse(water_avail == "Total", per_within, NA),
        outside = ifelse(water_avail == "Total", per_outside, NA)
    ) %>% 
    select(-c(24:28))

# spread water_source
water_source91 <- tidy91 %>%   
    select(-c(12:ncol(.))) %>%
    group_by(year, geo_section, country, state, district, subdistrict,
             societal_section, demo_section, water_avail) %>%
    spread(water_source, total_hh) %>%
    mutate(
        per_well = `Well` / `All Sources`,
        per_tap = `Tap` / `All Sources`,
        per_handpump_tubewell = `Handpump/Tubewell` / `All Sources`,
        per_river_canal = `River/Canal` / `All Sources`,
        per_tank = `Tank` / `All Sources`,
        per_others = `Others` / `All Sources`
    ) %>% 
    ungroup()

# join water source
tidy91_complete <- tidy91_wa %>% 
    left_join(water_source91) %>%
    mutate(
        num_all = ifelse(water_source == "All Sources", `All Sources`, NA),
        num_well = ifelse(water_source == "All Sources", `Well`, NA),
        num_tap = ifelse(water_source == "All Sources", `Tap`, NA),
        num_handpump_tubewell = ifelse(water_source == "All Sources", 
                                       `Handpump/Tubewell`, NA),
        num_river_canal = ifelse(water_source == "All Sources", 
                                 `River/Canal`, NA),
        num_tank = ifelse(water_source == "All Sources", `Tank`, NA),
        num_others = ifelse(water_source == "All Sources", `Others`, NA),
        well = ifelse(water_source == "All Sources", per_well, NA),
        tap = ifelse(water_source == "All Sources", per_tap, NA),
        handpump_tubewell = ifelse(water_source == "All Sources", 
                                   per_handpump_tubewell, NA),
        river_canal = ifelse(water_source == "All Sources", 
                             per_river_canal, NA),
        tank = ifelse(water_source == "All Sources", per_tank, NA),
        others = ifelse(water_source == "All Sources", per_others, NA)
    ) %>% 
    select(-c(29:41))

# create directories for final data output
if (!dir.exists("7-final-data")) {
    dir.create("7-final-data")
}

# final clean
final91 <- tidy91_complete %>% 
    mutate(
        district = str_squish(district),
        geo_section = factor(geo_section, 
                             levels = c("country", "state", "district", 
                                        "subdistrict"), 
                             ordered = TRUE),
        country = as_factor(country),
        state = str_to_title(state),
        societal_section = factor(societal_section, 
                                  levels = c("ALL", "SC", "ST"),
                                  ordered = TRUE),
        demo_section = factor(demo_section, 
                              levels = c("Total", "Rural", "Urban"),
                              ordered = TRUE),
        water_source = as_factor(water_source),
        water_avail = as_factor(water_avail),
        water_source = fct_recode(water_source, `All Others` = "Others")
    ) %>% 
    arrange(year, geo_section, country, state, district, subdistrict,
            societal_section, demo_section, water_source, water_avail)

saveRDS(final91, file = "7-final-data/1991.rds")

rm(list = ls())
