##### Spread out water_source and water_avail variables from tidy data and join back in
##### output final dataset for 2001

library(tidyverse)

tidy01 <- readRDS("6-tidy-data/2001.rds")

# spread water_avail into a new df
water_avail01 <- tidy01 %>%   
    select(-c(12:21)) %>% # change to 12:ncol(.)
    group_by(year, geo_section, country, state, district, subdistrict,
             societal_section, demo_section, water_source) %>%
    spread(water_avail, total_hh) %>%
    mutate(
        per_within = `Within Premises` / `Total`,
        per_near = `Near Premises` / `Total`,
        per_away = Away / `Total`
    ) %>% 
    ungroup()

# join water_avail df into main tidy df
tidy01_wa <- tidy01 %>% 
    left_join(water_avail01) %>%
    mutate(
        num_total = ifelse(water_avail == "Total", `Total`, NA),
        num_within = ifelse(water_avail == "Total", `Within Premises`, NA),
        num_near = ifelse(water_avail == "Total", `Near Premises`, NA),
        num_away = ifelse(water_avail == "Total", `Away`, NA),
        within = ifelse(water_avail == "Total", per_within, NA),
        near = ifelse(water_avail == "Total", per_near, NA),
        away = ifelse(water_avail == "Total", per_away, NA)
    ) %>%
    select(-c(22:28))

# spread water_source
water_source01 <- tidy01 %>%   
    select(-c(12:21)) %>%
    group_by(year, geo_section, country, state, district, subdistrict,
             societal_section, demo_section, water_avail) %>%
    spread(water_source, total_hh) %>%
    mutate(
        per_tap = `Tap` / `All Sources`,
        per_hand_pump = `Hand pump` / `All Sources`,
        per_tube_well = `Tube well` / `All Sources`,
        per_well = `Well` / `All Sources`,
        per_others = `All others` / `All Sources`
    ) %>% 
    ungroup()

# join water source
tidy01_complete <- tidy01_wa %>% 
    left_join(water_source01) %>%
    mutate(
        num_all = ifelse(water_source == "All Sources", `All Sources`, NA),
        num_tap = ifelse(water_source == "All Sources", `Tap`, NA),
        num_hand_pump = ifelse(water_source == "All Sources", `Hand pump`, NA),
        num_tube_well = ifelse(water_source == "All Sources", `Tube well`, NA),
        num_well = ifelse(water_source == "All Sources", `Well`, NA),
        num_others = ifelse(water_source == "All Sources", `All others`, NA),
        tap = ifelse(water_source == "All Sources", per_tap, NA),
        hand_pump = ifelse(water_source == "All Sources", per_hand_pump, NA),
        tube_well = ifelse(water_source == "All Sources", per_tube_well, NA),
        well = ifelse(water_source == "All Sources", per_well, NA),
        others = ifelse(water_source == "All Sources", per_others, NA)
    ) %>% 
    select(-c(29:39))

# create directories for final data output
if (!dir.exists("7-final-data")) {
    dir.create("7-final-data")
}

# final clean
final01 <- tidy01_complete %>% 
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
        water_source = fct_recode(water_source, 
                                  `All Others` = "All others") # match 2011
    ) %>%
    arrange(year, geo_section, country, state, district, subdistrict,
            societal_section, demo_section, water_source, water_avail)

saveRDS(final01, file = "7-final-data/2001.rds")

rm(list = ls())

