##### Spread out water_source and water_avail variables from tidy data and join back in
##### output final dataset for 2011

library(tidyverse)

tidy11 <- readRDS("6-tidy-data/2011.rds")

# spread water_avail into a new df
water_avail11 <- tidy11 %>%   
    select(-c(12:ncol(.))) %>%
    group_by(year, geo_section, country, state, district, subdistrict,
             societal_section, demo_section, water_source) %>%
    spread(water_avail, total_hh) %>%
    mutate(
        per_within = `Within premises` / `Total number of households`,
        per_near = `Near premises` / `Total number of households`,
        per_away = Away / `Total number of households`
    ) %>% 
    ungroup()

# join water_avail df into main tidy df
tidy11_wa <- tidy11 %>% 
    left_join(water_avail11) %>%
    mutate(
        num_total = ifelse(water_avail == "Total number of households", 
                           `Total number of households`, NA),
        num_within = ifelse(water_avail == "Total number of households", 
                            `Within premises`, NA),
        num_near = ifelse(water_avail == "Total number of households", 
                          `Near premises`, NA),
        num_away = ifelse(water_avail == "Total number of households", 
                          `Away`, NA),
        within = ifelse(water_avail == "Total number of households", 
                        per_within, NA),
        near = ifelse(water_avail == "Total number of households", 
                      per_near, NA),
        away = ifelse(water_avail == "Total number of households", 
                      per_away, NA)
    ) %>%
    select(-c(24:30))

# spread water_source
water_source11 <- tidy11 %>%   
    select(-c(12:ncol(.))) %>%
    group_by(year, geo_section, country, state, district, subdistrict,
             societal_section, demo_section, water_avail) %>%
    spread(water_source, total_hh) %>% # need to make it a factor variable!
    mutate(
        per_tap_treated = `Tap water from treated source` / `All Sources`,
        per_tap_untreated = `Tap water from un-treated source` / `All Sources`,
        per_covered_well = `Covered well` / `All Sources`,
        per_uncovered_well = `Un-Covered well` / `All Sources`,
        per_hand_pump = `Hand pump` / `All Sources`,
        per_tube_well = `Tube well/Borehole` / `All Sources`,
        per_others = `All Others` / `All Sources`
    ) %>% 
    ungroup()

# join water source
tidy11_complete <- tidy11_wa %>% 
    left_join(water_source11) %>%
    mutate(
        num_all = ifelse(water_source == "All Sources", `All Sources`, NA),
        num_tap_treated = ifelse(water_source == "All Sources", 
                                 `Tap water from treated source`, NA),
        num_tap_untreated = ifelse(water_source == "All Sources", 
                                   `Tap water from un-treated source`, NA),
        num_covered_well = ifelse(water_source == "All Sources", 
                                  `Covered well`, NA),
        num_uncovered_well = ifelse(water_source == "All Sources", 
                                    `Un-Covered well`, NA),
        num_hand_pump = ifelse(water_source == "All Sources", `Hand pump`, NA),
        num_tube_well = ifelse(water_source == "All Sources", 
                               `Tube well/Borehole`, NA),
        num_others = ifelse(water_source == "All Sources", `All Others`, NA),
        tap_treated = ifelse(water_source == "All Sources", 
                             per_tap_treated, NA),
        tap_untreated = ifelse(water_source == "All Sources", 
                               per_tap_untreated, NA),
        covered_well = ifelse(water_source == "All Sources", 
                              per_covered_well, NA),
        uncovered_well = ifelse(water_source == "All Sources", 
                                per_uncovered_well, NA),
        hand_pump = ifelse(water_source == "All Sources", per_hand_pump, NA),
        tube_well = ifelse(water_source == "All Sources", per_tube_well, NA),
        others = ifelse(water_source == "All Sources", per_others, NA)
    ) %>% 
    select(-c(31:45))

# create directories for final data output
if (!dir.exists("7-final-data")) {
    dir.create("7-final-data")
}

# final clean
final11 <- tidy11_complete %>%
    # rename to match simpler 2001 levels
    mutate(
        # remove whitespace causing problems in district name
        district = str_squish(district),
        
        water_avail = case_when(
            water_avail == "Total number of households" ~ "Total",
            water_avail == "Within premises" ~ "Within Premises",
            water_avail == "Near premises" ~ "Near Premises",
            TRUE ~ water_avail
        ),
        # correct spellings
        state = str_to_title(state),
        state = case_when(
            state == "Nct Of Delhi" ~ "Delhi",
            state == "Chattisgarh" ~ "Chhattisgarh",
            TRUE ~ state
        )
    ) %>% 
    mutate(
        geo_section = factor(geo_section, 
                                levels = c("country", "state", "district", 
                                           "subdistrict"), 
                                ordered = TRUE),
        country = as_factor(country),
        societal_section = factor(societal_section, 
                                  levels = c("ALL", "SC", "ST"),
                                  ordered = TRUE),
        demo_section = factor(demo_section, 
                              levels = c("Total", "Rural", "Urban"),
                                 ordered = TRUE),
        water_source = as_factor(water_source),
        water_avail = as_factor(water_avail)
    ) %>%
    arrange(year, geo_section, country, state, district, subdistrict,
            societal_section, demo_section, water_source, water_avail)


saveRDS(final11, file = "7-final-data/2011.rds")

rm(list = ls())
