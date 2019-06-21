library(tidyverse)
library(sf)


# create file of district level census data -------------------------------

d11 <- readRDS("shiny/data/district/district11.rds") %>% 
    st_set_geometry(NULL) %>% 
    as_tibble() %>% 
    select(-c(abb, district_abb)) %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(
        num_en = NA_integer_,
        num_ln = NA_integer_,
        num_tap = NA_integer_, 
        num_well = NA_integer_, 
        tap = NA_real_, 
        well = NA_real_,
        num_outside = NA_integer_,
        outside = NA_real_,
        num_handpump_tubewell = NA_integer_,
        num_river_canal = NA_integer_,
        num_tank = NA_integer_,
        handpump_tubewell = NA_real_,
        river_canal = NA_real_,
        tank = NA_real_
    )

d01 <- readRDS("shiny/data/district/district01.rds") %>% 
    st_set_geometry(NULL) %>% 
    as_tibble() %>% 
    select(-c(abb, district_abb)) %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate_if(is.logical, as.double) %>%
    mutate(
        num_ea_la = NA_integer_,
        num_ea_ln = NA_integer_,
        num_en_la = NA_integer_,
        num_en_ln = NA_integer_,
        num_covered_well = NA_integer_,
        num_uncovered_well = NA_integer_,
        num_tap_treated = NA_integer_,
        num_tap_untreated = NA_integer_,
        tap_treated = NA_real_,
        tap_untreated = NA_real_,
        covered_well = NA_real_,
        uncovered_well = NA_real_,
        num_outside = NA_integer_,
        outside = NA_real_,
        num_handpump_tubewell = NA_integer_,
        num_river_canal = NA_integer_,
        num_tank = NA_integer_,
        handpump_tubewell = NA_real_,
        river_canal = NA_real_,
        tank = NA_real_
    )

d91 <- readRDS("shiny/data/district/district91.rds") %>% 
    st_set_geometry(NULL) %>% 
    as_tibble() %>% 
    select(-c(abb, district_abb)) %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(
        num_near = NA_integer_,
        num_away = NA_integer_,
        near = NA_real_,
        away = NA_real_,
        num_tap_treated = NA_integer_,
        num_tap_untreated = NA_integer_,
        num_covered_well = NA_integer_,
        num_uncovered_well = NA_integer_,
        num_hand_pump = NA_integer_,
        num_tube_well = NA_integer_,
        tap_treated = NA_real_,
        tap_untreated = NA_real_,
        covered_well = NA_real_,
        uncovered_well = NA_real_,
        hand_pump = NA_real_,
        tube_well = NA_real_,
        num_en = NA_integer_,
        num_ln = NA_integer_
    )

district_census <- bind_rows(d11, d01, d91) %>% 
    mutate(
        year = case_when(
            year == '2011' ~ as.Date('2011-01-01'),
            year == '2001' ~ as.Date('2001-01-01'),
            year == '1991' ~ as.Date('1991-01-01')
        )
    ) %>% 
    select(geo_section, district, state, year, everything())

# create file of district shapes per year ---------------------------------

d11_shapes <- readRDS("shiny/data/district/district11.rds") %>% 
    filter(
        societal_section == "ALL",
        demo_section == "Total",
        water_source == "All Sources",
        water_avail == "Total"
    ) %>% 
    select(district, state, year) %>% 
    st_set_crs(4326)

d01_shapes <- readRDS("shiny/data/district/district01.rds") %>% 
    filter(
        societal_section == "ALL",
        demo_section == "Total",
        water_source == "All Sources",
        water_avail == "Total"
    ) %>% 
    select(district, state, year) %>% 
    st_set_crs(4326)

d91_shapes <- readRDS("shiny/data/district/district91.rds") %>% 
    filter(
        societal_section == "ALL",
        demo_section == "Total",
        water_source == "All Sources",
        water_avail == "Total"
    ) %>% 
    select(district, state, year) %>% 
    st_set_crs(4326)

district_shapes <- d11_shapes %>% rbind(d01_shapes) %>% rbind(d91_shapes) %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(
        year = case_when(
            year == '2011' ~ as.Date('2011-01-01'),
            year == '2001' ~ as.Date('2001-01-01'),
            year == '1991' ~ as.Date('1991-01-01')
        )
    )


# create file of state level census data ----------------------------------

s11 <- readRDS("shiny/data/state/state11.rds") %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(
        district = NA_character_,
        district_abb = NA_character_,
        num_en = NA_integer_,
        num_ln = NA_integer_,
        num_tap = NA_integer_, 
        num_well = NA_integer_, 
        tap = NA_real_, 
        well = NA_real_,
        num_outside = NA_integer_,
        outside = NA_real_,
        num_handpump_tubewell = NA_integer_,
        num_river_canal = NA_integer_,
        num_tank = NA_integer_,
        handpump_tubewell = NA_real_,
        river_canal = NA_real_,
        tank = NA_real_
    )

s01 <- readRDS("shiny/data/state/state01.rds") %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(
        district = NA_character_,
        district_abb = NA_character_,
        num_ea_la = NA_integer_,
        num_ea_ln = NA_integer_,
        num_en_la = NA_integer_,
        num_en_ln = NA_integer_,
        num_covered_well = NA_integer_,
        num_uncovered_well = NA_integer_,
        num_tap_treated = NA_integer_,
        num_tap_untreated = NA_integer_,
        tap_treated = NA_real_,
        tap_untreated = NA_real_,
        covered_well = NA_real_,
        uncovered_well = NA_real_,
        num_outside = NA_integer_,
        outside = NA_real_,
        num_handpump_tubewell = NA_integer_,
        num_river_canal = NA_integer_,
        num_tank = NA_integer_,
        handpump_tubewell = NA_real_,
        river_canal = NA_real_,
        tank = NA_real_
    )

s91 <- readRDS("shiny/data/state/state91.rds") %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(
        district = NA_character_,
        district_abb = NA_character_,
        num_near = NA_integer_,
        num_away = NA_integer_,
        near = NA_real_,
        away = NA_real_,
        num_tap_treated = NA_integer_,
        num_tap_untreated = NA_integer_,
        num_covered_well = NA_integer_,
        num_uncovered_well = NA_integer_,
        num_hand_pump = NA_integer_,
        num_tube_well = NA_integer_,
        tap_treated = NA_real_,
        tap_untreated = NA_real_,
        covered_well = NA_real_,
        uncovered_well = NA_real_,
        hand_pump = NA_real_,
        tube_well = NA_real_,
        num_en = NA_integer_,
        num_ln = NA_integer_
    )

state_census <- bind_rows(s11, s01, s91) %>%
    mutate(
        year = case_when(
            year == '2011' ~ as.Date('2011-01-01'),
            year == '2001' ~ as.Date('2001-01-01'),
            year == '1991' ~ as.Date('1991-01-01')
        )
    ) %>% 
    select(geo_section, state, year, everything()) %>% 
    select(-c(abb, district, district_abb))
    


# create file of state shapes per year ------------------------------------

state_shapes <- readRDS("shiny_dots/state_lines.rds") %>% 
    st_cast("MULTIPOLYGON") %>% 
    mutate(
        year = case_when(
            year == '2011' ~ as.Date('2011-01-01'),
            year == '2001' ~ as.Date('2001-01-01'),
            year == '1991' ~ as.Date('1991-01-01')
        )
    )

# create file of states, abb, region per year -----------------------------
uts <- c("Chandigarh", "Dadra & Nagar Haveli", "Daman & Diu",
         "Lakshadweep", "Andaman & Nicobar Islands", "Delhi", 
         "Puducherry", "Pondicherry")

abb_region <- readRDS("8-abb-region/abb_region.rds") 

states_abb_region_ut <- bind_rows(
    abb_region %>% 
        select(state11, abb, region) %>% 
        mutate(year = as.Date("2011-01-01")) %>% 
        rename(state = state11),
    abb_region %>% 
        select(state01, abb, region) %>% 
        mutate(year = as.Date("2001-01-01")) %>% 
        rename(state = state01),
    abb_region %>%
        select(state91, abb, region) %>% 
        mutate(year = as.Date("1991-01-01")) %>% 
        rename(state = state91)
) %>% 
    mutate(ut_status = ifelse(state %in% uts, TRUE, FALSE)) %>% 
    filter(!is.na(state))


# create file of country-level data ---------------------------------------

c11 <- readRDS("shiny/data/country/country11.rds") %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(
        num_en = NA_integer_,
        num_ln = NA_integer_,
        num_tap = NA_integer_, 
        num_well = NA_integer_, 
        tap = NA_real_, 
        well = NA_real_,
        num_outside = NA_integer_,
        outside = NA_real_,
        num_handpump_tubewell = NA_integer_,
        num_river_canal = NA_integer_,
        num_tank = NA_integer_,
        handpump_tubewell = NA_real_,
        river_canal = NA_real_,
        tank = NA_real_
    )

c01 <- readRDS("shiny/data/country/country01.rds") %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(
        num_ea_la = NA_integer_,
        num_ea_ln = NA_integer_,
        num_en_la = NA_integer_,
        num_en_ln = NA_integer_,
        num_covered_well = NA_integer_,
        num_uncovered_well = NA_integer_,
        num_tap_treated = NA_integer_,
        num_tap_untreated = NA_integer_,
        tap_treated = NA_real_,
        tap_untreated = NA_real_,
        covered_well = NA_real_,
        uncovered_well = NA_real_,
        num_outside = NA_integer_,
        outside = NA_real_,
        num_handpump_tubewell = NA_integer_,
        num_river_canal = NA_integer_,
        num_tank = NA_integer_,
        handpump_tubewell = NA_real_,
        river_canal = NA_real_,
        tank = NA_real_
    )

c91 <- readRDS("shiny/data/country/country91.rds") %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(
        num_near = NA_integer_,
        num_away = NA_integer_,
        near = NA_real_,
        away = NA_real_,
        num_tap_treated = NA_integer_,
        num_tap_untreated = NA_integer_,
        num_covered_well = NA_integer_,
        num_uncovered_well = NA_integer_,
        num_hand_pump = NA_integer_,
        num_tube_well = NA_integer_,
        tap_treated = NA_real_,
        tap_untreated = NA_real_,
        covered_well = NA_real_,
        uncovered_well = NA_real_,
        hand_pump = NA_real_,
        tube_well = NA_real_,
        num_en = NA_integer_,
        num_ln = NA_integer_
    )

country_census <- bind_rows(c11, c01, c91) %>% 
    mutate(
        year = case_when(
            year == '2011' ~ as.Date('2011-01-01'),
            year == '2001' ~ as.Date('2001-01-01'),
            year == '1991' ~ as.Date('1991-01-01')
        )
    ) %>% 
    select(geo_section, everything())

# create file of dot geometry per electricity/latrine category ------------

electricity_latrine_dots <- readRDS("shiny_thematic_mapping/final_dots.rds") %>% 
    ungroup() %>% 
    mutate(
        year = case_when(
            year == 2011 ~ as.Date('2011-01-01'),
            year == 2001 ~ as.Date('2001-01-01'),
            year == 1991 ~ as.Date('1991-01-01')
        )
    )    

# output census tables to csv ---------------------------------------------

census_tables <- c("district_census", "state_census",
                   "country_census", "states_abb_region_ut")

if (!dir.exists("sql")) {
    dir.create("sql")
}

walk(census_tables, ~ {
    write_csv(get(.x), paste0("sql/", .x, ".csv"))
})

spatial_tables <- c("district_shapes", "state_shapes", 
                    "electricity_latrine_dots")

walk(spatial_tables, ~ {
    st_write(get(.x), paste0("sql/", .x, ".shp"))
})
