### Script to generate country and state level data

# extract country level totals --------------------------------------------

library(tidyverse)

country11 <- readRDS("9-final-abb-data/2011.rds") %>% 
    select(-c(country, state, district, subdistrict, abb, 
              region, state01, district_abb)) %>% 
    filter(geo_section == "country")

country01 <- readRDS("9-final-abb-data/2001.rds") %>% 
    select(-c(country, state, district, subdistrict, abb, 
              region, state11, district_abb)) %>% 
    filter(geo_section == "country")

country91 <- readRDS("9-final-abb-data/1991.rds") %>%
    select(-c(country, state, district, subdistrict, abb, 
              region, state11, state01, district_abb)) %>% 
    filter(geo_section == "country") %>% 

if (!dir.exists("shiny/data/country")) {
    dir.create("shiny/data/country")
}

saveRDS(country11, "shiny/data/country/country11.rds")
saveRDS(country01, "shiny/data/country/country01.rds")
saveRDS(country91, "shiny/data/country/country91.rds")

# Extract state level totals ----------------------------------------------

state11 <- readRDS("9-final-abb-data/2011.rds") %>%
    select(-c(country, district, subdistrict, 
              region, state01, district_abb)) %>% 
    filter(geo_section == "state")

state01 <- readRDS("9-final-abb-data/2001.rds") %>% 
    select(-c(country, district, subdistrict, 
              region, state11, district_abb)) %>% 
    filter(geo_section == "state")

state91 <- readRDS("9-final-abb-data/1991.rds") %>%
    select(-c(country, district, subdistrict, 
              region, state11, state01, district_abb)) %>% 
    filter(geo_section == "state")


if (!dir.exists("shiny/data/state")) {
    dir.create("shiny/data/state")
}

saveRDS(state11, "shiny/data/state/state11.rds")
saveRDS(state01, "shiny/data/state/state01.rds")
saveRDS(state91, "shiny/data/state/state91.rds")

rm(list = ls())
