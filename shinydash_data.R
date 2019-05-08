library(tidyverse)
library(sf)

# create missing columns so each year matches -----------------------------

d11 <- readRDS("shiny/data/district/district11.rds") %>% 
    filter(
        water_source == "All Sources",
        water_avail == "Total"
    ) %>% 
    mutate_if(is.factor, as.character) %>% 
    select(1:6, 9:11, 18:19)

d01 <- readRDS("shiny/data/district/district01.rds") %>% 
    filter(
        water_source == "All Sources",
        water_avail == "Total"
    ) %>% 
    mutate_if(is.factor, as.character) %>% 
    select(1:6, 9:11, 16:17)

d91 <- readRDS("shiny/data/district/district91.rds") %>% 
    filter(
        water_source == "All Sources",
        water_avail == "Total"
    ) %>% 
    mutate_if(is.factor, as.character) %>% 
    select(1:6, 9:11, 18:19) %>% 
    mutate(state = recode(state, "Puducherry" = "Pondicherry"))


# do the same for state-level ---------------------------------------------

s11 <- readRDS("shiny/data/state/state11.rds") %>% 
    filter(
        water_source == "All Sources",
        water_avail == "Total"
    ) %>% 
    mutate_if(is.factor, as.character) %>%
    select(1:4, 7:8, 15:16, abb)

s01 <- readRDS("shiny/data/state/state01.rds") %>%
    filter(
        water_source == "All Sources",
        water_avail == "Total"
    ) %>% 
    mutate_if(is.factor, as.character) %>%
    select(1:4, 7:8, 13:14, abb) 

s91 <- readRDS("shiny/data/state/state91.rds") %>% 
    filter(
        water_source == "All Sources",
        water_avail == "Total"
    ) %>% 
    mutate_if(is.factor, as.character) %>%
    select(1:4, 7:8, 15:16, abb) %>% 
    mutate(state = recode(state, "Puducherry" = "Pondicherry"))

# and again for country-level ---------------------------------------------

c11 <- readRDS("shiny/data/country/country11.rds") %>% 
    filter(
        water_source == "All Sources",
        water_avail == "Total"
    ) %>% 
    mutate_if(is.factor, as.character) %>%
    select(1:4, 7, 14:15)

c01 <- readRDS("shiny/data/country/country01.rds") %>% 
    filter(
        water_source == "All Sources",
        water_avail == "Total"
    ) %>% 
    mutate_if(is.factor, as.character) %>%
    select(1:4, 7, 12:13)

c91 <- readRDS("shiny/data/country/country91.rds") %>%
    filter(
        water_source == "All Sources",
        water_avail == "Total"
    ) %>% 
    mutate_if(is.factor, as.character) %>%
    select(1:4, 7, 14:15)

# bind years together and arrange -----------------------------------------

districts <- d11 %>% 
    rbind(d01) %>% 
    st_set_crs(4326) %>% 
    rbind(d91)

state_shapes <- readRDS("shiny_dots/state_lines.rds") %>% 
    st_cast("MULTIPOLYGON")

states <- s11 %>% 
    rbind(s01) %>% 
    rbind(s91) %>% 
    mutate(
        year = as.character(year),
        district = NA_character_,
        district_abb = NA_character_
    ) %>% 
    left_join(state_shapes) %>% 
    st_as_sf()

states_districts <- states %>% 
    rbind(districts) %>% 
    select(geo_section, state, abb, district, 
           district_abb, year, everything())

country <- c11 %>% rbind(c01) %>% rbind(c91) %>% 
    select(-geo_section) %>% 
    mutate(
        ai_tip = str_c(
            "Electricity: ", round(ea, 2), "</b>",
            "<br>Latrine: ", round(la, 2), "</b>",
            "</span></div>"
        )
    )    



# cleaning for dash data --------------------------------------------------
uts <- c("Chandigarh", "Dadra & Nagar Haveli", "Daman & Diu",
         "Lakshadweep", "Andaman & Nicobar Islands", "Delhi", 
         "Puducherry", "Pondicherry")

states_districts <- states_districts %>% 
    mutate(
        geo_section = case_when(
            geo_section == "state" ~ "State",
            geo_section == "district" ~ "District"
        ),
        ut_status = ifelse(state %in% uts, TRUE, FALSE),
        zone = case_when(
            abb %in% c("CH","DL","HR","HP","JK","PB","RJ") ~ "Northern",
            abb %in% c("AS","AR","MN","ML","MZ","NL","TR","SK") ~ "North Eastern",
            abb %in% c("CT","MP","UT","UP") ~ "Central",
            abb %in% c("BR","JH","OR","WB") ~ "Eastern",
            abb %in% c("DN","DD","GA","GJ","MH") ~ "Western",
            abb %in% c("AP","KA","KL","PY","TN","AN","LD") ~ "Southern",
            TRUE ~ "Other"
        ),
        zone_color = case_when(
            zone == "Northern" ~ "#1F77B4FF", 
            zone == "North Eastern" ~ "#2CA02CFF",
            zone == "Central" ~ "#FF7F0EFF", 
            zone == "Eastern" ~ "#D62728FF", 
            zone == "Western" ~ "#9467BDFF", 
            zone == "Southern" ~ "#8C564BFF"
        ),
        my_subtitle = str_c(demo_section, " ", societal_section, 
                            " Households in ", year, ": ", geo_section, 
                            "-level") %>% 
            str_remove("ALL") %>% 
            str_squish(),
        greater = ifelse(ea - la > 0, "electricity", "latrine"),
        dumbbell_y = case_when(
            geo_section == "State" ~ state,
            geo_section == "District" ~ district_abb
        ),
        tip = case_when(
            geo_section == "State" ~ str_c(
                state, "</b>",
                "<br>Electricity: ", round(ea, 2), "</b>",
                "<br>Latrine: ", round(la, 2), "</b>",
                "<br>Households: ", prettyNum(total_hh, big.mark = ","), "</b>",
                "</span></div>"),
            geo_section == "District" ~ str_c(
                district_abb, "</b>",
                "<br>Electricity: ", round(ea, 2), "</b>",
                "<br>Latrine: ", round(la, 2), "</b>",
                "<br>Households: ", prettyNum(total_hh, big.mark = ","), "</b>",
                "</span></div>")
        ),
        x = cut(ea, breaks = 3, labels = c(1,2,3)),
        y = cut(la, breaks = 3, labels = c(1,2,3)),
        x = as.integer(x),
        y = as.integer(y)
    ) %>% 
    arrange(desc(year))

# calculate centroids and fix diu center
diu <- st_sfc(st_point(c(70.9805363, 20.71512))) %>% 
    st_set_crs(4326) %>% 
    st_transform(24343)

# transform to calculate centers
my_sf <- states_districts %>% 
    st_transform(crs = 24343) %>% 
    mutate(
        mid = st_centroid(.$geometry),
        mid = st_sfc(ifelse(state == "Daman & Diu", diu, mid)),
        COORDS = purrr::map(mid, st_coordinates),
        COORDS_X = purrr::map_dbl(COORDS, 1),
        COORDS_Y = purrr::map_dbl(COORDS, 2)
    ) %>%
    select(-c(mid, COORDS))

# save output -------------------------------------------------------------

saveRDS(my_sf, "shinydash/data/states_districts.rds")
saveRDS(country, "shinydash/data/country.rds")

