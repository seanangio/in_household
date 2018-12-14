### prepare simplified district and state level shapes

library(tidyverse)
library(sf)
library(rmapshaper)

# 640 districts after removing Data Not Available
s11 <- st_read("shapefiles/Census_2011") %>%
    filter(!censuscode == 0) %>% 
    select(1:2) %>% 
    rename(
        state = ST_NM,
        district = DISTRICT
    ) %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(
        state = case_when(
            state == "Andaman & Nicobar Island" ~ "Andaman & Nicobar Islands",
            state == "Arunanchal Pradesh" ~ "Arunachal Pradesh",
            state == "Dadara & Nagar Havelli" ~ "Dadra & Nagar Haveli",
            state == "NCT of Delhi" ~ "Delhi",
            TRUE ~ state
        )
    ) %>% 
    arrange(state, district)

# 593 after removing Data Not Available
s01 <- st_read("shapefiles/Census_2001") %>% 
    filter(!DISTRICT == "Data Not Available") %>%
    select(c(1, 4)) %>% 
    rename(
        state = ST_NM,
        district = DISTRICT
    ) %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(
        state = case_when(
            state == "Andaman & Nicobar Island" ~ "Andaman & Nicobar Islands",
            state == "Arunanchal Pradesh" ~ "Arunachal Pradesh",
            state == "Dadara & Nagar Havelli" ~ "Dadra & Nagar Haveli",
            state == "Delhi & NCR" ~ "Delhi",
            state == "Delhi  & NCR" ~ "Delhi",
            state == "Jammu and Kashmir" ~ "Jammu & Kashmir",
            state == "Uttarakhand" ~ "Uttaranchal",
            state == "Puducherry" ~ "Pondicherry",
            TRUE ~ state
        )
    ) %>% 
    arrange(state, district)

# 466 after grouping into multipolygons where needed
s91 <- st_read("shapefiles/Census_1991") %>% 
    group_by(DIST91_ID, NAME, STATE_UT) %>% 
    summarize(dist = mean(DIST91_ID)) %>% 
    ungroup() %>% 
    filter(!DIST91_ID %in% c(9999, 0)) %>%
    select(c(2:3)) %>% 
    rename(
        state = STATE_UT,
        district = NAME
    ) %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(
        district = str_replace_all(district, "_", " "),
        state = str_replace_all(state, "_", " "),
        district = str_to_title(district),
        state = str_to_title(state),
        state = case_when(
            state == "Andaman & Nicobar Is" ~ "Andaman & Nicobar Islands",
            state == "Karnatak" ~ "Karnataka",
            TRUE ~ state
        )
    ) %>% 
    arrange(state, district)


# simplify geometry
simp11 <- ms_simplify(s11, keep = 0.01, keep_shapes = TRUE)
simp01 <- ms_simplify(s01, keep = 0.01, keep_shapes = TRUE)
simp91 <- ms_simplify(s91, keep = 0.01, keep_shapes = TRUE)

# create directories for simplified sf's
if (!dir.exists("10-simple-shapes")) {
    dir.create("10-simple-shapes")
}

saveRDS(simp11, "10-simple-shapes/simp11.rds")
saveRDS(simp01, "10-simple-shapes/simp01.rds")
saveRDS(simp91, "10-simple-shapes/simp91.rds")
