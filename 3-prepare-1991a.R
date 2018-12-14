##### Script to clean 1991 state total files (all, sc, st) into one tidy file

library(tidyverse)

# create directory for aggregated data output
if (!dir.exists("1-raw-data-91-totals")) {
    dir.create("1-raw-data-91-totals")
}

tot91_files <- list.files("1-raw-data-91-totals", 
                          recursive = TRUE, full.names = TRUE)

### Step 1: first handle "ALL" files (SC/ST later)

tot91a <- read_csv(tot91_files[1], skip = 8) %>% 
    rename_at(vars(names(.)), ~ 1:8) %>% 
    mutate_at(1:8, as.character)

# UP at end of first file is missing rows of data!
missing_UP_rows <- as_tibble(matrix("MISSING", nrow = 54, ncol = 8)) %>% 
    rename_at(vars(names(.)), ~ 1:8)

tot91b <- read_csv(tot91_files[2], skip = 9) %>% 
    rename_at(vars(names(.)), ~ 1:8) %>% 
    mutate_at(1:8, as.character)

# bind both parts of divided file, including placeholders for missing data
tot91 <- tot91a %>% 
    bind_rows(missing_UP_rows) %>% 
    bind_rows(tot91b) %>% 
    select(-6)

# extract only numeric data into a df
numbers91 <- tot91 %>%
    select(`3`:`8`) %>% 
    na.omit()

# from first column create vectors for identifying columns
column1 <- tot91 %>% 
    select(1) %>% 
    na.omit() %>% 
    pull

# remove extra missing values
column1 <- column1[-c(601:635)]

state_indices <- seq(from = 1, to = length(column1), by = 25)
states91 <- column1[state_indices]
state_col <- rep(states91, each = 63)

# 32 because 32 states/uts including india
demo_options <- c("Total", "Rural", "Urban")
demo_col <- rep(demo_options, each = 3 * 7, times = 32)

water_sources <- c("All Sources", "Well", "Tap", "Handpump/ Tubewell", 
                   "River/ Canal", "Tank", "Others")
water_source_col <- rep(water_sources, each = 3, times = 3 * 32)

water_avail <- c("T", "W", "O")
water_avail_col <- rep(water_avail, each = 1, times = 7 * 3 * 32)

# create the tidy df structure
cols <- tibble(
    year = 1991,
    geo_section = "state",
    country = "INDIA",
    state = state_col,
    district = NA,
    subdistrict = NA,
    societal_section = "ALL",
    demo_section = demo_col,
    water_source = water_source_col,
    water_avail = water_avail_col
) 

new_names <- c("total_hh", "num_ea_la", "num_ea_ln", "num_en_la", "num_en_ln")

# bind numeric data back into tidy df structure
tidy_totals_91 <- cols %>% bind_cols(numbers91) %>%
    rename_at(vars(names(.)[11:15]), ~ new_names) %>%
    mutate(
        water_avail = case_when(
            water_avail == "T" ~ "Total",
            water_avail == "W" ~ "Within premises",
            water_avail == "O" ~ "Outside premises"
        ),
        geo_section = ifelse(state == "INDIA", "country", geo_section)
    ) %>%
    mutate_at(11:15, as.integer)  # coerce UP missing values to NA

### Step 2: repeat process for SC

# no missing rows; ends with completed mizoram
tot91sc_a <- read_csv(tot91_files[3], skip = 8) %>% 
    rename_at(vars(names(.)), ~ 1:7) %>% 
    mutate_at(1:7, as.character)

tot91sc_b <- read_csv(tot91_files[4], skip = 8) %>% 
    rename_at(vars(names(.)), ~ 1:7) %>% 
    mutate_at(1:7, as.character)

# correct INDIA* to INDIA
tot91sc_a <- tot91sc_a %>% 
    mutate(`1` = ifelse(`1` == "INDIA*", "INDIA", `1`))

# PONDICHERRY aligned next to LAKSHADWEEP; 
# should be PONDY as LAKSHADWEEP has no SC, only ST
tot91sc_b <- tot91sc_b %>% 
    #filter(`2` == "PONDICHERRY") %>% 
    mutate(
        `2` = ifelse(`2` == "PONDICHERRY", NA, `2`),
        `1` = ifelse(`1` == "LAKSHADWEEP", "PONDICHERRY", `1`)
           )

# bind both parts of divided file
tot91sc <- tot91sc_a %>% 
    bind_rows(tot91sc_b)

# extract only numeric data into a df
numbers91sc <- tot91sc %>%
    select(`3`:`7`) %>% 
    na.omit()

# from first column create vectors for identifying columns
column1sc <- tot91sc %>% 
    select(1) %>% 
    na.omit() %>% 
    pull

state_indices_sc <- seq(from = 1, to = length(column1sc), by = 25)
states91sc <- column1sc[state_indices_sc]
state_col_sc <- rep(states91sc, each = 63)

# change to 29 because of 3 states/uts that don't have SC
demo_col_sc <- rep(demo_options, each = 3 * 7, times = 29)
water_avail_col_sc <- rep(water_avail, each = 1, times = 7 * 3 * 29)
water_source_col_sc <- rep(water_sources, each = 3, times = 3 * 29)
water_avail_col_sc <- rep(water_avail, each = 1, times = 7 * 3 * 29)

# create the tidy df structure
cols_sc <- tibble(
    year = 1991,
    geo_section = "state",
    country = "INDIA",
    state = state_col_sc,
    district = NA,
    subdistrict = NA,
    societal_section = "SC",
    demo_section = demo_col_sc,
    water_source = water_source_col_sc,
    water_avail = water_avail_col_sc
) 

# bind numeric data back into tidy df structure
tidy_totals_91_sc <- cols_sc %>% 
    bind_cols(numbers91sc) %>%
    rename_at(vars(names(.)[11:15]), ~ new_names) %>%
    mutate(
        water_avail = case_when(
            water_avail == "T" ~ "Total",
            water_avail == "W" ~ "Within premises",
            water_avail == "O" ~ "Outside premises"
        ),
        geo_section = ifelse(state == "INDIA", "country", geo_section)
    ) %>%
    mutate_at(11:15, as.integer) 


### Step 3: repeat process for ST

# no missing rows; ends with completed nagaland
tot91st_a <- read_csv(tot91_files[5], skip = 8) %>% 
    rename_at(vars(names(.)), ~ 1:7) %>% 
    mutate_at(1:7, as.character)

tot91st_b <- read_csv(tot91_files[6], skip = 8) %>% 
    rename_at(vars(names(.)), ~ 1:7) %>% 
    mutate_at(1:7, as.character)

# correct *INDIA to INDIA
tot91st_a <- tot91st_a %>% 
    mutate(`1` = ifelse(`1` == "*INDIA", "INDIA", `1`))

# error: LAKSHADWEEP aligned next to LAKSHADWEEP; remove
tot91st_b <- tot91st_b %>% 
    #filter(`2` == "LAKSHADWEEP") %>% 
    mutate(`2` = ifelse(`2` == "LAKSHADWEEP", NA, `2`))

# bind both parts of divided file
tot91st <- tot91st_a %>% 
    bind_rows(tot91st_b)

# extract only numeric data into a df
numbers91st <- tot91st %>%
    select(`3`:`7`) %>% 
    na.omit()

# from first column create vectors for identifying columns
column1st <- tot91st %>% 
    select(1) %>% 
    na.omit() %>% 
    pull

state_indices_st <- seq(from = 1, to = length(column1st), by = 25)
states91st <- column1st[state_indices_st]
state_col_st <- rep(states91st, each = 63)

# change to 27 because 5 states/ut have no ST population
demo_col_st <- rep(demo_options, each = 3 * 7, times = 27)
water_avail_col_st <- rep(water_avail, each = 1, times = 7 * 3 * 27)
water_source_col_st <- rep(water_sources, each = 3, times = 3 * 27)
water_avail_col_st <- rep(water_avail, each = 1, times = 7 * 3 * 27)

# create the tidy df structure
cols_st <- tibble(
    year = 1991,
    geo_section = "state",
    country = "INDIA",
    state = state_col_st,
    district = NA,
    subdistrict = NA,
    societal_section = "ST",
    demo_section = demo_col_st,
    water_source = water_source_col_st,
    water_avail = water_avail_col_st
) 

# bind numeric data back into tidy df structure
tidy_totals_91_st <- cols_st %>% 
    bind_cols(numbers91st) %>%
    rename_at(vars(names(.)[11:15]), ~ new_names) %>%
    mutate(
        water_avail = case_when(
            water_avail == "T" ~ "Total",
            water_avail == "W" ~ "Within premises",
            water_avail == "O" ~ "Outside premises"
        ),
        geo_section = ifelse(state == "INDIA", "country", geo_section)
    ) %>%
    mutate_at(11:15, as.integer) 


# bind all 3 tidy dfs together
tidy_totals_91 <- tidy_totals_91 %>% 
    bind_rows(tidy_totals_91_sc) %>% 
    bind_rows(tidy_totals_91_st) %>% 
    mutate(
        state = ifelse(geo_section == "country", NA, state)
    )

saveRDS(tidy_totals_91, file = "5-joined-data/1991totals.rds")

rm(list = ls())
