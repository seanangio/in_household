##### Script to create one tidy file of district and subdistrict 1991 data

library(tidyverse)

# this file seems good
agg91 <- readRDS("3-agg-data/1991.rds")

# create vector of loc_codes that have the complete 21 observations
# about 30 districts only have 20; one row of data is missing for them
loc_20_21 <- agg91 %>%
    
    # extract stratum code from LOC_CODE instead of `Stratum Code`
    mutate(stratum_code = substring(LOC_CODE, 15, 15)) %>%
    
    # remove cities and town to only work with district and subdistrict levels
    filter(!stratum_code %in% c(5:6)) %>%
    
    # extract a vector of only remaining LOC_CODES that have 21 or 20 observations
    count(LOC_CODE) %>%
    filter(n == 21 | n == 20) %>%
    pull(LOC_CODE)

agg91_distinct <- agg91 %>% 
    
    # separate out LOC_CODE to avoid errors present in other columns
    mutate(
        project_code = substring(LOC_CODE, 1, 2),
        file_code = substring(LOC_CODE, 3, 4),
        record_length = substring(LOC_CODE, 5, 7),
        report_code = substring(LOC_CODE, 8, 10),
        state_code = substring(LOC_CODE, 11, 12),
        district_code = substring(LOC_CODE, 13, 14),
        stratum_code = substring(LOC_CODE, 15, 15),
        tehsil_town_city = substring(LOC_CODE, 16, 19)
    ) %>%
    
    # remove cities and town to only work with district and subdistrict levels
    filter(!stratum_code %in% c(5:6)) %>% 
    
    # select wanted variables
    select(-c(2:3, 5, 7:10)) %>%
    select(LOC_CODE, project_code, file_code, record_length, report_code,
           state_code, district_code, stratum_code, tehsil_town_city, everything()) %>%
    
    # remove non-complete observational units
    filter(LOC_CODE %in% loc_20_21) %>% 
    
    # note that in 91, subdistricts all named Tehsil; tehsil level has no Total/Urban/Rural level
    mutate(
        country = "INDIA",
        geo_section = ifelse(stratum_code %in% c("1", "2", "3"), 
                             "district", "subdistrict"),
        subdistrict = ifelse(tehsil_town_city == "XXXX", NA, 
                             tehsil_town_city),
        demo_section = case_when(
            stratum_code == "1" ~ "Total",
            stratum_code == "2" ~ "Rural",
            stratum_code == "3" ~ "Urban",
            stratum_code == "4" ~ "NA"
        )
    ) %>%
    
    rename(
        total_hh = TOTAL_HH,
        num_ea_la = EA_TA,
        num_ea_ln = EA_TNA,
        num_en_la = ENA_TA,
        num_en_ln = ENA_TNA,
        societal_section = section
    ) %>%
    
    # convert to integer to reduce all observations to 2 digits before returning to character
    mutate(LINE_PAR = as.character(as.integer(LINE_PAR))) %>% 
    
    # divide LINE_PAR into water_source and water_avail components
    extract(LINE_PAR, into = c("water_source_code", "water_avail_code"), 
            "(.{1})(.{1})", remove = FALSE) %>%
    mutate_at(c("LINE_PAR", "water_source_code", "water_avail_code"), as.integer) %>%
    mutate(
        water_source_code = ifelse(LINE_PAR <= 2, 0, water_source_code),
        water_avail_code = ifelse(LINE_PAR <= 2, LINE_PAR, water_avail_code),
        water_source = case_when(
            water_source_code == 0 ~ "All Sources",
            water_source_code == 1 ~ "Well",
            water_source_code == 2 ~ "Tap",
            water_source_code == 3 ~ "Handpump/Tubewell",
            water_source_code == 4 ~ "River/Canal",
            water_source_code == 5 ~ "Tank",
            water_source_code == 6 ~ "Others"
        ),
        water_avail = case_when(
            water_avail_code == 0 ~ "Total",
            water_avail_code == 1 ~ "Within Premises",
            water_avail_code == 2 ~ "Outside Premises"
        )
    ) %>%
    
    # reorder variables
    select(year, geo_section, country, state_code, district_code, subdistrict, 
           societal_section, demo_section, water_source, water_avail, everything()) %>% 
    select(-one_of(c("LOC_CODE", "project_code", "file_code", "record_length",
                     "report_code", "stratum_code", "tehsil_town_city", "LINE_PAR", 
                     "water_source_code", "water_avail_code")))

# create table of state and district names and codes
states_districts_91 <- agg91 %>% 
    select(`State_code`, `State_Name`, `Dist_Code`, `Dist_Name`) %>%
    
    # district codes of 0 suggest a state total (which can't be...so remove)
    filter(!`Dist_Code` == "0") %>%
    unique %>%
    rename(
        state_code = `State_code`,
        state = `State_Name`,
        district_code = `Dist_Code`,
        district = `Dist_Name`
    )

# join in names of states and districts from their codes
joined91 <- agg91_distinct %>% 
    inner_join(states_districts_91) %>% # don't want any rows with dist_code of 0
    select(-c(state_code, district_code, State_code, Dist_Code)) %>% 
    select(year, geo_section, country, state, district, everything())

saveRDS(joined91, file = "5-joined-data/1991non_totals.rds")

rm(list = ls())