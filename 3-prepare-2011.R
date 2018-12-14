##### Script returns tidy dataset for given year

library(tidyverse)

# create directory for tidy data output
if (!dir.exists("6-tidy-data")) {
    dir.create("6-tidy-data")
}

joined11 <- readRDS("5-joined-data/2011.rds")

tidy11 <- joined11 %>% 

    # change raw numbers to integer type
    mutate_at(10:14, as.integer) %>% 
    
    mutate(
        # correct rows listing INDIA as a state to a country level variable
        country = "INDIA",
        state = ifelse(state == "INDIA", NA, state),
        
        num_ea = `11` + `12`,
        num_la = `11` + `13`,
        
        # calculate percentages
        ea = (`11` + `12`) / `10`, # % having electricity (regardless of latrine)
        la = (`11` + `13`) / `10`, # % having latrine (regardless of electricity)
        ea_la = `11` / `10`, # % having electricity and latrine
        ea_ln = `12` / `10`, # % having electricity, no latrine
        en_la = `13` / `10`, # % having no electricity, yes latrine
        en_ln = `14` / `10`, # % having no electricity, no latrine
        
        # add societal section variable
        societal_section = case_when(
            str_detect(`1`, "C$") ~ "SC",
            str_detect(`1`, "T$") ~ "ST",
            TRUE ~ "ALL"
        ),
        
        # mutate geo_section
        geo_section = case_when(
            is.na(state) & is.na(district) & is.na(subdistrict) ~ "country",
            !is.na(state) & is.na(district) & is.na(subdistrict) ~ "state",
            !is.na(state) & !is.na(district) & is.na(subdistrict) ~ "district",
            !is.na(state) & !is.na(district) & !is.na(subdistrict) ~ "subdistrict",
            TRUE ~ "other"
        )
    ) %>%
    
    # reorganize columns; check on year
    select(-c(1:6)) %>%
    select(`15`, geo_section, country, state, district, subdistrict, 
           societal_section, everything()) %>%
    rename(demo_section = `7`,
           water_source = `8`,
           water_avail = `9`,
           total_hh = `10`,
           num_ea_la = `11`, 
           num_ea_ln = `12`,
           num_en_la = `13`,
           num_en_ln = `14`,
           year = `15`)

saveRDS(tidy11, file = "6-tidy-data/2011.rds")

rm(list = ls())
