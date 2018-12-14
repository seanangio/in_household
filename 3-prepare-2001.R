##### Script returns tidy dataset for given year

library(tidyverse)

# create directory for tidy data output
if (!dir.exists("6-tidy-data")) {
    dir.create("6-tidy-data")
}

joined01 <- readRDS("5-joined-data/2001.rds")

pre_tidy_01 <- joined01 %>% 
    
    # change raw numbers to integer type
    mutate_at(10:14, as.integer) %>% 
    
    mutate(
        # calculate percentages
        ea = `11` / `10`, # % having electricity (regardless of latrine)
        la = `13` / `10`, # % having latrine (regardless of electricity)
        ea_la = NA, # % having electricity and latrine
        ea_ln = NA, # % having electricity, no latrine
        en_la = NA, # % having no electricity, yes latrine
        en_ln = NA, # % having no electricity, no latrine
        
        # add societal section variable
        societal_section = case_when(
            str_detect(`1`, "C$") ~ "SC",
            str_detect(`1`, "T$") ~ "ST",
            TRUE ~ "ALL"
        ),
        # add country variable
        country = "INDIA",
        
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
    
    select(`15`, geo_section, country, state, district, subdistrict, societal_section, everything()) %>%
    rename(demo_section = `7`,
           water_source = `8`,
           water_avail = `9`,
           total_hh = `10`,
           num_ea = `11`,
           num_en = `12`,
           num_la = `13`,
           num_ln = `14`,
           year = `15`)

# calculate all india totals
all_india_01 <- pre_tidy_01 %>%
    filter(geo_section == "state") %>% 
    group_by(year, geo_section, country, district, subdistrict, 
             societal_section, demo_section, water_source, water_avail) %>% 
    summarise(total_hh = sum(total_hh),
              num_ea = sum(num_ea),
              num_en = sum(num_en),
              num_la = sum(num_la),
              num_ln = sum(num_ln)
              ) %>% 
    mutate(
        # calculate percentages
        ea = num_ea / total_hh, # % having electricity (regardless of latrine)
        la = num_la / total_hh, # % having latrine (regardless of electricity)
        ea_la = NA, # % having electricity and latrine
        ea_ln = NA, # % having electricity, no latrine
        en_la = NA, # % having no electricity, yes latrine
        en_ln = NA # % having no electricity, no latrine
    ) %>% 
    add_column(state = NA, .after = "country") %>% 
    ungroup()

# bind together
tidy_01 <- bind_rows(pre_tidy_01, all_india_01) %>% 
    mutate(
        # mutate geo_section
        geo_section = case_when(
            is.na(state) & is.na(district) & is.na(subdistrict) ~ "country",
            !is.na(state) & is.na(district) & is.na(subdistrict) ~ "state",
            !is.na(state) & !is.na(district) & is.na(subdistrict) ~ "district",
            !is.na(state) & !is.na(district) & !is.na(subdistrict) ~ "subdistrict",
            TRUE ~ "other"
        )
    )

saveRDS(tidy_01, file = "6-tidy-data/2001.rds")

rm(list = ls())