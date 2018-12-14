##### Script to bind together, clean state total and district/subdistrict data

library(tidyverse)

# create directory for tidy data output
if (!dir.exists("6-tidy-data")) {
    dir.create("6-tidy-data")
}

tot91 <- readRDS("5-joined-data/1991totals.rds")
non_tot91 <- readRDS("5-joined-data/1991non_totals.rds")

tidy91 <- tot91 %>% 
    bind_rows(non_tot91) %>% 
    mutate(
        # reconcile slight differences in spellings between total and non-total files
        state = str_to_title(state),
        state = case_when(
            state == "Maharastra" ~ "Maharashtra",
            state == "Pondicherry" ~ "Puducherry",
            state == "Daman And Diu" ~ "Daman & Diu",
            state == "Dadra And Nagar Haveli" ~ "Dadra & Nagar Haveli",
            state == "Andaman Nicobar Islands" ~ "Andaman & Nicobar Islands",
            state == "Andaman And Nicobar Islands" ~ "Andaman & Nicobar Islands",
            TRUE ~ state
        ),
        societal_section = ifelse(societal_section == "All", 
                                  "ALL", societal_section),
        water_source = case_when(
            water_source == "Handpump/ Tubewell" ~ "Handpump/Tubewell",
            water_source == "River/ Canal" ~ "River/Canal",
            TRUE ~ water_source
        ),
        water_avail = case_when(
            water_avail == "Outside premises" ~ "Outside Premises",
            water_avail == "Within premises" ~ "Within Premises",
            TRUE ~ water_avail
        ),
        num_ea = num_ea_la + num_ea_ln,
        num_la = num_ea_la + num_en_la,
        
        # calculate percentages
        ea = (num_ea_la + num_ea_ln) / total_hh, # % having electricity (regardless of latrine)
        la = (num_ea_la + num_en_la) / total_hh, # % having latrine (regardless of electricity)
        ea_la = num_ea_la / total_hh, # % having electricity and latrine
        ea_ln = num_ea_ln / total_hh, # % having electricity, no latrine
        en_la = num_en_la / total_hh, # % having no electricity, yes latrine
        en_ln = num_en_ln / total_hh # % having no electricity, no latrine
        
    )

saveRDS(tidy91, file = "6-tidy-data/1991.rds")

rm(list = ls())