##### 2001/11 require creating separate tables to match names with states, districts, etc
##### Script creates necessary code tables and joins them into the main dfs for each year

library(tidyverse)

# create directories for code table output
if (!dir.exists("4-code-tables")) {
    dir.create("4-code-tables")
}

# get vector of all aggregated files
agg_files <- list.files("3-agg-data", recursive = TRUE, full.names = TRUE)

create_code_tables <- function(file_name) {
    
    a <- readRDS(file_name)
    fname_only <- word(file_name, -1, sep = "/")
    
    state <- a %>%
        filter(str_detect(`6`, "^STATE|State|INDIA")) %>% # INDIA treated as a state code of 00
        group_by(`2`) %>% 
        distinct(`6`) %>%
        mutate(state = word(`6`, start = -1, sep = "- ")) %>%
        select(`2`, "state") %>% 
        ungroup() %>% 
        distinct()
    saveRDS(state, file = str_c("4-code-tables/state", fname_only))
    
    district <- a %>% 
        filter(str_detect(`6`, "^District")) %>% 
        group_by(`2`,`3`) %>% 
        distinct(`6`) %>%
        mutate(district = word(`6`, start = -1, sep = "- ")) %>%
        select(`2`, `3`, "district") %>% 
        ungroup() %>% 
        distinct()
    saveRDS(district, file = str_c("4-code-tables/district", fname_only))
    
    subdistrict <- a %>% 
        filter(str_detect(`6`, "^Sub-District|Mandal|Tehsil|C.D. Block|
                          Circle|Taluk|Tehsil/Sub-tehsil|Sub-Division|
                          R.D. Block|Police Station|Communue Panchayat|
                          Development Block")) %>% 
        group_by(`2`,`3`,`4`) %>% 
        distinct(`6`) %>% 
        mutate(subdistrict = str_split(`6`, " - ", n = 2)[[1]][2]) %>%
        select(`2`, `3`, `4`, "subdistrict") %>% 
        ungroup() %>% 
        distinct()
    saveRDS(subdistrict, file = str_c("4-code-tables/subdistrict", fname_only))
    
}

# create code tables for 2001 and 2011
agg_files %>% 
    str_subset("2001|2011") %>% 
    map(create_code_tables)

# read in files and code tables
agg01 <- readRDS("3-agg-data/2001.rds")
agg11 <- readRDS("3-agg-data/2011.rds")

states01 <- readRDS("4-code-tables/state2001.rds")
districts01 <- readRDS("4-code-tables/district2001.rds")
subdistricts01 <- readRDS("4-code-tables/subdistrict2001.rds")

states11 <- readRDS("4-code-tables/state2011.rds")
districts11 <- readRDS("4-code-tables/district2011.rds")
subdistricts11 <- readRDS("4-code-tables/subdistrict2011.rds")

# create directories for code table output
if (!dir.exists("5-joined-data")) {
    dir.create("5-joined-data")
}

agg01 %>% 
    # remove empty and other non-meaningful rows
    filter(!is.na(`1`),
           !str_detect(`1`, "^Note"),
           !str_detect(`1`, "^2. ")) %>% 
    filter(!str_detect(`6`, "^Town")) %>% # remove Towns
    left_join(states01) %>% 
    left_join(districts01) %>% 
    left_join(subdistricts01) %>%
    saveRDS("5-joined-data/2001.rds")

agg11 %>% 
    filter(!is.na(`1`), # remove NA observations (notes at bottom of xls sheets)
           !str_detect(`1`, "^Note"),
           !str_detect(`1`, "^2. ")) %>%
    filter(`5` == "000000" | is.na(`5`)) %>% # remove observations with town codes (CTs etc); might not be sufficient; Indian Telephone Industry; but retain non subdistricts
    left_join(states11) %>%
    left_join(districts11) %>%
    left_join(subdistricts11) %>% 
    saveRDS("5-joined-data/2011.rds")

rm(list = ls())
