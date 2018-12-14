##### scrape a df of state abbreviations and region; join that data into main df

library(rvest)
abb_url <- "https://kb.bullseyelocations.com/support/solutions/articles/5000695302-india-state-codes"
abb_scrape <- read_html(abb_url) %>% 
    html_nodes("td") %>% 
    html_text(trim = TRUE)

abb_df <- matrix(abb_scrape, ncol = 3, byrow = TRUE) %>% 
    data.frame() %>% 
    slice(-1) %>% 
    select(1:2) %>% 
    rename("state" = "X1", "abb" = "X2") %>%
    mutate_if(is.factor, as.character) %>% 
    mutate(
        state = str_replace_all(state, " and", " &"),
        state = case_when(
            state == "Uttarakhand,Uttaranchal" ~ "Uttarakhand",
            state == "Odisha, Orissa" ~ "Odisha",
            TRUE ~ state)
    )

# wrangle region data
region_url <- "https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_area"
region_scrape <- read_html(region_url) %>% 
    html_nodes("td") %>% 
    html_text(trim = TRUE)

regions_mat <- matrix(region_scrape[4:length(region_scrape)], 
                      ncol = 7, byrow = TRUE)

region_df <- data.frame(regions_mat) %>% 
    select(2, 4) %>%
    slice(1:36) %>% 
    rename_at(vars(names(.)), ~ c("state", "region")) %>% 
    mutate_if(is.factor, as.character) %>% 
    arrange(state) %>% 
    mutate(state = str_replace_all(state, " and", " &"))

# need column of 1991, 2001 state names
f01 <- readRDS("7-final-data/2001.rds")
states01 <- unique(f01$state)[-1]

#states91 <- unique(f91$state)[-1]
# match 91 from 01
states91 <- tibble(states01) %>% 
    mutate(
        states91 = case_when(
            states01 == "Chhattisgarh" ~ NA_character_,
            states01 == "Jharkhand" ~ NA_character_,
            states01 == "Pondicherry" ~ "Puducherry",
            states01 == "Uttaranchal" ~ NA_character_,
            TRUE ~ states01
            )
        ) %>% 
    select(states91) %>%
    pull(states91)

# Telangana is post-2011; keep state name changes over time
abb_region_df <- abb_df %>% 
    left_join(region_df) %>% 
    rename(state11 = state) %>%
    filter(!state11 == "Telangana") %>% 
    mutate(state01 = states01,
           state91 = states91)

# create directories for abb region df
if (!dir.exists("8-abb-region")) {
    dir.create("8-abb-region")
}

saveRDS(abb_region_df, "8-abb-region/abb_region.rds")

# Join abb data to main df ------------------------------------------------

abb_region_df <- readRDS("8-abb-region/abb_region.rds")

# add region and abb to final dataframes; mutate unique dist abb name
f11 <- readRDS("7-final-data/2011.rds")
f01 <- readRDS("7-final-data/2001.rds")
f91 <- readRDS("7-final-data/1991.rds")
#f91_new <- readRDS("7-final-data/1991_new.rds")

f11 <- f11 %>% 
    left_join(abb_region_df, by = c("state" = "state11")) %>% 
    mutate(district_abb = str_c(district, " (", abb, ")"))

f01 <- f01 %>% 
    left_join(abb_region_df, by = c("state" = "state01")) %>% 
    mutate(district_abb = str_c(district, " (", abb, ")"))

f91 <- f91 %>%
    left_join(abb_region_df, by = c("state" = "state91")) %>%
    mutate(district_abb = str_c(district, " (", abb, ")"))

f91 <- f91 %>%
    left_join(
        abb_region_df %>% filter(!is.na(state91)), 
        by = c("state" = "state91")) %>%
    mutate(district_abb = str_c(district, " (", abb, ")"))

# create directories for final data with abb joined
if (!dir.exists("9-final-abb-data")) {
    dir.create("9-final-abb-data")
}

saveRDS(f11, file = "9-final-abb-data/2011.rds")
saveRDS(f01, file = "9-final-abb-data/2001.rds")
saveRDS(f91, file = "9-final-abb-data/1991.rds")

rm(list = ls())
