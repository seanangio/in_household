### reconcile district names that differ in census and shape data 
### join spatial data to district df

library(tidyverse)
library(sf)
library(vwr) # for levenshtein distance to reconcile mismatched district names

d11 <- readRDS("11-completed-data/2011.rds")
d01 <- readRDS("11-completed-data/2001.rds")
d91 <- readRDS("11-completed-data/1991.rds")

abb_region_df <- readRDS("8-abb-region/abb_region.rds") %>% 
    select(-region)

simp11 <- readRDS("10-simple-shapes/simp11.rds")
simp01 <- readRDS("10-simple-shapes/simp01.rds")
simp91 <- readRDS("10-simple-shapes/simp91.rds")

# join abb data to sf object to give same unique key value
simp11 <- simp11 %>% 
    left_join(abb_region_df, by = c("state" = "state11")) %>%
    select(-c(state01, state91)) %>% 
    mutate(district_abb = str_c(district, " (", abb, ")"))

simp01 <- simp01 %>% 
    left_join(abb_region_df, by = c("state" = "state01")) %>% 
    select(-c(state11, state91)) %>% 
    mutate(district_abb = str_c(district, " (", abb, ")"))

simp91 <- simp91 %>% 
    mutate(state = ifelse(state == "Pondicherry", "Puducherry", state)) %>% 
    left_join(abb_region_df, by = c("state" = "state91")) %>% 
    select(-c(state11, state01)) %>% 
    mutate(district_abb = str_c(district, " (", abb, ")"))

# address problem of mismatched names in df and sf
# first inner join where district names match
# put sf object first to retain sf class
matches11 <- simp11 %>% 
    inner_join(d11, by = "district_abb") %>%
    rename(district = district.x,
           state = state.x,
           abb = abb.x) %>%
    select(-c(state.y, district.y, abb.y))
    
matches01 <- simp01 %>% 
    inner_join(d01, by = "district_abb") %>% 
    rename(district = district.x,
           state = state.x,
           abb = abb.x) %>%
    select(-c(state.y, district.y, abb.y)) %>% 
    select(district, state, everything())

matches91 <- simp91 %>%
    inner_join(d91, by = "district_abb") %>% 
    rename(district = district.x,
           state = state.x,
           abb = abb.x) %>%
    select(-c(state.y, district.y, abb.y)) %>% 
    select(district, state, everything())

# confirm 614 matches in 11; 584 in 01; 429 in 91
#matches11 %>% st_set_geometry(NULL) %>% distinct(district_abb) %>% nrow()
#matches01 %>% st_set_geometry(NULL) %>% distinct(district_abb) %>% nrow()
#matches91 %>% st_set_geometry(NULL) %>% distinct(district_abb) %>% nrow()

# then anti-join to get misses; df first because geometry is what needs to be added
misses11 <- d11 %>% anti_join(simp11, by = "district_abb")
misses01 <- d01 %>% anti_join(simp01, by = "district_abb")
misses91 <- d91 %>% anti_join(simp91, by = "district_abb")

# 26 names in d11 that have a different name in simp11; 9 in 01; 37 in 91
missed_districts11 <- misses11 %>% pull(district_abb) %>% unique
missed_districts01 <- misses01 %>% pull(district_abb) %>% unique
missed_districts91 <- misses91 %>% pull(district_abb) %>% unique

# all unique district names in simp11
sdistricts11 <- simp11 %>% pull(district_abb) %>% unique
sdistricts01 <- simp01 %>% pull(district_abb) %>% unique
sdistricts91 <- simp91 %>% pull(district_abb) %>% unique

# next 3 functions should definitely be 1 but problem of second input being a different length
# input: district names from f-- and district names from s--
# output: suggested closest names from shapefile district names
get_close_name11 <- function(fdist) {
    
    vwr::levenshtein.distance(fdist, sdistricts11) %>% 
        as_tibble() %>% 
        rownames_to_column() %>% 
        arrange(value) %>% 
        head(1) %>% 
        pull(rowname)
} 

get_close_name01 <- function(fdist) {
    
    vwr::levenshtein.distance(fdist, sdistricts01) %>% 
        as_tibble() %>% 
        rownames_to_column() %>% 
        arrange(value) %>% 
        head(1) %>% 
        pull(rowname)
} 

get_close_name91 <- function(fdist) {
    
    vwr::levenshtein.distance(fdist, sdistricts91) %>% 
        as_tibble() %>% 
        rownames_to_column() %>% 
        arrange(value) %>% 
        head(1) %>% 
        pull(rowname)
} 

# close_dists are names in sdistricts that likely match with names in missed_districts
close_dists11 <- purrr::map_chr(missed_districts11, get_close_name11)
close_dists01 <- purrr::map_chr(missed_districts01, get_close_name01)
close_dists91 <- purrr::map_chr(missed_districts91, get_close_name91)

# compare suggested names and make manual corrections where needed
reconciled_table11 <- misses11 %>%
    select(district_abb) %>% 
    distinct() %>% 
    rename(fdist = district_abb) %>% 
    mutate(sdist = purrr::map_chr(missed_districts11, get_close_name11)) %>%
    # manual corrections where suggested name is wrong
    mutate(
        sdist = replace(sdist, sdist == "Siwan (BR)", "Saran (chhapra) (BR)"),
        sdist = replace(sdist, sdist == "Chamba (HP)", "Janjgir-champa (CT)"),
        sdist = replace(sdist, sdist == "East Nimar (MP)", "East (SK)"),
        sdist = replace(sdist, sdist == "North East (DL)", "North (SK)"),
        sdist = replace(sdist, sdist == "South West (DL)", "South (SK)"),
        sdist = replace(sdist, sdist == "West Nimar (MP)", "West (SK)")
    )

reconciled_table01 <- misses01 %>%
    select(district_abb) %>% 
    distinct() %>% 
    rename(fdist = district_abb) %>% 
    mutate(sdist = purrr::map_chr(missed_districts01, get_close_name01)) %>%
    # manual corrections where suggested name is wrong
    mutate(
        sdist = replace(sdist, sdist == "Dhamtari (CT)", "Janjgir - Champa (CT)"),
        sdist = replace(sdist, sdist == "Sonipat (HR)", "Senapati (Excl. 3 sub-divisions) (MN)")
    )

reconciled_table91 <- misses91 %>%
    select(district_abb) %>% 
    distinct() %>% 
    rename(fdist = district_abb) %>% 
    mutate(sdist = purrr::map_chr(missed_districts91, get_close_name91)) %>%
    # manual corrections where suggested name is wrong
    mutate(
        sdist = replace(sdist, sdist == "East Nimar (MP)", "East (SK)"),
        sdist = replace(sdist, sdist == "South Arcot (TN)", "South (SK)"),
        sdist = replace(sdist, sdist == "West Nimar (MP)", "West (SK)")
    )
# template for finding manual corrections; search for districts in fdist state    
#simp11 %>% filter(abb == "BR") %>% count(district_abb) %>% View

# sf object of missing entries with district name matching f--
corrected_sf11 <- simp11 %>% 
    left_join(reconciled_table11, by = c("district_abb" = "sdist")) %>%
    select(fdist) %>% 
    filter(!is.na(fdist))

corrected_sf01 <- simp01 %>% 
    left_join(reconciled_table01, by = c("district_abb" = "sdist")) %>%
    select(fdist) %>% 
    filter(!is.na(fdist))

corrected_sf91 <- simp91 %>% 
    left_join(reconciled_table91, by = c("district_abb" = "sdist")) %>%
    select(fdist) %>% 
    filter(!is.na(fdist))

# full data from missing districts with correct names and geometry
misses_filled11 <- corrected_sf11 %>% 
    left_join(misses11, by = c("fdist" = "district_abb")) %>% 
    rename(district_abb = fdist) %>% 
    select(district, state, abb, district_abb, everything())

misses_filled01 <- corrected_sf01 %>% 
    left_join(misses01, by = c("fdist" = "district_abb")) %>% 
    rename(district_abb = fdist) %>% 
    select(district, state, abb, district_abb, everything())

misses_filled91 <- corrected_sf91 %>% 
    left_join(misses91, by = c("fdist" = "district_abb")) %>% 
    rename(district_abb = fdist) %>% 
    select(district, state, abb, district_abb, everything())

# confirm binding of matches and misses possible
#identical(names(matches11), names(misses_filled11))

# use rbind instead of bind_rows because they are sf objects
completed11 <- rbind(matches11, misses_filled11) 
completed01 <- rbind(matches01, misses_filled01) 
completed91 <- rbind(matches91, misses_filled91) 

# confirm all districts present
# completed91 %>%
#     st_set_geometry(NULL) %>%
#     count(district_abb) %>%
#     nrow()
#plot(st_geometry(completed91))


if (!dir.exists("shiny/data/district")) {
    dir.create("shiny/data/district")
}

saveRDS(completed11, "shiny/data/district/district11.rds")
saveRDS(completed01, "shiny/data/district/district01.rds")
saveRDS(completed91, "shiny/data/district/district91.rds")

rm(list = ls())
