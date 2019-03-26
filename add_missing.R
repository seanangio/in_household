library(tidyverse)
library(sf)
my_sf <- readRDS("shiny_all/final_sf.rds")

# add missing -------------------------------------------------------------

# without equivalent of leaflet::colorBin()
# each possible combination needs all levels (0-10% - 90-100% NA)

my_sf <- my_sf %>% 
    mutate_at(
        vars(societal_section, demo_section),
        as.character
    )

all_levels <- c("0-10%","10-20%","20-30%","30-40%","40-50%",
  "50-60%","60-70%","70-80%","80-90%","90-100%", NA_character_)

categories <- names(my_sf)[12:17]

# generic grid of combinations
my_grid <- expand.grid(
    societal_section = c("ALL", "SC", "ST"),
    demo_section = c("Total", "Urban", "Rural"),
    year = c(1991, 2001, 2011),
    var = all_levels,
    stringsAsFactors = FALSE
)

# for each category, create a grid with that category
rename_grid <- function(var) {
    names(my_grid)[4] <- var
    return(my_grid)
}

# get a list of dataframes for each category
my_dfs <- list()
for (i in seq_along(categories)) {
    my_dfs[[i]] <- rename_grid(categories[i])
}

# create list of rows needed for each category
get_new_rows <- function(df) {
    df %>% 
        left_join(my_sf) %>% 
        filter(is.na(district_abb))
}

new_rows <- list()
for (i in seq_along(my_dfs)) {
    new_rows[[i]] <- get_new_rows(my_dfs[[i]])
}

# bind together with original
missing_rows <- do.call(rbind, new_rows)
new_sf <- missing_rows %>% 
    select(district_abb, everything()) %>% 
    select(1:4, 6:12, Electricity, everything()) %>% 
    st_as_sf()

my_list <- list(my_sf, new_sf)
final_sf <- do.call(rbind, my_list)

saveRDS(final_sf, "shiny_all/final_sf2.rds")
