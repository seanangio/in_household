##### organize state geometries per year into one sf object
##### will be used to draw state lines

library(tidyverse)
library(sf)

simp11 <- readRDS("10-simple-shapes/simp11.rds")
simp01 <- readRDS("10-simple-shapes/simp01.rds")
simp91 <- readRDS("10-simple-shapes/simp91.rds")

state_shapes11 <- simp11 %>% 
    group_by(state) %>% 
    summarise(n = n()) %>% 
    select(-n) %>% 
    mutate(year = 2011)

state_shapes01 <- simp01 %>% 
    group_by(state) %>% 
    summarise(n = n()) %>% 
    select(-n) %>% 
    mutate(year = 2001)

state_shapes91 <- simp91 %>% 
    group_by(state) %>% 
    summarise(n = n()) %>% 
    select(-n) %>% 
    mutate(year = 1991) %>% 
    st_set_crs(NA) # might need to do this earlier; simp91 has a CRS; simp11-01 don't

state_lines <- rbind(state_shapes11, state_shapes01) %>%
    rbind(state_shapes91) %>% 
    mutate(year = as.character(year))

if (!dir.exists("shiny/data")) {
    dir.create("shiny/data")
}

saveRDS(state_lines, "shiny/data/state_lines.rds")

rm(list = ls())
