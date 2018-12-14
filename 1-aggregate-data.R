##### Script reads in raw data and outputs 
##### one minimally cleaned aggregated dataset for each year

library(tidyverse)
library(tools)
library(readxl)

# Step 1 Basic conversion of raw xls and csv files to rds files -----------


# create directories for raw data organized by year
years <- c(1991, 2001, 2011)

if (!dir.exists(file.path("1-raw-data"))) {
    for (i in seq_along(years)) {
                dir.create(file.path("1-raw-data", years[[i]]), 
                           recursive = TRUE)
    }
}

# manually place downloaded files into directory

# create empty directory for rds data output
if (!dir.exists(file.path("2-read-data"))) {
    for (i in seq_along(years)) {
        dir.create(file.path("2-read-data", years[[i]]), 
                   recursive = TRUE)
    }
}

# get vector of raw data file paths
raw_files <- list.files("1-raw-data", recursive = TRUE, full.names = TRUE)

# function to read in raw xls files and output minimally formatted rds files in read_data directory
read_raw <- function(file_name) {
    
    all_india <- str_split(word(file_name, -1), "\\.")[[1]][1] == "India"
    
    year <- str_split(file_name, "/")[[1]][[2]]
    
    if (year == "1991") {
        a <- read_csv(file_name)
        
    } else if (year == "2001") {
        a <- read_excel(file_name, skip = 2)
        a <- a[-c(1:3),]
        names(a) <- 1:13
        
    } else {
        a <- read_excel(file_name, skip = 2)
        colnames(a) <- a[3,]
        a <- a[-c(1:3),]
        sc_st <- ifelse(sum(str_detect(a$`1`, "C|T$")) == 0, FALSE, TRUE)
    }
    
    # only keep all india data from india files; remove state totals (otherwise data is repeated)
    if (all_india) {
        if (sc_st) {
            a <- a[a$`4` == "INDIA",]
        } else {
            a <- a[a$`6` == "INDIA",]
        }
    }
    
    saveRDS(a, file = str_c("2-read", substring(file_path_sans_ext(file_name), 6), ".rds"))
}

# map read_raw function to raw files
purrr::map(raw_files, read_raw)

# 2001 Andhra Pradesh had a second sheet I missed the first time!
# first sheet read correctly so just add second sheets as a separate file
path <- "1-raw-data/2001/s2_H - 12 Distribution of Households by Source and Location of Drinking Water and Availability of Electricity and Latrine Table For Andhra Pradesh.xlsx"
ap2 <- read_excel(path, skip = 2)
ap2 <- ap2[-c(1:3),]
names(ap2) <- 1:13
saveRDS(ap2, file = str_c("2-read", substring(file_path_sans_ext(path), 6), ".rds"))


# Step 2 Aggregate state rds files into one file per year -----------------
# while negotiating different formats (SC/ST)


# create directory for aggregated data output
if (!dir.exists("3-agg-data")) {
    dir.create("3-agg-data")
}

# get vector of all read files
read_files <- list.files("2-read-data", recursive = TRUE, full.names = TRUE)


# Step 2a Bind 2011 files -------------------------------------------------


# function to add NA tehsil and town columns missing in 2011 SC/ST files before merging
add_tehsil_town <- function(file_name) {
    a <- readRDS(file_name)
    a$tehsil <- NA
    a$town <- NA
    a <- a[c(1:3, "tehsil", "town", 4:12)]
    names(a) <- 1:14
    saveRDS(a, file = file_name)
}

# add NA columns to 2011 SC/ST files to match non SC/ST dimensions
read_files %>%
    str_subset("2011") %>%  
    str_subset("SC|ST") %>% 
    map(add_tehsil_town) 

# now 2011 files can be merged regardless of SC/ST
read_files %>%
    str_subset("2011") %>%
    map(readRDS) %>% 
    reduce(rbind) %>%
    mutate(year = 2011) %>% 
    rename_at(vars(names(.)), ~ 1:15) %>% 
    saveRDS(file = "3-agg-data/2011.rds")


# Step 2b Bind 2001 files -------------------------------------------------


# function to remove extra row at top of 2001 sc/st files
clean_01sc_st <- function(file_name) {
    a <- readRDS(file_name)
    a <- a[-c(1),]
    saveRDS(a, file = file_name)
}

# final cleaning on 01 sc st files before merging
read_files %>%
    str_subset("2001") %>%
    str_subset("SC|ST") %>% 
    map(clean_01sc_st)

# merge 2001 files
read_files %>%
    str_subset("2001") %>%
    map(readRDS) %>% 
    reduce(rbind) %>%
    add_column(town = NA, .after = "4") %>% # add extra row to match 2011
    mutate(year = 2001) %>%
    separate(`5`, into = c("before", "after"), sep = "  \\d") %>% # extract name, discarding code
    select(-after) %>% 
    mutate(before = word(before, start = 1, sep = " \\*")) %>% # fix trailing * in a few cases
    rename_at(vars(names(.)), ~ 1:15) %>% 
    saveRDS(file = "3-agg-data/2001.rds")


# Step 2c Bind 1991 files -------------------------------------------------


# function to add SC/ST/all column in 1991 files
identify_SC_ST <- function(file_name) {
    
    a <- readRDS(file_name)
    if (str_detect(file_name, "SC")) {
        a$section <- "SC"
    } else if (str_detect(file_name, "ST")) {
        a$section <- "ST"
    } else {
        a$section <- "All"
    }
    saveRDS(a, file = file_name)
}

# 91 bihar sc and st is corrupted

# map function to 1991 files adding SC/ST/All where necessary
read_files %>%
    str_subset("1991") %>%
    map(identify_SC_ST)

# merge 1991 files
read_files %>%
    str_subset("1991") %>%
    map(readRDS) %>% 
    reduce(rbind) %>% 
    mutate(year = 1991) %>%
    saveRDS(file = "3-agg-data/1991.rds")

rm(list = ls())
