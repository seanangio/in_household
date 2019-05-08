library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(sf)
library(ggiraph)

source("scatter.R")
source("dumbbell.R")
source("map.R")
source("table.R")

# read data ---------------------------------------------------------------

data <- readRDS("data/states_districts.rds")
cntry <- readRDS("data/country.rds")

# global variables --------------------------------------------------------

col <- as.character(data$zone_color)
names(col) <- as.character(data$zone)

tooltip_css <- "background-color:gray;color:white;padding:5px;border-radius:5px;font-family:sans-serif;font-size:12px;"
hover_css <- "cursor:pointer;stroke-width:5px;stroke:black"
plot_title <- "\nRates of Household Access to Electricity & Latrines in India"

# functions ---------------------------------------------------------------


filter_sf <- function(geo, yr, soc, demo, zones, states) {
    
    data %>% 
        filter(
            geo_section == geo,
            year == yr,
            societal_section == soc,
            demo_section == demo,
            zone %in% zones, 
            state %in% states
        )
}

filter_all_india <- function(yr, soc, demo) {
    
    cntry %>% 
        filter(
            year == yr,
            societal_section == soc,
            demo_section == demo
        )
}

