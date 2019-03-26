library(dplyr)
library(sf)
library(ggplot2)

# read data ---------------------------------------------------------------

my_sf <- readRDS("final_sf.rds")
my_dots <- readRDS("final_dots.rds")

# set variables -----------------------------------------------------------

category_opts <- c("Electricity", "Latrine",
                "Electricity and Latrine",
                "Electricity but no Latrine",
                "No Electricity but Latrine",
                "Neither Electricity nor Latrine")

viz_opts <- c("Choropleth", "Proportional Symbols", 
              "Dot Density", "3D Choropleth")

clat <- 22.5
clon <- 82.5
key <- "pk.eyJ1Ijoic2VhbmFuZ2lvIiwiYSI6ImNqbDM3ZDA2MzF5Znkzd3F0cGVrZ2p2aGsifQ.8GljGBJeyKvFC61bSCTdQg"
streets_tileset <- "mapbox://styles/seanangio/cjtlzk1cz29rs1fl5xnznx436"

legend_height <- "max-height: 300px;"
my_fill_opacity <- 200
dot_radius <- 1000
my_stroke_width <- 1000
my_stroke_opacity <- 100
my_stroke_colour <- "#000000"

# functions for server ----------------------------------------------------

get_category <- function(var) {
    # convert user input text to corresponding columns in dataset
    switch(var,
        "Electricity" = c("num_ea", "Electricity", "num_ea_sy"),
        "Latrine" = c("num_la", "Latrine", "num_la_sy"),
        "Electricity and Latrine" = c("num_ea_la", "Electricity</br>& Latrine", "num_ea_la_sy"),
        "Electricity but no Latrine" = c("num_ea_ln", "Electricity</br>but no</br>Latrine", "num_ea_ln_sy"),
        "No Electricity but Latrine" = c("num_en_la", "No Electricity</br>but Latrine", "num_en_la_sy"),
        "Neither Electricity nor Latrine" = c("num_en_ln", "Neither</br>Electricity</br>nor Latrine", "num_en_ln_sy")
    )
}

draw_base_map <- function() {
    mapdeck(
        style = streets_tileset
        , zoom = 4
        , location = c(clon, clat)
    )
}

choose_obs <- function(v, var, yr, society, demo) {
    # dot density uses a different dataset (multipoints)
    # select correct dataset and filter accordingly
    if (v == "Dot Density") {
        sf <- my_dots %>% 
            filter(category == var[1])
    } else {
        sf <- my_sf
    }
    
    sf %>%
        filter(
            year == yr,
            societal_section == society,
            demo_section == demo
        )
}


update_map <- function(mymap, sf, vars, v) {
    
    m <- mapdeck_update(map_id = mymap) %>% 
        clear_polygon(layer_id = "layer") %>%
        clear_scatterplot(layer_id = "layer")
    
    if (v == "Choropleth") {
        m %>% 
            add_polygon(
                data = sf
                , layer = "layer"
                , fill_colour = vars[2]
                , fill_opacity = my_fill_opacity
                , tooltip = "district_abb"
                , update_view = FALSE
                , stroke_colour = my_stroke_colour
                , stroke_width = my_stroke_width
                , stroke_opacity = my_stroke_opacity
                , legend = list( 
                    fill_colour = TRUE, 
                    stroke_colour = FALSE 
                )
                , legend_options = list(
                    css = legend_height
                )
            )
        
    } else if (v == "Proportional Symbols") {
        m %>% 
            add_scatterplot(
                data = sf %>% st_set_geometry(.$CENTROID)
                , radius = vars[3]
                , fill_colour = vars[2]
                , fill_opacity = my_fill_opacity
                , layer_id = "layer"
                , tooltip = "district_abb"
                , update_view = FALSE
                , legend = TRUE
                , legend_options = list(
                    css = legend_height
                )
                , auto_highlight = TRUE
            )
        
    } else if (v == "Dot Density") {
        m %>%
            add_scatterplot(
                data = sf
                , radius = dot_radius
                , layer_id = "layer"
                , update_view = FALSE
                , legend = FALSE
                , legend_options = list(
                    css = legend_height
                )
            )

    } else { # 3d choropleth
        m %>%
            add_polygon(
                data = sf
                , layer = "layer"
                , fill_colour = vars[2]
                , fill_opacity = my_fill_opacity
                , elevation = vars[1]
                , tooltip = "district_abb"
                , update_view = FALSE
                , legend = TRUE
                , legend_options = list(
                    css = legend_height
                )
            )
    }
}


