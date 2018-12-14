library(ggplot2)
library(dplyr)
library(tibble)
library(sf)
library(leaflet)
library(leaflet.extras)

# Read in data files ------------------------------------------------------

# district, country, state files given separately per year 
# because of several different variables, different levels

district11 <- readRDS("data/district/district11.rds")
district01 <- readRDS("data/district/district01.rds")
district91 <- readRDS("data/district/district91.rds")

country11 <- readRDS("data/country/country11.rds")
country01 <- readRDS("data/country/country01.rds")
country91 <- readRDS("data/country/country91.rds")

state11 <- readRDS("data/state/state11.rds")
state01 <- readRDS("data/state/state01.rds")
state91 <- readRDS("data/state/state91.rds")

state_lines <- readRDS("data/state_lines.rds")

# Set variables -----------------------------------------------------------

el_options <- c("Electricity", "Latrine",
                "Electricity and Latrine",
                "Electricity but no Latrine",
                "No Electricity but Latrine",
                "Neither Electricity nor Latrine")

n_bins <- 50
decile_bins <- 10
normal_point_size <- 3
line_width <- 1.2
xint_size <- 2
plot_height <- 200
spinner_type <- 7

all_india_color <- "#E69F00" # orange
state_color <- "#56B4E9" # sky blue
district_color <- "#009E73" # bluish green
spinner_color <- "#D9230F" # matches shiny theme

my_map_tile <- "https://api.mapbox.com/styles/v1/seanangio/cjp3l0a5b0um42rpgxx1krirk/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic2VhbmFuZ2lvIiwiYSI6ImNqbDM3ZDA2MzF5Znkzd3F0cGVrZ2p2aGsifQ.8GljGBJeyKvFC61bSCTdQg"

# Functions to display correct UI options --------------------------------------

get_plot_var_choices <- function(year, data_type) {

    if (data_type == "Electricity/Latrine Access") {

        el_options

    } else if (data_type == "Water Source") {

        switch(year,
               "1991" = levels(district91$water_source)[-1],
               "2001" = levels(district01$water_source)[-1],
               "2011" = levels(district11$water_source)[-1])

    } else if (data_type == "Water Availability") {

        switch(year,
               "1991" = levels(district91$water_avail)[-1],
               "2001" = levels(district01$water_avail)[-1],
               "2011" = levels(district11$water_avail)[-1])

    } else if (data_type == "Household Count") {
        
        "Household Count"
        
    }

}

get_pv_from_dt_change <- function(plot_var, choices) {
    # when dt changes, pv selection is first in the list of choices
    head(choices, 1)
}

get_pv_from_yr_change <- function(plot_var, choices, yr, data_type) {
    # when yr changes, pv selection follows these cases
    case_when(
        plot_var %in% choices ~ plot_var,
        plot_var == "Outside Premises" ~ "Away",
        plot_var == "Near Premises" |
            plot_var == "Away" ~ "Outside Premises",
        
        plot_var == "Tap water from treated source" | 
            plot_var == "Tap water from un-treated source"~ "Tap",
        plot_var == "Tap" ~ "Tap water from treated source",
        plot_var == "Covered well" | 
            plot_var == "Un-Covered well"~ "Well",
        plot_var == "Well" ~ "Covered well",
        plot_var == "Hand pump" ~ "Handpump/Tubewell",
        plot_var == "Handpump/Tubewell" ~ "Hand pump",
        plot_var == "River/Canal" | 
            plot_var == "Tank" ~ "All Others",
        plot_var == "Tube well/Borehole" & 
            yr == "2001" ~ "Tube well",
        plot_var == "Tube well/Borehole" & 
            yr == "1991" ~ "Handpump/Tubewell",
        plot_var == "Tube well" & 
            yr == "1991" ~ "Handpump/Tubewell",
        plot_var == "Tube well" & 
            yr == "2011" ~ "Tube well/Borehole",
        TRUE ~ head(choices, 1)
    )
}

get_ws_choices <- function(year) {
    
    switch(year,
        "2011" = levels(district11$water_source),
        "2001" = levels(district01$water_source),
        "1991" = levels(district91$water_source))    
        
}

get_ws_selection <- function(ws, choices, yr, data_type) {
    # default to All Sources if WS/WA data type changes
    # else take level if it exists
    # lastly, switch to the closest equivalent level
    case_when(
        data_type == "Water Source" |
            data_type == "Water Availability" ~ "All Sources",
        ws %in% choices ~ ws,
        ws == "Tap water from treated source" | 
            ws == "Tap water from un-treated source"~ "Tap",
        ws == "Tap" ~ "Tap water from treated source",
        ws == "Covered well" | 
            ws == "Un-Covered well"~ "Well",
        ws == "Well" ~ "Covered well",
        ws == "Hand pump" ~ "Handpump/Tubewell",
        ws == "Handpump/Tubewell" ~ "Hand pump",
        ws == "River/Canal" | ws == "Tank" ~ "All Others",
        ws == "Tube well/Borehole" & 
            yr == "2001" ~ "Tube well",
        ws == "Tube well/Borehole" & 
            yr == "1991" ~ "Handpump/Tubewell",
        ws == "Tube well" & 
            yr == "1991" ~ "Handpump/Tubewell",
        ws == "Tube well" & 
            yr == "2011" ~ "Tube well/Borehole",
        TRUE ~ head(choices, 1)
    )
}

get_wa_choices <- function(year) {
    
    switch(year,
        "2011" = levels(district11$water_avail),
        "2001" = levels(district01$water_avail),
        "1991" = levels(district91$water_avail))
    
}

get_wa_selection <- function(wa, choices, data_type) {
    # default to Total if data type changed to WA
    # else, choose the same level if present;
    # lastly, make the closest selection
    case_when(
        data_type == "Water Availability" ~ "Total",
        wa %in% choices ~ wa,
        wa == "Outside Premises" ~ "Away",
        wa == "Near Premises" | wa == "Away" ~ "Outside Premises",
        TRUE ~ head(choices, 1)
    )    
}


# Functions to get correct dataframes based on parameters ------------------

choose_district <- function(yr, society, demo, ws, wa) {
    
    switch(yr,
           "1991" = district91,
            "2001" = district01,
            "2011" = district11) %>% 
        filter(
            societal_section == society,
            demo_section == demo,
            water_source == ws,
            water_avail == wa
        )
    
}

choose_country <- function(yr, society, demo, ws, wa) {
    
    switch(yr,
           "1991" = country91,
           "2001" = country01,
           "2011" = country11) %>% 
        filter(
            societal_section == society,
            demo_section == demo,
            water_source == ws,
            water_avail == wa
        )
}

choose_state <- function(yr, society, demo, ws, wa) {
    
    switch(yr,
           "1991" = state91,
           "2001" = state01,
           "2011" = state11) %>% 
        filter(
            societal_section == society,
            demo_section == demo,
            water_source == ws,
            water_avail == wa
        )
}

# helper function for get_country_figures() and get_state_figures()
fncols <- function(data, cname) {
    # add NA column to df if variable is missing
    # needed when missing variables across years
    add <- cname[!cname %in% names(data)]
    
    if (length(add) != 0) {
        data[add] <- NA
    }
    
    data
}

get_country_figures <- function(country_df, society, demo, ws, wa, var) {
    
    country_df %>% 
        fncols(var) %>% 
        filter(
            societal_section == society,
            demo_section == demo,
            water_source == ws,
            water_avail == wa
        ) %>% 
        select(year, var) %>% 
        rename(var = var)
    
}

get_state_figures <- function(state_df, society, demo, ws, wa, var) {
    
    state_df %>%
        fncols(var) %>%
        filter(
            societal_section == society,
            demo_section == demo,
            water_source == ws,
            water_avail == wa
        ) %>% 
        select(year, abb, var) %>% 
        rename(var = var)
}

# Functions for drawing map -----------------------------------------------

choose_state_lines <- function(yr) {
    
    state_lines %>% 
        filter(year == yr)
    
}

# converts long form variable names into corresponding data name and palette arguments
get_args <- function(plot_var, data) {
    
    args <- list()
    
    args$var_nm <- case_when(
        
        plot_var == "Electricity" ~ "ea",
        plot_var == "Latrine" ~ "la",
        plot_var == "Electricity and Latrine" ~ "ea_la",
        plot_var == "Electricity but no Latrine" ~ "ea_ln",
        plot_var == "No Electricity but Latrine" ~ "en_la",
        plot_var == "Neither Electricity nor Latrine" ~ "en_ln",
        
        plot_var == "Tap water from treated source" ~ "tap_treated",
        plot_var == "Tap water from un-treated source" ~ "tap_untreated",
        plot_var == "Covered well" ~ "covered_well",
        plot_var == "Un-Covered well" ~ "uncovered_well",
        plot_var == "Hand pump" ~ "hand_pump",
        plot_var == "Tube well/Borehole" ~ "tube_well",
        plot_var == "All Others" ~ "others",
        
        plot_var == "Within Premises" ~  "within",
        plot_var == "Near Premises" ~ "near",
        plot_var == "Away" ~ "away",
        
        plot_var == "Household Count" ~ "total_hh",
        
        # 2001
        plot_var == "Tap" ~ "tap",
        plot_var == "Tube well" ~ "tube_well",
        plot_var == "Well" ~ "well",
        
        # 1991
        plot_var == "Handpump/Tubewell" ~ "handpump_tubewell",
        plot_var == "River/Canal" ~ "river_canal",
        plot_var == "Tank" ~ "tank",
        plot_var == "Outside Premises" ~ "outside"
        
    )
    
    countPal <- colorNumeric(palette = "plasma", 
                             domain = NULL) 
    perPal <- colorBin(palette = "viridis", 
                       domain = c(0,1), bins = decile_bins)
    
    if (plot_var == "Household Count") {
        
        args$pal <- countPal
        args$fill_color <- log1p(data[[args$var_nm]])
        args$legend_values <- log1p(data[[args$var_nm]])
        args$legend_lab_format <- labelFormat(
            transform = function(x) round(exp(x)/1e4)*1e4) 
        
    } else {
        
        args$pal <- perPal
        args$fill_color <- data[[args$var_nm]]
        args$legend_values <- c(0,1)
        args$legend_lab_format <- labelFormat(
            suffix = "%", transform = function(x) 100 * x)
        
    }
    
    args
}

# returns legend and histogram titles in a list
get_titles <- function(data_type, args, plot_var) {
    
    titles <- list()
    
    titles$legend <- case_when(
        data_type == "Electricity/Latrine Access" ~ plot_var,
        data_type == "Water Source" ~ paste0("Water Source:</br>", plot_var),
        data_type == "Water Availability" ~ paste0("Water Available</br>", plot_var),
        TRUE ~ "Household</br>Count"
    )
    
    # add breaks to long cases
    titles$legend <- case_when(
        args$var_nm == "ea_la" ~ "Electricity</br>and Latrine",
        args$var_nm == "ea_ln" ~ "Electricity but</br>no Latrine",
        args$var_nm == "en_la" ~ "No Electricity</br>but Latrine",
        args$var_nm == "en_ln" ~ "Neither</br>Electricity</br>nor Latrine",
        args$var_nm == "tap_treated" ~ paste0("Water Source:</br>Tap water from</br>treated source"),
        args$var_nm == "tap_untreated" ~ paste0("Water Source:</br>Tap water from</br>un-treated source"),
        TRUE ~ titles$legend
    )
    
    titles$hist <- case_when(
        data_type == "Electricity/Latrine Access" ~ paste0("% of Households Having ", plot_var),
        data_type == "Water Source" ~ paste0("% of Households Having '", plot_var, "' as Water Source"),
        data_type == "Water Availability" ~ paste0("% of Households Having Water ", plot_var),
        data_type == "Household Count" ~ "Number of Households"
    )
    
    # add breaks to long cases
    titles$hist <- case_when(
        args$var_nm == "tap_treated" ~ "% of Households Having 'Tap water\nfrom treated source' as Water Source",
        args$var_nm == "tap_untreated" ~ "% of Households Having 'Tap water\nfrom un-treated source' as Water Source",
        TRUE ~ titles$hist
    )
    
    titles
    
}

draw_base_map <- function() {
    
    leaflet(
        options = leafletOptions(minZoom = 5, maxZoom = 9)
    ) %>%
        addTiles(urlTemplate = my_map_tile) %>% 
        setMaxBounds(lng1 = 55, lat1 = 6.5, lng2 = 111.25, lat2 = 38) %>% 
        addResetMapButton() %>% 
        addMapPane("polygons", zIndex = 410) %>% # separate panes for polygons and state lines
        addMapPane("borders", zIndex = 420) # allows state lines to remain fixed when colors drawn again

}

update_map_colors <- function(mymap, district_data, args) {
    
    leafletProxy(mymap, data = district_data) %>%
        clearGroup("colors") %>% 
        addPolygons(
            stroke = TRUE,
            weight = 1,
            opacity = 1,
            color = "white",
            fillOpacity = 0.6, 
            fillColor = ~ args$pal(args$fill_color),
            highlight = highlightOptions(
                weight = 3,
                fillOpacity = 0.8,
                color = "#666",
                bringToFront = FALSE),
            label = ~ district_abb,
            labelOptions = labelOptions(
                style = list("font-family" = c("Open Sans", "sans-serif"))),
            layerId = ~ district_abb,
            group = "colors",
            options = pathOptions(pane = "polygons")
        )
}

update_map_legend <- function(mymap, district_data, args, titles) {

    leafletProxy(mymap, data = district_data) %>%
        clearControls() %>%
        addLegend(
            "bottomleft",
            pal = args$pal,
            values = ~ args$legend_values,
            title = ~ titles$legend,
            labFormat = args$legend_lab_format,
            opacity = 1,
            className = "legend"
        )
}

update_map_lines <- function(mymap, state_lines) {

    leafletProxy("mymap", data = state_lines) %>%
        clearGroup("lines") %>% 
        addPolylines(
            color = "black",
            weight = 2,
            opacity = 1,
            highlight = highlightOptions(
                stroke = TRUE,
                fill = FALSE,
                weight = 4,
                fillOpacity = 0.8,
                color = "#666",
                bringToFront = TRUE),
            group = "lines",
            options = pathOptions(pane = "borders")
        )
}

# Functions for drawing histogram -----------------------------------------

draw_histogram <- function(district_data, args, country_data,
                           plot_var, titles) {
    
    if (is.na(country_data[[args$var_nm]])) { # case of entirely missing data
        
        ggplot(data = district_data)
        
    } else {
        
        p <- district_data %>%
            ggplot() +
            theme_minimal() +
            labs(title = titles$hist,
                 y = "District Count") +
            theme(axis.text = element_text(size = 12, face = "bold"))
    
        if (plot_var != "Household Count") {
            
            p +
                geom_histogram(aes(x = district_data[[args$var_nm]]),
                               bins = n_bins) +
                geom_vline(aes(xintercept = country_data[[args$var_nm]]),
                           color = all_india_color, size = xint_size) +
                labs(x = "")
            
        } else {
            
            # when plot_var == "Household Count", intercept is a different quantity
            p +
                geom_histogram(aes(x = district_data[[args$var_nm]] / 1e3),
                               bins = n_bins) +
                geom_vline(
                    aes(xintercept = median(district_data[[args$var_nm]] / 1e3,
                                                   na.rm = TRUE)),
                           color = all_india_color, size = xint_size) +
                scale_x_continuous("Thousand Households")
            
        }
        
    }    
}

add_xintercepts <- function(p, data_type, sub, args, chosen_st_all_yr, yr) {

    if (is.na(sub[[args$var_nm]])) { # case of grey districts

        p

    } else {

        if (data_type != "Household Count") {
            p +
                geom_vline(
                    aes(xintercept = sub[[args$var_nm]]),
                    color = district_color, size = xint_size) +
                geom_vline(
                    aes(xintercept = chosen_st_all_yr[chosen_st_all_yr$year == yr,]$var),
                    color = state_color, size = xint_size)

        } else {
            p +
                geom_vline(
                    aes(xintercept = sub[[args$var_nm]] / 1e3),
                    color = district_color, size = xint_size)

        }
    }

}

# Functions for drawing line plot -----------------------------------------

draw_line_plot <- function(year_df, data_type, yr, titles) {
    
    # base plot
    p <- year_df %>% 
        ggplot() +
        theme_minimal() + 
        scale_x_continuous("", breaks = c(1991, 2001, 2011),
                           limits = c(1991, 2011)) +
        theme(axis.text = element_text(size = 12, face = "bold")) +
        labs(title = titles$hist)
    
    if (data_type != "Household Count") {
        
        p +
            # normal points
            geom_point(
                aes(x = year, y = var), 
                color = all_india_color, size = normal_point_size) +
            # connect them
            geom_line( 
                data = year_df[!is.na(year_df$var),],
                aes(x = year, y = var),
                color = all_india_color,
                group = 1, size = line_width) +
            # larger point for currently selected year
            geom_point( 
                data = year_df %>% filter(year == yr),
                aes(x = year, y = var), 
                size = 4, shape = 23, fill = all_india_color, stroke = 1) +
            scale_y_continuous("", limits = c(0,1))
        
    } else {
        
        p + 
            # normal points
            geom_point(
                aes(x = year, y = var / 1e6), 
                color = all_india_color, 
                size = normal_point_size) +
            # connect them
            geom_line( 
                data = year_df[!is.na(year_df$var),],
                aes(x = year, y = var / 1e6),
                color = all_india_color,
                group = 1, 
                size = line_width) +
            # larger point for currently selected year
            geom_point(
                data = year_df %>% filter(year == yr),
                aes(x = year, y = var / 1e6), 
                size = 4, 
                shape = 23, 
                fill = all_india_color, 
                stroke = 1) +
            scale_y_continuous("Million(s)", limits = c(0, NA))
    
    }

}

add_state_trend <- function(p, chosen_st_all_yr, yr, sub, args, data_type) {
    
    if (is.na(sub[[args$var_nm]])) { # case of grey districts
        
        p
        
    } else {
        
        if (data_type != "Household Count") {
            
            p +
                # ordinary state points
                geom_point(
                    data = chosen_st_all_yr,
                    aes(x = year, y = var),
                    color = state_color, 
                    size = normal_point_size) +
                # connect even if missing
                geom_line(
                    data = chosen_st_all_yr[!is.na(chosen_st_all_yr$var),],
                    aes(x = year, y = var),
                    linetype = "dashed",
                    color = state_color,
                    group = 1, 
                    size = line_width) +
                # large point for current year
                geom_point(
                    data = chosen_st_all_yr %>% filter(year == yr),
                    aes(x = year, y = var),
                    size = normal_point_size, 
                    color = state_color, 
                    shape = 17) + # triangle
                # special district point 
                geom_point(
                    aes(x = sub$year, y = sub[[args$var_nm]]),
                    size = normal_point_size, 
                    color = district_color, 
                    shape = 8) # asterisk
            
        } else {
            
            p +
                # ordinary state points
                geom_point(
                    data = chosen_st_all_yr,
                    aes(x = year, y = var / 1e6),
                    color = state_color, 
                    size = normal_point_size) +
                # connect even if missing
                geom_line(
                    data = chosen_st_all_yr[!is.na(chosen_st_all_yr$var),],
                    aes(x = year, y = var / 1e6),
                    linetype = "dashed",
                    color = state_color,
                    group = 1, 
                    size = line_width) +
                # large point for current year
                geom_point(
                    data = chosen_st_all_yr %>% filter(year == yr),
                    aes(x = year, y = var / 1e6),
                    size = normal_point_size, 
                    color = state_color, 
                    shape = 17) + # triangle
                # special district point 
                geom_point(
                    aes(x = sub$year, y = sub[[args$var_nm]] / 1e6),
                    size = normal_point_size, 
                    color = district_color, 
                    shape = 8) + # asterisk
                labs(caption = "State trend in blue") +
                theme(plot.caption = element_text(color = state_color,
                                                  size = 14))
        }
    }    
}

# Functions for district table and all-india string -----------------------

render_all_india_string <- function(country_df, args, data_type, district_data) {
    
    if (is.na(country_df[[args$var_nm]])) {

        HTML("<p style='color:#E69F00'>All data missing for the selected parameters.</p>")

    } else {

        if (data_type == "Household Count") {
            
            HTML(
                paste0("<p style='color:#E69F00'>Median district size: ",
                      scales::comma(median(district_data[[args$var_nm]],
                                           na.rm = TRUE)),
                      " (in orange)</p>")
            )
            
        } else {
            
            HTML(
                paste0("<p style='color:#E69F00'>All-India Percentage: ",
                      scales::percent(country_df[[args$var_nm]]),
                      " (in orange)</p>")
            )
            
        }
    }
}

draw_district_table <- function(rv, sub, args, district_data, 
                                chosen_st_all_yr, yr, data_type) {
    
    # no click: return an empty tibble
    if (is.null(rv$click)) {
        return(tribble(~colA, ~colB))
    }
    
    else {
        # common table header
        district_table <- tribble(
            ~colA, ~colB,
            "District (State/UT)", paste0(sub$district, " (", sub$state, ")")
        )
        
        # grey districts
        if (is.na(sub[[args$var_nm]])) {
            district_table %>%
                add_row(
                    colA = "",
                    colB = "Failure to match the specified criteria."
                )
            
        } else {

            if (data_type == "Household Count") {
                
                district_table %>%
                    bind_rows(
                        tribble(
                            ~colA, ~colB,
                            "District Household Count", scales::comma(sub$total_hh)
                        )
                    )
                
            } else { # i.e. case of Electricity, Water Source, Water Availability
                
                district_table %>%
                    bind_rows(
                        tribble(
                            ~colA, ~colB,
                            "In terms of households",
                            paste0(scales::comma(sub[[paste0("num_", args$var_nm)]]), 
                                  " / ", scales::comma(sub$total_hh)),
                            "District Percentage",
                            scales::percent(sub[[args$var_nm]]),
                            "State/UT Percentage",
                            scales::percent(chosen_st_all_yr[chosen_st_all_yr$year == yr,]$var)
                        )
                    )
            }
        }    
    }
}
