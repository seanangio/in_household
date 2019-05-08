library(tidyr)

my_legend <- expand.grid(
        x = c(1/3, 2/3, 1),
        y = c(1/3, 2/3, 1)
    ) %>% 
    ggplot(aes(x, y, fill = atan(y/x), alpha = x + y)) +
    geom_tile() +
    scale_fill_viridis_c() +
    theme(
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_text(size = 15, color = "black"),
        axis.text = element_text(size = 13, color = "black"),
        panel.background = element_blank()
    ) +
    scale_x_continuous(
        "Electricity", breaks = c(0.18, 0.505, 0.835, 1.15),
        labels = c("0%","33%","66%","100%")
    ) +
    scale_y_continuous(
        "Latrine", breaks = c(0.18, 0.505, 0.835, 1.15),
        labels = c("0%","33%","66%","100%")
    )

build_map <- function(sf) {
    
    my_map <- sf %>% 
        complete(x, y) %>% # missing boxes messes color scheme
        st_as_sf() %>%
        ggplot() +
        geom_sf(fill = "white") +
        geom_point_interactive(
            aes(x = COORDS_X, y = COORDS_Y, size = total_hh / 1e6,
                color = atan(y/x), alpha = x + y,
                tooltip = tip, data_id = tip)
        ) +
        scale_size_area(
            "Household Count", 
            max_size = 100,
            labels = scales::unit_format(unit = "m", sep = "", accuracy = 0.5)
        ) +
        coord_sf(datum = NA) +
        theme_void() +
        scale_color_viridis_c() +
        guides(alpha = FALSE, color = FALSE) +
        labs(
            title = plot_title,
            subtitle = paste0(sf$my_subtitle, " (", length(unique(sf$state)), " States/UTs)"),
            x = "",
            y = ""
        ) +
        theme_minimal(base_size = 62)
    
    x <- girafe(ggobj = my_map, width_svg = 42, height_svg = 36, width = 1)
    
    girafe_options(x,
                   opts_tooltip(css = tooltip_css, use_fill = TRUE),
                   opts_zoom(max = 5),
                   opts_hover(css = hover_css),
                   opts_toolbar(
                       position = "topright",
                       saveaspng = TRUE
                   )
    )
}

