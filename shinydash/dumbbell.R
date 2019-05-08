library(forcats)

build_dumbbell <- function(df) {

    my_gg <- df %>% 
        filter(!is.na(greater)) %>%
        ggplot() +
        theme_minimal(base_size = 80) +
        geom_point_interactive(
            aes(x = ea, y = fct_reorder(dumbbell_y, ea), 
                size = total_hh / 1e6, color = zone,
                tooltip = tip, data_id = tip)
        ) +
        geom_point_interactive(
            aes(x = la, y = fct_reorder(dumbbell_y, la), 
                size = total_hh / 1e6, color = zone,
                tooltip = tip, data_id = tip)
        ) +
        geom_segment(
            aes(x = la, y = dumbbell_y, xend = ea,
                yend = dumbbell_y, linetype = greater), 
            size = 3
        ) +
        scale_x_continuous(
            "Electricity and Latrine Rates",
            breaks = seq(0, 1, 0.25), 
            limits = c(0, 1)
        ) +
        scale_y_discrete("") +
        scale_color_manual(
            "Zone", values = col, 
            breaks = c("Northern", "North Eastern", "Central",
                       "Eastern", "Western", "Southern")
        ) +
        scale_size_area(
            "Household Count", max_size = 40, 
            labels = scales::unit_format(unit = "m", sep = "", accuracy = 0.5)
        ) +
        scale_linetype_manual(
            "Higher Percentage", values = c("solid", "dotted"), 
            labels = c("Electricity","Latrine")
        ) +
        guides(
            color = guide_legend(override.aes = list(size = 35), order = 1),
            size = guide_legend(order = 2),
            linetype = guide_legend(override.aes = list(size = 3))
        ) +
        theme(legend.key.width = unit(6,"cm")) +
        labs(
            title = plot_title,
            subtitle = paste0(df$my_subtitle, " (", length(unique(df$state)), " States/UTs)")
        )
    
    x <- girafe(ggobj = my_gg, width_svg = 56, height_svg = 50, width = 1)
    
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

