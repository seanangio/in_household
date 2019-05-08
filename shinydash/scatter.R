
build_scatter <- function(df, ai_df) {
    
    my_gg <- ggplot() +
        theme_minimal(base_size = 70) +
        geom_hline(
            data = ai_df,
            aes(yintercept = la),
            linetype = 2
        ) +
        geom_vline(
            data = ai_df,
            aes(xintercept = ea),
            linetype = 2
        ) +
        geom_text_interactive(
            data = ai_df,
            aes(x = 0.06, y = la + 0.03, label = "All India Avg.",
                tooltip = ai_tip),
            size = 18
        ) +
        scale_x_continuous(
            "\nElectricity\n",
            breaks = seq(0, 1, 0.25), 
            limits = c(0, 1)
        ) +
        scale_y_continuous(
            "Latrine\n", 
            breaks = seq(0, 1, 0.25), 
            limits = c(0, 1)
        ) +
        geom_point_interactive(
            data = df,
            aes(x = ea, y = la, size = total_hh / 1e6, 
                color = zone, tooltip = tip, data_id = tip),
            alpha = 0.8
        ) +
        scale_size_area(
            "Household Count", max_size = 40, 
            labels = scales::unit_format(unit = "m", sep = "", accuracy = 0.5)
        ) +
        scale_color_manual(
            "Zone", values = col, 
            breaks = c("Northern", "North Eastern", "Central",
                       "Eastern", "Western","Southern")
        ) +
        guides(
            color = guide_legend(override.aes = list(size = 35), order = 1),
            size = guide_legend(order = 2)
        ) +
        labs(
            title = plot_title,
            subtitle = paste0(df$my_subtitle, " (", length(unique(df$state)), " States/UTs)")
        ) +
        theme(legend.title.align = 0.5)

    x <- girafe(ggobj = my_gg, width_svg = 36, height_svg = 30, width = 1)
    
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

