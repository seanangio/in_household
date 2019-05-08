
function(input, output, session) {
    
    session$onSessionEnded(stopApp)

# logic for geographic filter ---------------------------------------------

    rv <- reactiveValues(
        
        states = unique(data[data$year == "2011",]$state),
        zones = unique(data$zone)
    )
    
    observeEvent({input$year}, {
        
        rv$states <- data %>% 
            filter(
                year == input$year, 
                zone %in% rv$zones
            ) %>% 
            pull(state) %>% 
            unique()
    })
    
    observeEvent({input$zones}, { 
        
        if (is.null(input$zones)) { 
            
            rv$zones <- c("")
            rv$states <- c("")
            
        } else {
            
            rv$zones <- input$zones
            rv$states <- data %>%
                filter(
                    year == input$year,
                    zone %in% rv$zones
                ) %>% 
                pull(state) %>% 
                unique()
        }
        
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # when states change, need to update states and regions
    observeEvent({input$states}, {
        
        if (is.null(input$states)) {
            
            rv$states <- c("")
            rv$zones <- c("")
            
        } else {
            
            rv$states <- input$states
            rv$zones <- data %>% 
                filter(
                    year == input$year, 
                    state %in% rv$states
                ) %>% 
                pull(zone) %>% 
                unique()
        }
        
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

# reactive sf and df ------------------------------------------------------

    my_sf <- reactive({
        filter_sf(input$geo, input$year, input$society, 
                  input$demo, rv$zones, rv$states)
    })
    
    # drop geometry when not required
    my_df <- reactive({
        my_sf() %>%
            st_set_geometry(NULL)
    })
    
    ai_df <- reactive({
        filter_all_india(
            input$year, input$society, input$demo
        )
    })

# dynamic UI for geo filters ----------------------------------------------

    output$choose_zones <- renderUI({
        
        pickerInput(
            inputId = "zones",
            label = "Select/deselect zones to plot",
            choices = unique(data$zone),
            selected = unique(my_df()$zone), 
            options = list(`actions-box` = TRUE),
            multiple = TRUE
        )
    })
    
    output$choose_states <- renderUI({
        
        pickerInput(
            inputId = "states",
            label = "Select/deselect states to plot",
            choices = unique(data[data$year == input$year,]$state),
            selected = unique(my_df()$state), 
            options = list(`actions-box` = TRUE),
            multiple = TRUE
        )
    })


# plot tab outputs --------------------------------------------------------

    output$scatter_plot <- renderGirafe({
        build_scatter(my_df(), ai_df()) 
    })
    
    output$dumbbell_plot <- renderGirafe({
        build_dumbbell(my_df())
    })

# map tab outputs ---------------------------------------------------------

    output$map <- renderGirafe({
        build_map(my_sf())
    })
    
    output$map_legend <- renderPlot({
        my_legend
    })


# table tab outputs -------------------------------------------------------

    output$selection <- renderText({
        paste0(
            "Current Data Selection: ",
            my_df()$my_subtitle[1], 
            " (", length(unique(my_df()$state)), 
            " States/UTs)"
        )
    })

    output$tbl <- renderDT({
        datatable(
            build_table(my_df(), input$geo),
            rownames = FALSE,
            options = list(
                pageLength = 10,
                lengthMenu = c(10, 20, 35, 50)
            )
        ) %>%     
            formatPercentage(c('Electricity', 'Latrines'), digits = 2) %>%
            formatRound('Households', digits = 0)
    })
    

# standouts tab outputs ---------------------------------------------------

    output$selected_filter <- renderText({
        paste0(
            my_df()$my_subtitle[1], 
            " (", length(unique(my_df()$state)), 
            " States/UTs)"
        )
    })
    
    output$ai_ea <- renderValueBox({
        valueBox(
            value = sprintf("%1.2f%%", 100*ai_df()$ea),
            subtitle = "All-India Electricity",
            icon = icon("lightbulb"),
            color = "yellow"
        )
    })
    output$ai_la <- renderValueBox({
        valueBox(
            value = sprintf("%1.2f%%", 100*ai_df()$la), 
            subtitle = "All-India Latrine",
            icon = icon("toilet"),
            color = "green"
        )
    })

    output$best_ea <- renderValueBox({
        valueBox(
            value = sprintf("%1.2f%%", 100*max(my_df()$ea, na.rm = TRUE)), 
            subtitle = my_df()[which(my_df()$ea == max(my_df()$ea, na.rm = TRUE)),][["dumbbell_y"]],
            icon = icon("lightbulb"),
            color = "yellow"
        )
    })

    output$best_la <- renderValueBox({
        valueBox(
            value = sprintf("%1.2f%%", 100*max(my_df()$la, na.rm = TRUE)),
            subtitle = my_df()[which(my_df()$la == max(my_df()$la, na.rm = TRUE)),][["dumbbell_y"]],
            icon = icon("toilet"),
            color = "green"
        )
    })

    output$worst_ea <- renderValueBox({
        valueBox(
            value = sprintf("%1.2f%%", 100*min(my_df()$ea, na.rm = TRUE)), 
            subtitle = my_df()[which(my_df()$ea == min(my_df()$ea, na.rm = TRUE)),][["dumbbell_y"]],
            icon = icon("lightbulb"),
            color = "yellow"
        )
    })

    output$worst_la <- renderValueBox({
        valueBox(
            value = sprintf("%1.2f%%", 100*min(my_df()$la, na.rm = TRUE)),
            subtitle = my_df()[which(my_df()$la == min(my_df()$la, na.rm = TRUE)),][["dumbbell_y"]],
            icon = icon("toilet"),
            color = "green"
        )
    })
    
    observe({
        print(length(unique(my_df()$state)))
    })

}