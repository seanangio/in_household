library(shiny)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(scales)
library(DT)
library(purrr)
source("global.R")

shinyServer(function(input, output, session) {
    session$onSessionEnded(stopApp)
# Set UI choices and selections -------------------------------------------

    # set choice vectors for pv, ws, wa as reactives
    pv_choices <- reactive({
        get_plot_var_choices(input$year, input$data_type)
    })
    
    ws_choices <- reactive({
        get_ws_choices(input$year)
    })
    
    wa_choices <- reactive({
        get_wa_choices(input$year)
    })
    
    # assign selections to reactive values list to avoid leaflet double render
    rv <- reactiveValues(click = NULL)
    
    # plot_var selection needs to change differently based on 3 events
    # when data type changes, simply take the first option in new pv_choices()
    observeEvent(input$data_type, {
        rv$pv <- get_pv_from_dt_change(input$plot_var, pv_choices())

    })

    # when year changes, the new pv depends on the pv itself
    observeEvent(input$year, {
        rv$pv <- get_pv_from_yr_change(input$plot_var, pv_choices(), 
                                        input$year) 
    })
    
    # user picks a new option from the existing dropdown menu; 
    # downside is that it runs even when others change
    observeEvent(input$plot_var, {
        rv$pv <- input$plot_var
    })
    
    # selections for ws and wa are simpler
    observe({
        rv$ws <- get_ws_selection(input$ws, ws_choices(), input$year, 
                                  input$data_type)
    })
    
    observe({
        rv$wa <- get_wa_selection(input$wa, wa_choices(), input$data_type)
    })
    
    # lastly update ui with correct choices and selections
    observe({
        updateSelectInput(session, "plot_var",
                          choices = pv_choices(),
                          selected = rv$pv) 
    })

    observe({
        updateSelectInput(session, "ws",
                          choices = ws_choices(),
                          selected = rv$ws)
    })

    observe({
        updateSelectInput(session, "wa",
                          choices = wa_choices(),
                          selected = rv$wa)
    })

# Data Manipulations ------------------------------------------------------

    # create correct district sf
    district_sf <- reactive({
        choose_district(input$year, input$society,
                        input$demo, rv$ws, rv$wa)
    })
    
    # drop geometry and use district df where possible
    districtInput <- reactive({
        district_sf() %>% 
            st_set_geometry(NULL)
    })
    
    # get similar df for country and state data
    countryInput <- reactive({
        choose_country(input$year, input$society,
                       input$demo, rv$ws, rv$wa) 
    })
    
    stateInput <- reactive({
        choose_state(input$year, input$society,
                     input$demo, rv$ws, rv$wa)
    })
    
    # get all map arguments given chosen data
    args <- reactive({
        get_args(rv$pv, districtInput())
    })
    
    # get legend and histogram titles given parameters
    titles <- reactive({
        get_titles(input$data_type, args(), rv$pv)
    })
    
# Map Output --------------------------------------------------------------

    # output base map and update components in observers
    output$mymap <- renderLeaflet({
        draw_base_map()
    })
    
    # only place where sf is needed
    observeEvent(args(), {
        update_map_colors("mymap", district_sf(), args())
    })
    
    observe({
        update_map_legend("mymap", districtInput(), args(), titles())
    })
    
    # state lines only change with year
    state_lines <- reactive({
        choose_state_lines(input$year)
    })
    
    observeEvent(input$year, {
        update_map_lines("mymap", state_lines())
    })

# Sidebar Output ----------------------------------------------------------

    # output all india percentage above histogram
    output$all_india_text <- renderUI({
        render_all_india_string(countryInput(), args(),
                                input$data_type, districtInput())
    })
    
    # all_year and all_state needed for line plot
    all_year_df <- reactive({
        purrr::map_dfr(
            list(country91, country01, country11), 
            get_country_figures, 
                input$society, input$demo, rv$ws, 
                rv$wa, args()$var_nm)
    })

    all_state_df <- reactive({
        purrr::map_dfr(
            list(state91, state01, state11), 
            get_state_figures, 
                input$society, input$demo, rv$ws, 
                rv$wa, args()$var_nm)
    })

    my_hist <- reactive({
        draw_histogram(districtInput(), args(), countryInput(),
                       rv$pv, titles())
    })
    
    my_line_plot <- reactive({
        draw_line_plot(all_year_df(), input$data_type,
                       input$year, titles())
    })
    
# District Click Event ----------------------------------------------------

    # show clear district button when click is not null
    output$clear_district <- renderUI({
        if (!is.null(rv$click)) {
            actionButton("reset", "Remove District")
        }
    })
    
    # assign leaflet map click info to rv$click 
    observeEvent(input$mymap_shape_click, {
        rv$click <- input$mymap_shape_click
    })
    
    # reset to null when button clicked
    observeEvent(input$reset, {
        rv$click <- NULL
    })
    
    # also reset to null when district name isn't found 
    # (eg when a year changes and district no longer there)
    observeEvent(input$year, {
        if (!is.null(rv$click$id)) {
            if (!rv$click$id %in% unique(districtInput()[["district_abb"]])) {
                rv$click <- NULL
                showNotification("Note: District removed because of name or state change in new year.",
                                 type = "message", duration = 8)
            } 
        }
    })
    
    # district click event
    observe({
        
        sub <- districtInput()[districtInput()$district_abb == rv$click$id,]
        chosen_st_all_yr <- all_state_df()[which(all_state_df()$abb == sub$abb),]
        
        # if no district click, output ordinary histogram and line plot
        if (is.null(rv$click)) {
            
            output$myhist <- renderPlot({
                my_hist()
            })
            
            output$line_plot <- renderPlot({
                my_line_plot()
            })

        # otherwise output district table and updated histogram, lineplot    
        } else {
             
            # either outputs district table or nothing; 
            # needed to fully remove district table after 
            # clear district button clicked or district not found
            output$district_result <- renderUI({

                if (is.null(input$reset) || is.null(rv$click)) {
                    NULL
                } else {
                    # buttons shouldn't rely on a value but it works...
                    if (input$reset %% 2 == 0) {
                        DTOutput("district_table")
                    }
                }
            })
            
            output$district_table <- renderDT({
                DT::datatable(
                    draw_district_table(rv, sub, args(), districtInput(),
                                        chosen_st_all_yr, input$year,
                                        input$data_type),
                    rownames = FALSE, colnames = c("",""), filter = "none",
                    style = "bootstrap",
                    class = "compact",
                    options = list(
                        dom = 'b', ordering = FALSE
                    )
                ) %>% formatStyle(
                    'colA',
                    target = "row",
                    color = styleEqual(
                        c("District Percentage", "State/UT Percentage", "District Household Count"), 
                        c(district_color, state_color, district_color))
                )
            })

            output$myhist <- renderPlot({
                add_xintercepts(my_hist(), input$data_type, sub, args(),
                                chosen_st_all_yr, input$year)
            }) 
                
            output$line_plot <- renderPlot({
                add_state_trend(my_line_plot(), chosen_st_all_yr, input$year,
                                sub, args(), input$data_type)
            }) 
        }
    })
    
# Notifications -----------------------------------------------------------
    
    # level is no longer found when changing year
    observeEvent(input$year, {
        if (!input$plot_var %in% pv_choices() ||
            !input$ws %in% ws_choices() ||
            !input$wa %in% wa_choices()) {
            showNotification("Note: Level not found in given year;
                             closest default provided",
                             type = "warning", duration = 8)
        }
    })

    # lack of 2001 data in electricity/latrine fields
    observe({
        if (input$year == "2001" &
            rv$pv %in% tail(el_options, 4)) {
            showNotification("Unlike 1991 and 2011, 2001 data reported
                             without simultaneous regards for Electricity
                             and Latrine status.",
                             type = "message", duration = 10)
        }
    })

    # missing 91 Bihar SC ST data
    observe({
        if (input$year == "1991" &
            (input$society %in% c("SC", "ST"))) {
            showNotification("Note: 1991 Bihar SC/ST data could not be processed.",
                             type = "message", duration = 8)
        }
    })

    # missing 91 JK data
    observe({
        if (input$year == "1991") {
            showNotification("Note: All 1991 Jammu & Kashmir data absent
                             due to conflict in the region.",
                             type = "message", duration = 8)
        }
    })
})