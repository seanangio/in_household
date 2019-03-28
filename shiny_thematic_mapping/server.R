library(shiny)
library(mapdeck)
source("global.R")

shinyServer(function(input, output, session) {
    session$onSessionEnded(stopApp)
    
    set_token(Sys.getenv("MAPBOX"))
    
    # render base map
    map_base <- reactive({
        draw_base_map()
    })
    
    output$myMap <- renderMapdeck({ map_base() })

    # translate category choice into vector of corresponding vars
    my_vars <- reactive({
        get_category(input$category)
    })
    
    # filter dataset based on all inputs
    sf_reactive <- reactive({
        choose_obs(input$viz, my_vars(), input$year, 
                   input$society, input$demo)
    })
    
    # re-draw base map when inputs change
    observe({
        update_map("myMap", sf_reactive(), my_vars(), input$viz)
    })
    

# notifications -----------------------------------------------------------
    
    # lack of 2001 data in electricity/latrine fields
    observe({
        if (input$year == "2001" &
            input$category %in% tail(category_opts, 4)) {
            showNotification("Unlike 1991 and 2011, 2001 data reported
                             without simultaneous regards for Electricity
                             and Latrine status.",
                             type = "message", duration = 8)
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