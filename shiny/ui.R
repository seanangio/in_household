library(shiny)
library(shinyWidgets)
library(leaflet)
library(markdown)
library(shinythemes)
library(shinycssloaders)
source("global.R")

shinyUI(
    navbarPage(
        title = "Electricity, Latrine & Water Access in India",
        theme = shinytheme("simplex"),
        tabPanel("Map",
            div(class = "outer",
                tags$head(
                    includeCSS("styles.css")
                ),
                
                leafletOutput("mymap", width = "100%", height = "100%"),
                
                absolutePanel(
                    id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 55, left = "8%", #125 
                    right = "auto", bottom = "auto",
                    width = 0, height = 0,
                    dropdownButton(
                        label = "",
                        icon = icon("gear"),
                        status = "primary",
                        circle = TRUE,
                        width = 250,
                        
                        radioGroupButtons("year", "Year",
                                         choices = c("1991", "2001", "2011"),
                                         selected = "2011"),
                        
                        radioGroupButtons("society", "Societal Cross Section",
                                          choices = c("ALL", "SC", "ST"), 
                                          selected = "ALL"),
                        
                        radioGroupButtons("demo", "Demographic Cross Section",
                                          choices = c("Total", "Urban", "Rural"),
                                          selected = "Total"),
                        
                        hr(),
                        
                        selectInput("data_type",
                                    "Select Category of Data to Plot",
                                    choices = c("Electricity/Latrine Access",
                                                "Water Source", 
                                                "Water Availability",
                                                "Household Count"),
                                    selected = "Electricity/Latrine Access"),
                        
                        selectInput("plot_var",
                                    "Variable to Plot",
                                    choices = el_options),
                        
                        hr(),

                        conditionalPanel(
                            condition = "input.data_type == 'Electricity/Latrine Access' | 
                            input.data_type == 'Household Count'",
                            selectInput("ws", tags$em("Filtered by Water Source"),
                                        choices = levels(district11$water_source))
                        ),
                        
                        conditionalPanel(
                            condition = "input.data_type != 'Water Availability'",
                            selectInput("wa", tags$em("Filtered by Water Availability"),
                                        choices = levels(district11$water_avail))
                        )
                    )
                ), 
                absolutePanel(
                      id = "hist_panel", class = "panel panel-default",
                      fixed = TRUE, draggable = TRUE,
                      top = 40, left = "auto", right = 0,
                      bottom = "auto",
                      width = "27%", height = "auto", #430
    
                      h3("Use the gear icon to select map parameters"),
                      h4(tags$em("Click on a district for further details")),
                      uiOutput("clear_district", align = "center"),
                      uiOutput("district_result"),
                      htmlOutput("all_india_text"),
                      h5(strong("Given Parameters, Distribution of Districts with Respect to:")),
                      plotOutput("myhist", height = plot_height) %>% 
                          withSpinner(type = spinner_type, color = spinner_color),
                      h5(strong("Trend over Time for the Same Parameters:")),
                      plotOutput("line_plot", height = plot_height) %>% 
                          withSpinner(type = spinner_type, color = spinner_color)
                ),
                tags$div(id = "cite",
                         'Data compiled from Census Digital Library by Sean Angiolillo (2018).'
                )
            )
        ),
        tabPanel("About",
                 includeMarkdown("about.md"),
                 br()
        )
  ) 
)
