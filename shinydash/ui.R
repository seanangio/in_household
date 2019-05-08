library(shinyWidgets)
library(shinydashboardPlus)
library(DT)
library(markdown)

dashboardPagePlus(
    
    header <- dashboardHeaderPlus(
        
        title = "Indian Census Viz",
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "gears",
        dropdownMenu(
            type = "notifications", badgeStatus = "info",
            notificationItem(
                icon = icon("filter"), status = "info",
                text = tags$b("Access data filters via the gear",
                              tags$br(), "icon at the top right")
            ),
            notificationItem(
                icon = icon("search"), status = "info",
                text = tags$b("Scroll over plots for zoom options")
            ),
            notificationItem(
                icon = icon("database"), status = "info",
                text = tags$b("Data compiled from Census Digital",
                              tags$br(), "Library by Sean Angiolillo (2018)")
            ),
            notificationItem(
                icon = icon("github"), status = "info",
                text = tags$b("Find the code on Github"),
                href = "https://github.com/seanangio/in_household/tree/master/shinydash"
            )
        )
    ),
    
    sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem("Scatter Plot", 
                     tabName = "scatter", icon = icon("braille")),
            menuItem("Dumbbell Plot", 
                     tabName = "dumbbell", icon = icon("chart-bar")),
            menuItem("Bubble Map",
                     tabName = "map", icon = icon("map")),
            menuItem("Standouts",
                     tabName = "standouts", icon = icon("trophy")),
            menuItem("Table", 
                     tabName = "table", icon = icon("table")),
            menuItem("About", 
                     tabName = "about", icon = icon("readme"))
        )
    ),
    
    body <- dashboardBody(
        tabItems(
            tabItem("scatter",
                fluidRow(
                    column(width = 12,
                       box(width = NULL, solidHeader = TRUE, 
                           status = "info",
                           ggiraphOutput("scatter_plot", height = "800px", width = "850px")
                       )   
                    ) 
                ) 
            ), 
            tabItem("dumbbell",
                fluidRow(
                    column(width = 12,
                       box(width = NULL, solidHeader = TRUE, 
                           status = "info",
                           ggiraphOutput("dumbbell_plot", height = "700px", width = "850px")
                       ) 
                    ) 
                ) 
            ),
            tabItem("map",
                div(
                    tags$head(includeCSS("styles.css")),
                fluidRow(
                    column(width = 12,
                       box(width = NULL, solidHeader = TRUE, 
                           status = "info",
                           ggiraphOutput("map", height = "850px", width = "850px"),
                           absolutePanel(
                               id = "controls", class = "panel panel-default", 
                               fixed = FALSE, draggable = FALSE, 
                               top = "auto", left = "1%", 
                               right = "auto", bottom = "30%",
                               width = "20%", height = 50,
                               plotOutput("map_legend", height = "150px")
                           )
                       )
                    ) 
                ) 
                )
            ), 
            tabItem("standouts",
                fluidRow(
                    column(2,
                       box(width = NULL, solidHeader = TRUE, 
                           status = "info", 
                           h4(strong(textOutput("selected_filter")))
                       )
                    ),
                    column(10,
                       fluidRow(
                           column(width = 5, 
                              box(width = NULL, solidHeader = TRUE, 
                                  status = "info", h4("Electricity"))
                            ),
                           column(width = 5, 
                              box(width = NULL, solidHeader = TRUE, 
                                  status = "info", h4("Latrine"))
                            )
                       )
                    )
                ),
                fluidRow(
                    column(2, 
                       box(width = NULL, solidHeader = TRUE, 
                           status = "info", h4("All-India")
                        )
                    ),
                    column(10, 
                       fluidRow(
                          column(width = 5, valueBoxOutput("ai_ea", width = NULL)),
                          column(width = 5, valueBoxOutput("ai_la", width = NULL))
                      )
                    )
                ),
                fluidRow(
                    column(2, 
                       box(width = NULL, solidHeader = TRUE, 
                           status = "info", h4("Best Performer")
                        )
                    ),
                    column(10, 
                       fluidRow(
                           column(width = 5, valueBoxOutput("best_ea", width = NULL)),
                           column(width = 5, valueBoxOutput("best_la", width = NULL))
                       )
                    )
                ),
                fluidRow(
                    column(2, 
                       box(width = NULL, solidHeader = TRUE, 
                           status = "info", h4("Worst Performer")
                        )
                    ),
                    column(10,
                       fluidRow(
                           column(width = 5, valueBoxOutput("worst_ea", width = NULL)),
                           column(width = 5, valueBoxOutput("worst_la", width = NULL))
                       )
                    )
                )
            ),
            tabItem("table",
                fluidRow(
                    column(7,
                       box(width = NULL, solidHeader = TRUE, status = "info",
                           h4(textOutput("selection"))
                       ) 
                    )
                ),
                fluidRow(
                    DTOutput("tbl")
                )    
            ),
            tabItem("about",
                includeMarkdown("about.md"),
                br()
            )
        ) 
    ),
    
    rightsidebar = rightSidebar(
        background = "dark",
        width = 250,
        rightSidebarTabContent(
            id = 1,
            title = "Basic Filters",
            icon = "filter",
            active = TRUE,
            radioGroupButtons(
                "geo", "Geographic Level",
                choices = c("State", "District"),
                selected = "State"
            ),
            radioGroupButtons(
                "year", "Year",
                choices = c("1991","2001","2011"),
                selected = "2011"
            ),
            radioGroupButtons(
                "society", "Societal Cross Section",
                choices = c("ALL", "SC", "ST"),
                selected = "ALL"
            ),
            radioGroupButtons(
                "demo", "Demographic Cross Section", 
                choices = c("Total", "Urban", "Rural"),
                selected = "Total"
            )
        ),
        rightSidebarTabContent(
            id = 2,
            title = "Geographic Filters",
            icon = "globe",
            uiOutput("choose_zones"),
            uiOutput("choose_states")
        )
    )
)