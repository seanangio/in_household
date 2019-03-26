library(shiny)
library(shinyWidgets)
library(shinythemes)
library(mapdeck)
library(markdown)

shinyUI(
    navbarPage(
        title = "Comparative Thematic Mapping",
        theme = shinytheme("simplex"),
        tabPanel("Map",
            div(class = "outer",
                tags$head(includeCSS("styles.css")),
                
                 mapdeckOutput(
                     outputId = "myMap", 
                     width = "100%", height = "100%"
                 ),
                 
                 absolutePanel(
                     id = "controls", class = "panel panel-default", fixed = TRUE,
                     draggable = TRUE, top = 55, left = "4%",
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
                         selectInput("category",
                                     "Plot Household Access To:",
                                     choices = category_opts,
                                     selected = "Electricity")
                     ) # closes dropdown button
                 ), # closes control panel
                
                 absolutePanel(
                     id = "hist_panel", class = "panel panel-default",
                     fixed = TRUE, draggable = TRUE,
                     top = 40, left = "auto", right = 0,
                     bottom = "auto",
                     width = "20%", height = "auto",

                     h3(HTML("Select an option from the menu below to visualize household Electricity and Latrine access in India")),
                     em(h4(HTML("Then use the gear icon to adjust map parameters"))),
                     selectInput("viz", "", 
                                 choices = viz_opts, 
                                 selected = "Choropleth"),
                     conditionalPanel(
                         condition = "input.viz == 'Dot Density'",
                         em(h5("* 1 dot represents 25,000 households"))
                     ),
                     em(h5("* Pinch to zoom; cmd/ctrl + click + drag to tilt/rotate"))
                 ), # closes hist panel
                tags$div(id = "cite",
                         'Data compiled from Census Digital Library by Sean Angiolillo (2019).'
                )
            )     
        ), # closes map tabPanel
        tabPanel("About",
             includeMarkdown("about.md"),
             br()
        ) # closes about tabPanel
    ) # closes navbar Page
) # closes shinyUI