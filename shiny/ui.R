library(leaflet)
library(markdown)
library(shinyWidgets)
library(DT)

navbarPage("Spatial Subsetting in R", id = "nav",
           
           tabPanel("Map",
                    div(class = "outer",
                        
                        tags$head(includeCSS("styles.css")),
                        
                        leafletOutput("map", width = "100%", height = "100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", 
                                      fixed = TRUE, draggable = FALSE, top = 50, 
                                      left = "auto", right = 0, bottom = "auto",
                                      width = 430, height = "auto",
                                      
                                      em(h4("Choose a buffer radius and topological relation;
                                            \n then click to perform the selected spatial subset")),

                                      fluidRow(
                                          column(6, sliderInput("buffer", "Buffer Radius (ft)",
                                                                min = 0, max = 50000,
                                                                value = 15000, step = 1000, 
                                                                ticks = FALSE)),
                                          column(6, selectInput("topo", "Topological Relation", 
                                                                topo_relations, selected = "Intersects"))
                                      ),
                                      uiOutput("sf_call"),
                                      fluidRow(column(
                                          width = 6, align = "center", offset = 3,
                                          uiOutput("reset")
                                      )),
                                      hr(),
                                          
                                      plotOutput("hist", height = plot_height)
                        ),
                        
                        tags$div(id = "cite",
                                 'Data downloaded via tidycensus by Sean Angiolillo (2019).'
                        )
                    )
           ),
           
           tabPanel("Table",
                    DT::dataTableOutput("table")
           ),
           
           tabPanel("About",
                    includeMarkdown("about.md"),
                    br())
)