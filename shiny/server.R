library(leaflet)
library(shiny)
library(scales)
library(dplyr)
library(DT)

function(input, output, session) {
    session$onSessionEnded(stopApp)

    # draw base map
    output$map <- renderLeaflet({
        draw_base_map()
    })

    # draw circle as you hover inside polygon area
    observeEvent(input$map_shape_mouseover,{
        draw_circle("map", input$map_shape_mouseover$lng,
                    input$map_shape_mouseover$lat, input$buffer)
    })
    
    # need rv list because eg input$map_click$lng cannot be reset to NULL
    rv <- reactiveValues(proj_point = NULL,
                         data = tracts_geo)
    
    observeEvent(input$map_click, {
        
        # from map click, create a projected point
        rv$proj_point <- make_proj_point(input$map_click$lng, 
                                         input$map_click$lat)
        
        # perform spatial subset if it lies within outer shape
        if (is_within_boundary(rv$proj_point, tracts_proj_union)) {
            rv$data <- subset_data(circle_proj(), input$topo) %>%
                is_zero_geo_transform()
        } else {
            NULL
        }
        
        # print subsetting call only after a click
        output$sf_call <- renderUI({
            if (is.null(rv$proj_point)) {
                return()
            } else {
                verbatimTextOutput("subset_text")
            }
        })
    })
    
    output$subset_text <- renderText({
        get_subset_call(input$topo)
    })
    
    # reset projected point and dataset on button click
    observeEvent(input$reset, {
        rv$proj_point <- NULL
        rv$data <- tracts_geo
    })
    
    # changing topo should effectively do the same; otherwise sf_call can mismatch
    observeEvent(input$topo, {
        rv$proj_point <- NULL
        rv$data <- tracts_geo
    })

    # use map click point to calculate projected circle
    circle_proj <- reactive({
        if (!is.null(input$map_click)) {
            rv$proj_point %>% st_buffer(dist = input$buffer)
        } else {
            NULL
        }
    })

    # df follows right from sf
    df <- reactive({
        rv$data %>% st_set_geometry(NULL)
    })

    # histogram depends on df
    output$hist <- renderPlot({
        draw_histogram(df(), rv$proj_point)
    })
    
    # change map colors when data changes
    observeEvent(rv$data, {
        draw_map_colors("map", rv$data)
    })
    
    # same for legend
    observeEvent(df(), {
        draw_map_legend("map", df())
    })
    
    # output data table
    output$table <- DT::renderDataTable({
        DT::datatable(
            df(),
            rownames = FALSE,
            colnames = c("GEOID", "Census Tract", "County", 
                         "State", "Median Household Income", "Margin of Error"),
            caption = 'N.B.: 2012-16 Median HHI Data downloaded via tidycensus package.',
            options = list(
                columnDefs = list(
                    list(className = 'dt-center', targets = "_all"),
                    list(className = 'dt-head-center dt-center', targets = "_all")
                ),
                lengthMenu = c(10, 25, 50), 
                pageLength = 10
            ) 
        ) %>%
            formatCurrency(c('hhincome', 'moe'), 
                           digits = 0)
    })
    
    # only show reset map button when a click is present
    output$reset <- renderUI({
        if (!is.null(rv$proj_point)) {
            actionButton("reset", "Reset Map")
        }
    })
    
}