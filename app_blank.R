###################################
# BLANK APP                       #
###################################

#### PACKAGE GROUPS #### 

# shiny app support packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(shinyjs)
library(shinyalert)
library(shinycssloaders)
library(htmltools) # check this one 


# data processing (spatial and nonspatial)
library(sf)
library(aws.s3)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(lubridate)

# leaflet packages
library(leaflet)
library(leaflet.extras)

# figure and table creation
library(ggplot2)
library(plotly)
library(gt)

#-------------------------------------------------------------------------------

### GLOBAL ENVIRONMENT DATA LOADING AND PREPROCESSING PLACE HERE ###

### REACTIVE DATES ###
update_date <- lubridate::today(tzone = 'America/New_York') %>% 
  format('%b %d, %Y')
last_sample_date <- today() %>% 
  format('%b %d, %Y')

# Generate random data for the plot examples
set.seed(25)

county <- c("county 1", "county 2", "county 3")
county <- sample(county, 26, replace = TRUE)
site_id <- paste(
  letters[seq( from = 1, to = 26 )],
  seq(from = 1, to = 26, by = 1),
  sep = ""
)
state <- c("NY")
sample_collect_date <- seq.Date(from = (today()-days(25)),
                                to = today(),
                                by = 1
)
path_1 <- rlnorm(
  n = 26, meanlog = 0, sdlog = 1
)

path_2 <- rlnorm(
  n = 26, meanlog = 0, sdlog = 2
)

# bind the pcr data together
data <- as.data.frame(cbind(
  county,
  site_id, 
  state,
  sample_collect_date,
  path_1,
  path_2
)) %>%
  mutate(
    sample_collect_date = as.Date(as.numeric(sample_collect_date),
                                  origin = "1970-01-01"),
    path_1 = as.numeric(path_1),
    path_2 = as.numeric(path_2)
  ) %>%
  pivot_longer(
    cols = c(path_1, path_2)
  ) %>%
  rename(concentration = value)
  
# add the case data
path_1 <- sample.int(200, size = 26, replace = TRUE)
path_2 <- sample.int(100, size = 26, replace = TRUE)
negatives <- sample.int(300, size = 26, replace = TRUE)

# make it a dataframe
c_data <- as.data.frame(cbind(
  sample_collect_date,
  path_1,
  path_2,
  negatives
))  %>%
  mutate(
    sample_collect_date = as.Date(as.numeric(sample_collect_date),
                                  origin = "1970-01-01"),
    path_1 = as.numeric(path_1),
    path_2 = as.numeric(path_2),
    negatives = as.numeric(negatives)
  ) %>%
  pivot_longer(
    cols = c(path_1, path_2)
  ) %>%
  rename(cases = value) %>%
  mutate(tests = negatives + cases) %>%
  mutate(positivity = (cases/tests) * 100)

# join to the main data object
data <- left_join(data, c_data, by = c("sample_collect_date",
                                       "name")) %>%
  rename(pathogen = name)

# spatial data for the leaflet map
ny_sf <- tigris::states(cb = FALSE) %>%
  filter(NAME == "New York")

# transform data
ny_sf <- ny_sf %>%
  sf::st_transform(ny_sf,
                   crs = c("+proj=longlat +datum=WGS84"))

# create fields to toggle in the map

# trend - pathogen 1
ny_sf1 <- ny_sf
ny_sf1$Trend_factor <- "Increasing"


# risk level - pathogen 1
ny_sf1$Alert_factor <- "Moderate"


ny_sf1$pathogen <- "path_1"

# trend - pathogen 2
ny_sf2 <- ny_sf
ny_sf2$pathogen <- "path_2"
ny_sf2$Trend_factor <- "Decreasing"

# risk level - pathogen 2
ny_sf2$Alert_factor <- "High"

ny_sf_final <- bind_rows(ny_sf1, ny_sf2) %>%
  add_row(
    Alert_factor = "Low",
    Trend_factor = "Stable"
  )

# make the fields factor variables
ny_sf_final$Trend_factor <- factor(
  ny_sf_final$Trend_factor,
  levels = c("Decreasing",
             "Stable",
             "Increasing")
)
ny_sf_final$Alert_factor <- factor(
  ny_sf_final$Alert_factor,
  levels = c("Low",
             "Moderate",
             "High")
)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#############  APP STARTS HERE #######################

# Define UI 
ui <-  
  function(req){
    
    dashboardPage(
      
      dashboardHeader(
        title = "Title", # title in the box at top of the page
        titleWidth = 450) # this titlewidth is linked to the sidebar
      , 
      
      # SIDEBAR CONTENT #
      dashboardSidebar( 
        width = 225,
        
        # SIDEBAR MENU #
        sidebarMenu(
          id = "sidebarid",
          # STYLE ARGUMENT USES CSS CODE
          # keeps the sidebar content from scrolling
          style = "position:fixed; width:auto; overflow-x: clip;", 
                                    
          # MENU ITEMS THAT ARE TABS ON THE SIDEBAR
          menuItem("State Dashboard", 
                   tabName = "dashboard", 
                   icon = icon("dashboard")), # tabName is called below
          
          menuItem("Resources and toolkit", 
                   tabName = "resources", 
                   icon = icon("file-alt")),
          
          menuItem("Information", 
                   icon = icon("info"), 
                   tabName = "Information"),
          
          ### CONDITIONAL PANEL FOR SIDEBAR BUTTONS ON MAIN DASHBAORD PAGE ###
          # we add this here so  that the buttons only show up on the dashboard 
          # page (see condition = below)
          conditionalPanel(
            condition = 'input.sidebarid == "dashboard"',
            
            # Dropdown for changing between pathogens
            # Could also be buttons
            selectInput(
              width = 200,
              inputId = "pathogen_toggle", 
              label = "Pathogen selection",
              choices = 
                c("Pathogen 1" = "path_1", 
                  "Pathogen 2" = "path_2"),
              selected = "Pathogen 1"
            ), # end select input
            
            # Dropdown for pathogens tooltip
            bsTooltip("pathogen_toggle", 
                      "Select disease/target of interest", 
            ),
            
            # RADIO BUTTONS FOR FIRST PLOT SHOWING TRENDS
            radioButtons("ww_trend", # id for radio button series
                         "Wastewater trend", # title of button series
                         c( # plot 1 is  the intensity, log refers to the ID of 
                           # the object
                           "pathogen intensity" = "intensity",
                           #plot 2 is the gene copies, gene is the ID of
                           # the object to display
                           "Gene copies" = "raw gene copies" 
                           
                         ), 
                         width = 225),
                                      
                        # TOOLTIP FOR TREND PLOTS
                        bsTooltip("ww_trend", 
                                  "Switch between intensity and gene copies ", 
                                  ),
                        
                        # RADIO BUTTONS FOR SECOND PLOT SHOWING CASE DATA
                        radioButtons("case_switch", # id for the radio buttons
                                     "Cases data", # title of button series
                                     c("New cases" = "new_cases", 
                                       "Active cases" = "active_cases",
                                       "Test positivity" = "positivity"),
                                     width = 225),
                        
                        # TOOLTIP FOR CASE PLOT
                        bsTooltip(
                          "case_switch", #"county_case_plotly", 
                          paste("Switch the case plots between new cases,",
                          " active cases, and positivity."),
                          "bottom", 
                          )
                      ) # close conditional panel 
              ) # close sidebar menu
                        
                        
      ), # close dashboard sidebar argument
      
      dashboardBody(
        useShinyjs(), #for shinyjs code to work

        # CSS style arguments (e.g., font size)
        # increase size of acutal map display based on window
        tags$style(
          type = "text/css", "#NYBetaMap {height: calc(100vh - 80px) !important;}"
                   ), # NYBetaMap is the ID of the object to modify
        tags$head(tags$style('.selectize-dropdown {z-index: 10000}')
                  ), # this makes the state_map_toggle overlay the leaflet
        
        tabItems(
          
          tabItem(
            tabName = "dashboard",
            
            # web issues box
            # update list
            fluidRow(
              box(width = 12,
                 title = "Report web issues here", background = "red",
                "This is an example box to post updates."

            ) # end update box

                ), # end fluid row update box
            
            # update list
             fluidRow(
               box(width = 12,
                   title = "Update box", background = "green",
                   p("This dashboard is meant for educational purposes only and 
                     does not represent any actual data.")
                   
            
               ) # end update box

            ), # end fluid row update box
            fluidRow(
              column(12, 
                     h2("Intro title"),
                     p("Optional introductory text"),
                     br(), # line break between paragraphs
              )# end column argument
            ), # end fluid row opening paragraph
            
            #  VALUE BOXES WITH STATE SUMMARIES  #
            fluidRow(
              column(
                width = 12, 
                h2(HTML(paste("Statewide participation summary", 
                              "<sup>", 
                              "1", 
                              "</sup>"))))
              ), # close fluid row
            
            fluidRow(
              # number of counties actively reporting
              valueBox( value = 1000,
                        subtitle = "Participating counties",
                        color = "light-blue",
                        icon = icon("map")
              ),
              # number of  treatment plants reporting
              valueBox( value = 100,
                        #subtitle = "Participating WWTPs",
                        subtitle = "Participating WWTPs",
                        color = "aqua",
                        icon = icon("water")
              ),
              # population covered
              valueBox( value = formatC(14500000, format="d", 
                                        big.mark=","),
                        subtitle = "Population covered",
                        color = "blue",
                        icon = icon("users"))
            ), # end fluid row summary boxes
            
            fluidRow(
              column(
                width = 12, 
                p(HTML(paste("<sup>", "1", "</sup>", 
                             "Participation as of ", 
                             lubridate::today(tzone = 'America/New_York') 
                             %>% format('%b %d, %Y')))))
              ), # close fluid row
            
            # UPDATE INFORMATION ROW
            fluidRow(
              column(
                12,
                titlePanel(h3(paste("Last Updated: ", 
                                    update_date, sep = ""))),
                titlePanel(h3(paste("Most recent sample: ", 
                                    last_sample_date, sep = "")))
            )
            
            ), # end fluid row
            
            br(),
            
            # MAIN DASHBOARD MAP (ID = NYBetaMap)
            fluidRow(
              box(width = 12, id = "map_container",
                 title = "Map of participating treatment plants and counties",
                 column(
                  width = 8,
                  # withspinner adds loading icon
                  withSpinner(
                    leafletOutput('NYBetaMap'), 
                    type = 8), # type of loading icon to show
                  fluidRow(# create action button to show or hide description
                    actionButton(inputId = "button_map", 
                                 label = "Map description show / hide", 
                                 width = 250)
                  ) # close fluid row
                 ), # close column
                         
                 # RIGHT OF MAP SIDEBAR #
                 column(width = 4, 
                        # MAIN MAP TOGGLE SELECTIONS AND BUTTONS
                        fluidRow(
                          selectInput(
                             inputId = "state_map_toggle", 
                             label = "Wastewater Metric",
                             choices = 
                               c("pathogen detection level" = "Alert_factor", 
                                         "Two week trend" = "Trend_factor"),
                             selected = "pathogen detection level"
                        )# end select input
                        
                        ), # close fluid row
                        
                        # CATEGORY CHANGE TABLES
                        fluidRow(
                          wellPanel(
                           id = "category_tables",
                           conditionalPanel(
                             condition = "input.state_map_toggle == 'Alert_factor'",
                             gt_output("alert_table")
                           ), # close conditional panel
                           conditionalPanel(
                             condition = "input.state_map_toggle == 'Trend_factor'",
                             gt_output("trend_table")
                                   ) # close conditional panel
                        ) # close well panel
                        ), # close fluid row
                        
                        # CATEGORY CHANGE DESCRIPTION 
                        fluidRow(wellPanel(
                          conditionalPanel(
                            condition = "input.state_map_toggle == 'Alert_factor'",
                            box(width = 12, title = "Pathogen detection level",
                                p("Paste text here"))
                          ), # close conditional panel
                          conditionalPanel(
                            condition = "input.state_map_toggle == 'Trend_factor'",
                            box(width = 12, title = "Two-week trend", 
                                p("paste text here")
                                           ) # close box
                          )# close conditional panel
                        )# close well panel
                        ) # close fluid row
                 ) # close column
                         
            ), #close map box
            
            ), # end map fluidrow
            
            # new tooltip to try
            # NYBetaMap TOOLTIP #
            bsTooltip("NYBetaMap", 
                      paste("Zoom in or click a county to see sewersheds.",
                      " Click a marker or catchment to see wastewater trends ",
                      "(displayed below)."),
                      trigger = "hover",
                      "bottom", ),
            
            # map container second TOOLTIP #
            bsTooltip("map_container", 
                      paste("Zoom in or click a county to see sewersheds.",
                      " Click a marker or catchment to see wastewater trends ",
                      "(displayed below)."),
                      trigger = "hover",
                      "top", ),
            
            br(),
            
            # NYBetaMap DESCRIPTION (observe event is in the server)
            hidden(div(id = "maptext_wrapper",
                       box(id = "mapText", title = "Map description", width = 12,
                           h4("Header"),
                           # disclaimer on state of the science
                           p("optional description text")
                       ) # end box
            )# end div
            ), # end hidden,
            
            # WASTEWATER TREND PLOTS 
            # created in a well panel and conditional panel linked to the radio
            # buttons above
            br(),
            fluidRow(
              box(width = 12, title = "Wastewater trend",
                  wellPanel(id = "sewershed_plots", 
                            conditionalPanel(
                              condition = "input.ww_trend == 'intensity'",
                              withSpinner(
                                plotlyOutput("sewershed_plotly_default_log"), 
                                type = 8) # add loading icon
                            ),
                            conditionalPanel(
                              condition = "input.ww_trend == 'raw gene copies'",
                              withSpinner(plotlyOutput("sewershed_plotly_default"),
                                          type = 8) # add loading icon
                            )
                  ),# end well panel
                  column(12, actionButton(
                    inputId = "button_trendText", 
                    label = "Trend graph description show / hide", 
                    width = 250)
                  )# end  column
              )# end box
            ), # end fluidrow
            
            bsTooltip("sewershed_plots", #"sewershed_plotly_default", 
                      paste("This plot shows the trend values for pathogen ",
                      "detected in wastewater at the selected treatment plant."),
                      "bottom", ),
            
            # sewershed trend plot description 
            
            hidden(
              div(id = "trendPlotText_wrapper",
                  box(id = "trendPlotText", 
                      title = "Wastewater trend description", 
                      width = 12,
                      h4("Header"),
                      p("optional text")
                  ) # end box
              )# end div
            ), # end hidden
            br(),
            #fluidRow(
            # box(width = 12,
            # plotlyOutput("county_case_plotly"))
            #),
            fluidRow(
              box(width = 12, title = "Case data",
                  wellPanel(id = "case_plots", conditionalPanel(
                    condition = "input.case_switch == 'new_cases'",
                    withSpinner(plotlyOutput("county_case_plotly"), 
                                type = 8) # add loading icon
                  ),
                  conditionalPanel(
                    condition = "input.case_switch == 'active_cases'",
                    withSpinner(plotlyOutput("county_active_plotly"), 
                                type = 8) # add loading icon
                  ),
                  conditionalPanel(
                    condition = "input.case_switch == 'positivity'",
                    withSpinner(plotlyOutput("county_positivity_plotly"), 
                                type = 8) # add loading icon
                  )
                  ), 
                  column(12, actionButton(
                    inputId = "caseText_button", 
                    label = "Case plot description show / hide", 
                    width = 250))
              )# end box
            ), # end fluid row
            
            # case plot tooltip
            bsTooltip(
              "case_plots", #"county_case_plotly", 
              "This plot shows the total positive test results for the county.",
              "bottom", ),
            
            # case plot description popup
            
            hidden(
              div(id = "caseText_wrapper",
                  box(id = "caseText", title = "Case plot description", 
                      width = 12,
                      h4("Header"),
                      p("optional text")
                  ) # end box
              )# end div
            )# end hidden
            
            
            ###############
            
            
          ),
          tabItem(tabName = "resources",
                  fluidRow(
                    column(12
                    ) # close column
                  ) # close fluid row
                  
          ),
          
          tabItem(tabName = "Information",
                  fluidRow(
                    column(12
                    ) # end column
                  )# end fluid row
          ) # end tab item
          
        )# end tab items plural
        
      ) # end dashboard body
      
    )# end dashboardPage
    
  } # close function for ui

###################### SERVER ############################

server <- function(input, output, session) {
  
  ## helper command to keep app open longer ##
  output$clock = renderText({
    invalidateLater(4500)
    Sys.time()
  })
  
  # Color palettes for leaflet map
  # create color level and value vectors for each factor variable
  trend_colors <- c("dodgerblue3", "papayawhip", "orangered")
  trend_levels <- c("Decreasing", "Stable", "Increasing")
  alert_colors <- c("steelblue4", "#b4cbf0", "#fac898")
  alert_levels <- c("Low", "Moderate", "High")
  
  # Map legends
  output$alert_table <- render_gt(
    as.data.frame(rbind(
                       "Low",
                       "Moderate",
                       "High")) %>%
      rename("Detection level" = V1) %>%
      gt() %>%
      data_color(
        columns = "Detection level",
        fn = scales::col_factor(alert_colors,
                                    levels = alert_levels, ordered = TRUE)
      )
  )
  output$trend_table <- render_gt(
    as.data.frame(rbind(
                       "Decreasing",
                       "Stable",
                       "Increasing"))%>%
      rename("Trend" = V1) %>%
      gt() %>%
      data_color(
        columns = "Trend",
        fn = scales::col_factor(trend_colors,
                                    levels = trend_levels,
                                    ordered = TRUE)
      )
  )
  
  ###### LEAFLET MAP #######
  
  # create map for renders
  nybetamap_preset <- leaflet() %>% 
    # map panes (order of layers to appear). Higher zIndex = higher layer
    addMapPane("nymap", zIndex = 420) %>%
    addMapPane("tiles", zIndex = 400) %>%
    
    # basemap tiles
    addProviderTiles(providers$CartoDB.Voyager,group = "Base map",
                     options = pathOptions(pane = "tiles")) %>%
    addPolygons(data = ny_sf,
                smoothFactor = 0.2, #fillOpacity = 0.7,
                fillColor = "white",
                stroke = TRUE,
                color = "black",
                weight = 3,
                options = pathOptions(pane = "nymap",
                                      clickable = FALSE) # cannot click counties
    )
  
  ##### PRESET STATE MAP FOR STATE MAP VIEW BUTTON #####
  # render output
  output$NYBetaMap <- renderLeaflet({
    nybetamap_preset
    
  })  
  
  # button press to display description of map
  observeEvent(input$button_map, {
    toggle("maptext_wrapper")
    
  })
  
  # spatial data is reactive according to what pathogen is selected
  ny_pathogen_filtered <<- reactive({
    if(input$pathogen_toggle == "path_1"){
      ny_pathogen_filtered <- ny_sf_final %>%
        filter(pathogen == "path_1")
    } else if(input$pathogen_toggle == "path_2"){
      ny_pathogen_filtered <- ny_sf_final %>%
        filter(pathogen == "path_2")
    }
    return( ny_pathogen_filtered)
  })
  
  # Leaflet Proxy map for changing color according to trend or alert level
  observe({
    
    # reactive color palettes
    
    ##### CHANGE COLOR FACTOR SETS BASED ON USER INPUT FOR WHAT PATTERN TO DISPLAY ####
    # ALERT LEVEL PATHOGEN 1
    if (input$pathogen_toggle == "path_1" & input$state_map_toggle == "Alert_factor") {
      wastewater_pal <- colorFactor(alert_colors,
                                    levels = alert_levels, ordered = TRUE)
      # TREND LEVEL PATHOGEN 1
    } else if (input$pathogen_toggle == "path_1" & input$state_map_toggle == "Trend_factor") {
      wastewater_pal <- colorFactor(trend_colors,
                                    levels = trend_levels,
                                    ordered = TRUE)
      # ALERT LEVEL PATHOGEN 2
    } else if (input$pathogen_toggle == "path_2" & input$state_map_toggle == "Alert_factor") {
      wastewater_pal <- colorFactor(trend_colors,
                                    levels = trend_levels,
                                    ordered = TRUE)
      # TREND LEVEL PATHOGEN 2
    } else if (input$pathogen_toggle == "path_2"   & input$state_map_toggle == "Trend_factor") {
      wastewater_pal <- colorFactor(trend_colors,
                                    levels = trend_levels,
                                    ordered = TRUE)
    } 
    
    proxy_map <- leafletProxy("NYBetaMap") %>%
    # map panes (order of layers to appear). Higher zIndex = higher layer
    addMapPane("nymap", zIndex = 420) %>%
    addMapPane("tiles", zIndex = 400) %>%
    
    # basemap tiles
    addProviderTiles(providers$CartoDB.Voyager,group = "Base map",
                     options = pathOptions(pane = "tiles")) %>%
    addPolygons(data = ny_pathogen_filtered(),
                smoothFactor = 0.2, #
                fillOpacity = 0.7,
                # change fill color according to selection
                fillColor = 
                  ~wastewater_pal(eval(as.symbol(input$state_map_toggle))),
                stroke = TRUE,
                color = "black",
                weight = 3,
                options = pathOptions(pane = "nymap",
                                      clickable = FALSE) # cannot click counties
    )
  })
  
  # ---------------------------------------------------------------------------
  
  # Plots for trends and case data
  
  # Create reactive data for input into the line and case data graphs
  plot_data <<- reactive({
    plot_data <- data %>%
      filter(pathogen == input$pathogen_toggle)})

  # gene copies plot
  output$sewershed_plotly_default <- renderPlotly(ggplotly(
    ggplot(plot_data(),
           aes(x = sample_collect_date,
               y = concentration)
           )+
      geom_point()+
      geom_line()+
      labs(
        x = "",
        y = "Concentration",
        title = paste(head(plot_data()$pathogen, 1))
      )+
      theme(plot.title = element_text(hjust = 0.5))
    
    )# close ggplotly
    )# close renderplotly


  
  # sample ggplot for the second plot behind the wellpanel
  # In our example, this displays the log transformed data with smoothed trend
  output$sewershed_plotly_default_log <- renderPlotly(ggplotly(
    ggplot(plot_data(),
           aes(x = sample_collect_date,
               y = log1p(concentration))
    )+
      geom_point()+
      geom_line()+
      labs(
        x = "",
        y = "log(Concentration)",
        title = paste(head(plot_data()$pathogen, 1),
                       "Log transformed",
                      sep = " ")
      )+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_smooth(method = "loess",
                  span = 0.25)
    
  )# close ggplotly
  )# close renderplotly
  
  # button press to display description of plot
  observeEvent(input$button_trendText, {
    toggle("trendPlotText_wrapper")
    
  })
  
  # barplots for case or other count data
  output$county_case_plotly <- renderPlotly(
    ggplotly(
      ggplot(plot_data(),
             aes(x = sample_collect_date,
                 y = cases)
      )+
        geom_bar(position = "dodge",
                 stat = "identity")+
        labs(
          x = "",
          y = "Cases",
          title = paste(head(plot_data()$pathogen, 1),
                        "Case Counts",
                        sep = " ")
        )+
        theme(plot.title = element_text(hjust = 0.5))
    )
  )

  # second case plots. this could be active cases based on a serial interval of
  # infection or something similar. Here we are simply log transforming them
  output$county_active_plotly <- renderPlotly(
    ggplotly(
      ggplot(plot_data(),
             aes(x = sample_collect_date,
                 y = log(cases))
      )+
        geom_bar(position = "dodge",
                 stat = "identity")+
        labs(
          x = "",
          y = "Cases",
          title = paste(head(plot_data()$pathogen, 1),
                        "Case Counts - log transformed",
                        sep = " ")
        )+
        theme(plot.title = element_text(hjust = 0.5))
    )
  )
  

  # Test positivity plot (total positive cases / total tests)
  output$county_positivity_plotly <- renderPlotly(
    ggplotly(
      ggplot(plot_data(),
             aes(x = sample_collect_date,
                 y = positivity)
      )+
        geom_bar(position = "dodge",
                 stat = "identity")+
        labs(
          x = "",
          y = "Test positivity (percent)",
          title = paste(head(plot_data()$pathogen, 1),
                        "Test positivity",
                        sep = " ")
        )+
        theme(plot.title = element_text(hjust = 0.5))
    )
  )
  
  
  # button press to display description of case plot
  observeEvent(input$caseText_button, {
    toggle("caseText_wrapper")
    
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)