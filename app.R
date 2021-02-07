library(readxl)
library(tidyverse)
library(dplyr)
library(here)
library(leaflet)
library(shiny)
library(shinydashboard)

bird_data_by_record_id_clean <-
  read_csv(here("data_clean/bird_data_by_record_id_clean.csv"))

ship_data_by_record_id_clean <-
  read_csv(here("data_clean/ship_data_by_record_id_clean.csv"))

bird_data_codes_clean <-
  read_csv(here("data_clean/bird_data_codes_clean.csv"))

ship_data_codes_clean <-
  read_csv(here("data_clean/ship_data_codes_clean.csv"))



# Join observation & species data

species_and_observations  <-
  bird_data_by_record_id_clean %>%
  left_join(ship_data_by_record_id_clean, by = "record_id")  %>%
  select(record_id,
         species_common_name,
         species_scientific_name,
         species_abbreviation,
         count,
         age,
         sact,
         date,
         lat,
         long,
         seasn) %>%
  mutate(year = as.numeric(substring(date,1,4))) %>% 
  mutate(
    ship_activity = case_when(
      is.na(sact) ~ "Not recorded",
      sact == 1 ~ "steaming, sailing",
      sact == 2 ~ "dropping trash",
      sact == 3 ~ "trawling",
      sact == 4 ~ "oceanography",
      sact == 5 ~ "potting",
      sact == 6 ~ "line fishing",
      sact == 7 ~ "cleaning fish",
      sact == 8 ~ "stationary",
      sact == 9 ~ "flying helicopters",
      sact == 10 ~ "whaling",
      TRUE       ~ "error - please check record"
    )
  ) %>% 
  filter(!is.na(year))

# species list for dropdown
species_list  <- unique(sort(species_and_observations$species_common_name))

# year list for dropdown
year_list  <- unique(sort(species_and_observations$year))

ui <- dashboardPage(
  dashboardHeader(title = "Seabird observations"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "Map", icon = icon("map")),
      menuItem("Graph", tabName = "Graph", icon = icon("bar-chart")),
      selectInput("species_select", "Select species", choices = species_list),
      sliderInput("year_select",
                  tags$h5("Select date range"),
                  min = min(species_and_observations$year),
                  max = max(species_and_observations$year),
                  value = c(min(species_and_observations$year), max(species_and_observations$year)),
                  sep ="",
                  width = '95%'
                  
      )
        
      
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Map",

              
              fluidRow(
                leafletOutput("species_map")
              ) # end fluid row 2
              
      ),
      
      # Second tab content
      tabItem(tabName = "Graph",
              
              
              fluidRow(
                plotOutput("species_plot")
              ) # end fluid row 2
              
      )
    )
  )
)

server <- function(input, output) { 
 
   
  years_from_slider <- reactive({
    seq(input$year_select[1], input$year_select[2], by = 1)
  })
  
  # Create map inputs
  
  output$species_map <- renderLeaflet({
  observation_locations <- species_and_observations %>% 
    filter(species_common_name == input$species_select) %>%
    filter(year %in% years_from_slider()) %>% 
    group_by(species_common_name, lat, long) %>% 
    summarise(number_of_individuals = sum(count)) %>% 
    arrange(desc(number_of_individuals))
  
  
  # Create colour scheme for markers based on number of individuals
  observation_locations <- observation_locations %>% 
    mutate(observations_group = cut(number_of_individuals, breaks = c(0, 10, 100, 500, 1000, Inf),
                                    labels = c("orange", "light yellow","darkred", "red", "purple"),
                                    include.lowest = TRUE)) 
  
  icons <- awesomeIcons(icon = "whatever",
                        iconColor = "black",
                        library = "ion",
                        markerColor = observation_locations$observations_group)
  
  
  # Data for leaflet
  observation_data_map <- observation_locations %>% 
    leaflet() %>% 
    addTiles() %>% 
    addAwesomeMarkers(lng = ~ observation_locations$long, 
                      lat = ~ observation_locations$lat,
                      icon = icons,
                      popup = paste("Count:",as.character(observation_locations$number_of_individuals))
                      
                      
    )
  })
  # data for graph
  output$species_plot <- renderPlot({
    species_and_observations %>% 
      filter(species_common_name == input$species_select) %>%
      filter(year %in% years_from_slider()) %>% 
      group_by(year) %>% 
      summarise(number_of_individuals = sum(count)) %>% 
      ggplot() +
      aes(x = year, y = number_of_individuals) +
      geom_col(fill = "steel blue") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = min(species_and_observations$year):max(species_and_observations$year)) +
      theme_minimal() +
      labs(
        title =  "Number of individuals observed by year \n",
        x = "\n Year",
        y = "Number of individuals \n"
      ) 
  })
  
  }

shinyApp(ui, server)


