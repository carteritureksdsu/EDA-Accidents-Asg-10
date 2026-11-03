################################################################
# Shiny app by Carter Turek
# Nov 2, 2026
#
# Description
# Deployed at
# Source code at GitHub: 
################################################################

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(usmap)


#Load cleaned data
data24 <- readRDS("accidents_24.rds")
data23 <- readRDS("accidents_23.rds")


ui <- page_sidebar(
  title = "Analyzing Fatal Accidents in the US",
#Choose Year  
  sidebar = sidebar(
    selectInput(
      inputId = "year_choice",
      label = "Year:",
      choices = c("2024", "2023"),
      selected = "2024"
    ),
    
#Choose Metric    
    radioButtons(
      inputId = "pct_choice",
      label = "Show map as:",
      choices = c("Percentage" = "pct", "Rate per 100,000 people" = "rate"),
      selected = "pct"
    ),

#Choose State    
    selectInput(
      inputId = "state_choice",
      label = "State:",
      choices = c("All States", sort(unique(data24$STATENAME))),
      selected = "All States"
    ),
    
    
#Choose Time of Day
    selectInput(
      inputId = "time_choice",
      label = "Time of Day:",
      choices = c("All Day", "Morning", "Afternoon", "Night"),
      selected = "All Day"
    ),

#Choose Crash Type
    radioButtons(
      inputId = "crash_choice",
      label = "Type of Crash:",
      choices = c("All Crashes" = "all", "Impaired crashes" = "impaired", "Distracted crashes" = "distracted"),
      selected = "all"
    ),

#Crashes or Fatalities
    radioButtons(
      inputId = "fatal_choice",
      label = "Look at:",
      choices = c("Crashes" = "crashes", "Fatalities" = "fatalities"),
      selected = "crashes"
    )
  ),

  
  
  navset_tab(
    
    nav_panel(
      "Map",
      plotOutput(outputId = "state_map")
    ),
    nav_panel(
      "State Numbers",
      tableOutput(outputId = "state_table")
      
    )
    
  )
  
)



server <- function(input, output) {
#Year  
  map_data_code <- reactive({
    if (input$year_choice == "2024") {
      data <- data24
    } else {
      data <- data23
    }
#Time of Day    
    if (input$time_choice != "All Day") {
      data <- data[data$time_of_day == input$time_choice, ]
    }
#Add up by state    
    data <- data |>
      group_by(STATE, STATENAME) |>
      summarise(
        total_crashes = sum(total_crashes, na.rm = TRUE),
        total_fatalities = sum(total_fatalities, na.rm = TRUE),
        distracted_crashes = sum(distracted_crashes, na.rm = TRUE),
        distracted_fatalities = sum(distracted_fatalities, na.rm = TRUE),
        impaired_crashes = sum(impaired_crashes, na.rm = TRUE),
        impaired_fatalities = sum(impaired_fatalities, na.rm = TRUE),
        Population = first(Population),
        .groups = "drop"
      )
# Counts for crash type
    if (input$crash_choice == "all" && input$fatal_choice == "crashes") {
      data$chosen_count <- data$total_crashes
    } else if (input$crash_choice == "all" && input$fatal_choice == "fatalities") {
      data$chosen_count <- data$total_fatalities
    } else if (input$crash_choice == "distracted" && input$fatal_choice == "crashes") {
      data$chosen_count <- data$distracted_crashes
    } else if (input$crash_choice == "distracted" && input$fatal_choice == "fatalities") {
      data$chosen_count <- data$distracted_fatalities
    } else if (input$crash_choice == "impaired" && input$fatal_choice == "crashes") {
      data$chosen_count <- data$impaired_crashes
    } else {
      data$chosen_count <- data$impaired_fatalities
    }
    
    if (input$fatal_choice == "crashes") {
      data$chosen_total <- data$total_crashes
    } else {
      data$chosen_total <- data$total_fatalities
    }
    
    data$pct_value <- data$chosen_count / data$chosen_total * 100
    data$rate_value <- data$chosen_count / data$Population * 100000
    
    if (input$pct_choice == "pct") {
      data$map_value <- data$pct_value
    } else {
      data$map_value <- data$rate_value
    }
    
    data
  })
  
# Map title and legend
  map_title <- reactive({
    crash_label <- switch(input$crash_choice,
                          "all" = "All Crashes",
                          "impaired" = "Imparied crashes",
                          "distracted" = "Distracted Crashes")
    paste(crash_label, "-", input$fatal_choice, "-", input$time_choice, "-", input$year_choice)
  })
  
  map_legend <- reactive({
    if (input$pct_choice == "pct") {
      if (input$fatal_choice == "crashes") {
        "Percent of Crashes"
      } else {
        "Percent of Fatalities"
      }
    } else {
      if (input$fatal_choice == "crashes") {
        "Crashes per 100,000 People"
      } else {
        "Fatalities per 100,000 people"
      }
    }
  })
  
# Render the Map
  output$state_map <- renderPlot({
    data <- map_data_code()
    
    plot_data <- data.frame(
      state = data$STATENAME,
      map_value = data$map_value
    )
    
    if (input$state_choice == "All States") {
      include_states <- c()
    } else {
      include_states <- input$state_choice
    }
    
    plot_usmap(data = plot_data, values = "map_value",
               include = include_states, color = "white") +
      scale_fill_gradient(low = "#deebf7", high = "navy",
                          limits = c(min(data$map_value, na.rm = TRUE), max(data$map_value, na.rm = TRUE)),
                          na.value = "grey") +
      labs(title = map_title(), fill = map_legend())
    
  })
  
#Setup Table
  output$state_table <- renderTable({
    data <- map_data_code()
    
    if (input$state_choice != "All States") {
      data <- data[data$STATENAME == input$state_choice, ]
    } else {
      data <- data[order(-data$map_value), ]
    }
    
    data.frame(
      State = data$STATENAME,
      Crashes = data$total_crashes,
      Fatalities = data$total_fatalities,
      Chosen_Count = data$chosen_count,
      Percent = round(data$pct_value, 1),
      Rate_per_100k = round(data$rate_value, 1),
      Population = data$Population
    )
  })
  
  
  
  
}



shinyApp(ui = ui, server = server)

