library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plotly)

source("helper.R", local = TRUE)
data_github_path <- "https://raw.githubusercontent.com/biolpereira/data_iotree_unicamp/master/database-test.csv"

server <- function(input, output) {
  #import data when press the button
  data_iotree <- reactive({
    input$btn_refreash_data
    get_iotree_data(paste0(data_github_path))
  })
  
  data_iotree_filter <- reactive({
    datetime_filter <- input$date_time_filter
    data_iotree() %>% 
      filter(datetime >= datetime_filter[1],
             datetime <= as.Date(datetime_filter[2]) + 1) %>% 
      group_by(id) %>% 
      slice_sample(n = 500, replace = TRUE)
  })
  
  output$data_range_ui <- renderUI({
    date_max <- max(data_iotree()$datetime)
    date_min <- min(data_iotree()$datetime)
    iotree_ids <- sort(unique(data_iotree()$id))

    column(
      width = 12,
      box(
        title = "Setup",
        width = 12,
        column(
          width = 4,
          dateRangeInput("date_time_filter",
                          label = 'Date range:',
                          start = as.Date(date_max) - 2,
                          end = date_max,
                          min = date_min,
                          max = date_max)
        ),
        column(
          width = 4,
          fluidRow(
            checkboxInput(
              "enable_atm_pressure",
              "Remove atmosferic pressure from your data"
            )
          ),
          fluidRow(
            selectInput(
              "atm_pressure_id",
              "Select your device with atmosferic pressure:",
              choices = iotree_ids
            )
          )
        ),
        column(
          width = 4,
          actionButton("btn_refreash_data", "Refreash Data"),
        )
      )
    )
  })
  
  output$iotree_pressure_plot <- renderPlotly({
    data <- data_iotree_filter()
    if (input$enable_atm_pressure) {
      data_atm <- data %>% 
        filter(id == input$atm_pressure_id) %>% 
        select(pressure, datetime)
      
      data <- data %>% 
        filter(id != input$atm_pressure_id) %>% 
        rowwise() %>% 
        mutate(pressure = pressure - atm_pressure(data_atm, datetime))
    }
    
    p <- ggplot(data %>%
                  mutate(id = factor(id)) %>%
                  filter(
                    pressure > 0),
                aes(x = datetime)) +
      geom_point(aes(y = pressure, color = id)) +
      scale_x_datetime(date_labels = "%b %d") +
      xlab("") +
      theme_bw()
    
    ggplotly(p)
  })
  
  output$iotree_pressure_ui <- renderUI({
    boxname <- paste("Pressure")
    plotname <- paste0("iotree_pressure_plot")
    column(width = 12,
           box(title = boxname,
               width = 12,
               plotlyOutput(plotname)))
  })
  
  output$iotree_temperature_plot <- renderPlotly({
    p <- ggplot(data_iotree_filter() %>%
                  mutate(id = factor(id)) %>%
                  filter(
                    temperature > 0),
                aes(x = datetime)) +
      geom_point(aes(y = temperature, color = id)) +
      scale_x_datetime(date_labels = "%b %d") +
      xlab("") +
      theme_bw()
    
    ggplotly(p)
  })
  
  output$iotree_temperature_ui <- renderUI({
    boxname <- paste("Temperature")
    plotname <- paste0("iotree_temperature_plot")
    column(width = 12,
           box(title = boxname,
               width = 12,
               plotlyOutput(plotname)))
  })
  
  output$iotree_humid_plot <- renderPlotly({
    p <- ggplot(data_iotree_filter() %>%
                  mutate(id = factor(id)) %>%
                  filter(
                    humid > 0),
                aes(x = datetime)) +
      geom_point(aes(y = humid, color = id)) +
      scale_x_datetime(date_labels = "%b %d") +
      xlab("") +
      theme_bw()
    
    ggplotly(p)
  })
  
  output$iotree_humid_ui <- renderUI({
    boxname <- paste("Humid")
    plotname <- paste0("iotree_humid_plot")
    column(width = 12,
           box(title = boxname,
               width = 12,
               plotlyOutput(plotname)))
  })
  
  output$iotree_battery_plot <- renderPlotly({
    p <- ggplot(data_iotree_filter() %>%
                  mutate(id = factor(id)) %>%
                  filter(
                    battery > 0),
                aes(x = datetime)) +
      geom_point(aes(y = battery, color = id)) +
      scale_x_datetime(date_labels = "%b %d") +
      xlab("") +
      theme_bw()
    
    ggplotly(p)
  })
  
  output$iotree_battery_ui <- renderUI({
    boxname <- paste("Battery")
    plotname <- paste0("iotree_battery_plot")
    column(width = 12,
           box(title = boxname,
               width = 12,
               plotlyOutput(plotname)))
  })
}