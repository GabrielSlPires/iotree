library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plotly)

source("helper.R", local = TRUE)


server <- function(input, output) {
  #import data when press the button
  data_iotree <- reactive({
    input$btn_refreash_data
    get_iotree_data(paste0("../data/iotree_database.csv"))
  })
  
  output$iotree_pressure_plot <- renderPlotly({
    p <- ggplot(data_iotree() %>%
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
    p <- ggplot(data_iotree() %>%
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
    p <- ggplot(data_iotree() %>%
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
    p <- ggplot(data_iotree() %>%
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