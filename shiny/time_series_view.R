library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(magrittr)
library(zoo)
library(tidyr)

my_ts_df <- function(sensor, type, time, raw_value, my_ts_decomposed) {
  data.frame(
    sensor = sensor,
    type = type,
    time = as.numeric(time),
    raw_value = as.numeric(raw_value),
    observed = as.numeric(my_ts_decomposed$x),
    trend = as.numeric(my_ts_decomposed$trend),
    seasonal = as.numeric(my_ts_decomposed$seasonal)
  )
}

ts_custom_theme <- list(
  geom_line(aes(time,
                value,
                group = paste(sensor, type),
                color = type)),
  theme_bw(),
  theme(legend.position = "top"),
  labs(y = "iotree_output",
       x = "day",
       color = "")
)

time_series_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    column(
      width = 12,
      height = NULL,
      box(
        width = 12,
        height = NULL,
        title = "Time series Plots",
        column(
          width = 12,
          fluidRow(
            selectInput(ns("serial_var"),
            "Select your interest Variable:",
            choices = list("V1" = "pressure",
                           "V2" = "humid"),
            selected = 1)
          ),
          fluidRow(
            h4("Time Series - Missing Values"),
            plotOutput(ns("missing_values"), height = "90vh")
          ),
          fluidRow(
            h4("Raw Time Series"),
            plotOutput(ns("raw_time_series"), height = "90vh")
          ),
          fluidRow(
            h4("ATM vs PLANT - Normalization with group_by(sensor)"),
            plotOutput(ns("atm_plant_time_series"), height = "90vh")
          ),
          fluidRow(
            h4("Time Series - Normalization with group_by(sensor)"),
            plotOutput(ns("normalized_time_series"), height = "90vh")
          )
        )
      )
    )
    
  )
}

time_series_server <- function(id, data_iotree) {
  moduleServer(id, function(input, output, session) {

    #make reactive by selecting IDs, initial datetime, grouping IDs in ATM vs PLANT
    result_plot <- reactive({
      
      iotree_type <- c("101" = "plant",
                       "102" = "plant",
                       "103" = "atm",
                       "105" = "plant",
                       "116" = "plant",
                       "117" = "atm",
                       "118" = "plant")
      
      data <- data_iotree %>% 
        filter(pressure > 0,
               id %in% as.numeric(names(iotree_type)),
               datetime > lubridate::ymd_hm("2023-04-10 14:30")) %>% 
        mutate(id = factor(id))
      
      humid_result <- data.frame()
      
      for (iotree_id in unique(data$id)) {
        #filter loop data
        data_ts <- data %>% 
          filter(id == iotree_id) %>% 
          select(datetime, all_of(input$serial_var)) %>% 
          group_by(datetime = floor_date(datetime, "30 mins")) %>%
          summarize_all(mean, na.rm = TRUE)
        
        #create ts object
        zoo_ts <- read.zoo(data_ts)
        time(zoo_ts) <- (as.numeric(time(zoo_ts)) - as.numeric(start(zoo_ts))) / (24 * 60 * 60)
        my_ts <- zoo::na.spline(as.ts(zoo_ts))
      
        my_ts_decomposed <- decompose(my_ts)
        
        humid_result <- rbind(humid_result,
                              my_ts_df(iotree_id,
                                       iotree_type[as.character(iotree_id)],
                                       time(my_ts),
                                       as.ts(zoo_ts),
                                       my_ts_decomposed))
      }
      result_plot <- pivot_longer(humid_result,
                                  cols = c("raw_value",
                                           "observed",
                                           "trend",
                                           "seasonal"))
      return(result_plot)
      sample_scale <- scale(result_plot$value[1:(length(result_plot$value*0.1))])
      result_plot <- result_plot %>% 
        mutate(value_scale = scale(value,
                                   attr(sample_scale, "scaled:center"),
                                   attr(sample_scale, "scaled:scale")))
    }) 
    
    output$missing_values <- renderPlot(
      ggplot(result_plot() %>% 
               mutate(value = scale(value)) %>% 
               pivot_wider(names_from = "name",
                           values_from = "value"),
             aes(x = time)) +
        geom_line(aes(y = observed, color = "Filled")) +
        geom_line(aes(y = raw_value, color = "Raw")) +
        facet_wrap(~sensor,
                   ncol = 1,
                   strip.position = "right",
                   scales = "free") +
        #scale_color_manual(values = c("Raw" = "grey80",
        #                              "Filled" = "red")) +
        theme_bw() +
        theme(legend.position = "top") +
        labs(title = "Missing values",
             subtitle = "Values were filled with zoo:na.spline()",
             y = "iotree_output",
             x = "day",
             color = "Values")
    )

    output$raw_time_series <- renderPlot(
      ggplot(result_plot() %>% 
               filter(name != "raw_value") %>% 
               mutate(value = scale(value))) +
        ts_custom_theme +
        facet_wrap(~name,
                   scales = "free",
                   nrow = 3,
                   strip.position = "right")
    )
    
    output$atm_plant_time_series <- renderPlot(
      ggplot(result_plot() %>% 
               filter(name != "raw_value") %>% 
               group_by(sensor, name) %>% 
               mutate(value = scale(value))) +
        ts_custom_theme +
        ggh4x::facet_grid2(name~type,
                           scales = "free",
                           independent = "y")
    )
    
    output$normalized_time_series <- renderPlot(
      ggplot(result_plot() %>% 
               filter(name != "raw_value") %>% 
               group_by(sensor, name) %>% 
               mutate(value = scale(value))) +
        ts_custom_theme +
        facet_wrap(~name,
                   scales = "free",
                   nrow = 3,
                   strip.position = "right")
    )
  })
}