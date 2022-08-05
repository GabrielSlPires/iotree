library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Running Sensors",
             tabName = "sensor_view",
             icon = icon("folder") #filter-list
    ),
    menuItem("About",
             icon = icon("th"),
             tabName = "about_vire"
    )
  ),
  menuItem("Source code",
           icon = icon("file-code"),
           href = "https://github.com/GabrielSlPires/pneumatron")
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "sensor_view",
      fluidRow(
        column(
          width = 6,
          h3("IoTree - Data by Paramenter"),
        ),
        column(
          width = 6,
          actionButton("btn_refreash_data", "Refreash Data"),
        )
      ),
      fluidRow(
        uiOutput("iotree_pressure_ui")
      ),
      fluidRow(
        uiOutput("iotree_temperature_ui")
      ),
      fluidRow(
        uiOutput("iotree_humid_ui")
      ),
      fluidRow(
        uiOutput("iotree_battery_ui")
      )
    )
  )
)

dashboardPage(
  dashboardHeader(title = "IoTree"),
  sidebar,
  body
)