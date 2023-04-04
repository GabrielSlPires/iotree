library(shiny)
library(shinydashboard)

shiny_busy <- function() { #https://community.rstudio.com/t/shiny-app-show-some-message-while-user-is-waiting-for-output/12822
  # use &nbsp; for some alignment, if needed
  HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", paste0(
    '<span data-display-if="',
    '$(&#39;html&#39;).attr(&#39;class&#39;)==&#39;shiny-busy&#39;',
    '">',
    '<i class="fa fa-spinner fa-pulse fa-fw" style="color:orange"></i>',
    '</span>'
  ))
}

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
          shiny_busy()
        )
      ),
      fluidRow(
        uiOutput("data_range_ui")
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