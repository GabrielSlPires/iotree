#source("shiny/required_libs.R")

rstudioapi::jobRunScript("shiny/read_iotree.R")
rstudioapi::jobRunScript("shiny/run_app.R")

#rsconnect::deployApp(appDir = "shiny/", server = "shinyapps.io", appName = "iotree")

#use mvtsplot() tipo um mapa de calor para time series pg 7 do livro  