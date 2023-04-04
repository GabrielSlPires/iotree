get_iotree_data <- function(file_name) {
  data <- data.table::fread(file_name, col.names = c("id",
                                                     "temperature",
                                                     "humid",
                                                     "pressure",
                                                     "battery",
                                                     "datetime"))
  data$id = factor(data$id)
  return(data)
}