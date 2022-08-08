require(serial, quietly = TRUE)

reciver_serial_port = "COM4"
file_name = "iotree_database"

message("Reading Pneumatron from port: ",
        reciver_serial_port,
        "\nSaving data to file: ",
        file_name,
        ".csv\n\n")

#Create file with headers in here

con <- serialConnection(port = reciver_serial_port,
                        mode = "115200,n,8,1",
                        newline = 1,
                        translation = "crlf")

open(con)
while (1) {
  tryCatch({
    serial_messages <- read.serialConnection(con)
    message("\rReading IoTree   ", appendLF = FALSE)
    Sys.sleep(0.2)
    message("\rReading IoTree.  ", appendLF = FALSE)
    Sys.sleep(0.2)
    message("\rReading IoTree.. ", appendLF = FALSE)
    Sys.sleep(0.2)
    message("\rReading IoTree...", appendLF = FALSE)
    Sys.sleep(0.2)
    if (serial_messages != "") {
      time <- Sys.time()
      serial_messages <- unlist(strsplit(serial_messages, "\n"))
      for (split_message in serial_messages) {
        line <- paste(split_message, lubridate::ymd_hms(time), sep = ",")
        write(line,
              file = paste0("../data/", file_name, ".csv"),
              append = TRUE)
      }
    }
  }, error = function(e){})
}
close(con)
