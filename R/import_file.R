import_file <- function(x, baseName, rawData, numFiles) {

  # create file path and name for input file
  fileName <- paste(baseName, x, sep = "/")

  # read each plate individually and set column names
  rawPlate <- read.csv(file = fileName,
                       skip = 1,
                       header = TRUE,
                       col.names = c("readtime", "plategroup", "well", "position", "counttime", "CPS"),
                       row.names = NULL,
                       strip.white = TRUE,
                       blank.lines.skip = TRUE,
                       stringsAsFactors = FALSE)

  # coerce into data.table
  data <- data.table::data.table(rawPlate)

  # add fileList ID to data
  data.table::set(data, j = "fileListID",
                  value = x)

  # read in plate metadata to get plate start time and read temp
  metaDate <- pipe(paste("awk '/Plate Sequence/ {print $6,$7}'",
                         fileName, sep = " "))
  data.table::set(data, j = "time",
                  value = as.POSIXct(strptime(readLines(metaDate),
                                              format = "%d/%m/%Y %H:%M:%S")))
  close(metaDate)

  metaTemp <- pipe(paste("awk '/Plate Sequence/ {print $9}'",
                         fileName, sep = " "))
  data.table::set(data, j = "temp",
                  value = as.numeric(gsub("[A-Z]|[a-z]|:",
                                          "\\1",
                                          readLines(metaTemp))))
  close(metaTemp)

  fileID <- as.numeric(tools::file_ext(x))

  ifelse(fileID %in% seq(1, numFiles, 5),
         data.table::set(data, j = "group",
                         value = "plate01"),
         ifelse(fileID %in% seq(2, numFiles, 5),
                data.table::set(data, j = "group",
                                value = "plate02"),
                ifelse(fileID %in% seq(3, numFiles, 5),
                       data.table::set(data, j = "group",
                                       value = "plate03"),
                       ifelse(fileID %in% seq(4, numFiles, 5),
                              data.table::set(data, j = "group",
                                              value = "plate04"),
                              data.table::set(data, j = "group",
                                              value = "plate05")))))


  minrow <- (fileID*48)-47
  maxrow <- (fileID*48)

  rawData[minrow:maxrow, ] <- data

  #if (exists("rawData")) {
  #  rawData <- rbind(rawData, data)
  #} else {
  #  rawData <- data
  #}
}
