library(data.table)

import_file <- function() {
  # GLOBALS
  cn <- c("readtime", "plategroup", "well", "position", "counttime", "CPS")

  # generate a list of files
  data_files <- list.files("testdata", full.names = TRUE)
  names(data_files) <- basename(data_files)

  # read the data
  data_list <- lapply(data_files,
                      fread,
                      skip = 1,
                      header = TRUE,
                      col.names = cn,
                      strip.white = TRUE,
                      blank.lines.skip = TRUE)

  # make a data table
  raw_data <- rbindlist(data_list, idcol = "fileListID")

  # generate the metadata
  metadata_list <- lapply(data_files,
                          fread,
                          nrows = 1)
  metadata <- rbindlist(metadata_list,
                        idcol = "fileListID",
                        fill = TRUE)
  # mung the metadata
  metadata[, time := as.POSIXct(strptime(
    paste(V6, V7),
    format = "%d/%m/%Y %H:%M:%S"))]
  metadata[, temp := as.numeric(gsub("^[^[:digit:]]+(.*)C$", "\\1", V9))]

  # merge the metadata
  tidy_data <- merge(raw_data,
                     metadata[, .(fileListID, time, temp)])

  # not sure what the if/else is for, this is a guess
  tidy_data[, fileID := as.numeric(gsub(".*\\.", "", fileListID))]
  tidy_data[(fileID - 1) %% 5 == 0, group := "plate01"]
  tidy_data[(fileID - 2) %% 5 == 0, group := "plate02"]
  tidy_data[(fileID - 3) %% 5 == 0, group := "plate03"]
  tidy_data[(fileID - 4) %% 5 == 0, group := "plate04"]
  tidy_data[(fileID) %% 5 == 0, group := "plate05"]

  setkey(tidy_data, group, time)

  return(tidy_data)
}
