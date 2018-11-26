import_file <- function() {
  ## Choose first file in data set to set working directory

  #initFile <- file.choose()

  #baseName <- dirname(initFile)
  baseName <- "/home/jason/Dropbox/R/LucifR/testdata"

  fileList <- list.files(baseName)

  numFiles <- length(fileList)

  import_data <- function(fileList) {

    ## preallocate data.table
    rawData <- data.table::data.table(readtime = character(48),
                                       plategroup = integer(48),
                                       well = integer(48),
                                       position = character(48),
                                       counttime = numeric(48),
                                       CPS = integer(48),
                                       fileListID = character(48),
                                       time = .POSIXct(character(48)),
                                       temp = numeric(48),
                                       group = character(48))

    # create file path and name for input file
    fileName <- paste(baseName, fileList, sep = "/")

    # read each plate individually and set column names
    rawPlate <- data.table::fread(file = fileName,
                                  skip = 1,
                                  header = TRUE,
                                  col.names = c("readtime", "plategroup", "well", "position", "counttime", "CPS"),
                                  #row.names = NULL,
                                  strip.white = TRUE,
                                  blank.lines.skip = TRUE,
                                  stringsAsFactors = FALSE)

    # coerce into data.table
    data <- data.table::data.table(rawPlate)

    # add fileList ID to data
    data.table::set(data, j = "fileListID",
                    value = fileList)

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

    fileID <- as.numeric(tools::file_ext(fileList))

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
    return(data)

  }

  mylist <- lapply(fileList, import_data)
  mydata <<- data.table::rbindlist(mylist)
  data.table::setkey(mydata, group, time)

}
