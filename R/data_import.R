library(data.table)

data_import <- function() {
  ## Choose first file in data set to set working directory

  initFile <<- file.choose()

  baseName <<- dirname(initFile)

  fileList <<- list.files(baseName)

  numFiles <<- length(fileList)

  ## preallocate data.table

  n <- 48 * numFiles
  rawData <<- data.table::data.table(readtime = character(n),
                                    plategroup = integer(n),
                                    well = integer(n),
                                    position = character(n),
                                    counttime = numeric(n),
                                    CPS = integer(n),
                                    fileListID = character(n),
                                    time = .POSIXct(character(n)),
                                    temp = numeric(n),
                                    group = character(n))

  ## create progress bar
  pb <- tcltk::tkProgressBar(title = "progress bar", min = 0,
                             max = numFiles, width = 300)

  for (i in 1:length(fileList)) {

    # create file path and name for input file
    fileName <- paste(baseName, fileList[i], sep = "/")

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
                    value = fileList[i])

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

    fileID <- as.numeric(tools::file_ext(fileList[i]))

    ifelse(fileID %in% seq(1, length(fileList), 5),
           data.table::set(data, j = "group",
                           value = "plate01"),
           ifelse(fileID %in% seq(2, length(fileList), 5),
                  data.table::set(data, j = "group",
                                  value = "plate02"),
                  ifelse(fileID %in% seq(3, length(fileList), 5),
                         data.table::set(data, j = "group",
                                         value = "plate03"),
                         ifelse(fileID %in% seq(4, length(fileList), 5),
                                data.table::set(data, j = "group",
                                                value = "plate04"),
                                data.table::set(data, j = "group",
                                                value = "plate05")))))


    minrow <- (i*48)-47
    maxrow <- (i*48)

    rawData[minrow:maxrow, ] <<- data

    tcltk::setTkProgressBar(pb, i, label=paste( round(i/numFiles*100, 0),
                                                "% done"))

  }

  close(pb)

}
