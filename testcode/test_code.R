library(data.table)
library(ggplot2)

fileName <- file.choose()

baseName <- dirname(fileName)

fileList <- list.files(baseName)

for (i in 1:length(fileList)) {
  data <- read.table(file = paste(baseName, fileList[i], sep = "/"),
                     header = FALSE,
                     sep = ",",
                     col.names = c("well", "count"))
  data <- data.table::data.table(data)

  fileTime <- file.info(paste(baseName, fileList[i], sep = "/"))$ctime
  data.table::set(data, j = "time", value = fileTime)

  if (exists("plate01")) {
    plate01 <- rbind(plate01, data[1:48, ])
    plate02 <- rbind(plate02, data[49:96, ])
    plate03 <- rbind(plate03, data[97:144, ])
    plate04 <- rbind(plate04, data[145:192, ])
    plate05 <- rbind(plate05, data[193:240, ])
  } else {
    plate01 <- data[1:48, ]
    plate02 <- data[49:96, ]
    plate03 <- data[97:144, ]
    plate04 <- data[145:192, ]
    plate05 <- data[193:240, ]
  }

}
data.table::set(plate01, j = "plate", value = "plate01")
data.table::set(plate02, j = "plate", value = "plate02")
data.table::set(plate03, j = "plate", value = "plate03")
data.table::set(plate04, j = "plate", value = "plate04")
data.table::set(plate05, j = "plate", value = "plate05")

dt <- rbind(plate01, plate02, plate03, plate04, plate05)

pl1w1 <- plate01[well %in% 14]

p <- ggplot(data = pl1w1, mapping = aes(x = time, y = count)) +
  geom_line()

p


dtmean <- dt[, .(ave = mean(count)),
             by = c("plate", "time")]

dtmean <- dtmean[plate %in% c("plate01", "plate02")]

p <- ggplot(data = dtmean, mapping = aes(x = time, y = ave, colour = plate)) +
  geom_line()

p
