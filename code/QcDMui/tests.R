input <- list(losNum_hour = 24,
              freqNum = 2,
              exclusion = as.numeric(c(TRUE,TRUE,FALSE)), 
              hypocutoffs = sort(c(4, 3, 2.5),decreasing = TRUE), 
              hypercutoffs = sort(c(14, 20, 24),decreasing = FALSE),  
              normalrange = c(4, 10),
              hgicutoff = 10)
input$wkdir <- "../../Test_data/NUH_processed"

dataDir <- paste0(input$wkdir, "/data_new")
histDir <- paste0(input$wkdir, "/data_historical")
gluDir <- paste0(input$wkdir, "/GLU_data")
#outDir <- paste0(input$wkdir, "/output")
if (!file.exists(histDir)) {
  dir.create(histDir, recursive = TRUE)
}
if (!file.exists(dataDir)) {
  dir.create(dataDir, recursive = TRUE)
}
if (!file.exists(gluDir)) {
  dir.create(gluDir, recursive = TRUE)
}
#if (!file.exists(outDir)) {
#  dir.create(outDir, recursive = TRUE)
#}
files <- dir(dataDir, pattern = ".csv")
if (length(files) == 0) {
  return("No file is found in folder 'data_new' in selected data folder.\n")
} else {
  n <- length(files)
  for (i in 1:n) {
    file <- files[i]
    if (nchar(file) >= (7 + 4)) {
      monyyyy <- substr(file, 1, 7)
      mon <- tolower(substr(monyyyy, 1, 3))
      month <- match(mon, tolower(month.abb))
      year <- as.numeric(substr(monyyyy, 4, 7))
      # Read in data
      data <- fread(input = paste(dataDir, file, sep = "/"),
                    header = TRUE, stringsAsFactors = FALSE)
      # data <- data.frame(data)
      # Save and process data
      # Format dates
      data <- FormatDate(dat = data, yy = year, mm = month)
      # Split by wards
      WARDS <- sort(unique(data$LOCATION))
      for (ward in WARDS) {
        filedir = paste0(input$wkdir, "/GLU_data/", ward, "/", year, "/", month)
        if (!file.exists(filedir)) {
          dir.create(filedir, recursive = TRUE)
        }
        filepath = paste(filedir, "GLU_data.csv", sep = "/")
        dat = data[data$LOCATION == ward, ]
        cat(ward, "\n")
        write.csv(dat, file = filepath, row.names = FALSE)
      }
      # Move process data to folder `data_historical`
      file.rename(
        from = paste(dataDir, file, sep = "/"),
        to = paste(histDir, file, sep = "/")
      )
    } 
    incProgress(1 / n)
  }
  out <- dir(paste0(input$wkdir, "/GLU_data"))
  if (length(out) == 0) {
    out <- NULL
  }
}

# -----
# 
dat <- data
yy = year %% 100
mm = month
function(dat, yy, mm) {
  dat = data.table(dat)
  is.data.table(dat)
  dat[, Result.Date := recogniseDateTime(RESULT.DATE, mm, yy), by = RESULT.DATE] # need to specify month and year
  dat[, RESULT.DATE := Result.Date]
  dat[, Result.Date := NULL]
  tmp <- dat
  tmp1 <- Sys.time()
  tmp1 = as.POSIXct(strptime(tmp$RESULT.DATE, "%d/%m/%y %H:%M:%S"))
  #id = which(is.na(tmp1))
  tmp$RESULT.DATE = tmp1
  dat = tmp
  return(dat)
}

recogniseDateTime(dateTime = data$RESULT.DATE, month = mm, year = yy)
data$RESULT.DATE

function(dateTime, month, year) {
  date <- unlist(strsplit(dateTime, " "))[1]
  time <- unlist(strsplit(dateTime, " "))[-1]
  tmp <- time
  time <- NULL
  for (i in 1:length(tmp)) {
    time <- paste(time, tmp[i])
  }
  
  date <- recogniseDate(date, month, year)
  if (is.na(date)) {
    return(NA)
  }
  
  timeVec <- unlist(strsplit(time, ":"))
  timeLen <- length(timeVec)
  hour <- as.numeric(timeVec[1])
  
  if (timeLen == 2) {
    min <- as.numeric(unlist(strsplit(timeVec[2], " "))[1])
    
    if (length(grep(pattern = "am", tolower(timeVec[timeLen]))) > 0) {
      if (hour >= 12) {
        hour <- hour - 12
      } else{
        hour <- hour
      }
    } else if (length(grep(pattern = "pm", tolower(timeVec[timeLen]))) > 0) {
      if (hour + 12 >= 24) {
        hour <- hour
      } else {
        hour <- hour + 12
      }
    }
    time <- sprintf("%02d:%02d:%02d", hour, min, 0)
  } else if (timeLen == 3) {
    min <- as.numeric(timeVec[2])
    second <- as.numeric(unlist(strsplit(timeVec[3], " "))[1])
    if (length(grep(pattern = "am", tolower(timeVec[timeLen]))) > 0) {
      if (hour >= 12) {
        hour <- hour - 12
      } else {
        hour <- hour
      }
    }
    if (length(grep(pattern = "pm", tolower(timeVec[timeLen]))) > 0) {
      if (hour >= 12) {
        hour <- hour
      } else {
        hour <- hour + 12
      }
    }
    time <- sprintf("%02d:%02d:%02d", hour, min, second)
  } else {
    return(NA)
  }
  paste(date, time)
}

out1 <- dir(paste0(input$wkdir, "/data_new"), full.names = TRUE)
out2 <- dir(paste0(input$wkdir, "/data_historical"), full.names = TRUE)
if (length(out1) > 0) {
  file <- out1[1]
  data <- fread(input = file,
                header = TRUE, stringsAsFactors = FALSE)
}else{
  file <- out2[1]
  data <- fread(input = file,
                header = TRUE, stringsAsFactors = FALSE)
}
ifelse(mean(data$RESULT>33)>0.95,2,1)

input$yearFrom <- 2016
input$yearTo <- 2016
input$monthFrom <- 7
input$monthTo <- 7
wards <- dir(file.path(input$wkdir, "GLU_data"))
