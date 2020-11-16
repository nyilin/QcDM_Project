#' Helper function used in Rmd files to summarise count and percentage.
print_n_perc <- function(n, total) {
  sprintf("%d (%.1f)", n, n / total * 100)
}
#' Helper function used in Rmd files to summarise percentiles of a numeric 
#' variable or a timestamp.
summarise_num <- function(x, n_digits = 2) {
  x <- x[!is.na(x)]
  v <- c(min(x), quantile(x, c(1, 5, 25) / 100), mean(x), 
         quantile(x, c(50, 75, 95, 99) / 100), max(x))
  if (is.numeric(x)) {
    round(v, n_digits)
  } else {
    # For timestamp
    v
  }
}
kable_table <- function(table, colnames, width_row) {
  cat("\n")
  cat(paste(colnames, collapse = "|"), "\n")
  cat(width_row, "\n")
  for (i in 1:nrow(table)) {
    cat(paste(table[i, ], collapse = "|"), "\n")
  }
  cat("\n")
}
#' Creates the folder structure required by QcDMui
#' 
#' @param path Path to input file.
create_folders <- function(path) {
  # Find the folder containing the input file:
  foldername <- file.path(dirname(tools::file_path_as_absolute(path)))
  # Create a folder named `foldername_processed` next to `foldername`:
  data_folder <- file.path(dirname(foldername), 
                           paste0(basename(foldername), "_processed"))
  # Within `foldername_processed`, create `data_new`, `data_historical`, 
  # `GLU_data` and `Import_data_report`:
  folder_list <- list(
    data_new = file.path(data_folder, "data_new"), 
    data_historical = file.path(data_folder, "data_historical"), 
    GLU_data = file.path(data_folder, "GLU_data"), 
    Import_data_report = file.path(data_folder, "Import_data_report")
  )
  lapply(folder_list, function(folder) {
    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }
  })
  folder_list
}
#' Reads and processes the input data
#' 
#' @param path Path to input file.
#' @param header Whether the input file contains column names. Default is true.
#' @param sheet_number If `path` leads to an Excel file, specify the index of
#'   the sheet to read in. Default is the first sheet.
#' @param id_name&time_name&result_name&location_name Column names or column
#'   index for the admission id, result data/timestamp, result and location
#'   columns. The column name for location can be `NULL` if `data_has_location =
#'   FALSE`, but needs to be specified otherwise.
#' @param data_has_location Indicates whether there is a location column in the
#'   input data. Default is there is.
#' @param location If `data_has_location = FALSE`, user must specify the
#'   location information for the input file.
#' @param date_format A string specifying the format of date part in the
#'   timestamp. Values allowed are: "dmy", "dym", "mdy", "myd", "ymd", "ydm".
#' @param time_format A string specifying the format of time part in the
#'   timestamp. Values allowed are: "Not available" (when only date is
#'   available), "hms", "hm". The part of time that is missing will be replaced
#'   with "11" in the output data. Timestamp can be in 24-hour or AM/PM format.
#' @param timezone A string specifying the timezone of the timestamp in the
#'   input data. See base::timezones for values allowed. Default is the system
#'   default. The complete list allowed in R can be found with `OlsonNames()`.
#' 
#' @details Uses `data.table::fread` to read in `csv` and `txt` files. Uses
#'   `readxl::read_excel` to read in Excel files. Interprets empty strings and
#'   "NA" strings in the input data (including the `location` information) as
#'   `NA`.
process_data <- function(path, header = TRUE, sheet_number = 1, 
                         id_name, time_name, result_name, location_name = NULL, 
                         data_has_location = TRUE, location = "Please specify", 
                         date_format, time_format, 
                         timezone = Sys.timezone(location = TRUE)) {
  if (!data_has_location & location == "Please specify") {
    stop(simpleError("Please specify location information for the input data."))
  }
  if (data_has_location) {
    if (is.null(location_name)) {
      stop(simpleError("Please specify the column name for location."))
    }
  }
  date_format <- match.arg(
    date_format, 
    choices = c("dmy", "dym", "mdy", "myd", "ymd", "ydm")
  )
  time_format <- match.arg(time_format, c("Not available", "hms", "hm"))
  # Read in data ---
  dat_ext <- tools::file_ext(path)
  if (dat_ext %in% c("txt", "csv")) {
    dat0 <- as.data.frame(data.table::fread(file = path, header = header, 
                                            na.strings = c("", "NA")), 
                          stringsAsFactors = FALSE)
  } else if (dat_ext %in% c("xls", "xlsx")) {
    dat0 <- as.data.frame(readxl::read_excel(path = path, col_names = header, 
                                             na = c("", "NA")), 
                          stringsAsFactors = FALSE)
  }
  # Identify columns ---
  dat <- data.frame(admission_id = dat0[, id_name], 
                    result_time_raw = dat0[, time_name], 
                    result_raw = dat0[, result_name], 
                    stringsAsFactors = FALSE)
  if (data_has_location) {
    dat$location <- dat0[, location_name]
  } else {
    dat$location <- location
  }
  # Process result time ---
  if (time_format == "Not available") {
    # If only dates are available, add `11:11:11` for time part and read in.
    time_function <- get(paste(date_format, "hms", sep = "_"))
    dat$result_time <- time_function(paste(dat$result_time_raw, "11:11:11"), 
                                     tz = timezone)
  } else {
    # If time is available, then read in timestamp as is, and add 11 seconds if
    # seconds are missing.
    time_function <- get(paste(date_format, time_format, sep = "_"))
    dat$result_time <- time_function(dat$result_time_raw, tz = timezone)
    if (time_format == "hm") {
      dat$result_time <- dat$result_time + as.difftime(11, units = "secs")
    }
  }
  dat$date <- date(dat$result_time)
  dat$month <- month(dat$result_time)
  dat$year <- year(dat$result_time)
  dat$monyyyy <- format(dat$result_time, format = "%b%Y")
  # Process result --- 
  dat$result <- as.numeric(dat$result_raw)
  # Perform quality check ---
  # If all BG values/BG timestamps are NA, stop and return 0. Otherwise return 1.
  if (all(is.na(dat$result)) | all(is.na(dat$result_time))) {
    return(0)
  }
  # Create folders ---
  folder_list <- create_folders(path = path)
  # Save data and reports to folders ---
  # Append the name of monthly data and reports with the name of the input file.
  file_name <- unlist(strsplit(basename(path), split = "\\."))[1]
  lapply(unique(na.omit(dat$monyyyy)), function(monyyyy) {
    dat_monyyyy <- dat[dat$monyyyy == monyyyy, 
                       c("admission_id", "result_time", "result", "location")]
    # Make sure the files written out match the requirement of QcDMui:
    names(dat_monyyyy) <- c("ADMISSION.ID", "RESULT.DATE", "RESULT", "LOCATION")
    write.csv(dat_monyyyy, 
              file = file.path(folder_list$data_new, 
                               paste0(monyyyy, "_", file_name, ".csv")), 
              row.names = FALSE)
  })
  # Produce reports ---
  # Overall summary
  bp_file <- "boxplot.pdf"
  if (file.exists(bp_file)) file.remove(bp_file) 
  # So that old plot is not used to make new report
  rmarkdown::render(
    "overall_summary.Rmd", clean = TRUE, 
    output_file = file.path(folder_list$Import_data_report, 
                            paste0("Overall summary_", file_name, ".pdf")), 
    params = list(dat = dat, print_n_perc = print_n_perc, 
                  summarise_num = summarise_num, kable_table = kable_table, 
                  time_format = time_format, bp_file = bp_file)
  )
  if (file.exists(bp_file)) file.remove(bp_file) # Remove temp plot file
  # Detailed summary
  rmarkdown::render(
    "detailed_summary.Rmd", clean = TRUE, 
    output_file = file.path(folder_list$Import_data_report, 
                            paste0("Detailed summary_", file_name, ".pdf")), 
    params = list(dat = dat, print_n_perc = print_n_perc, 
                  summarise_num = summarise_num, kable_table = kable_table)
  )
  return(1)
}

readFileIn <- function(path, lines, header, sep)
{   
  if (is.null(path))
    return(NULL)
  
  if((substrRight(tolower(path), 3) == "xls") || (substrRight(tolower(path), 4) == "xlsx"))
  {
    if(lines>0)
    {
      print(header)
      datain <<- as.data.frame(read_excel(path, col_names = header, range = cell_rows(1:lines)))
    }
    else
    {
      print(header)
      datain <<- as.data.frame(read_excel(path, col_names = header))
    }
  }
  else
  {
    if(lines>0)
    {
      datain <<- readFileReserveHeader(file = path, 
                                       header = header, sep = sep, 
                                       nrows = lines)
    }
    else
    {
      datain <<- readFileReserveHeader(file = path, 
                                       header = header, sep = sep)
    }
  }
  
  print(class(datain))
  return(datain)
}

readFileReserveHeader <- function(file, header = TRUE, ...) {
  if (header) {
    datain <- fread(input = file, header = FALSE, ...)
    datain <- data.frame(datain)
    cnames <- as.vector(sapply(datain[1, ], as.character))
    datain <- datain[-1, ]
    names(datain) <- cnames
    datain
  } else {
    fread(input = file, header = FALSE, ...)
  }
}

substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}
