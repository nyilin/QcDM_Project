## Get the unit of analysis: 1 for mmol/L, 2 for mg/dL-----------
#' Get the unit of analysis based on new or stored data
getUnit <- function(wkdir){
  out1 <- dir(paste0(wkdir, "/data_new"), full.names = TRUE)
  out2 <- dir(paste0(wkdir, "/data_historical"), full.names = TRUE)
  if (length(out1) > 0) {
    file <- out1[1]
    data <- fread(input = file,
                  header = TRUE, stringsAsFactors = FALSE)
  }else{
    file <- out2[1]
    data <- fread(input = file,
                  header = TRUE, stringsAsFactors = FALSE)
  }
  ifelse(mean(data$RESULT > 33, na.rm = TRUE) > 0.95, 2, 1)
}


# Process data -----
#' Clean data, split and save the data by ward, year and month
processData <- function(data, year, month, wkdir) {
  # Remove completely duplicated rows
  #data <- unique(data)
  # Format dates
  data <- FormatDate(dat = data, yy = year, mm = month)
  # Split by wards
  WARDS <- sort(unique(data$LOCATION))
  for (ward in WARDS) {
    filedir = paste0(wkdir, "/GLU_data/", ward, "/", year, "/", month)
    if (!file.exists(filedir)) {
      dir.create(filedir, recursive = TRUE)
    }
    filepath = paste(filedir, "GLU_data.csv", sep = "/")
    dat = data[data$LOCATION == ward, ]
    cat(ward, "\n")
    write.csv(dat, file = filepath, row.names = FALSE)
  }
  return(TRUE)
}
# Input time range -----
#' Counting number of months between two dates.
#' @details Note that both the starting and ending months are counted. For 
#'   example, the number of months between 2015-01-03 and 2015-02-14 will be 2
#'   instead of 1.
#' @param yearFrom, yearTo Years for selected time range. Numeric.
#' @param monthFrom, monthTo Months for selected time range. String from 1 to 12.
#' @return Returns a list with 3 components, the first being the number of 
#'   months between the two dates, the second being the name of the starting
#'   and ending months, and the third being the matrix of year and month for 
#'   all the months selected.
countMonths <- function(yearFrom, yearTo, monthFrom, monthTo) {
  dayFrom <- as.Date(sprintf("%d-%s-1", yearFrom, monthFrom))
  dayTo <- as.Date(sprintf("%d-%s-1", yearTo, monthTo))
  # Make sure dateTo is later than dateFrom
  dates <- sort(c(dayFrom, dayTo))
  # Extract month and year of the 2 dates as numeric
  yearMonth <- lapply(as.list(dates), 
                      function(d) {
                        list(year = as.numeric(format(d, "%Y")), 
                             month = as.numeric(format(d, "%m")))
                      })
  # All the pairs of year and month selected
  if (yearMonth[[2]]$year == yearMonth[[1]]$year) {
    yearMonthMat <- cbind(
      year = yearMonth[[1]]$year, 
      month = (yearMonth[[1]]$month):(yearMonth[[2]]$month))
  } else {
    d <- yearMonth[[2]]$year - yearMonth[[1]]$year - 1
    if (d == 0) {
      yearMonthMat <- cbind(
        year = c(rep(yearMonth[[1]]$year, 12 - yearMonth[[1]]$month + 1), 
                 rep(yearMonth[[2]]$year, yearMonth[[2]]$month)), 
        month = c((yearMonth[[1]]$month):12, 1:(yearMonth[[2]]$month)))
    } else {
      yearMonthMat <- cbind(
        year = c(rep(yearMonth[[1]]$year, 12 - yearMonth[[1]]$month + 1), 
                 rep((yearMonth[[1]]$year + 1):(yearMonth[[1]]$year + d), 
                     each = 12), 
                 rep(yearMonth[[2]]$year, yearMonth[[2]]$month)), 
        month = c((yearMonth[[1]]$month):12, rep(1:12, d), 
                  1:(yearMonth[[2]]$month)))
    }
  }
  # Return: number of months selected, names of starting and ending months,
  #   and a matrix of all pairs of year and month selected
  list(nMonths = as.numeric(12 * (yearMonth[[2]]$year - yearMonth[[1]]$year) + 
                              (yearMonth[[2]]$month - yearMonth[[1]]$month) + 
                              1), 
       namesMonths = unlist(lapply(yearMonth, 
                                   function(ym) {
                                     paste(month.abb[ym$month], ym$year)
                                   })), 
       yearMonthMat = yearMonthMat, dayFrom = dayFrom, dayTo = dayTo)
}
# Glucometrics tables -----
# Normal cells:
cellHTML <- function(tableCell, just = "right") {
  cellStyle <- paste0("style='text-align:", just, 
                      ";vertical-align:middle;font-family:verdana'")
  paste0("<td ", cellStyle, ">", tableCell, "</td>")
}
# Row (with row name in the first cell if there is row name):
rowHTML <- function(tableRow, just = "right", hasRowName = TRUE, ## has row name
                                              meanormedian = FALSE) {
  if (hasRowName) {
    rowNameStyle <- "style='text-align:left;font-family:verdana'"
    rowName <- paste0("<td ", rowNameStyle, "><strong>", tableRow[1], 
                      "</strong></td>")
    row <- paste0(sapply(tableRow[-1], 
                         function(r) {
                           cellHTML(tableCell = r, just = just)
                         }), collapse = "")
    paste0("<tr>", rowName, row, "</tr>")
  } else if (meanormedian){
    rowNameStyle <- "style='text-align:right;font-family:verdana'"
    rowName <- paste0("<td ", rowNameStyle, ">", tableRow[1], 
                      "</td>")
    row <- paste0(sapply(tableRow[-1], 
                         function(r) {
                           cellHTML(tableCell = r, just = just)
                         }), collapse = "")
    paste0("<tr>", rowName, row, "</tr>")
  } else {
    row <- paste0(sapply(tableRow, 
                         function(r) {
                           cellHTML(tableCell = r, just = just)
                         }), collapse = "")
    paste0("<tr>", row, "</tr>")
  }
}
# Column names (should start from the second cell if there is row name):
colNamesHTML <- function(colNames, hasRowName = TRUE) {
  colNameStyle <- "style='text-align:right;font-family:verdana;background-color:#838B8B'"
  colNames <- paste0("<th ", colNameStyle, "><strong>", colNames, 
                     "</strong></th>", collapse = "")
  if (hasRowName) {
    paste0("<tr><th></th>", colNames, "</tr>")
  } else {
    paste0("<tr>", colNames, "</tr>")
  }
}
# Section name that spans a whole row:
sectionNameHTML <- function(sectionName, ncolSpan) {
  sectionNameStyle <- "style='text-align:left;font-family:verdana;background-color:#C1CDCD'"
  paste0("<tr><td colspan='", ncolSpan, "' ", sectionNameStyle, "><strong>", 
         sectionName, "</strong></td></tr>")
}
subsectionNameHTML <- function(subsectionName, ncolSpan) {
  subsectionNameStyle <- "style='text-align:left;font-family:verdana;background-color:#E0EEEE'"
  paste0("<tr><td colspan='", ncolSpan, "' ", subsectionNameStyle, ">", 
         subsectionName, "</td></tr>")
}

# from light to dark: c("#F0FFFF", "#E0EEEE", "#C1CDCD", "#838B8B")
# colnames: darkest
# colnames(bold) > section(bold) = subsection > rownames(bold, no color)

tableFormat <- "border-bottom='1px solid black' align='center'"
makeTable1 <- function(summaryList, unitVal, tableWidth) {
  
  tableFormat <- paste0(tableFormat, " style='width:", tableWidth, "'")
  #counts <- scrubList$ProbValues
  # Format and style of the table:
  sectionName <- "Summary of BG measurements"
  unitVal <- c('mmol/L','mg/dL')[unitVal]
  rowNames <- c("Total (N)", 
                paste0(c("Minimum", "5-th percentile", "25-th percentile", 
                         "75-th percentile", "95-th percentile", "Maximum"), 
                       " (",unitVal,")"),
                'Non-numeric values (N)')
  colNames <- c("Summary")
  
  cells <- summaryList
  rows <- cbind(rowNames, cells)
  paste0("<table ", tableFormat, ">",  
         sectionNameHTML(sectionName = sectionName, ncolSpan = 2), 
         colNamesHTML(colNames = colNames), 
         paste0(apply(rows, 1, rowHTML, hasRowName = TRUE, ## has row name
                      meanormedian = FALSE), collapse = ""), 
         "</table>")
}
makeTable2 <- function(exlList, exclusion, ageRange, losNum_hour, freqNum, tableWidth) {
  tableFormat <- paste0(tableFormat, " style='width:", tableWidth, "'")
  counts <- exlList[[1]]
  # Possible elements of counts are crt.age, crt.los, and crt.freq
  # crt.1stday will not be reported here
  # Only report those selected
  # If none of the two check boxes are checked, exclusion=c(0). 
  # Then exlNames=character(0)
  exlNames <- c("crt.los", "crt.freq", NA)[exclusion] # NA for 1st 24 hours
  exlNames <- c(NA,
                #"crt.age", 
                exlNames)
  exlNames <- exlNames[!is.na(exlNames)]
  # If selected exclusion criteria do not appear in counts, then the 
  # corresponding value is 0.
  values <- sapply(exlNames, function(exl) {
    if (!(exl %in% names(counts))) {
      sprintf("%d (%.1f)", 0, 0)
    } else if (is.na(counts[exl])) {
      "Information not available"
    } else {
      sprintf("%d (%.1f)", counts[exl], 
              counts[exl] / counts["totalcases"] * 100)
    }
  })
  exlCriteria <- c(
    paste0("Length of hospital stay no greater than ",losNum_hour," hours"), 
    paste0("Less than ",freqNum," glucose readings during hospital stay"), 
    NA # NA for 1st 24 hours
  )[exclusion]
  exlCriteria <- c(NA,
    #paste("Age below", min(ageRange), "or above", max(ageRange)), 
                   exlCriteria)
  exlCriteria <- exlCriteria[!is.na(exlCriteria)]
  # Format and style of the table:
  rowNames1 <- c("Total number of patient-stays")
  sectionName <- "Number of patient-stays excluded with"
  rowNames2 <- exlCriteria
  colNames <- c("Count (%)")
  cells <- values
  rows <- cbind(rowNames2, cells)
  if (length(exlNames) == 0) {
    paste0("<table ", tableFormat, ">",  
           rowHTML(tableRow = c(rowNames1, counts["totalcases"])), 
           rowHTML(
             tableRow = "No exclusion criterion was selected.", 
             just = "left", hasRowName = FALSE, meanormedian = FALSE
           ), 
           "</table>")
  } else {
    paste0("<table ", tableFormat, ">",  
           rowHTML(tableRow = c(rowNames1, counts["totalcases"])), 
           sectionNameHTML(sectionName = sectionName, ncolSpan = 2), 
           colNamesHTML(colNames = colNames), 
           paste0(apply(rows, 1, rowHTML, hasRowName = TRUE, meanormedian = FALSE), collapse = ""), 
           rowHTML(
             tableRow = "Note that these criteria are not mutually exclusive.", 
             just = "left", hasRowName = FALSE, meanormedian = FALSE
           ), 
           "</table>")
  }
}
makeTable3 <- function(metricList, hypocutoffs, hypercutoffs, normalrange, hgicutoff,
                       unitVal, 
                       tableWidth, width) {
  r_dig <- c(1,0)[unitVal]
  hypocutoffs <- round(sort(hypocutoffs, decreasing = TRUE),r_dig)
  hypercutoffs <- round(sort(hypercutoffs, decreasing = FALSE), r_dig)
  recur_cutoff <- round(hypocutoffs[1], r_dig)
  hgicutoff <- round(hgicutoff, r_dig)
  normalrange <- round(normalrange, r_dig)
  tableFormat <- paste0(tableFormat, " style='width:", tableWidth, "'")
  metricList <- ProGluTable(metricList = metricList, unitVal = unitVal)
  unitVal <- c('mmol/L','mg/dL')[unitVal]
  metricTable <- metricList[[1]]
  metricLegend <- metricList[[2]] # *, #1, and #2
  # Change *, #1 and #2 to superscripts
  lgnd <- sapply(metricLegend, 
                 function(m) unlist(strsplit(x = m, split = ":")))
  dimnames(lgnd) <- NULL
  metricLegend <- apply(lgnd, 2, 
                        function(l) {
                          paste0("<sup>", l[1], "</sup>:", l[2])
                        })
  colNames <- c("Patient-sample", "Patient-day<sup>#1</sup>", 
                "Patient-stay<sup>#2</sup>")
  sectionNames <- list(sec1 = "GLYCEMIC CONTROL", 
                       sec2 = "HYPOGLYCEMIA",
                       sec3 = "GLYCEMIC VARIABILITY")
  subsectionNames <- list(
    sec1 = list(sub1 = "Hyperglycemia", 
                sub2 = "Other metrics")
  )
  rowNames <- list(
    sec1 = list(
      sub0 = c("Count"), 
      sub1 = c(
        paste("Percent with glucose >=", hypercutoffs[1], unitVal), 
        paste("Percent with glucose >=", hypercutoffs[2], unitVal), 
        paste("Percent with glucose >=", hypercutoffs[3], unitVal), 
        paste0("Hyperglycemia index (HGI): AUC (> ",hgicutoff," ",unitVal,")/LOS (in hours)<sup>*</sup>"),
        "Median (IQR)", "Mean (SD)"),
      sub2 = c(
        paste("Percent with glucose >=", min(normalrange), " and <", 
              max(normalrange), unitVal),
        paste0("Glucose (",unitVal,")<sup>*</sup>"),
        "Median (IQR)", "Mean (SD)",
        paste0("Average patient-day mean glucose for a patient-stay (",unitVal,")<sup>*</sup>"),
        "Median (IQR)", "Mean (SD)"
      )),
    sec2 =  c(
      paste("Percent with glucose <", hypocutoffs[1], unitVal), 
      paste("Percent with glucose <", hypocutoffs[2], unitVal), 
      paste("Percent with glucose <", hypocutoffs[3], unitVal),
      paste0("Percent of patient-stays with a recurrent hypoglycemia (< ",recur_cutoff," ",unitVal,") day (10-240 mins)")),
    sec3 = c(paste0("Standard deviation: SD (",unitVal,")<sup>2*</sup>"), 
             "Median (IQR)", "Mean (SD)", 
             paste0("J-index (",unitVal,")<sup>2*</sup>"), 
             "Median (IQR)", "Mean (SD)")
    
  )
  rows <- list(
    sec1 = list(
      sub0 = metricTable[1, ], 
      sub1 = rbind(metricTable[2:4, ], 
                   rep("",3),
                   metricTable[5:6, ]),
      sub2 = rbind(metricTable[7, ],
                   rep("",3),
                   metricTable[8:9, ],
                   rep("",3),
                   metricTable[10:11, ])), 
    sec2 = metricTable[12:15, ], 
    sec3 =rbind(rep("",3),
                metricTable[16:17, ],
                rep("",3),
                metricTable[18:19, ]))

  # Format and style of the table:
  list(
    table = paste0(
      "<table ", tableFormat, " style='width:", tableWidth, "'", ">",  
      # Set the width of the column containing row names
      paste0("<col width='", width, "'></col>"), 
      # Column names
      colNamesHTML(colNames = colNames), 
      # Total count
      rowHTML(tableRow = c(rowNames$sec1$sub0, rows$sec1$sub0), hasRowName = TRUE, ## has row name
              meanormedian = FALSE), 
      # Section 1: Glycemic control
      sectionNameHTML(sectionName = sectionNames$sec1, 
                      ncolSpan = ncol(metricTable) + 1), 
      # Subsection 1: %hyper
      subsectionNameHTML(subsectionName = subsectionNames$sec1$sub1, 
                         ncolSpan = ncol(metricTable) + 1), 
      paste0(c(apply(cbind(rowNames$sec1$sub1[1:4], rows$sec1$sub1[1:4,]), 1, rowHTML, hasRowName = TRUE, meanormedian = FALSE), 
             apply(cbind(rowNames$sec1$sub1[5:6], rows$sec1$sub1[5:6,]), 1, rowHTML, hasRowName = FALSE, meanormedian = TRUE)),
             collapse = ""), 
      # Subsection 2: Other metrics
      subsectionNameHTML(subsectionName = subsectionNames$sec1$sub2, 
                         ncolSpan = ncol(metricTable) + 1), 
      paste0(c(apply(cbind(rowNames$sec1$sub2[1:2], rows$sec1$sub2[1:2,]), 1, rowHTML, hasRowName = TRUE, meanormedian = FALSE), 
             apply(cbind(rowNames$sec1$sub2[3:4], rows$sec1$sub2[3:4,]), 1, rowHTML, hasRowName = FALSE, meanormedian = TRUE),
             rowHTML(tableRow = c(rowNames$sec1$sub2[5], rows$sec1$sub2[5,]), hasRowName = TRUE, meanormedian = FALSE), 
             apply(cbind(rowNames$sec1$sub2[6:7], rows$sec1$sub2[6:7,]), 1, rowHTML, hasRowName = FALSE, meanormedian = TRUE)),
             collapse = ""), 
      # Section 2: Hypo
      sectionNameHTML(sectionName = sectionNames$sec2, 
                      ncolSpan = ncol(metricTable) + 1), 
      paste0(apply(cbind(rowNames$sec2, rows$sec2), 1, rowHTML, hasRowName = TRUE, meanormedian = FALSE), 
             collapse = ""), 
      # Section 3: Glycemic variability
      sectionNameHTML(sectionName = sectionNames$sec3, 
                      ncolSpan = ncol(metricTable) + 1), 
      paste0(c(rowHTML(tableRow = c(rowNames$sec3[1], rows$sec3[1,]), hasRowName = TRUE, meanormedian = FALSE), 
             apply(cbind(rowNames$sec3[2:3], rows$sec3[2:3,]), 1, rowHTML, hasRowName = FALSE, meanormedian = TRUE),
             rowHTML(tableRow = c(rowNames$sec3[4], rows$sec3[4,]), hasRowName = TRUE, meanormedian = FALSE), 
             apply(cbind(rowNames$sec3[5:6], rows$sec3[5:6,]), 1, rowHTML, hasRowName = FALSE, meanormedian = TRUE)),
             collapse = ""), 
      
      "</table>"
    ), 
    legend = paste0(
      "<table ", tableFormat, ">",  
      paste0(sapply(metricLegend, 
                    function(l) {
                      rowHTML(tableRow = l, just = "left", hasRowName = FALSE, meanormedian = FALSE)
                    }), collapse = ""), 
      "</table>"
    )
  )
}
saveGlucometrics <- function(wkdir, yearFrom, yearTo, monthFrom, monthTo, 
                             #timeUnit, 
                             wards, useAdmin, 
                             ageRange, losNum_hour, freqNum, exclusion, hypocutoffs, hypercutoffs, 
                             normalrange,
                             hgicutoff) {
  nMonths <- countMonths(yearFrom, yearTo, monthFrom, monthTo)
  # Combine data for selected months and wards
  months <- nMonths$yearMonthMat
  preDat <- do.call("rbind", lapply(as.list(wards), function(ward) {
    do.call("rbind", lapply(as.list(1:nrow(months)), function(i) {
      m <- months[i, "month"]
      yy <- months[i, "year"]
      path.input <- paste0(wkdir, "/GLU_data/", ward, "/", yy, "/", 
                           m, "/GLU_data.csv")
      if(!file.exists(path.input)){
        return(NULL)
      }
      data <- fread(input = path.input, 
                    header = TRUE, stringsAsFactors = FALSE)
      data <- as.data.frame(data)
      # Extract columns
      dat <- data[, c("ADMISSION.ID", "RESULT", "RESULT.DATE", "LOCATION")]
      if (useAdmin) {
        if ("admission.id" %in% names(data)) {
          dat$caseid <- data$caseid
        } else {
          dat$caseid <- rep("caseid", nrow(data))
        }
      }
      if ("BIRTH.DATE" %in% names(data)) {
        dat$BIRTH.DATE <- data$BIRTH.DATE
      } else {
        dat$BIRTH.DATE <- "BIRTH.DATE"
      }
      if ("AGE" %in% names(data)) {
        dat$AGE <- data$AGE
      } else {
        dat$AGE <- "AGE"
      }
      FormatDate(dat = dat, yy = yy, mm = m)
    }))
  }))
  # If more than 95% of the BG readings are having value greater than 33, then the unit of measurement can be inferred as md/dL, otherwise mmol/L
  preDat[, RESULT.MEAN:= as.numeric(RESULT)]
  unitVal <- ifelse(mean(preDat$RESULT>33, na.rm = TRUE)>0.95,2,1) 
  # If any dataset has value `caseid` in case ID column, it means that data 
  # does not have case ID information, and we should use `Pseudo` mode instead
  # if (useAdmin & ("caseid" %in% preDat$caseid)) {
  #   useAdmin <- FALSE
  #   preDat <- preDat[, c("ADMISSION.ID", "RESULT", "RESULT.DATE", "LOCATION", 
  #                        "BIRTH.DATE", "AGE"), with = FALSE]
  # }
  # Remove invalid results and deal with results containing "<" or ">"
  
  ## 
  #scrubList <- DataScrubbing(preDat, unitVal)
 
  #preDat <- scrubList[[1]] # Data after cleaning
  
  ## To find the 
  summaryList <- c(
    total = length(preDat$RESULT.MEAN), 
    round(quantile(preDat$RESULT.MEAN, probs = c(0,0.05,0.25,0.75,0.95,1), na.rm = TRUE),
          c(1,0)[unitVal]), 
    length(which(is.na(preDat$RESULT.MEAN)))
  )
  if(length(which(is.na(preDat$RESULT.MEAN)))>0){
    preDat <- preDat[!is.na(RESULT.MEAN)]
  }
  
  # Generate episodes using selected method
  epiMethod <- ifelse(useAdmin, "Admininfo", "Pseudo")
  preDat <- GenEpisode(preDat, epiMethod = epiMethod)
  # Generate number of episodes removed by each exclusion criteria selected
  crtVec <- (1:3) %in% exclusion # crt.los, crt.freq, crt.1stday
  exlList <- PerformExclusion(preDat, crtVec = crtVec, 
                              ageRange = ageRange, 
                              losNum_hour = losNum_hour, freqNum = freqNum)
  Dat.Exl <- exlList[[2]] # Data after exclusion
  # Generate glucometrics based on cutoffs set by user
  # div_factor <- ifelse(unitVal==1,18,1)
  # hypocutoffs <- hypocutoffs/div_factor
  # hypercutoffs <- hypercutoffs/div_factor
  # normalrange <- normalrange/div_factor
  # hgicutoff <- hgicutoff/div_factor
  metricList <- GenGluM(Dat.Exl, hypocutoffs, hypercutoffs, normalrange, hgicutoff, unitVal)
  # For summary table:
  nDays2 <- days_in_month(nMonths$dayTo)
  periodInfo <- paste(format(min(preDat$RESULT.DATE), "%d %b %Y"), "to", 
                      format(max(preDat$RESULT.DATE), "%d %b %Y"))
  
  table1 = makeTable1(summaryList = summaryList, unitVal = unitVal, tableWidth = "55%") 
  table2 = makeTable2(exlList = exlList, exclusion = exclusion, 
                      ageRange = ageRange, 
                      losNum_hour = losNum_hour,
                      freqNum = freqNum,
                      tableWidth = "55%")
  table3 = makeTable3(metricList = metricList, 
                      hypocutoffs = hypocutoffs, 
                      hypercutoffs = hypercutoffs, 
                      normalrange = normalrange, 
                      hgicutoff = hgicutoff,
                      unitVal = unitVal,
                      tableWidth = "80%", width = "40%")
  
  summary_table = paste0(
    "<table ", tableFormat, " style='width:80%'>",  
    sectionNameHTML(sectionName = "Summary", ncolSpan = 2), 
    # Row 1: location
    rowHTML(tableRow = c("Location", toString(wards)), hasRowName = TRUE, meanormedian = FALSE), 
    # Row 2: period
    rowHTML(tableRow = c("Period", periodInfo), hasRowName = TRUE, meanormedian = FALSE), 
    "</table>"
  )
    
  table1_h <- html_table(read_html(table1), header = FALSE, fill = TRUE)[[1]]
  colnames(table1_h) <- c('V1', 'V2')
  
  table2_h <- html_table(read_html(table2), header = FALSE, fill = TRUE)[[1]]
  colnames(table2_h) <- c('V1', 'V2')
  
  table0_h <- html_table(read_html(summary_table), header = FALSE, fill = TRUE)[[1]]
  colnames(table0_h) <- c('V1', 'V2')
  
  table3_h <- html_table(read_html(table3$table), header = FALSE, fill = TRUE)[[1]] 
  colnames(table3_h) <- c('V1', 'V2','V3','V4')
  
  legend_h <- html_table(read_html(table3$legend), header = FALSE, fill = TRUE)[[1]] 
  colnames(legend_h) <- c('V1')
  save_tables = rbindlist(list(rbindlist(list(data.table(rbind(c('Data Summary',''),
                                                               table1_h)),
                      data.table(rbind(c('Exclusion Summary',''),
                                       table2_h))), fill = TRUE),
                      data.table(rbind(c('Glucometrics',''),
                                 table0_h))), fill = TRUE)
                    
  save_tables = cbind(save_tables)
  #colnames(save_tables) <- paste0('V',1:8)
  save_tables = rbindlist(list(save_tables,data.table(table3_h)), fill = TRUE)
  save_tables = rbindlist(list(save_tables,data.table(legend_h)), fill = TRUE)
  list(table1 = table1, table2 = table2, table3 = table3, summary_table = summary_table, crtVec = crtVec, save_tables = save_tables)
}
