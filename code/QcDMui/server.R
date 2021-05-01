library(tcltk)
library(shiny)
library(QcDM)
library(utils)
library(stats)
library(lubridate) # Get number of days in a month
library(data.table) # To use fread
library(rvest)
library(DT) # To properly display dataTable
options(shiny.trace = TRUE)
data <- NULL
out <- read.csv("sampledata.csv", header = TRUE, stringsAsFactors = FALSE)

options(shiny.maxRequestSize = 1000*1024^2)
source("functions.R")

shinyServer(function(input, output) {
  # Tab: About the interface -----
  output$sampledata <- renderTable({
    head(out)
  })
  
  # output$glucometric_definition <- renderImage({
  #   
  #   
  #   # Return a list containing the filename and alt text
  #   list(src = 'Glucometrics_definition.png', 
  #        alt = paste(""))
  #   
  # }, deleteFile = FALSE)
  
  # Tab: Data -----
  # Select working directory =====
  output$wkdir <- renderUI({
    # selectInput(inputId = "wkdir", label = "Data folder",
    #             ifelse(Sys.info()[["sysname"]] == "Windows",
    #                    dirname(paste0(choose.dir(), "\\data")),
    #                    tk_choose.dir()))
    selectInput(inputId = "wkdir", label = "Data folder", tk_choose.dir())
  })
  
  
  # Process data =====
  # Look for new data in folder `new_data`, and then split each dataset by 
  # ward and month.
  observe({
    
    output$batchMsg <- renderText({
      validate(need(input$wkdir, ""))
      dataDir <- paste0(input$wkdir, "/new_data")
      histDir <- paste0(input$wkdir, "/processed_data")
      gluDir <- paste0(input$wkdir, "/glucometrics_output")
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
        return("No file is found in folder 'new_data' in selected data folder.\n")
      } else {
        n <- length(files)
        withProgress(min = 0, max = n, {
          setProgress(message = "Processing input datasets\n",
                      detail = "This will take a while...")
          for (i in 1:n) {
            file <- files[i]
            setProgress(detail = paste("Processing file", file, ":"))
            if (nchar(file) >= (7 + 4)) {
              monyyyy <- substr(file, 1, 7)
              mon <- tolower(substr(monyyyy, 1, 3))
              month <- match(mon, tolower(month.abb))
              year <- as.numeric(substr(monyyyy, 4, 7))
              # Read in data
              data <- fread(input = paste(dataDir, file, sep = "/"),
                            header = TRUE, stringsAsFactors = FALSE)
              data <- data.frame(data)
              # Save and process data
              processed <- processData(data, year = year, month = month, 
                                       wkdir = input$wkdir)
              if (processed) {
                setProgress(
                  detail = paste("successful.", file,
                                 "has been moved to folder 'processed_data'.")
                )
              } else {
                setProgress(detail = paste(msgi, "failed.", processed))
              }
              # Move process data to folder `processed_data`
              file.rename(
                from = paste(dataDir, file, sep = "/"),
                to = paste(histDir, file, sep = "/")
              )
            } else {
              setProgress(
                detail = "failed because name of file is not in required format."
              )
            }
            incProgress(1 / n)
          }
        })
        if(input$btn_confirm == 1){
          searchWard()
        }
        
        return("Process complete.")
      }
    })
    
    
    
    
  })
  
  output$hypo3 <- renderUI({
    if (!is.null(input$wkdir)) {
      # unitVal <- getUnit(input$wkdir)
      unitVal <- as.integer(input$unitVal)
      numericInput(
        "hypo3",
        label = paste0(
          "Please specify the first hypo cutoff (",
          c("mmol/L", "mg/dL")[unitVal],
          "):"
        ),
        min = 0,
        max = 600,
        value = c(4, 70)[unitVal])
    }
    
  })
  output$hypo2 <- renderUI({
    if (!is.null(input$wkdir)) {
      # unitVal <- getUnit(input$wkdir)
      unitVal <- as.integer(input$unitVal)
      numericInput(
        "hypo2",
        label = paste0(
          "Please specify the second hypo cutoff (",
          c("mmol/L", "mg/dL")[unitVal],
          "):"
        ),
        min = 0,
        max = 600,
        value = c(3, 60)[unitVal])
    }
    
    
  })
  output$hypo1 <- renderUI({
    if (!is.null(input$wkdir)) {
      # unitVal <- getUnit(input$wkdir)
      unitVal <- as.integer(input$unitVal)
      numericInput(
        "hypo1",
        label = paste0(
          "Please specify the third hypo cutoff (",
          c("mmol/L", "mg/dL")[unitVal],
          "):"
        ),
        min = 0,
        max = 600,
        value = c(2.5, 40)[unitVal])
    }
    
    
  })
  
  output$hyper3 <- renderUI({
    if (!is.null(input$wkdir)) {
      # unitVal <- getUnit(input$wkdir)
      unitVal <- as.integer(input$unitVal)
      numericInput(
        "hyper3",
        label = paste0(
          "Please specify the first hyper cutoff (",
          c("mmol/L", "mg/dL")[unitVal],
          "):"
        ),
        min = 0,
        max = 600,
        value = c(14, 180)[unitVal])
    }
    
    
  })
  output$hyper2 <- renderUI({
    if (!is.null(input$wkdir)) {
      # unitVal <- getUnit(input$wkdir)
      unitVal <- as.integer(input$unitVal)
      numericInput(
        "hyper2",
        label = paste0(
          "Please specify the second hyper cutoff (",
          c("mmol/L", "mg/dL")[unitVal],
          "):"
        ),
        min = 0,
        max = 600,
        value = c(20, 250)[unitVal])
    }
    
    
  })
  output$hyper1 <- renderUI({
    if (!is.null(input$wkdir)) {
      # unitVal <- getUnit(input$wkdir)
      unitVal <- as.integer(input$unitVal)
      numericInput(
        "hyper1",
        label = paste0(
          "Please specify the third hyper cutoff (",
          c("mmol/L", "mg/dL")[unitVal],
          "):"
        ),
        min = 0,
        max = 600,
        value = c(24, 300)[unitVal])
    }
    
    
  })
  
  output$normalrange_lower <- renderUI({
    if (!is.null(input$wkdir)) {
      # unitVal <- getUnit(input$wkdir)
      unitVal <- as.integer(input$unitVal)
      numericInput(
        "normalrange_lower",
        label = paste0(
          "Please specify the lower value of the target range (",
          c("mmol/L", "mg/dL")[unitVal],
          "):"
        ),
        min = 0,
        max = 600,
        value = c(4, 70)[unitVal])
    }
    
    
  })
  output$normalrange_upper <- renderUI({
    if (!is.null(input$wkdir)) {
      # unitVal <- getUnit(input$wkdir)
      unitVal <- as.integer(input$unitVal)
      numericInput(
        "normalrange_upper",
        label = paste0(
          "Please specify the lower value of the target range (",
          c("mmol/L", "mg/dL")[unitVal],
          "):"
        ),
        min = 0,
        max = 600,
        value = c(10, 180)[unitVal])
    }
    
    
  })
  output$hgicutoff <- renderUI({
    if (!is.null(input$wkdir)) {
      # unitVal <- getUnit(input$wkdir)
      unitVal <- as.integer(input$unitVal)
      numericInput(
        "hgicutoff",
        label = paste0(
          "Please specify the cutoff for HGI (",
          c("mmol/L", "mg/dL")[unitVal],
          "):"
        ),
        min = 0,
        max = 600,
        value = c(10, 180)[unitVal])
    }
    
    
  })
  
  # Exclusion criteria =====
  output$exclusion_freq <- renderUI({
    checkboxGroupInput(
      inputId = "exclusion_freq", label = NULL,
      choices = c(
        "Exclude patient stays having fewer BG measurements than the number specified below" = 2
        
      ),
      selected = NULL, inline = FALSE
    )
  })
  output$exclusion_los <- renderUI({
    checkboxGroupInput(
      inputId = "exclusion_los", label = NULL,
      choices = c(
        
        "Exclude patient stays having length of stay less than or equal to the number specified below in terms of hours" = 1
        
      ),
      selected = NULL, inline = FALSE
    )
  })
  
  # output$exclusion_first24 <- renderUI({
  #   checkboxGroupInput(
  #     inputId = "exclusion_first24", label = NULL,
  #     choices = c(
  #       
  #       
  #       "Exclude glucose readings taken within the first 24 hours of hospital stay" = 3
  #     ),
  #     selected = NULL, inline = FALSE
  #   )
  # })
  # Input time range =====
  # output$nMonthSelected <- renderPrint({
  #   # Start from the 1st day of the starting month and end at the last day of
  #   # the ending month.
  #   # Since we only care about the month and year, it is easier to assign the
  #   # date to 1 for both the starting and ending days. Then we can still use
  #   # the old functions for `dateRangeInput`.
  #   nMonths <- countMonths(yearFrom = input$yearFrom, yearTo = input$yearTo, 
  #                          monthFrom = input$monthFrom, monthTo = input$monthTo)
  #   nDays2 <- days_in_month(nMonths$dayTo)
  #   # If to produce quarterly report, number of months should be multiple of 3
  #   msg <- paste("You have selected", nMonths$nMonths,
  #                "month(s), starting from 1", nMonths$namesMonths[1],
  #                "and ending at", nDays2, nMonths$namesMonths[2], ".")
  #   if (input$timeUnit == "quarterly") {
  #     if (nMonths$nMonths == 3) {
  #       msg <- paste(msg, "You are strongly advised to select at least 6 months to produce quarterly report.")
  #     } else if (nMonths$nMonths %% 3 != 0) {
  #       msg <- paste(msg, "The number of months you have selected is not a multiple of 3. In order to produce quarterly report, you are strongly advised to select at least 6 months, and please make sure that the number of months you have selected is a multiple of 3.")
  #     }
  #   }
  #   cat(msg)
  # })
  # Select wards =====
  searchWard <- reactive({
    
    out <- dir(paste0(input$wkdir, "/glucometrics_output"))
    if (length(out) == 0) {
      return(NULL)
    }
    out
  })
  output$selectWardUI <- renderUI({
    # validate(
    # need(
    # ,
    # "Please make sure you have specified the correct data folder and clicked 'Confirm' on the bottom left of this tab."
    # )
    # )
    if(input$btn_confirm == 1){
      selectizeInput(
        inputId = "wardsSelected",
        label = "Please select wards to view (multiple selection allowed)",
        choices = searchWard(), multiple = TRUE
      )
    }
    
  })
  # Report number of patients by month and ward =====
  dataSummary <- reactive({
    nMonths <- countMonths(yearFrom = input$yearFrom, yearTo = input$yearTo, 
                           monthFrom = input$monthFrom, monthTo = input$monthTo)
    dataFolder <- paste0(input$wkdir, "/glucometrics_output")
    if (input$wardMode == "all") {
      wards <- searchWard()
    } else {
      wards <- input$wardsSelected
    }
    # Get selected year and months
    yearMonthMat <- nMonths$yearMonthMat
    result <- lapply(as.list(wards), function(ward) {
      # Find link to all wards and months selected
      files <- paste0(dataFolder, "/", ward, "/", yearMonthMat[, "year"],
                      "/", yearMonthMat[, "month"], "/GLU_data.csv")
      res <- lapply(as.list(files), function(file) {
        if (file.exists(file)) {
          # Read file for a ward-month
          d <- fread(file, header = TRUE, stringsAsFactors = FALSE)
          d <- as.data.frame(d)
          # Count number of unique patients
          n <- length(unique(d$ADMISSION.ID))
          # Check whether there is `caseid` column in the data
          hasCaseID <- "admission.id" %in% tolower(names(d))
          list(n = n, hasCaseID = hasCaseID)
        } else {
          list(n = 0, hasCaseID = NA)
        }
      })
      n <- unlist(lapply(res, function(r) r$n))
      hasCaseID <- unlist(lapply(res, function(r) r$hasCaseID))
      list(n = n, hasCaseID = hasCaseID)
    })
    # Summary of number of patients
    countPatients <- do.call("rbind", lapply(result, function(r) r$n))
    countPatients[countPatients == 0] <- "No data"
    months <- paste(month.abb[yearMonthMat[, "month"]], yearMonthMat[, "year"])
    countPatients <- cbind(wards, countPatients)
    colnames(countPatients) <- c("Wards", months)
    rownames(countPatients) <- 1:nrow(countPatients)
    # Whether all ward-months have `caseid` column 
    hasCaseID <- all(unlist(lapply(result, function(r) r$hasCaseID)), 
                     na.rm = TRUE)
    list(countPatients = countPatients, hasCaseID = hasCaseID)
  })
  # output$hasCaseID <- renderText({
  #   # Through error if no wards are selected yet.
  #   if (input$wardMode == "all") {
  #     wards <- searchWard()
  #   } else {
  #     wards <- input$wardsSelected
  #   }
  #   validate(
  #     need(wards, "Please select wards to view and press 'Confirm ward' above.")
  #   )
  #   #hasCaseID <- dataSummary()$hasCaseID
  #   # if (!input$useAdmin) {
  #   #   "Pseudo patient episodes will be inferred from length of stay."
  #   # } else {
  #   #   if (hasCaseID) {
  #       "Hospital stay will be defined by admission column in the data."
  #     # } else {
  #     #   "Hospital stay information is not available in the data. Pseudo hospital stays will be inferred from length of stay instead."
  #     # }
  #   }
  # })
  output$summariseWards <- renderDataTable({
    # Through error if no wards are selected yet.
    if (input$wardMode == "all") {
      wards <- searchWard()
    } else {
      wards <- input$wardsSelected
    }
    validate(
      need(wards, "Please select wards to view and press 'Confirm ward' above.")
    )
    # Through error if to produce reports quarterly but number of months
    # selected is not a multiple of 3.
    # if (input$timeUnit == "quarterly") {
    #   if (nMonths$nMonths %% 3 == 0) {
    #     checkTimeUnit <- "Correct"
    #   } else {
    #     checkTimeUnit <- NULL
    #   }
    #   validate(
    #     need(checkTimeUnit,
    #          "In order to produce quarterly report, you are strongly advised to select at least 6 months, and please make sure that the number of months you have selected is a multiple of 3.")
    #   )
    # }
    dataSummary()$countPatients
  })
  # Tab: Glucometrics -----
  # Subtab: Overall -----
  glu <- reactive({
    validate(need(input$wkdir,
                  "Please choose working directory in panel 'Data' first."))
    # Selected months (Mon yyyy)
    validate(need(list(input$yearFrom, input$yearTo, input$monthFrom, 
                       input$monthTo),
                  "Please choose months to view in panel 'Data' first."))
    # Selected wards
    if (input$wardMode == "all") {
      wards <- dir(paste0(input$wkdir, "/glucometrics_output"))
    } else {
      wards <- input$wardsSelected
    }
    validate(need(units,
                  "Please choose wards to view in panel 'Data' first."))
    tableList <- saveGlucometrics(
      wkdir = input$wkdir, yearFrom = input$yearFrom, yearTo = input$yearTo,
      monthFrom = input$monthFrom, monthTo = input$monthTo, 
      #timeUnit = input$timeUnit, 
      wards = wards, useAdmin = TRUE, #input$useAdmin
      ageRange = c(16,120), 
      losNum_hour = input$losNum_hour,
      freqNum = input$freqNum,
      exclusion = as.numeric(c(input$exclusion_los,input$exclusion_freq,FALSE)), 
      hypocutoffs = sort(c(input$hypo1, input$hypo2, input$hypo3),decreasing = TRUE), 
      hypercutoffs = sort(c(input$hyper1, input$hyper2, input$hyper3),decreasing = FALSE),  
      normalrange = c(input$normalrange_lower, input$normalrange_upper),
      hgicutoff = input$hgicutoff
    )
    tableList
  })
  output$tableQuality <- renderText({
    glu()$table1
  })
  output$tableExclusion <- renderText({
    glu()$table2
  })
  output$tableSummary <- renderText({
    glu()$summary_table
  })
  output$tableGlucometrics <- renderText({
    glu()$table3$table
  })
  output$tableGlucometricsLegend <- renderText({
    glu()$table3$legend
  })
  output$saveMsg <- renderText({
    save_table <- glu()$save_tables
    file_path <- paste0(input$wkdir, "/Glucometric_report")
    if (!dir.exists(file_path)) dir.create(file_path)
    projectfolderpath <- unlist(strsplit(input$wkdir,'/'))
    folder_name <- projectfolderpath[length(projectfolderpath)]
    time_stamp <- format(Sys.time(),'%Y_%m_%d_%H_%M_%S')
    output_name <- paste0(folder_name, "_", time_stamp)
    rmarkdown::render(
      "glu_tables.Rmd", clean = TRUE, 
      output_file = file.path(file_path, paste0(output_name, ".html")), 
      params = list(table1 = glu()$table1, table2 = glu()$table2, 
                    summary_table = glu()$summary_table, 
                    table3 = glu()$table3)
    )
    write.csv(save_table, file = file.path(file_path, paste0(output_name, ".csv")), 
              row.names = FALSE, col.names = FALSE, na = '')
    return(paste0(
      "Summary Statistics report is saved in the folder: '", file_path, 
      "' as a CSV file named '", paste0(output_name, ".csv"), 
      "' and as a HTML report named '", paste0(output_name, ".html"), "'."))
  })
})
