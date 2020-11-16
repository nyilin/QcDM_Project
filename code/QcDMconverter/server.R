library(shiny)
options(shiny.trace = FALSE)
library(tcltk)
library(digest)
library(plyr)
library(data.table)
library(stringi)
library(lubridate)
source('functions.R')

options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
pathvar <<- getwd();
tempdirpath <<- NULL;
filesindir <<- NULL;
status <<- "Please Configure QcDM using the controls on the left and click the 'Import' button\nPlease monitor the job status using the progress bar below. If an error occurs information will be shown here.\n\nAfter clicking 'Import', existing files processed from the same input files selected will be overwritten.";
countprogress <<- 0

shinyServer(function(input, output, session) {
  
  output$warnings <- renderText({ status })
  
  observeEvent(input$runFunc, {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing Data Files", value = 0)
    
    countprogress <<- 0
    #status <<- "Processing please wait...";
    #output$warnings <- renderText({ status })
    tryCatch({
      filesindir <<- list.files(input$wkdir, recursive = FALSE, 
                                pattern="*.csv|*.CSV")
      x <- list()
      errlisting <<- ""
      suclisting <<- ""
      i <- 1
      result <<- 1
      resultoverall <<- 1
      resultinterp <- ""
      
      if (identical("Yes", input$multip)) {
        #Data processing multi file
        for (curfile in filesindir) {
          val1 <- paste0(input$wkdir,"/",curfile)
          x[[i]] <- val1
          i <- i + 1
        }
      } else {
        #Data processing single file
        val1 <- paste0(input$wkdir,"/",input$file1)
        x[[i]] <- val1
      }
      
      for (s in x) {
        # Increment the progress bar, and update the detail text.
        progress$inc(1 / length(x), 
                     detail = paste("Processing File:", countprogress + 1, 
                                    "of", length(x)))
        
        if (input$loc == '<<Enter Manually>>') {
          result <<- process_data(path = s, header = input$header,
                                  id_name = input$admid, time_name = input$resdate, 
                                  result_name = input$res, 
                                  data_has_location = FALSE, location = input$locval,
                                  date_format = input$datef, time_format = input$timef)
        } else if (input$loc == '<<From Filename>>') {
          loc <- as.vector(stri_locate_last_regex(s, "_")[,1])
          ward <- substr(s, loc + 1, nchar(s) - 4)
          result <<- process_data(path = s, header = input$header,
                                  id_name = input$admid, time_name = input$resdate, 
                                  result_name = input$res, 
                                  data_has_location = FALSE, location = ward,
                                  date_format = input$datef, time_format = input$timef)
        } else {
          result <<- process_data(path = s, header = input$header,
                                  id_name = input$admid, time_name = input$resdate, 
                                  result_name = input$res, location_name = input$loc,
                                  date_format = input$datef, time_format = input$timef)
        }
        
        if (result == 0) {
          resultoverall <<- 0
          errlisting <<- paste0(errlisting,"\n","ERR: ",s)
        }
        if (result == 1) {	
          countprogress <<- countprogress + 1
          suclisting <<- paste0(suclisting,"\n","SUC: ",s)
        }
      }
      input$runFunc == 0	
    }, error = function(err) {
      resultoverall <<- 0
      errlisting <<- paste0(errlisting,"\n","ERR: ",s,"\n",err)
    })
    
    if (resultoverall == 0) {
      if (identical("Yes", input$multip)) {
        resultinterp <- paste0(
          resultinterp, 
          "Multi File Processing Mode:\n\nAn error was returned from the processing function, please verify the settings supplied to QcDM", suclisting, "\n\n", 
          countprogress, " of ", length(x), 
          " processed successfully\n\nThe following files failed to process\n", 
          errlisting
        )
      } else {
        resultinterp <- paste0(
          resultinterp, 
          "Single File Processing Mode:\n\nAn error was returned from the processing function, please verify the settings supplied to QcDM.\n\nThe following files failed to process\n",
          errlisting
        )
      }
      output$warnings <- renderText({ resultinterp })
      print("Error returned from process data function")
    }
    
    if (resultoverall == 1) {
      if (identical("Yes", input$multip)) {
        resultinterp <- paste0(
          resultinterp, 
          "Multi File Processing Mode:\n\nProcessing succeeded for the following files:", 
          suclisting, "\n\n", 
          countprogress, " of ", length(x), " processed successfully"
        )
      } else {
        resultinterp <- paste0(
          resultinterp, 
          "Single File Processing Mode:\n\nProcessing succeeded for the following files:", 
          suclisting, "\n\n", 
          countprogress, " of ", length(x), " processed successfully")
      }
    }
    output$warnings <- renderText({ resultinterp })
    print("Job completed without error")
  })
  
  observe({
    if (input$step1b == 1) {
      updateCheckboxInput(session, "step1", value = TRUE)
      updateCheckboxInput(session, "step1next", value = FALSE)
      s <- input$file1
      loc <- as.vector(stri_locate_last_regex(s, "_")[, 1])
      ward <- substr(s, loc + 1, nchar(s) - 4)
    }
  })
  
  output$admid <- renderUI({ 
    inFile <- paste0(input$wkdir,"/",input$file1)
    if (is.null(inFile)) return('')
    datain <<- readFileIn(inFile, 2, input$header, input$sep)
    selectInput(inputId = "admid", label = "A) Admission ID", 
                choices = as.list(colnames(datain)))
  })
  
  output$resdate <- renderUI({ 
    inFile <- paste0(input$wkdir,"/",input$file1)
    if (is.null(inFile))
      return('')
    datain <<- readFileIn(inFile, 2, input$header, input$sep)
    selectInput(inputId = "resdate", label = "B) Result Date", 
                choices = as.list(colnames(datain)))
  })
  
  output$res <- renderUI({ 
    inFile <- paste0(input$wkdir,"/",input$file1)
    if (is.null(inFile)) return('')
    datain <<- readFileIn(inFile, 2, input$header, input$sep)
    selectInput(inputId = "res", label = "C) Result Value", 
                choices = as.list(colnames(datain)))
  })
  
  output$loc <- renderUI({ 
    inFile <- paste0(input$wkdir,"/",input$file1)
    if (is.null(inFile)) return('')
    datain <<- readFileIn(inFile, 2, input$header, input$sep)
    list1 = list("<<Enter Manually>>")
    list2 = list("<<From Filename>>")
    colstp <- c(as.list(colnames(datain)),list1,list2)	
    selectInput(inputId = "loc", label = "D) Location", choices = colstp)
  })
  
  output$datef <- renderUI({ 
    list1 = list("dmy", "dym", "mdy", "myd", "ymd", "ydm")
    selectInput(inputId = "datef", label = "B1) Date Format", choices = list1)
  })
  
  output$wkdir <- renderUI({
    selectInput(inputId = "wkdir", label = "Data folder", tk_choose.dir())
  })
  
  output$multip <- renderUI({ 
    list1 = list("No","Yes")
    selectInput(inputId = "multip", label = "E) Process all files in this folder with these formats", choices = list1)
  })
  
  output$file1 <- renderUI({
    filesindir <<- list.files(input$wkdir, recursive = FALSE, 
                              pattern = "*.csv|*.CSV")
    if (length(filesindir) <= 1) {
      updateCheckboxInput(session, "hideE", value = TRUE)
    } else {
      updateCheckboxInput(session, "hideE", value = FALSE)
    }
    selectInput(inputId = "file1", "Select File", 
                choices = filesindir, selected = 1)
  })
  
  output$timef <- renderUI({ 
    list1 = list("Not available", "hms", "hm")
    selectInput(inputId = "timef", label = "B2) Time Format", choices = list1)
  })
  
  output$legal <- reactive({ 
    htmlcode <- paste(readLines("license.txt"), collapse=" ")
  })
  
  output$preview_step1_name <- reactive({ 
    paste(paste0(input$wkdir,"/",input$file1))
  })  
  
  output$preview_step1 <- renderTable({
    inFile <- paste0(input$wkdir,"/",input$file1)
    if (is.null(inFile))
      return(NULL)
    updateCheckboxInput(session, "step1next", value = TRUE)
    datain <<- readFileIn(inFile, 5, input$header, input$sep)
  })
})
