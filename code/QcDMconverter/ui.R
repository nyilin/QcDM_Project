library(shiny)
library(shinythemes)


shinyUI(navbarPage(
  "QcDM Converter",
  theme = shinytheme("united"),
  
  tabPanel(
    strong("License"),
    tags$div(
      HTML('<iframe src="license.htm" width="100%" style="height: 35em;"></iframe>'))
  ),
  
  tabPanel(
    strong("Import Data"),
    fluidPage(
      conditionalPanel(
        "input.step1b < 0",
        checkboxInput("step1", "Done", value = FALSE),
        checkboxInput("step1next", "Done", value = FALSE),
        checkboxInput("inputok", "Done", value = FALSE),
        checkboxInput("hideE", "Done", value = TRUE),
        checkboxInput("step2", "Done", value = FALSE)
      ),
      conditionalPanel(
        "input.step1 != true",
        fluidRow(
          column(12, offset = 2, 
                 "Please select a text file to upload and use the radio buttons to format the file into columns. Click Next when done.")
        ),
        HTML("<br>"),
        htmlOutput("wkdir"),      
        htmlOutput("file1"),
        fluidRow(
          column(3, checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma = ',', Semicolon = ';',Tab = '\t'),
                              ',')),
          column(9, tableOutput('preview_step1'),
                 textOutput("preview_step1_name"))
        )
      ),
      conditionalPanel(
        "input.step1next == true", 
        actionButton("step1b", "Next")
      ),
      conditionalPanel(
        "input.step1 == true && input.step2 == false",
        fluidRow(
          column(4, htmlOutput("admid"), htmlOutput("resdate"), 
                 htmlOutput("datef"), htmlOutput("timef"), htmlOutput("res"), 
                 htmlOutput("loc")),
          column(8,verbatimTextOutput("warnings"))
        ),
        fluidRow(
          column(4, 
                 conditionalPanel("(input.loc == '<<Enter Manually>>')", 
                                  textInput("locval",label = "D1) Enter location")), 
                 conditionalPanel("(input.hideE == false)", htmlOutput("multip")), 
                 actionButton("runFunc", "Import"))
        )
      )
    )
  )
))
