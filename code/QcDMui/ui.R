library(shiny)
library(DT) # To properly display dataTable
library(shinythemes)

shinyUI(navbarPage("QcDM UI",theme=shinytheme("united"),
  # Tab: About the interface -----
  tabPanel(
    strong("About the interface"),
    mainPanel(
      tags$h2(strong("Diabetes Mellitus")),
      p(
        "Diabetes mellitus (DM) is a significant public issue in Singapore.
        Individuals with DM are more likely to be hospitalized,
        and therefore form a substantial proportion of the inpatient population.
        An estimated 40% of all hospitalized patients in National University Hospital (NUH) have DM as a comorbidity.
        Inpatient hypoglycemia and hyperglycemia are common and are associated with increased mortality and morbidity. "
      ),
      br(),
      tags$h2(strong(
        "Inpatient Capillary Blood Glucose monitoring protocol"
      )),
      p(
        "According to ADA recommendations, BG tests are performed for different type of inpatients:"
      ),
      tags$ol(
        tags$li(
          "For patients eating usual meals (majority of inpatients): pre-meals and bedtime. In NUH, the meal times and bedtime are fixed at around 0800h, 1200h, 1800h, 2200h respectively"
        ),
        tags$li("For patients who are fasting: every 4-6 h"),
        tags$li(
          "For patients who are receiving IV insulin infusions: ranging from every 30 min to every 2 hour"
        )
      ),
      p("The protocol will change when an hypoglycmia event occurs"),
      tags$ol(
        tags$li(
          "Treatment of hypoglycemia (in an alert patient) with oral 15g dextrose powder folowed by a repeat capillary blood glucose (CBG) measurement 15 minutes later. "
        ),
        tags$li(
          "If the hypoglycemia is reversed a complex carbohydrate is served and a CBG is repeated 60 minutes later. "
        )
      ),
      tags$h2(strong("Instructions on using the interface
                     ")),
      br(),
      p("By clicking the tab above, you will be prompted to different page."),
      tags$ol(tags$li(
        em(
          strong("Data:"),
          "to specify your data folder, customize glucometrics thresholds, specify exlcusion criteria, and select periods and locations."
        ),
        tags$li(
          em(strong("Glucometrics:")),
          "to generate the glucometrics report."
        )#,
        # tags$li(
        #   em(strong("Timeliness:")),
        #   "to visualize the adherence to BG monitoring protocols."
        # ),
        # tags$li(
        #   em(strong("Hypoglycemia timeliness:")),
        #   "to visualize the the adherence to hypoglycemia management."
        # )
      )),
      tags$h3(strong("Sample data format")),
      tableOutput('sampledata'),
      tags$h2(strong("Glucometric definitions")),
      # imageOutput('glucometric_definition'),
      tags$div(img(src = "Glucometrics_definition.png", align = "left", width = 1000)),
      br(),
      br(),
      tags$div(p(
          "*Hyperglycemia index (HGI) is calculated by taking the area under the interpolated curve of all BG values within the admission above the cutoff value divided by the length of stay [1]."
        ),
       p(
          "**J-index is computed as a constant factor of the square of the sum of mean and SD of BG. The factor is set to 0.324 when unit is mmol/L and 0.001 when unit is mg/dL [2]."
        )),
      p('References:'),
      tags$ol(
        tags$li(
          "Vogelzang M, van der Horst IC, Nijsten MW. Hyperglycaemic index as a tool to assess glucose control: a retrospective study. Crit Care 2004;8(3):R122-7. doi: 10.1186/cc2840."
        ),
        tags$li(
          "Standl, E., O. Schnell, and A. Ceriello, Postprandial Hyperglycemia and Glycemic Variability. Should we care?, 2011. 34(Supplement 2): p. S120-S127 DOI: 10.2337/dc11-s206."
        )
      ),
      br()
    )
  ),
  # Tab: Data -----
  tabPanel(
    strong("Data"),
    sidebarLayout(
      sidebarPanel(
        width = 6, 
        # Select working directory =====
        htmlOutput("wkdir"),
        radioButtons("unitVal", label = "After selecting the data folder, please specify the unit of BG readings used in ALL data files within this folder:", 
                     choices = c("mmol/L" = 1, "mg/dL" = 2)),
        br(),
        submitButton("Confirm data folder and unit of BG readings"),
        hr(), 
        fluidRow(
          column(3, offset = 0.5,
                 h4("Hypo cutoff"),
                 htmlOutput('hypo3'),
                 htmlOutput('hypo2'),
                 htmlOutput('hypo1')
          ),
          column(3, h4("Hyper cutoff"), 
                 htmlOutput('hyper3'),
                 htmlOutput('hyper2'),
                 htmlOutput('hyper1')
          ),
          column(3, h4("Target range"),
                 
                 htmlOutput('normalrange_lower'),
                 htmlOutput('normalrange_upper')
          ),
          column(3, h4("Hyperglycemia index (HGI)"),
                
                 htmlOutput('hgicutoff')
                       
          ) 
        ),
        hr(), 
        # Exclusion criteria =====
        # h4("Exclusion criteria"), 
        # Age range
        # sliderInput("ageRange", 
        #             "Only include patients with age within selected range (inclusive of boundary values):",
        #             min = 0, max = 120, value = c(16, 120)),
        # losNum
        h4("Exclusion criteria"),
        htmlOutput('exclusion_freq'),
        fluidRow(
          column(5, h4(""),
                 numericInput("freqNum", label = "",
                              min = 0, max = 10, value = 2)
                 
          )
        ), 
        
        # Other criteria
        htmlOutput("exclusion_los"), 
        fluidRow(
          column(5, h4(""),
                 numericInput("losNum_hour", label = "",
                              min = 0, max = 48, value = 24)
                 
          )
        ), 
        # htmlOutput("exclusion_first24"), 
        hr(), 
        tags$strong("After selecting the right data folder and parameters, please click on 'Confirm' buttom bellow before procedding to the panel on the right."),
        br(),
        actionButton("btn_confirm","Confirm"), 
        # Process data =====
        # Show data processing messages
        textOutput("batchMsg")
      ),
      mainPanel(
        width = 5,
        # Define patient episodes =====
        # h4("Defining patient episodes"), 
        # checkboxInput(
        #   inputId = "useAdmin",
        #   label = "Each admission id corresponds to one hospital stay",
        #   value = FALSE,
        #   width = "100%"
        # ),
        # hr(),
        # Select time unit: monthly or quarterly =====
        # radioButtons("timeUnit", NULL, inline = TRUE,
        #              c("Produce monthly report" = "monthly",
        #                "Produce quarterly report" = "quarterly"), "monthly"),
        # submitButton("Confirm choice"),
        # Input time range =====
        h4("Time range:"), 
        fluidRow(
          column(5, offset = 0.5, h4("From"),
                 numericInput("yearFrom", label = "Year",
                              min = 1900, max = 9999,
                              value = as.numeric(format(Sys.Date(), "%Y"))),
                 selectInput("monthFrom", label = "Month",
                             choices = as.list(1:12),
                             selected = as.numeric(format(Sys.Date(), "%m")))
          ),
          column(5, h4("To"),
                 numericInput("yearTo", label = "Year",
                              min = 1900, max = 9999,
                              value = as.numeric(format(Sys.Date(), "%Y"))),
                 selectInput("monthTo", label = "Month",
                             choices = as.list(1:12),
                             selected = as.numeric(format(Sys.Date(), "%m")))
          )
        ),
        # dateRangeInput("timeRange", label = h4("Time range")),
        textOutput("nMonthSelected"),
        submitButton("Confirm time range"),
        hr(),
        # Select wards =====
        h4("Wards"),
        radioButtons("wardMode", NULL, inline = TRUE,
                     c("All wards with data in the selected time range" = "all",
                       "Selected wards" = "select"),
                     "select"),
        submitButton("Confirm choice"),
        conditionalPanel(
          condition = "input.wardMode == 'select'",
          htmlOutput("selectWardUI")#,
          # p("Please do not select more than 12 wards.")
        ),
        submitButton("Confirm ward"),
        hr(),
        # textOutput("hasCaseID"), 
        tags$strong(
          "Number of unique patients in each selected ward and month:"
        ),
        dataTableOutput("summariseWards")
      )
    )
  ),
  # Tab: Glucometrics -----
  navbarMenu(
    strong("Glucometrics"),
    # Subtab: Overall -----
    tabPanel(
      "Summary Statistics", 
      HTML("<h4 style='text-indent:10%'>Data Summary</h4>"), 
      conditionalPanel(
        condition = "$('html').hasClass('shiny-busy')",
        tags$p("Producing table...", style = "text-align:center")
      ), 
      htmlOutput("tableQuality"), 
      HTML("<h4 style='text-indent:10%'>Exclusion Summary</h4>"), 
      conditionalPanel(
        condition = "$('html').hasClass('shiny-busy')",
        tags$p("Producing table...", style = "text-align:center")
      ), 
      htmlOutput("tableExclusion"), 
      HTML("<h4 style='text-indent:10%'>Glucometrics</h4>"), 
      conditionalPanel(
        condition = "$('html').hasClass('shiny-busy')",
        tags$p("Producing table...", style = "text-align:center")
      ), 
      htmlOutput("tableSummary"), 
      htmlOutput("tableGlucometrics"), 
      htmlOutput("tableGlucometricsLegend"),
      br(),
      textOutput("saveMsg")
    )
  )
  # tabPanel(strong("License"),
  #   tags$div(
  #   HTML('<iframe src="license.htm" width="100%" style="height: 35em;"></iframe>'))
  #   )
))
