# LIBRARY--------------------------- 
library(dplyr)
library(stats)
library(devtools)
library(ggplot2)
library(caret)
library(e1071)
library(shiny)
library(shinydashboard)
library(plotly)
library(rpart.plot)
library(rattle)
library(DT)
library(caTools)
library(shinycssloaders)
library(shinyWidgets)
library(wordcloud)
library(RColorBrewer)
library(formattable)
library(lime)
library(mice)
library(VIM)
library(xgboost)
library(randomForest)
library(C50)

data_sets <- list(
  'Select a dataset' = "Selection",
  'Brazil Set' = read.csv("data/brazil.csv", fileEncoding="latin1"),
  'Wuhan Set' = read.csv("data/completedWUHAN.csv", fileEncoding="latin1")
)

test_data_sets <- list(
  'Select a test dataset' = "Selection",
  'Brazil Test Set' = read.csv("data/testbrazil.csv", fileEncoding="latin1"),
  'Wuhan Test Set' = read.csv("data/testwuhan.csv", fileEncoding="latin1")
)

# UI-------------------
#0275D8, 
options(spinner.color="#E53935", spinner.color.background="#ffffff", spinner.size=1)

ui <- dashboardPage(
  #banner color  
  skin = "red",
  title = "Covid19PredictoR",
  
  # HEADER ---------------------------
  dashboardHeader(
    #banner title
    title = "Covid19PredictoR",
    dropdownMenu(
    )
    ),

    # SIDEBAR --------------------------
    dashboardSidebar(
      
      # Left Menu
      sidebarMenu(
            
        #Home    
        menuItem(
          "Home",
          tabName = "Home",
          icon = icon("home")
          ),
        
        #Data Input, choose csv file
        menuItem(
          "Step 1: Input data",
          tabName = "filee",
          icon = icon("upload")
          ),

        #Summarize: Data and Structure
        menuItem(
          "Step 2: Summarize data",
          tabName = "datasum",
          icon = icon("info-circle")
          ),
        
        #Data partition split ratio then traindata and testdata 
        menuItem(
          "Step 3: Set up for training",
          tabName = "choose",
          icon = icon("sliders")
          ),
            
        #Preprocess data
        menuItem(
            "Step 4: Pre-process data",
            tabName = "feat",
            icon = icon("table")
            ), 
            
        #Classification
        menuItem(
          "Step 5: Train ML models",
          tabName = "classmethod",
          icon = icon("chart-bar")
          ),
        
        #Test Data Input, choose csv file
        menuItem(
          "Step 6: Input test data",
          tabName = "filet",
          icon = icon("upload")
        ),
        
        #Prediction
        menuItem(
          "Step 7: Predict risk",
          tabName = "pred",
          icon = icon("question")
        ),
        
        div(tags$img(style="height:150px;width:150px",
                     src="cov19.png"),
            style="text-align:center;"
            ),

        div(tags$img(style="height:112px;width:174px",
                     src="bdl2.jpg"),
            style="text-align:center;"
        )
        )
      ),
    
    # BODY -------------------------------
    dashboardBody(
      tabItems(
        # Home ----------
        # This part is for HOME TAB
        tabItem(
          tabName = "Home",
          h3("Covid19PredictoR v1.0: A machine learning pipeline and web-based interface to predict risk of Covid-19",
             align = "center"),
          p("Covid19PredictoR is an R based  software which develops machine learning models to predict risk of Covid-19"),
          h5(strong("Models you can develop")),
          p("You can develop logistic regression, C5.0, decision tree, random forest and XGBoost model with Covid19PredictoR."),
          h5(strong("Functionalities")),
          HTML("<p>This framework includes summary statistics and data visualization, <a href='https://topepo.github.io/caret/pre-processing.html'> pre-processing </a> and tuning options <a href='https://topepo.github.io/caret/model-training-and-tuning.html#control'>(train control methods)</a>. User can develop and validate the model above and can test the model of interest for new patients in prediction module.  "),
          h5(strong("Using Covid19PredictoR")),
          p("The steps for generating results are as follows:"),
          ("1) Import your data"),
          br(),
          ("2) Check summaries for better data processing"),
          br(),
          ("3) Define and apply parameters for model development"),
          br(),
          ("4) Define and apply methods for data preprocessing"),
          br(),
          ("5) Train machine learning models and check for high risk factors"),
          br(),
          ("6) Import data of new patients for prediction"),
          br(),
          ("7) Use machine learning models for risk evaluation of new patients"),
           # Navigate results and download them for your reports
          br(),
          h5 ("Project team: "),
          h5("Volkan Kapucu,",span("volkan@mu.edu.tr",style = "color:blue")),
          h5("Sultan Turhan,",span("sultantrhn@hotmail.com",style = "color:blue")),
          h5("Eralp Dogu,",span("eralp.dogu@mu.edu.tr",style = "color:blue")),
          br(),
          ("BioData Lab"),
          br(),
          ("College of Science"),
          br(),
          ("Mugla Sitki Kocman University"),
          br(),
          ("Mugla, Turkey, 48000"),
          br(),
          h5(strong("Cite:")),
          ("Kapucu V, Turhan S, Dogu E. (2021). Covid19PredictoR v1.0: A machine learning pipeline and web-based interface to predict risk of Covid-19."),
          h3(""),
          h3(""),
          h4("")
          ),
        
        #Uploud Data ------
        tabItem(
          tabName = "filee",
          fluidRow(
            column(12,
                   uiOutput("choisedata")
            ),
            box(
              title = "Upload a Data",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = F,
              #closable = T,
              status = "primary",
              fileInput(
                inputId = "file1",
                label = "",
                accept = c("text/csv", "text/comma-seperated-values,text/plain", ".csv")),
              
              h4("Header ?"),
              checkboxInput(inputId = "header",
                            label = "Header",
                            value = TRUE),
              
              h4("AsFactors ?"),
              checkboxInput(inputId = "stringAsFactors",
                            label = "stringAsFactors",
                            value = FALSE),
                        
              h4("Separetor ?"),
              radioButtons(inputId = "sep",
                           label = "Separator",
                           choices = c(Comma=",", Semicolon=";", Tab="\t"),
                           selected = ','
                           )
              ),

            box(
              title = "Select Sample Data",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = F,
              #closable = T,
              status = "danger",
              selectInput(inputId = "crntdata",
                          label = "",
                          choices = names(data_sets))
              )
            )
            
          ),
        
        #Data Summary --------------
        tabItem(
          tabName = "datasum",
          h2("Descriptive analysis of data"),
          fluidRow(
            
            box(title = "Data table",
                solidHeader = T,
                width = 10,
                collapsible = T,
                collapsed = T,
                status = "info",
                uiOutput("checkbox"),
                dataTableOutput("contents")
                ),
            
            box(title = "Data structure",
                solidHeader = T,
                width = 10,
                collapsible = T,
                collapsed = T,
                status = "success",
                verbatimTextOutput("str")
                ),
            
            box(title = "Descriptive statistics and visualization",
                solidHeader = T,
                width = 10,
                height = "100%",
                collapsible = T,
                collapsed = T,
                status = "primary",
                #selectInput(
                #  inputId = "ycol",
                #  label = "Select Y variable",
                #  choices = names(data())
                #),
                h5("Select the variable below for which you want to see descriptive statistics against the response variable."),
                selectInput(
                  inputId = "xcol",
                  label = "Select X variable",
                  choices = names(data())
                ),
               # verbatimTextOutput("sumrz"),
                plotlyOutput("varplot",
                           width = "100%",
                           height = "300px")
                #plotOutput("missing")
                )
             )
          ),
        
        #Partition, fold, PrePRocess, TrCont------------
        tabItem(
          tabName = "choose",
          #h2("You can set data partitioning, K-fold and pre-process options here."),
          fluidRow(
            box(title = "Step 1: Set data partition ratio (For training and testing)",
                solidHeader = T,
                width = 10 ,
                collapsible = T,
                collapsed = F,
                status = "primary",
                sliderInput(inputId = "part",
                            label = "split",
                            min = 0.1,
                            max = 0.9,
                            value = 0.75)
                ),
            
            box(title = "Step 2: Set k for k-fold cross-validation (For training)",
                solidHeader = T,
                width = 10 ,
                collapsible = T,
                collapsed = F,
                status = "success",
                sliderInput(inputId = "fld",
                            label = "k-fold",
                            min = 1,
                            max = 15,
                            value = 10)
                ),
            
            box(title = "Step 3: Choose pre-processing method",
                solidHeader = T,
                width = 10 ,
                collapsible = T,
                collapsed = F,
                status = "danger",
                prettyCheckboxGroup(inputId = "chsprep",
                                   label = "Pre-processes",
                                   inline = T,
                                   status = "danger",
                                   shape = c("curve"),
                                   outline = T,
                                   choiceNames = list(
                                     "Center",
                                     "Scale",
                                     "Range",
                                     "ZV",
                                     "NZV",
                                     "Corr",
                                     "BoxCox",
                                     "YeoJohnson",
                                     "Expo Trans",
                                     "Knn Impute",
                                     "Bag Impute",
                                     "Median Impute",
                                     "PCA",
                                     "ICA",
                                     "Spatial Sign",
                                     "Conditional X"),
                                   choiceValues = list(
                                     "center",
                                     "scale",
                                     "range",
                                     "zv",
                                     "nzv",
                                     "corr",
                                     "BoxCox",
                                     "YeoJohnson",
                                     "expoTrans",
                                     "knnImpute",
                                     "bagImpute",
                                     "medianImpute",
                                     "pca",
                                     "ica",
                                     "spatialSign",
                                     "conditionalX"),
                                   selected = c("center", "scale", "nzv")
                                   )
                ),
            
            box(title = "Step 4: Choose train control method",
                solidHeader = T,
                width = 10 ,
                collapsible = T,
                collapsed = F,
                status = "warning",
                prettyRadioButtons(inputId = "chsmthd",
                                   label = "Methods",
                                   inline = T,
                                   status = "warning",
                                   outline = T,
                                   choiceNames = list(
                                     "Boot",
                                     "Boot632",
                                     "Optimism Boot",
                                     "Boot All",
                                     "CV (Cross Validation)",
                                     "Repeated CV",
                                     "LOOCV (Leave-One-Out CV)",
                                     "LGOCV",
                                     "None",
                                     "Out of Bag",
                                     "Adaptive CV",
                                     "Adaptive Boot",
                                     "Adaptive LGOCV"),
                                   choiceValues = list(
                                     "boot",
                                     "boot632",
                                     "optimism_boot",
                                     "boot_all",
                                     "cv",
                                     "repeatedcv",
                                     "LOOCV",
                                     "LGOCV",
                                     "none",
                                     "oob",
                                     "adaptive_cv",
                                     "adaptive_boot",
                                     "adaptive_LGOCV"),
                                   selected = c("cv")
                                   )
            )
            )
          ),       
  
        #Pre-Proccessing------------
        tabItem(
          tabName = "feat",
          h2("Data table after pre-processing"),
          fluidRow(
             tabBox(width = "100%",
                   title = h3(""),
                   id = "tabset1",
                   height = "250px",
                   tabPanel(
                     title =  "",(dataTableOutput("preproc"))
                     )
                   )
             )
          ),
            
        #Clasifications------------
        tabItem(
              tabName = "classmethod",
              h2("Performance evaluation"),
              fluidRow(
                tabBox(
                  width = "100%",
                  #title = h3("Classification"),
                  id = "tabset1",
                  #height = "250px",
                  tabPanel(
                    #icon = icon("info-circle"),
                    title =  "",
                    ""
                  ),
                  #LR---------
                  tabPanel(
                    icon = icon("circle"),
                    title =  "Logistic regression",
                    box(title = "Performance",
                        width = 6,
                        status = "warning",
                        withSpinner(verbatimTextOutput("glm"))
                    ),
                    box(title = "Risk evaluation",
                        width = 6,
                        status = "warning",
                        withSpinner(plotOutput("wordglm"))
                    ),
                    box(title = "Important Risks",
                        width = 6,
                        status = "warning",
                        withSpinner(verbatimTextOutput("imp_glm"))
                    )
                  ),
                  #C5---------
                  tabPanel(
                    icon = icon("circle"),
                    title =  "C5.0",
                    box(title = "Performance",
                        width = 6,
                        status = "primary",
                        withSpinner(verbatimTextOutput("cfive"))
                        ),
                    box(title = "Risk evaluation",
                        width = 6,
                        status = "primary",
                        withSpinner(plotOutput("wordcfive"))
                        ),
                    box(title = "Important Risks",
                        width = 6,
                        status = "primary",
                        withSpinner(verbatimTextOutput("imp_cfive"))
                        )
                  ),
                  #DT---------
                  tabPanel(
                    icon = icon("circle"),
                    title =  "Decision tree",
                    box(title = "Performance",
                        width = 6,
                        status = "success",
                        withSpinner(verbatimTextOutput("dtree"))
                        ),
                    box(title = "Risk evaluation",
                        width = 6,
                        status = "success",
                        withSpinner(plotOutput("worddtree"))
                        ),
                    box(title = "Important Risks",
                        width = 6,
                        status = "success",
                        withSpinner(verbatimTextOutput("imp_dtree"))
                        ),
                    box(title = "Tree",
                        width = 6,
                        status = "success",
                        withSpinner(plotOutput("tree_dtree"))
                        )
                    ),
                  #RF---------
                  tabPanel(
                    icon = icon("circle"),
                    title =  "Random forest",
                    box(title = "Performance",
                        width = 6,
                        status = "info",
                        withSpinner(verbatimTextOutput("rf"))
                        ),
                    box(title = "Risk evaluation",
                        width = 6,
                        status = "info",
                        withSpinner(plotOutput("wordrf"))
                        ),
                    box(title = "Important Risks",
                        width = 6,
                        status = "info",
                        withSpinner(verbatimTextOutput("imp_rf"))
                        )
                    ),
                  #XGB---------
                  tabPanel(
                    icon = icon("circle"),
                    title =  "XGBoost",
                    box(title = "Performance",
                        width = 6,
                        status = "danger",
                        withSpinner(verbatimTextOutput("xgb"))
                        ),
                    box(title = "Risk evaluation",
                        width = 6,
                        status = "danger",
                        withSpinner(plotOutput("wordxgb"))
                        ),
                    box(title = "Important Risks",
                        width = 6,
                        status = "danger",
                        withSpinner(verbatimTextOutput("imp_xgb"))
                        )
                    ),
                  #CompMdls---------
                  tabPanel(
                    icon = icon("compress-arrows-alt"),
                    title =  "Comparison of models",
                    box(title = "",
                        width = 8,
                        status = "primary",
                        withSpinner(verbatimTextOutput("compare"))),
                    box(title = "",
                        width = 4,
                        status = "primary",
                        withSpinner(plotOutput("pltcmpr"))
                        )
                    )
                  )
                )
              ),

        #Uploud Test Data ------
        tabItem(
          tabName = "filet",
          fluidRow(
            column(12,
                   uiOutput("choisetestdata")
            ),
            box(
              title = "Upload a Test Data",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = F,
              #closable = T,
              status = "primary",
              fileInput(
                inputId = "file2",
                label = "",
                accept = c("text/csv", "text/comma-seperated-values,text/plain", ".csv")),
              
              h4("Header ?"),
              checkboxInput(inputId = "testheader",
                            label = "Header",
                            value = TRUE),
              
              h4("AsFactors ?"),
              checkboxInput(inputId = "teststringAsFactors",
                            label = "stringAsFactors",
                            value = FALSE),
              
              h4("Separetor ?"),
              radioButtons(inputId = "testsep",
                           label = "Separator",
                           choices = c(Comma=",", Semicolon=";", Tab="\t"),
                           selected = ','
              )
            ),
            
            box(
              title = "Select Sample Test Data",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = F,
              #closable = T,
              status = "danger",
              selectInput(inputId = "crnttestdata",
                          label = "",
                          choices = names(test_data_sets))
            )
          )
          
        ),

        
        #Pred Result------------
        tabItem(
          tabName = "pred",
          h2("Prediction results"),
          fluidRow(
            tabBox(
              width = "100%",
              #title = h3("Classification"),
              id = "tabset1",
              #height = "250px",
              tabPanel(
                #icon = icon("info-circle"),
                title =  "",
                ""
              ),
              tabPanel(
                icon = icon("circle"),
                title =  "Logistic regression",
                #box(title = "Performance",
                #    width = 7,
                #    status = "warning",
                #    withSpinner(dataTableOutput("pred_glm_result"))
                #),
                box(title = "Which patient's outcome do you want to know?",
                    solidHeader = T,
                    width = 6,
                    height = 140,
                    collapsible = T,
                    collapsed = F,
                    status = "success",
                    numericInput(inputId = "num_pat_glm",
                                label = "Enter the patient's sequence number ",
                                value = 1,
                                min = 1,
                                max = 10
                                )
                ),
                box(title = "Set number of features to explain the model",
                    solidHeader = T,
                    width = 6 ,
                    height = 140,
                    collapsible = T,
                    collapsed = F,
                    status = "success",
                    sliderInput(inputId = "num_feat_glm",
                                label = "",
                                min = 1,
                                max = 10,
                                value = 5)
                ),
                box(title = "",
                    width = 12,
                    #width = "100%",
                    #height = "100%",
                    status = "info",
                    withSpinner(plotOutput("plot_lime_glm"))
                )
              ),
              tabPanel(
                icon = icon("circle"),
                title =  "C5.0",
                #box(title = "Performance",
                #    width = 7,
                #    status = "primary",
                #    withSpinner(dataTableOutput("pred_cfive_result"))
                #),
                box(title = "Which patient's outcome do you want to know?",
                    solidHeader = T,
                    width = 6,
                    height = 140,
                    collapsible = T,
                    collapsed = F,
                    status = "success",
                    numericInput(inputId = "num_pat_c5",
                                 label = "Enter the patient's sequence number ",
                                 value = 1,
                                 min = 1,
                                 max = 10
                    )
                ),
                box(title = "Set number of features to explain the model",
                    solidHeader = T,
                    width = 6 ,
                    height = 140,
                    collapsible = T,
                    collapsed = F,
                    status = "success",
                    sliderInput(inputId = "num_feat_c5",
                                label = "",
                                min = 1,
                                max = 10,
                                value = 5)
                ),
                box(title = "",
                    width = 12,
                    #width = "100%",
                    #height = "100%",
                    status = "info",
                    withSpinner(plotOutput("plot_lime_c5"))
                )
              ),
              tabPanel(
                icon = icon("circle"),
                title =  "Decision tree",
                #box(title = "Performance",
                #    width = 7,
                #    status = "success",
                #    withSpinner(dataTableOutput("pred_dtree_result"))
                #),
                box(title = "Which patient's outcome do you want to know?",
                    solidHeader = T,
                    width = 6,
                    height = 140,
                    collapsible = T,
                    collapsed = F,
                    status = "success",
                    numericInput(inputId = "num_pat_dtree",
                                 label = "Enter the patient's sequence number ",
                                 value = 1,
                                 min = 1,
                                 max = 10
                    )
                ),
                box(title = "Set number of features to explain the model",
                    solidHeader = T,
                    width = 6 ,
                    height = 140,
                    collapsible = T,
                    collapsed = F,
                    status = "success",
                    sliderInput(inputId = "num_feat_dtree",
                                label = "",
                                min = 1,
                                max = 10,
                                value = 5)
                ),
                box(title = "",
                    width = 12,
                    #width = "100%",
                    #height = "100%",
                    status = "info",
                    withSpinner(plotOutput("plot_lime_dtree"))
                )
              ),
              tabPanel(
                icon = icon("circle"),
                title =  "Random forest",
                #box(title = "Performance",
                #    #width = 7,
                #    width = "80%",
                #    status = "info",
                #    withSpinner(dataTableOutput("pred_rf_result"))
                #),
                box(title = "Which patient's outcome do you want to know?",
                    solidHeader = T,
                    width = 6,
                    height = 140,
                    collapsible = T,
                    collapsed = F,
                    status = "success",
                    numericInput(inputId = "num_pat_rf",
                                 label = "Enter the patient's sequence number ",
                                 value = 1,
                                 min = 1,
                                 max = 10
                                 )
                    ),
                box(title = "Set number of features to explain the model",
                    solidHeader = T,
                    width = 6 ,
                    height = 140,
                    collapsible = T,
                    collapsed = F,
                    status = "success",
                    sliderInput(inputId = "num_feat_rf",
                                label = "",
                                min = 1,
                                max = 10,
                                value = 5)
                    ),
                box(title = "",
                    width = 12,
                    #width = "100%",
                    #height = "100%",
                    status = "info",
                    withSpinner(plotOutput("plot_lime_rf"))
                    )
                ),
              tabPanel(
                icon = icon("circle"),
                title =  "XGBoost",
                #box(title = "Performance",
                #    width = 7,
                #    status = "danger",
                #    withSpinner(dataTableOutput("pred_xgb_result"))
                #),
                box(title = "Which patient's outcome do you want to know?",
                    solidHeader = T,
                    width = 6,
                    height = 140,
                    collapsible = T,
                    collapsed = F,
                    status = "success",
                    numericInput(inputId = "num_pat_xgb",
                                 label = "Enter the patient's sequence number ",
                                 value = 1,
                                 min = 1,
                                 max = 10
                    )
                ),
                box(title = "Set number of features to explain the model",
                    solidHeader = T,
                    width = 6 ,
                    height = 140,
                    collapsible = T,
                    collapsed = F,
                    status = "success",
                    sliderInput(inputId = "num_feat_xgb",
                                label = "",
                                min = 1,
                                max = 10,
                                value = 5)
                ),
                box(title = "",
                    width = 12,
                    #width = "100%",
                    #height = "100%",
                    status = "info",
                    withSpinner(plotOutput("plot_lime_xgb"))
                )
              )
            )
          )
        )
        
 )
 
        )
      )
  

# SERVER Title ----------------
server <- function(input, output, session) {
  set.seed(1)
  output$choisedata <- renderUI({
    selectInput("chs",
                label = "",
                choices = list(
                  "Upload Your Data",
                  "Choose Sample Data"
                     ),
                selected = "Upload Your Data")
  })
  
  #This is for Upload Yor Data    
  data1 <- reactive({
    req(input$file1)    # require that the input is available
    inFile <- input$file1
    df <- read.csv(inFile$datapath,
                   header = input$header,
                   sep = input$sep,
                   stringsAsFactors = input$stringAsFactors
                   )
    names(df)[1]<-"covid"
    df$covid <- factor(df$covid, labels = c("no", "yes"))
    updateSelectInput(session, inputId = 'xcol', label = 'Select a risk factor',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Select outcome variable:',
                      choices = names(df), selected = names(df)[2])
    return(df)
    })
  
  data2 <- reactive({
    req(input$crntdata)
    df <- data_sets[[input$crntdata]]
    names(df)[1]<-"covid"
    df$covid <- factor(df$covid, labels = c("no", "yes"))
    updateSelectInput(session, inputId = 'xcol', label = 'Select risk factor',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Select outcome variable:',
                      choices = names(df), selected = names(df)[2])
    return(df)
  })
  
  data <- reactive({
    if (input$chs == "Upload Your Data") {
      datam <- data1()
    }
    else
    {
      datam <- data2()
    }

 })
  

  output$choisetestdata <- renderUI({
    selectInput("chstst",
                label = "",
                choices = list(
                  "Upload Your Test Data",
                  "Choose Sample Test Data"
                ),
                selected = "Upload Your Test Data")
  })
  
  detchoiseX <- reactive({
    detc <- select(data(), input$xcol)
    return(detc)
  })
  
  detchoiseY <- reactive({
    detch <- select(data(), input$ycol)
    return(detch)
  })
  
  partition <- reactive({
    prtn <- createDataPartition(data()[,1], p=input$part, list = FALSE)
    return(prtn)
  })

  train_data <- reactive ({
    trn_dt <- data()[partition(),]
    return(trn_dt)
  })
  
  test_data <- reactive ({
    tst_dt <- data()[-partition(),]
    return(tst_dt)
  })
  
  output$checkbox <- renderUI({
    checkboxGroupInput(inputId = "select_var", 
                       label = "Select variables", 
                       choices = names(data()),
                       inline = T
                       )
  })
  df_sel <- reactive({
    req(input$select_var)
    df_sel <- data() %>% select(input$select_var)
  })
  
  output$contents <- renderDataTable({
    DT::datatable(df_sel(),
                  extensions = "ColReorder",
                  options = list(pageLength=5,
                                 colReorder = TRUE,
                                 scrollX = TRUE,
                                 searching = F
                                 )
                  )
  })
  
  output$str <- renderPrint({
    str(data())
    })
    
  output$sumrz <- renderPrint({
    data() %>%
      #group_by(detchoiseY()) %>%
      group_by(data()[,1]) %>%
      summarize(
        n_cases  = n(),
        Min = min(detchoiseX(), na.rm=TRUE),
        Max = max(detchoiseX(), na.rm=TRUE),
        Mean= mean(as.numeric(detchoiseX()[[1]]), na.rm=TRUE),
        Median=median(as.numeric(detchoiseX()[[1]]), na.rm=TRUE),
        Std=sd(as.numeric(detchoiseX()[[1]]), na.rm=TRUE)
      )

    })

  output$varplot <- renderPlotly({
    
    plot_ly(data(),
            y = ~get(input$xcol),
            x = data()[,1],
            color = "red",
            type = "violin"
            #boxpoints = "all",
            #jitter = 0.3,
            #pointpos = -1.8
    )
  })
  
  output$missing <- renderPlot({
    aggr_plot <- aggr(data(),
                      col=c('navyblue','red'),
                      numbers=TRUE,
                      sortVars=TRUE,
                      labels=names(data()),
                      cex.axis=.7,
                      gap=3,
                      ylab=c("Histogram of missing data","Pattern"))
    #mice::md.pattern(data())
  })
  

# Choosing pre-process------------
    txt1 <- reactive ({
      preppp <- paste(input$chsprep, sep = ",")
      return(preppp)
    })
    
    output$preproc <- renderDataTable({
      nr <- ncol(data())
      preProcess_model <- preProcess(train_data()[, 1:nr],
                                           method=c(txt1()), 
                                           rangeBounds = c(0,1))
      train_data <- predict(preProcess_model,
                            newdata = train_data()[, 1:nr])
      DT::datatable(train_data[,-1], extensions = "ColReorder",
                    options = list(pageLength=5,
                                   colReorder = TRUE,
                                   scrollX = TRUE))
      })

# Choosing method------------
    txt2 <- reactive ({
      preppp <- paste(input$chsmthd, sep = ",")
      return(preppp)
    })
    
    control <- reactive({
      cntrl <- trainControl(method=txt2(),
                              number=input$fld,
                              repeats=1) 
      return(cntrl)
    })
    
    # Classifications--------------
    #GLM-----------
    glm <- reactive ({
      set.seed(1)
      mdl<- train((covid)~.,
                  data=train_data(),
                  method = "glm",
                  trControl=control(),
                  preProc = c(txt1()),
                  na.action=na.pass)
      return(mdl)
    })
    
    order_importance_glm <- reactive({
      importance_mdl<-data.frame(varImp(glm())$importance)
      importance_mdl$Vars<-row.names(importance_mdl)
      ord_imp_mdl<-importance_mdl[order(-importance_mdl$Overall),][1:20,]
      return(ord_imp_mdl)
    })
    
    test_glm <- reactive({
      test_mdl <- predict(glm(),
                          test_data(),
                          na.action=na.pass,
                          prob=TRUE)
      return(test_mdl)
    })
    
    cm_glm <- reactive({
      conf_mtrx_mdl <- confusionMatrix(test_data()$covid,
                                       test_glm(),
                                       positive='yes',
                                       mode = "everything")
      return(conf_mtrx_mdl)
    })
    
    output$glm<- renderPrint({
      print(cm_glm())
      print(glm())
    })
    
    output$imp_glm <- renderPrint({
      print(order_importance_glm()[1:5,])
    })
    
    output$wordglm <- renderPlot({
      wordcloud(words = order_importance_glm()$Vars, freq = order_importance_glm()$Overall, scale=c(1.99,.5), min.freq = 1,
                max.words=2000, random.order=TRUE, rot.per=0.1, 
                colors=brewer.pal(8, "Paired"))
    })
    
    #C5.0-----------
    c5 <- reactive ({
      set.seed(1)
      mdl<- train((covid)~.,
                   data=train_data(),
                   method = "C5.0",
                   trControl=control(),
                   preProc = c(txt1()),
                   na.action=na.pass)
      return(mdl)
    })
    
    order_importance_c5 <- reactive({
      importance_mdl<-data.frame(varImp(c5())$importance)
      importance_mdl$Vars<-row.names(importance_mdl)
      ord_imp_mdl<-importance_mdl[order(-importance_mdl$Overall),][1:20,]
      return(ord_imp_mdl)
    })  

    test_c5 <- reactive({
      test_mdl <- predict(c5(),
                          test_data(),
                          na.action=na.pass,
                          prob=TRUE)
      return(test_mdl)
    })

    cm_c5 <- reactive({
      conf_mtrx_mdl <- confusionMatrix(test_data()$covid,
                                       test_c5(),
                                       positive='yes',
                                       mode = "everything")
      return(conf_mtrx_mdl)
    })
    
    output$cfive <- renderPrint({
      print(cm_c5())
      print(c5())
    })
    
    output$imp_cfive <- renderPrint({
      print(order_importance_c5()[1:5,])
    })
    
    output$wordcfive <- renderPlot({
      wordcloud(words = order_importance_c5()$Vars, freq = order_importance_c5()$Overall, scale=c(1.5,.5), min.freq = 1,
                max.words=2000, random.order=TRUE, rot.per=0.1, 
                colors=brewer.pal(8, "Paired"))
    })

    #Decision Tree----
    dtree <- reactive ({
      set.seed(1)
      mdl<- train((covid)~.,
                   data=train_data(),
                   method = "rpart",
                   trControl=control(),
                   preProc = c(txt1()),
                   na.action=na.pass)
      return(mdl)
    })
  
    order_importance_dt <- reactive({
      importance_mdl<-data.frame(varImp(dtree())$importance)
      importance_mdl$Vars<-row.names(importance_mdl)
      ord_imp_mdl<-importance_mdl[order(-importance_mdl$Overall),][1:20,]
      return(ord_imp_mdl)
    })    

    test_dt <- reactive({
      test_mdl <- predict(dtree(),
                         test_data(),
                         na.action=na.pass,
                         prob=TRUE)
      return(test_mdl)
    })

    cm_dt <- reactive({
      conf_mtrx_mdl <- confusionMatrix(test_data()$covid,
                                test_dt(),
                                positive='yes',
                                mode = "everything")
      return(conf_mtrx_mdl)
    })
            
    output$dtree <- renderPrint({
      print(cm_dt())
      print(dtree())
    })
    
    output$imp_dtree <- renderPrint({
      print(order_importance_dt()[1:5,])
    })

    output$worddtree <- renderPlot({
      wordcloud(words = order_importance_dt()$Vars, freq = order_importance_dt()$Overall, scale=c(2.1,.4), min.freq = 1,
                max.words=2000, random.order=TRUE, rot.per=0.1, 
                colors=brewer.pal(8, "Paired"))
    })
    
    output$tree_dtree <- renderPlot({
      fancyRpartPlot(dtree()$finalModel)
    })
    
    #Random Forest----
    rf <- reactive ({
      set.seed(1)
      mdl<- train((covid)~.,
                  data=train_data(),
                  method = "rf",
                  trControl=control(),
                  preProc = c(txt1()),
                  na.action=na.pass)
      return(mdl)
    })

    order_importance_rf <- reactive({
      importance_mdl<-data.frame(varImp(rf())$importance)
      importance_mdl$Vars<-row.names(importance_mdl)
      ord_imp_mdl<-importance_mdl[order(-importance_mdl$Overall),][1:20,]
      return(ord_imp_mdl)
    })
    
    test_rf <- reactive({
      test_mdl <- predict(rf(),
                          test_data(),
                          na.action=na.pass,
                          prob=TRUE)
      return(test_mdl)
    })

    cm_rf <- reactive({
      conf_mtrx_mdl <- confusionMatrix(test_data()$covid,
                                       test_rf(),
                                       positive='yes',
                                       mode = "everything")
      return(conf_mtrx_mdl)
    })
        
    output$rf <- renderPrint({
      print(cm_rf())
      print(rf())
      })
    
    output$imp_rf <- renderPrint({
      print(order_importance_rf()[1:5,])
      })
    
    output$wordrf <- renderPlot({
      wordcloud(words = order_importance_rf()$Vars, freq = order_importance_rf()$Overall, scale=c(2,.1), min.freq = 1,
                max.words=2000, random.order=TRUE, rot.per=0.1, 
                colors=brewer.pal(8, "Paired"))
    })
    
    #XGBoost------
    xgb <- reactive ({
      set.seed(1)
      mdl<- train((covid)~.,
                  data=train_data(),
                  method = "xgbTree",
                  trControl=control(),
                  preProc = c(txt1()),
                  na.action=na.pass,
                  verbosity = 0)
      return(mdl)
    })
    
    order_importance_xgb <- reactive({
      importance_mdl<-data.frame(varImp(xgb())$importance)
      importance_mdl$Vars<-row.names(importance_mdl)
      ord_imp_mdl<-importance_mdl[order(-importance_mdl$Overall),][1:20,]
      return(ord_imp_mdl)
    })
    
    test_xgb <- reactive({
      test_mdl <- predict(xgb(),
                          test_data(),
                          prob=TRUE,
                          na.action=na.pass)
      return(test_mdl)
    })
    
    cm_xgb <- reactive({
      conf_mtrx_mdl <- confusionMatrix(test_data()$covid,
                                       test_xgb(),
                                       positive='yes',
                                       mode = "everything")
      return(conf_mtrx_mdl)
    })
    
    output$xgb<- renderPrint({
      print(cm_xgb())
      print(xgb())
    })
    
    output$imp_xgb <- renderPrint({
      print(order_importance_xgb()[1:5,])
    })
    
    output$wordxgb <- renderPlot({
      wordcloud(words = order_importance_xgb()$Vars, freq = order_importance_xgb()$Overall, scale=c(2.7,.3), min.freq = 1,
                  max.words=2000, random.order=TRUE, rot.per=0.1, 
                  colors=brewer.pal(8, "Paired"))
    })
    
    #Compare Model---------
    modelscompare <- reactive({
      Sys.sleep(1) # system sleeping for 1 seconds for demo purpose
      mdlcmpr <- caret::resamples(list(C50=c5(),
                                       DTree=dtree(),
                                       RandomForest=rf(),
                                       GLM=glm(),
                                       XGBoost=xgb()))
      #summary(mdlcmpr)
      return(mdlcmpr)
    })
    
    output$compare <- renderPrint ({
      summary(modelscompare())
    })
    
    output$pltcmpr <- renderPlot ({
      scales <- list(x=list(relation="free"), y=list(relation="free"))
      bwplot(modelscompare(), scales=scales)
    })
    
    # Predict Upload -----    
    #This is for Upload Test Data    
    data3 <- reactive({
      req(input$file2)
      inFilet <- input$file2
      dftst <- read.csv(inFilet$datapath,
                        header = input$testheader,
                        sep = input$testsep,
                        stringsAsFactors = input$teststringAsFactors
      )
#      updateSelectInput(session, inputId = 'tstxcol', label = 'Select a risk factor',
#                        choices = names(dftst), selected = names(dftst))
#      updateSelectInput(session, inputId = 'tstycol', label = 'Select outcome variable:',
#                        choices = names(dftst), selected = names(dftst)[2])
      return(dftst)
    })
    
    data4 <- reactive({
      req(input$crnttestdata)
      dftst <- test_data_sets[[input$crnttestdata]]
#      updateSelectInput(session, inputId = 'tstxcol', label = 'Select risk factor',
#                        choices = names(dftst), selected = names(dftst))
#      updateSelectInput(session, inputId = 'tstycol', label = 'Select outcome variable:',
#                        choices = names(dftst), selected = names(dftst)[2])
      return(dftst)
    })
    
    datatst <- reactive({
      if (input$chstst == "Upload Your Test Data") {
        datat <- data3()
      }
      else
      {
        datat <- data4()
      }
      
    })
    
    
# GLM prediction -----
    pred_glm <- reactive({
      set.seed(1)
      test_pred <- predict(glm(),
                          datatst(),
                          type="raw",
                          #type="prob",
                          na.action=na.pass)
      return(test_pred)
    })

    output$pred_glm_result<- renderDataTable({
      glmdt<-data.frame(Result_for_GLM=pred_glm())
      DT::datatable(glmdt, extensions = "ColReorder",
                    options = list(pageLength=5,
                                   colReorder = TRUE,
                                   scrollX = TRUE))
    })
    
    last_glm_data <- reactive({
      set.seed(1)
      last_pred <- cbind(pred_glm(), datatst())
      return(last_pred)
    })
    
    explainer_glm <- reactive({
      set.seed(1)
      explainer <- lime(train_data(), glm())
      return(explainer)
    })
    
    explanation_glm <- reactive({
      set.seed(1)
      explanation <- explain(last_glm_data()[input$num_pat_glm,], explainer_glm(), quantile_bins=T, n_labels = 1, n_features = input$num_feat_glm)
      return(explanation)
    })
    
    output$plot_lime_glm <- renderPlot ({
      plot_features(explanation_glm()) +
        ggtitle("Logistic Regression") +
        theme(text = element_text(size = 14))
    })
    

# C5 prediction -----
    pred_cfive <- reactive({
      set.seed(1)
      test_pred <- predict(c5(),
                           datatst(),
                           type="raw",
                           #type="prob",
                           na.action=na.pass)
      return(test_pred)
    })
    
    output$pred_cfive_result<- renderDataTable({
      c5dt<-data.frame(Result_for_C5.0=pred_cfive())
      DT::datatable(c5dt, extensions = "ColReorder",
                    options = list(pageLength=5,
                                   colReorder = TRUE,
                                   scrollX = TRUE))
    })
    
    last_c5_data <- reactive({
      set.seed(1)
      last_pred <- cbind(pred_cfive(), datatst())
      return(last_pred)
    })
    
    explainer_c5 <- reactive({
      set.seed(1)
      explainer <- lime(train_data(), c5())
      return(explainer)
    })
    
    explanation_c5 <- reactive({
      set.seed(1)
      explanation <- explain(last_c5_data()[input$num_pat_c5,], explainer_c5(), quantile_bins=T, n_labels = 1, n_features = input$num_feat_c5)
      return(explanation)
    })
    
    output$plot_lime_c5 <- renderPlot ({
      plot_features(explanation_c5()) +
        ggtitle("C5.0") +
        theme(text = element_text(size = 14))
    })
    
# D.tree prediction -----
    pred_dtree <- reactive({
      set.seed(1)
      test_pred <- predict(dtree(),
                           datatst(),
                           type="raw",
                           #type="prob",
                           na.action=na.pass)
      return(test_pred)
    })
    
    output$pred_dtree_result<- renderDataTable({
      dtdt<-data.frame(Result_for_D.Tree=pred_dtree())
      DT::datatable(dtdt, extensions = "ColReorder",
                    options = list(pageLength=5,
                                   colReorder = TRUE,
                                   scrollX = TRUE))
    })
    
    last_dtree_data <- reactive({
      set.seed(1)
      last_pred <- cbind(pred_dtree(), datatst())
      return(last_pred)
    })
    
    explainer_dtree <- reactive({
      set.seed(1)
      explainer <- lime(train_data(), dtree())
      return(explainer)
    })
    
    explanation_dtree <- reactive({
      set.seed(1)
      explanation <- explain(last_dtree_data()[input$num_pat_dtree,], explainer_dtree(), quantile_bins=T, n_labels = 1, n_features = input$num_feat_dtree)
      return(explanation)
    })
    
    output$plot_lime_dtree <- renderPlot ({
      plot_features(explanation_dtree()) +
        ggtitle("Decision Tree") +
        theme(text = element_text(size = 14))
    })
    

# RF prediction -----
    pred_rf <- reactive({
      set.seed(1)
      test_pred <- predict(rf(),
                           datatst(),
                           type="raw",
                           #type="prob",
                           na.action=na.pass)
      return(test_pred)
    })
    
    output$pred_rf_result<- renderDataTable({
      rfdt<-data.frame(Result_for_RF=pred_rf())
      DT::datatable(rfdt, extensions = "ColReorder",
                    options = list(pageLength=5,
                                   colReorder = TRUE,
                                   scrollX = TRUE))
    })
    
    last_rf_data <- reactive({
      set.seed(1)
      last_pred <- cbind(pred_rf(), datatst())
      return(last_pred)
    })
    
    explainer_rf <- reactive({
      set.seed(1)
      explainer <- lime(train_data(), rf())
      return(explainer)
    })
    
    explanation_rf <- reactive({
      set.seed(1)
      explanation <- explain(last_rf_data()[input$num_pat_rf,], explainer_rf(), quantile_bins=T, n_labels = 1, n_features = input$num_feat_rf)
      return(explanation)
    })
    
    output$plot_lime_rf <- renderPlot ({
      plot_features(explanation_rf()) +
        ggtitle("Random Forest") +
        theme(text = element_text(size = 14))
    })
    
# XGB prediction -----
    pred_xgb <- reactive({
      set.seed(1)
      test_pred <- predict(xgb(),
                           datatst(),
                           type="raw",
                           #type="prob",
                           na.action=na.pass)
      return(test_pred)
    })
    
    output$pred_xgb_result<- renderDataTable({
      xgbdt<-data.frame(Result_for_XGB=pred_xgb())
      DT::datatable(xgbdt, extensions = "ColReorder",
                    options = list(pageLength=5,
                                   colReorder = TRUE,
                                   scrollX = TRUE))
    })      
    
    last_xgb_data <- reactive({
      set.seed(1)
      last_pred <- cbind(pred_xgb(), datatst())
      return(last_pred)
    })
    
    explainer_xgb <- reactive({
      set.seed(1)
      explainer <- lime(train_data(), xgb())
      return(explainer)
    })
    
    explanation_xgb <- reactive({
      set.seed(1)
      explanation <- explain(last_xgb_data()[input$num_pat_xgb,], explainer_xgb(), quantile_bins=T, n_labels = 1, n_features = input$num_feat_xgb)
      return(explanation)
    })
    
    output$plot_lime_xgb <- renderPlot ({
      plot_features(explanation_xgb()) +
        ggtitle("XGBoost") +
        theme(text = element_text(size = 14))
    })
    
              
}
shinyApp(ui, server)
