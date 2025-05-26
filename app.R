# LIBRARY---- 
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
#library(DiagrammeR)
library(rattle)
library(DT)
library(caTools)
library(shinycssloaders)
library(shinyWidgets)
library(wordcloud)
library(RColorBrewer)
library(formattable)
#devtools::install_github('skinner927/reprtree')
#library(reprtree)

data_sets <- list(
  'Select a dataset' = "Selection",
  'Brazil Set' = read.csv("data/brazil.csv", fileEncoding="latin1"),
  'Wuhan Set' = read.csv("data/wuhan.csv", fileEncoding="latin1")
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
  title = "CoVID19PredictoR",
  
  # HEADER ---------------------------
  dashboardHeader(
    #banner title
    title = "CoVID19PredictoR",
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
      #tabsetPanel(),
      tabItems(
        # Home ----------
        # This part is for HOME TAB
        tabItem(
          tabName = "Home",
          h3("CoVID19PredictoR v1.0: A machine learning pipeline and web-based interface to predict risk of CoVID-19",
             align = "center"),
          p("CoVID19PredictoR is an R based  software which develops machine learning models to predict risk of CoVID-19"),
          h5(strong("Models you can develop")),
          p("You can develop logistic regression, C5.0, decision tree, random forest and XGBoost model with CoVID19PredictoR."),
          h5(strong("Functionalities")),
          p("This framework includes summary statistics and data visualization, pre-processing and tuning options. User can develop and validate the model above and can test the model of interest for new patients in prediction module.  "),
          h5(strong("Using CoVID19PredictoR")),
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
          ("Kapucu V, Turhan S, Dogu E. (2021). CoVID19PredictoR v1.0: A machine learning pipeline and web-based interface to predict risk of CoVID-19."),
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
                  dataTableOutput("contents")
                ),
            
          #  box(title = "new data",
          #      solidHeader = T,
          #      width = 10,
          #      collapsible = T,
          #      collapsed = T,
          #      status = "info",
          #      dataTableOutput("contents1")
          #  ),
            
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
                selectInput(
                  inputId = "ycol",
                  label = "Select Y variable",
                  choices = names(data())
                ),
                selectInput(
                  inputId = "xcol",
                  label = "Select X variable",
                  choices = names(data())
                ),
                verbatimTextOutput("sumrz"),
                plotOutput("varplot",
                           width = "50%",
                           height = "300px")
                )
             )
          ),
        
        #Partition, fold, PrePRocess, TrCont------------
        tabItem(
          tabName = "choose",
          #h2("You can set data partitioning, K-fold and pre-process options here."),
          fluidRow(
            box(title = "Step 1: Set data partition ratio",
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
            
            box(title = "Step 2: Set k for k-fold cross-validation",
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
            
            #,
            #tabBox(
            #  width = "100%",
            #  title = h3("Data Information"),
            #  id = "tabset1", height = "250px",
            #  tabPanel(title = "Train Data", dataTableOutput("trndt")),              
            #  tabPanel(title = "Test Data", dataTableOutput("tstdt"))
            #)
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
                   # box(title = "Tree",
                   #     width = 6,
                   #     status = "info",
                   #     withSpinner(plotOutput("tree_rf"))
                   # )
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
                    #box(title = "Tree",
                    #    width = 6,
                    #    status = "danger",
                    #    withSpinner(plotOutput("tree_xgb"))
                    #)
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
          h2("Predictions results"),
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
                box(title = "Performance",
                    width = 7,
                    status = "warning",
                    withSpinner(dataTableOutput("pred_glm_result"))
                )
              ),
              tabPanel(
                icon = icon("circle"),
                title =  "C5.0",
                box(title = "Performance",
                    width = 7,
                    status = "primary",
                    withSpinner(dataTableOutput("pred_cfive_result"))
                )
              ),
              tabPanel(
                icon = icon("circle"),
                title =  "Decision tree",
                box(title = "Performance",
                    width = 7,
                    status = "success",
                    withSpinner(dataTableOutput("pred_dtree_result"))
                )
              ),
              tabPanel(
                icon = icon("circle"),
                title =  "Random forest",
                box(title = "Performance",
                    width = 7,
                    status = "info",
                    withSpinner(dataTableOutput("pred_rf_result"))
                )
              ),
              tabPanel(
                icon = icon("circle"),
                title =  "XGBoost",
                box(title = "Performance",
                    width = 7,
                    status = "danger",
                    withSpinner(dataTableOutput("pred_xgb_result"))
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
    req(input$file1)    ## ?req #  require that the input is available
    inFile <- input$file1
    df <- read.csv(inFile$datapath,
                   header = input$header,
                   sep = input$sep,
                   stringsAsFactors = input$stringAsFactors
                   )
    updateSelectInput(session, inputId = 'xcol', label = 'Select a risk factor',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Select outcome variable:',
                      choices = names(df), selected = names(df)[2])
    return(df)
    })
  
  data2 <- reactive({
    req(input$crntdata)
    df <- data_sets[[input$crntdata]]
    updateSelectInput(session, inputId = 'xcol', label = 'Select risk factor',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Select outcome variable:',
                      choices = names(df), selected = names(df)[2])
    return(df)
  })
  
  data <- reactive({
    #req(input$chs)
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
  
  outcomeyy <- reactive({
    outc <- data()[,1]
    return(outc)
  })
  
  output$outcomeY <- renderDataTable({
    
    DT::datatable(data.frame(data()[,1]), extensions = "ColReorder",
                  options = list(pageLength=5,
                                 colReorder = TRUE,
                                 scrollX = TRUE))
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
    set.seed(1)
    trn_dt <- data()[partition(),]
    return(trn_dt)
  })
    
  test_data <- reactive ({
    set.seed(1)
    tst_dt <- data()[-partition(),]
    return(tst_dt)
  })
  

  output$contents <- renderDataTable({
    DT::datatable(data(),
                  extensions = "ColReorder",
                  options = list(pageLength=5,
                                 colReorder = TRUE,
                                 scrollX = TRUE)
                  )
  })
  
  newdata <- reactive ({
    set.seed(1)
    #ndata <- data.frame(data()) %>% relocate(detchoiseY(), .before = data()[,1])
    ndata <- detchoiseY()
    return(ndata)
    
  })
  
  output$contents1 <- renderDataTable({
    DT::datatable(newdata(),
                  extensions = "ColReorder",
                  options = list(pageLength=5,
                                 colReorder = TRUE,
                                 scrollX = TRUE)
    )
  })
  
  
  output$str <- renderPrint({
    str(data())
    })
    
  output$sumrz <- renderPrint({
    data() %>%
      group_by(detchoiseY()) %>%
      summarise(Min = min(detchoiseX()),
                Max = max(detchoiseX()),
                Mean= mean(detchoiseX())
                #Mean= (sum(detchoiseX()/n()))
                #median=median(detchoiseX())
                #max=max(detchoiseX())
                #std=sd(detchoiseX())
      )
    
    #summary(detchoiseY())
    #dplyr ve group summarize. dene
    })

  output$varplot <- renderPlot({
    x<-data()[c(input$xcol, input$ycol)]
    plot(x,
         xlab = "Risk factor",
         ylab = "Outcome",
         col = "red",
         title("")
         )
  })
  

# Choosing pre-process------------
    txt1 <- reactive ({
      set.seed(1)
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
      set.seed(1)
      preppp <- paste(input$chsmthd, sep = ",")
      return(preppp)
    })
    
    control <- reactive({
      set.seed(1)
      cntrl <- trainControl(method=txt2(),
                              number=input$fld,
                              repeats=1) 
      return(cntrl)
    })
    
    # Classifications--------------
    #GLM-----------
    glm <- reactive ({
      set.seed(1)
      #Sys.sleep(1) # system sleeping for 1 seconds for demo purpose
      mdl<- train(as.factor(train_data()[,1])~.,
                  data=train_data()[,-1],
                  method = "glm",
                  trControl=control(),
                  preProc = c(txt1()),
                  na.action=na.pass)
      return(mdl)
    })
    
    order_importance_glm <- reactive({
      set.seed(1)
      importance_mdl<-data.frame(varImp(glm())$importance)
      importance_mdl$Vars<-row.names(importance_mdl)
      ord_imp_mdl<-importance_mdl[order(-importance_mdl$Overall),][1:20,]
      return(ord_imp_mdl)
    })
    
    test_glm <- reactive({
      set.seed(1)
      test_mdl <- predict(glm(),
                          test_data(),
                          na.action=na.pass,
                          prob=TRUE)
      return(test_mdl)
    })
    
    cm_glm <- reactive({
      set.seed(1)
      conf_mtrx_mdl <- confusionMatrix(as.factor(test_data()[,1]),
                                       test_glm(),
                                       positive='1',
                                       mode = "everything")
      return(conf_mtrx_mdl)
    })
    
    output$glm<- renderPrint({
      print(cm_glm())
      print(glm())
      #print(order_importance_glm())
      #print(test_glm())
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
      #Sys.sleep(1) # system sleeping for 1 seconds for demo purpose
      mdl<- train(as.factor(train_data()[,1])~.,
                   data=train_data()[,-1],
                   method = "C5.0",
                   trControl=control(),
                   preProc = c(txt1()),
                   na.action=na.pass)
      return(mdl)
    })
    
    order_importance_c5 <- reactive({
      set.seed(1)
      importance_mdl<-data.frame(varImp(c5())$importance)
      importance_mdl$Vars<-row.names(importance_mdl)
      ord_imp_mdl<-importance_mdl[order(-importance_mdl$Overall),][1:20,]
      return(ord_imp_mdl)
    })  

    test_c5 <- reactive({
      set.seed(1)
      test_mdl <- predict(c5(),
                          test_data(),
                          na.action=na.pass,
                          prob=TRUE)
      return(test_mdl)
    })

    cm_c5 <- reactive({
      set.seed(1)
      conf_mtrx_mdl <- confusionMatrix(as.factor(test_data()[,1]),
                                factor(test_c5(), levels=c(0,1)),
                                positive='1',
                                mode = "everything")
      return(conf_mtrx_mdl)
    })
            
    output$cfive <- renderPrint({
      print(cm_c5())
      print(c5())
      #print(order_importance_c5())
      #print(test_c5())
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
      #Sys.sleep(1) # system sleeping for 1 seconds for demo purpose
      mdl<- train(as.factor(train_data()[,1])~.,
                   data=train_data()[,-1],
                   method = "rpart",
                   trControl=control(),
                   preProc = c(txt1()),
                   na.action=na.pass)
      return(mdl)
    })
  
    order_importance_dt <- reactive({
      set.seed(1)
      importance_mdl<-data.frame(varImp(dtree())$importance)
      importance_mdl$Vars<-row.names(importance_mdl)
      ord_imp_mdl<-importance_mdl[order(-importance_mdl$Overall),][1:20,]
      return(ord_imp_mdl)
    })    

    test_dt <- reactive({
      set.seed(1)
      test_mdl <- predict(dtree(),
                         test_data(),
                         na.action=na.pass,
                         prob=TRUE)
      return(test_mdl)
    })

    cm_dt <- reactive({
      set.seed(1)
      conf_mtrx_mdl <- confusionMatrix(as.factor(test_data()[,1]),
                                test_dt(),
                                positive='1',
                                mode = "everything")
      return(conf_mtrx_mdl)
    })
            
    output$dtree <- renderPrint({
      print(cm_dt())
      print(dtree())
      #print(order_importance_dt())
      #print(test_dt())
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
      #rpart.plot(dtree()$finalModel)
      fancyRpartPlot(dtree()$finalModel)
    })
    
    
    #Random Forest----
    rf <- reactive ({
      set.seed(1)
      #Sys.sleep(1) # system sleeping for 1 seconds for demo purpose
      mdl<- train(as.factor(train_data()[,1])~.,
                  data=train_data()[,-1],
                  #cforest is ok but long running
                  method = "rf",
                  trControl=control(),
                  preProc = c(txt1()),
                  na.action=na.pass)
      return(mdl)
    })

    order_importance_rf <- reactive({
      set.seed(1)
      importance_mdl<-data.frame(varImp(rf())$importance)
      importance_mdl$Vars<-row.names(importance_mdl)
      ord_imp_mdl<-importance_mdl[order(-importance_mdl$Overall),][1:20,]
      return(ord_imp_mdl)
    })
    
    test_rf <- reactive({
      set.seed(1)
      test_mdl <- predict(rf(),
                          test_data(),
                          na.action=na.pass,
                          prob=TRUE)
      return(test_mdl)
    })

    cm_rf <- reactive({
      set.seed(1)
      conf_mtrx_mdl <- confusionMatrix(as.factor(test_data()[,1]),
                                       test_rf(),
                                       positive='1',
                                       mode = "everything")
      return(conf_mtrx_mdl)
    })
        
    output$rf <- renderPrint({
      print(cm_rf())
      print(rf())
      #print(order_importance_rf())
      #print(test_rf())
      })
    
    output$imp_rf <- renderPrint({
      print(order_importance_rf()[1:5,])
      })
    
    output$wordrf <- renderPlot({
      wordcloud(words = order_importance_rf()$Vars, freq = order_importance_rf()$Overall, scale=c(3,.1), min.freq = 1,
                max.words=2000, random.order=TRUE, rot.per=0.1, 
                colors=brewer.pal(8, "Paired"))
    })
    
    output$tree_rf <- renderPlot({
      #rpart.plot(dtree()$finalModel)
      #fancyRpartPlot(rf()$finalModel)
     #plot_tree(rf()$finalModel)
    })
    
    #XGBoost------
    xgb <- reactive ({
      set.seed(1)
      #Sys.sleep(1) # system sleeping for 1 seconds for demo purpose
      mdl<- train(as.factor(train_data()[,1])~.,
                  data=train_data()[,-1],
                  method = "xgbTree",
                  trControl=control(),
                  preProc = c(txt1()),
                  na.action=na.pass,
                  verbosity = 0)
      return(mdl)
    })
    
    order_importance_xgb <- reactive({
      set.seed(1)
      importance_mdl<-data.frame(varImp(xgb())$importance)
      importance_mdl$Vars<-row.names(importance_mdl)
      ord_imp_mdl<-importance_mdl[order(-importance_mdl$Overall),][1:20,]
      return(ord_imp_mdl)
    })
    
    test_xgb <- reactive({
      set.seed(1)
      test_mdl <- predict(xgb(),
                          test_data(),
                          prob=TRUE,
                          na.action=na.pass)
      return(test_mdl)
    })
    
    cm_xgb <- reactive({
      set.seed(1)
      conf_mtrx_mdl <- confusionMatrix(as.factor(test_data()[,1]),
                                       test_xgb(),
                                       positive='1',
                                       mode = "everything")
      return(conf_mtrx_mdl)
    })
    
    output$xgb<- renderPrint({
      print(cm_xgb())
      print(xgb())
      #print(order_importance_xgb())
      #print(test_xgb()) 
    })
    
    output$imp_xgb <- renderPrint({
      print(order_importance_xgb()[1:5,])
    })
    
    output$wordxgb <- renderPlot({
      wordcloud(words = order_importance_xgb()$Vars, freq = order_importance_xgb()$Overall, scale=c(3.3,.4), min.freq = 1,
                  max.words=2000, random.order=TRUE, rot.per=0.1, 
                  colors=brewer.pal(8, "Paired"))
    })
    
    output$tree_xgb <- renderPlot({
      #rpart.plot(dtree()$finalModel)
      #fancyRpartPlot(dtree()$finalModel)
      #xgb.plot.tree(model=xgb()$finalModel, trees=1)
    })
    

    #Compare Model---------
    modelscompare <- reactive({
      set.seed(1)
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
      req(input$file2)    ## ?req #  require that the input is available
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
      #req(input$chs)
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
      #print(format(pred_glm(), digits=1, nsmall=1))
      #print(format((round(pred_glm())), digits=2, nsmall=2))
      #print(formattable((pred_glm(), digits = 2, format = "f")))
      #p<-percent(pred_glm(), format = "d")
      #print(p)
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
      #print(pred_cfive())
      #print(data.frame(Result_for_C5.0=pred_cfive()))
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
      #print(pred_dtree())
      #print(data.frame(Result_for_Decision_Tree=pred_dtree()))
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
      rfdt<-data.frame(Result_for_R.Forest=pred_rf())
      DT::datatable(rfdt, extensions = "ColReorder",
                    options = list(pageLength=5,
                                   colReorder = TRUE,
                                   scrollX = TRUE))
      #print(pred_rf())
      #print(data.frame(Result_for_Random_Forest=pred_rf()))
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
      #print(pred_xgb())
      #print(data.frame(Result_for_XGBoost=pred_xgb()))
    })      
              
}
shinyApp(ui, server)
