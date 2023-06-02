#
# This is a Shiny web application for g theory visualization. 
#
# Data:
#     1. SyntheticDataSetNo.1.csv: Single facet design (p X i)
#     2. SyntheticDataSetNo.4.csv: two facet design (p X t X r)
# Tutorial: (Mastering Shiny) https://mastering-shiny.org/
# 【X】 1) 在dstudy界面可以選擇多個N，以便畫出曲線

rm(list = ls())
missingMethods = c(
  "na.omit (default)",
  "Zero Inputation",
  "Mean Inputation",
  "Median Inputation"
)
source("library_load.R")
source("advGtheoryFunctions.R")

## Source all modules
source("modules/inputFile_module.R") # Input CSV file
source("modules/LongToWide_module.R") # Long format to Wide format
nboot = 200

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  skin = "green",
  ## title
  dashboardHeader(title = "GtheoryShiny App"),
  ## Sidebar content -----
  dashboardSidebar(
    
    sidebarMenu(
      id = "sidebar", 
      menuItem("Tutorial", tabName = "tutorial", icon = icon("dashboard"), badgeLabel = "Ver.0.2", badgeColor = "green"),
      menuItem("Data Input", tabName = "datainput", icon = icon("th")),
      menuItem("Data Structure", tabName = "datastructure", icon = icon("table")),
      menuItem("Data Analysis", tabName = "dataanalysis", icon = icon("list-alt"))
    )
  ),
  ## Body content -----
  dashboardBody(
    tabItems(
      ### Tab Page 0: Tutorial ----
      tabItem(tabName = "tutorial",
              # Sidebar with a slider input for number of bins
              fluidPage(
                # title
                titlePanel("GtheoryShiny Tutorial"),
                
                #
                h4("Example data:"),
                p(
                  span("SyntheticDataSetNo.4.csv", style = "color:blue"),
                  " is a example data with two facets including",
                  strong("Test and Rater"),
                  "; ID variable is",
                  strong("Person"),
                  "; Outcome variable is",
                  strong("Score"),
                  "."
                ),
                
                
                h4("Step 1: Read in example data or transformation"),
                p(
                  "Click ",
                  strong("Data Input"),
                  " tab on the top navigation panel will open the page for data input and transformation. Chick Upload button and select the data. Check the ",
                  em('long-format'),
                  " checkbox under the Data property section."
                ),
                
                h4("Step 2: Specify each column's type"),
                p(
                  "Click ",
                  em("Data Structure"),
                  " tab. Select ",
                  strong("Person"),
                  " for ",
                  em("which column represents ID"),
                  " question."
                ),
                p(
                  "Then check ",
                  strong("Subset, Item, Rater, Ocasion"),
                  " for ",
                  em("Which column(s) represent facets"),
                  " question."
                ),
                p(
                  "Finally select ",
                  strong("Score"),
                  " for ",
                  em("Which column represents outcome"),
                  " question."
                ),
                
                h4("Step 3: Run data analysis"),
                p(
                  "Click ",
                  em("Data Analysis"),
                  " tab. You will notice the recommended formula for gtheory has been given to you. You can also specify your formula in ",
                  em("User-specified formula"),
                  " section. Choose link function for your model (defaulty is identity link). Finally decise on how many boostrap iterations for bootstrapping standard diviation estimation."
                ),
                p(
                  "Chick ",
                  strong("gstudy estimate"),
                  " button to print gstudy results. A button called ",
                  strong("Download gstudy result"),
                  " will pop up. Click that button to download the results into local machine."
                ),
                
                h3("Updates (2023-02-12):"),
                p(
                  "This app has the function to estimate the fixed effects of covariates. To use this function, check",
                  em("covariates"),
                  "in Data Structure tab page. Then, the fixed effects estimates will be printed after users click ",
                  em("gtheory estimate"),
                  "button in Data Analysis tag page."
                )
              )
      ),
      
      ### Tag Page 1: Transform data  ----
      tabItem(tabName = "datainput",
        fluidPage(
          box(title = "Choose CSV File", 
              status = "info", solidHeader=TRUE, width = 4, 
              csvFileUI("fileUpload", ""),
              checkboxInput("isLongFormat", "Long format", TRUE),
              conditionalPanel(
                condition = "input.isLongFormat == 0",
                hr(),
                h4("Pivot data from wide to long: "),
                uiOutput("nRowsSelection"),
                uiOutput("preFixText"),
                uiOutput("TagNamesText"),
                ## 转换
                actionButton("transform", tagList(icon("rotate"), "Transform")),
              ),
              ## 确定
              br(),
              hr(),
              actionButton("dataConfirm", label = "Confirm", icon = icon("circle"))
          ),
          tabBox(title = tagList(shiny::icon("th"), "Data"), 
            width = 8, id = "tabsetData", 
            tabPanel(title = "Raw", DTOutput("rawDataTable")),
            tabPanel(title = "Transformed", DTOutput("transDataTable"))
          ),
        )
      ),
      
      ### Tab Page 2: Data Structure  ----
      tabItem(tabName = "datastructure",
        fluidRow(
          box(title = "Data Structure：", width = 4, 
              status = "primary", solidHeader = TRUE,
              # show selection area for ID
              uiOutput("selectedID"),
              # show selection area for outcome
              uiOutput("selectedOutcome"),
              # show selection area for facets
              uiOutput("selectedMultipleFacets"),
              # Missing data
              selectInput(
                "missingMethod",
                label = "4. Missing Method:",
                choices = missingMethods,
                selected = missingMethods[1]
              ),
              # show selection area for covariates
              hr(),
              uiOutput("selectedCovariates"),
              uiOutput("mGtheory"),
              
              conditionalPanel(
                condition = "input.mGtheory == 1",
                uiOutput("selectedFixedFacet")
              ),
              actionButton("variableSettingConfirm", "Confirm", icon = icon("circle"))
          ),
          tabBox(id = "modelDesign", title = tagList(icon("diagram-next"), "Design"), 
            tabPanel(title = "Formular", textOutput("recommFormular"), tags$head(tags$style("#recommFormular{color:red; font-size:14px; font-style:bold;
          overflow-y:scroll; max-height: 50px; background: ghostwhite;}"))),
            tabPanel(title = "Structure table", DTOutput("nestedStrucTable")),
            tabPanel(title = "Summary table", DTOutput("factorNestTable")),
            footer = p(em('Formular')," = recommended formular for linear mixed model;", br(), 
                       em('Structure table')," = auto-detected nested design in data;", br(), 
                       em('Summary table'), "= sample size for each levels of facet")
          )
        )
      ),
      
      ### Tab Page 3: Data Analysis  ----
      tabItem(tabName = "dataanalysis",
        fluidRow(
          box(title = "Data Analysis: ", width = 4, status = "primary",
              textInput("selfFormular", "1. User-specified formula: ",
                        placeholder = "Default: recommeded formula"),
              selectInput(
                "linkFunc",
                label = "2. Link Function:",
                choices = c("identity", "logit", "probit", "poisson", "inverse gamma"),
                selected = "identity"
              ),
              sliderInput(
                inputId = "nboot",
                label = "3. Number of bootstrap for G-study and D-study",
                min = 100,
                max = 1000,
                value = 500
              ),
              uiOutput("nCores"),
              ## 运行gstudy
              hr(),
              strong("Gstudy："),
              br(), br(), 
              actionButton("runGstudyButton", "Run"),
              actionButton("runGstudyBootButton", "Bootstrapping SD"),
              progressBar(
                id = "gstudybar",
                value = 0,
                total = 100,
                title = NULL,
                striped = TRUE,
                display_pct = TRUE
              ),
              ### 下载按钮
              conditionalPanel(
                condition = "input.runGstudyButton >=1",
                downloadButton("downloadGstudyTheta", "Factor Score Table")
              ),
              conditionalPanel(
                condition = "input.runGstudyBootButton >=1",
                downloadButton("downloadGstudyBootResult", "Bootstrapping Result")
              ),
              #### Run dstudy -----
              hr(),
              checkboxInput("runDstudyBox", label = "Run Dstudy", FALSE),
              conditionalPanel(condition = "input.runDstudyBox == 1",
                h4("Dstudy："),
                ### 选择要修改的facet
                uiOutput("selectedFacetMenu"),
                #选择一个facet的levels：可以用一个数字，也可以选择一串数字
                uiOutput("selectedFacetLevels"),
                uiOutput("selectedMultipleFacetLevels"),
                #选择factor levels
                actionButton(inputId = "confirmFacetLevel",
                             label = "Add facet levels"),
                actionButton("runDstudyButton", "Run"),
                actionButton("runDstudyBootButton", "Run bootstrapping"),
                actionButton("plotDstudyCoef", "Plot Coefficients"),
                progressBar(id = "dstudybar", 
                            value = 0,
                            total = 100,
                            title = NULL,
                            striped = TRUE,
                            display_pct = TRUE),
                ### 下载按钮dstudy
                conditionalPanel(
                  condition = "input.runDstudyButton >=1",
                  downloadButton("downloadDstudyResult",
                                 "Download dstudy result"),
                  downloadButton("downloadDstudyBootResult",
                                 "Download bootstrap result")
                ),
              
              )
        ),
        box(title = "Result: ", width = 8,
            #### Output UI for gstudy  ----
            conditionalPanel(
              condition = "input.runGstudyButton >= 1",
              h4("Gstudy Output："),
              h5("Fixed Effects Output："),
              verbatimTextOutput("recommModelFixedEffectResult"),
              h5("Random Effects Output："),
              DTOutput("GstudyResultPrint")
            ),
            # gstudy with bootstrapping SD
            conditionalPanel(
              condition = "input.runGstudyBootButton >= 1",
              h4("Estimate Bootstrapping SD for G-study："),
              DTOutput("recommModelGStudyBootResult")
            ),
            #### Output UI for dstudy  ----
            conditionalPanel(condition = "input.confirmFacetLevel >= 1",
                             h4("Sample Size for Dstudy:"),
                             DTOutput("updatedNDT")),
            conditionalPanel(
              condition = "input.runDstudyButton >= 1",
              h4("Dstudy Output："),
              p("Result:"),
              verbatimTextOutput("recommModelDStudyResult"),
            ),
            # dstudy with bootstrapping SD
            conditionalPanel(
              condition = "input.runDstudyBootButton >= 1",
              h4("Estimate Bootstrapping SD for D-study："),
              verbatimTextOutput("recommModelDStudyBootResult")
            ),
            # plot dstudy coefficient path plot
            conditionalPanel(
              condition = "input.plotDstudyCoef >= 1",
              h4("Coefficient path plot for D-study："),
              plotOutput("DstudyCoefPlot")
            )
            
        )
      ),
    )
  )
  ) # End of dashboardBody
) # End of dashboardPage




# Server -----------------------------------------------------------------
server <- function(input, output, session) {

  ## Server for Page 1: Data read and transformation  ----------------------------------------
  ### Read in original data ----------------------------------------
  datRaw <- csvFileServer("fileUpload", stringsAsFactors = FALSE)
  
  ### Reactive dat -----
  dat <- eventReactive(input$dataConfirm, {
    if(input$transform == 1){
      datTrans()
    }else{
      datRaw()
    }
  })
  
  ### Output raw data table-----
  output$rawDataTable <- renderDT({datRaw()})
  
  ### UI selectors for facets specifications  ----------------------------------------
  #------------#
  # A bunch of reactive values:
  ## NText: 有多少行是tag/ID
  ## preFixText: tag的前綴是什麼比如第四個item—I4，為I
  ## TagNamesText: facet的name比如：Class;Rater;Item
  #------------#
  observeEvent(input$isLongFormat, {
    # 把数据转化为长数据
    # 按下"transform"按钮后，将原始数据转换为长数据格式:
    output$nRowsSelection <- renderUI({ # 选择多少行代表Facet Tags，每一行代表一个facet
      selectInput(
        "nRows",
        "Select the number of rows containing facets (not including headings)",
        choices = 0:nrow(datRaw()),
        selected = 0
      )
    })
    
    output$TagNamesText <- renderUI({
      textInput("TagNames",
                label = "Enter facet's names seperated by `,` (for example, `Occasion,Item`)",
                value = paste0(paste0(LETTERS[1:NText()], letters[NText():1]), collapse = ","))
    })
    
    output$preFixText <- renderUI({ # 为facets选择prefix
      textInput("TagPreFix", 
                label = "Enter prefix of facet levels seperated by `,`（for example, `O,I`)" , 
                value = paste0(LETTERS[1:NText()], collapse = ","))
    })
  })
  NText = reactive({input$nRows}) # 有多少行是tag/ID
  preFixText = reactive({input$TagPreFix}) # 前缀，比如A;B;C
  TagNamesText = reactive({input$TagNames}) # facet的name比如：Class;Rater;Item

  #------------#
  # Transformation logic
  ## 若觀察到用戶點擊"transform"按鈕,判斷是否提供了tag前綴(比如I)和tag名字(比如Item)
  ## 分支（1）：若tag前綴和tag名字都提供，將tag前綴和tag名字分離。判斷用戶提供的tag行數數目
  ### 分支（1.1）: 若tag行數數目為0，默認用戶tag信息放在heading裡面。直接用tag前綴進行transform。
  ### 分支（1.2）: 若tag行數數目大於0，默認用戶tag信息不放在heading裡面，而在數據的前N行。將前N行tag合併成一個特殊tag ID(mergeID)進行transform。mergeID將不同facet的tag用下劃線進行區別。最後再將mergeID分離成好幾個facet列。每一列都冠以tagNames。
  ## 分支（2）：若tag前綴和tag名字都不提供且tag行數（NText）為0。則直接將除一個列（ID）以外的列進行transform。適用於single facet design。
  #------------#
  datTrans <- eventReactive(input$transform, {
    N <- as.numeric(NText())
    dat <- datRaw()
    
    if (preFixText() != "" & TagNamesText() != "") { #分支1.如果preFixText和TagNames都提供
      TagPreFix <- str_split(preFixText(), ",")[[1]]
      TagNames <- str_split(TagNamesText(), ",")[[1]]
      if (N == 0) { # 分支1.1：如果没有facet行，默认facet藏在heading里且为single facet,将PreFix开头的转化为long，冠以TagNames
        if (length(TagPreFix) > 1) {
          shinyalert("Oops!", "Your specification of facets are not correct.", type = "error")
        }
        if (sum(str_detect(colnames(dat), pattern = TagPreFix)) == 0 ) {
          shinyalert("Oops!", "Your specification of facets are not correct.", type = "error")
        }
        datTrans <- dplyr::tibble(dat) |>
          pivot_longer(starts_with({{TagPreFix}}), names_to = TagNames, values_to = "Score")
      }else if (N > 0){ # 分支1.2:如果有facet行，将tags of facet转换为合并的ID再进行long-format转化
        tags = dat[1:N, -1] # 取出levels名字
        # 将所有facets合并成特殊ID
        mergedID = apply(tags, 2, \(x) paste0(x, collapse = "_"))
        dat_FacetNameOmitted <- dplyr::tibble(dat[-(1:N), ]) # 去掉了facet信息的纯数据
        colnames(dat_FacetNameOmitted) <- c("ID", mergedID)
        datTrans <- dat_FacetNameOmitted |>
          pivot_longer(cols = any_of(as.character(mergedID)), values_to = "Score") |>
          separate(name, into = TagNames, sep = "_")
      }
    }else if (preFixText() == "" & TagNamesText() == "" & N == 0){
      datTrans <- dplyr::tibble(dat) |>
        pivot_longer(any_of(colnames(dat)[-1]), names_to = "Facet", values_to = "Score")
    }else{
      # Show a modal when the button is pressed
      shinyalert("Oops!", "Your specification of facets are not correct.", type = "error")
    }
    
    datTrans
  })

  ### Output transformed data table if any -----
  output$transDataTable <- renderDT({datTrans()})
  

  ### Update UI for data confirm and switch to Tab page 2 -----
  observeEvent(input$dataConfirm, {
    updateActionButton(
      session,
      inputId = "dataConfirm",
      icon = icon("check")
    )
    
    updateTabsetPanel(
      session,
      inputId = "sidebar",
      selected = "datastructure"
    )
  })
  
  observeEvent(input$transform, {
    updateTabsetPanel(
      session,
      inputId = "tabsetData",
      selected = "Transformed"
    )
  })
  
  ## Server for Page 2: Auto detect design of data ----------------------------------------------------
  ### Selection UI for user-selected facets / outcomes / ID ----------------------------------------
  observeEvent(input$dataConfirm, {
    dat <- dat()
    selectedFacet <- selectedFacet()
    
    # 选择ID
    output$selectedID <- renderUI({
      selectInput(
        "selectedID",
        "1. ID variable:",
        choices = colnames(dat),
        selected = colnames(dat)[1]
      )
    })
    
    # 选择outcome
    output$selectedOutcome <- renderUI({
      selectInput(
        "selectedOutcome",
        "2. Outcome variable:",
        choices = setdiff(colnames(dat), input$selectedID),
        selected = setdiff(colnames(dat), input$selectedID)[1]
      )
    })
    
    # 选择facet
    output$selectedMultipleFacets <- renderUI({
      selectInput(
        "selectedMultipleFacets",
        "3. Facet(s):",
        choices = setdiff(colnames(dat), c(input$selectedID, input$selectedOutcome)),
        selected = setdiff(colnames(dat), c(input$selectedID, input$selectedOutcome)),
        multiple = TRUE
      )
    })
    
    # 选择covariates
    output$selectedCovariates <- renderUI({
      checkboxGroupInput(
        "selectedCovariates",
        "(Optional) Select covariate(s):",
        choices = setdiff(colnames(dat), c(input$selectedID, input$selectedOutcome)),
        selected = NULL
      )
    })
    
    # 选择是否要进行mGtheory
    output$mGtheory <- renderUI({
      checkboxInput(inputId = "mGtheory", 
                    label = strong("(Optional) mGtheory"),
                    value = FALSE, width = "400px")
    })
    
    # mgTheory选择fixed facet
    output$selectedFixedFacet <- renderUI({
      selectInput(
        "selectedFixedFacet",
        "5. Select fixed facet:",
        choices = selectedFacet,
        selected = selectedFacet[1]
      )
    })
  })
  

  ## missing data inputation
  datNARemoved <- eventReactive(input$variableSettingConfirm, {
    dat <- dat()
    method <- selectedMissingMethod()
    selectedOutcome <- selectedOutcome()
    
    ## Deal with missing method for outcome variables
    if(method == "Zero Inputation"){
      dat[[selectedOutcome]] <- replace(dat[[selectedOutcome]], 
                                        is.na(dat[[selectedOutcome]]), 
                                        0)
    }else if(method == "Mean Inputation"){
      dat[[selectedOutcome]] <- replace(dat[[selectedOutcome]], 
                                        is.na(dat[[selectedOutcome]]), 
                                        mean(dat[[selectedOutcome]], na.rm = TRUE))
    }else if(method == "Median Inputation"){
      dat[[selectedOutcome]] <- replace(dat[[selectedOutcome]], 
                                        is.na(dat[[selectedOutcome]]), 
                                        median(dat[[selectedOutcome]], na.rm = TRUE))
    }else{ ## listwise deletion
      dat <- dat[!is.na(dat[[selectedOutcome]]),]
    }
    
    dat
  })
  
  ### Read in user-selected facets / outcomes / ID ----------------------------------------
  unit = reactive({input$selectedID}) ## ID
  selectedOutcome = reactive({input$selectedOutcome}) ## Outcome (i.e., Score)
  selectedFacet = reactive({input$selectedMultipleFacets}) ## Facets used for gstudy/dstudy
  selectedCovariates = reactive({input$selectedCovariates}) ## covariates for fixed effects
  selectedFixedFacet = reactive({input$selectedFixedFacet}) ## mgtheory: fixed facet
  selectedMissingMethod = reactive({input$missingMethod}) ## selected missing data handling method
  
  ### nestedStrc: automatically detect design of data and return structure table-----
  nestedStrc <- eventReactive(input$variableSettingConfirm, {
    dat <- datNARemoved()
    selectedFacet <- selectedFacet()
    
    if (length(selectedFacet) == 1) { # if single facet
      nestedStrc = data.frame(NA)
      colnames(nestedStrc) = selectedFacet
      nestedStrc
    }else{ # if multiple facets
      # Each row represents a pair of facets
      TagPairs <- as.data.frame(t(combn(selectedFacet, 2))) 
      # Placeholder for structure table
      NestedOrCrossed = rep(NA, nrow(TagPairs))
      # Set two facets into "f1" and "f2"
      colnames(TagPairs) <- c("f1", "f2")
      # Compare facet with the other pair by pair
      for (pair in 1:nrow(TagPairs)) {
        whichpair = TagPairs[pair,]
        isNested <- sjmisc::is_nested(dat[[whichpair[[1]]]], dat[[whichpair[[2]]]])
        NestedOrCrossed[pair] = ifelse(isNested, "Nested", "Crossed")
      }
      nestedStrc = TagPairs
      nestedStrc$NestedOrCrossed = NestedOrCrossed
      nestedStrc
    }
  })
  
  ## Update UI for facet settings and switch to Tab page 3 -----
  observeEvent(input$variableSettingConfirm, {
    updateActionButton(inputId = "variableSettingConfirm", icon = icon("check"))
    updateTabsetPanel(
      session,
      inputId = "modelDesign",
      selected = "Structure table"
    )
  })
  
  


  ### Generate gtheory formula ----------------------------------------
  #------------#
  # 若點擊確認(confirm)按鈕，根據design structure進行逐行掃描:
  ## 分支1: 若design structure為單列，則為single facet。進行下列操作:
  ##        1, 生成selectedOutcome() ~ (1 | unit()) + (1 | selectedFacet())
  ## 分支2: 若design structure為多列，則為multiple facets。
  ### 分支2.1: 若是univariate gtheory （input$mGtheory == FALSE）
  ### 分支2.2: 若是multivariate gtheory （input$mGtheory == TRUE）
  #------------#

   gtheoryFormula <- eventReactive(input$variableSettingConfirm, {
     formularFacets <- NULL # placeholder for formular
     nestedStrcTable <- nestedStrc() # load nested structure
     selectedOutcome <- selectedOutcome() # user-defined DV
     selectedFacet <- selectedFacet() # user-defined facet(s)
     selectedCovariates <- selectedCovariates() # user-defined covariates
     selectedID <- unit()

     if (ncol(nestedStrcTable) == 1) { # 分支1:single facet

       formularText <- paste0(selectedOutcome, " ~ (1 |", unit(), ") + ", "(1 |", selectedFacet, ")")

     }else if (ncol(nestedStrcTable) > 1) { # 分支2:multiple facets

       if (input$mGtheory == FALSE) { # 分支2.1: univariate gtheory
         for (r in 1:nrow(nestedStrcTable)) {
           if (nestedStrcTable[r, "NestedOrCrossed"] == "Crossed") {
             formularFacets <-
               c(formularFacets,paste0("(1 | ", nestedStrcTable[r, 1:2], ")"))
           }
           if (nestedStrcTable[r, "NestedOrCrossed"] == "Nested") {
             tab <- table(nestedStrcTable[r, 1], nestedStrcTable[r, 2])
             nested <- !any(apply(tab, 1, \(x) sum(x != 0) > 1))
             if (nested) {
               formularFacets <-
                 c(
                   formularFacets,
                   paste0("(1 | ", paste0(nestedStrcTable[r, 1]), ")"),
                   paste0("(1 | ", selectedID, ":", nestedStrcTable[r, 1], ")"),
                   paste0(
                     "(1 | ",paste0(nestedStrcTable[r, 2], ":", nestedStrcTable[r, 1]),")"
                   )
                 )
             } else{
               formularFacets <-
                 c(formularFacets,
                   paste0("(1 | ", paste0(nestedStrcTable[r, 2]), ")"),
                   paste0("(1 | ", selectedID, ":", nestedStrcTable[r, 2], ")"),
                   paste0("(1 | ", paste0(nestedStrcTable[r, 1], ":", nestedStrcTable[r, 2]), ")"))
             }
           }
         }
         ## add covariates and facets into the formulate
         formularText <- paste0(c(selectedCovariates, unique(formularFacets)), collapse = " + ")
         formularText <- paste0(selectedOutcome, " ~ (1 |", selectedID, ") + ", formularText)
         formularText
       }else{ # 分支2.2: multivariate gtheory
         fixedfacet <- selectedFixedFacet()
         randomfacets <- setdiff(selectedFacet, fixedfacet)
         formularRHS <- paste0("us(", fixedfacet, " + 0 | ", c(unit(), randomfacets), ")", collapse = " + ")
         formularText <- paste0(selectedOutcome, " ~ ", formularRHS)
         formularText
       }
     }else{
       NULL
     }
   })

   ### Output simplified fomular text ----------------------------------------
   output$recommFormular <- renderText({
     makeeasyformular(gtheoryFormula())
     #gtheoryFormula()
   })

   ### Output data design table ----------------------------------------
   observeEvent(input$variableSettingConfirm, {
     dat <- datNARemoved()
     # 表格打印： facet嵌套
     output$nestedStrucTable <- renderDT({
       nestedStrc()
     })

     # 表格打印： 总结样本量
     output$factorNestTable <- renderDT({
       facets  <- selectedFacet()
       outcome  <- selectedOutcome()
       dat |>
         group_by(across(facets)) |>
         summarise(`Sample Size (Outcome)` = n_distinct(.data[[outcome]]))
     })
   })

  ## Run recommended model ------------------------------------------------------------------
  #------------#
  # 分支1: 若為univariate gstudy，運行lmer
  # 分支2: 若為multivariate gstudy，運行lmmTMB::glmmTMB
  #------------#

  ### Run gstudy  ----------------------------------------
  ###### --- 
  # Return a list with three elements:
  # lmmFit: lme4 object
  # fixedEffect: vector; fixed effect
  # VarComp: matrix; variance-covariance matrix
  # gstudy: gstudy Class
  ###### ---
  gstudyResult <- eventReactive(input$runGstudyButton, {
    nFacet <- NULL
    datG <- datNARemoved() # data used for gstudy
    ## make sure outcome as numeric and facet as factors
    datG[[selectedOutcome()]] <- as.numeric(datG[[selectedOutcome()]])
    datG[c(input$selectedID, selectedFacet())] <- lapply(datG[c(input$selectedID, selectedFacet())], as.factor)
    
    if (input$mGtheory == FALSE) { # 分支1: 若為univariate gstudy
      if (input$selfFormular == "") { # 分支1.1.若用戶沒有自定義公式
        formulaRecomm <- as.formula(gtheoryFormula())
        lmmFit <- lmer(data = datG, formula = formulaRecomm)
      } else{ # 分支1.1.若用戶自定義公式，則轉化爲lme4直接使用用戶的公式
        lmmFit <- lmer(data = datG, formula = as.formula(makehardformular(input$selfFormular)))
      }
      ## Random effects
      randomEffectEstimate <- ranef(lmmFit)
      randomEffectLevel <- sapply(lapply(datG[selectedFacet()], unique), length)
      allRandomFacets <<- unlist(randomEffectLevel[selectedFacet()])
      list(
        lmmFit = lmmFit,
        fixedEffect = fixef(lmmFit),
        VarComp = gstudy(lmmFit)$gstudy.out,
        gstudy = gstudy(lmmFit)
      )
      
    }else{# 分支2: 若為multivariate gstudy
      if (input$selfFormular == "") { # 分支2.1.若用戶沒有自定義公式
        formulaTxt <- gtheoryFormula()
        formulaRecomm <- as.formula(formulaTxt)
        lmmFit <<- glmmTMB::glmmTMB(
          data = datG,
          formula = formulaRecomm,
          family = gaussian,
          dispformula = ~0
        )
        ## extract residual var-cov matrix
        residuals_Person <- cbind(residuals = residuals(lmmFit, "response"),
                                  datG[c(unit(), selectedFacet())]) %>%
          pivot_wider(names_from = selectedFixedFacet(), values_from = residuals, names_prefix = "facet") %>%
          ungroup()
        residual_cor = cor(residuals_Person |> dplyr::select(starts_with("facet")))
        
        ## run second time
        dat2 = datG
        dat2$Residual = residuals(lmmFit, "response")
        formulaWtResidTxt <- paste0(formulaTxt, "+ diag(", selectedFixedFacet(), " + 0 | Residual)")
        formulaRecommWtResid <- as.formula(formulaWtResidTxt)
        lmmFit <- glmmTMB::glmmTMB(
          formula = formulaRecommWtResid,
          data =  dat2,
          family = gaussian,
          dispformula =~0
        )
        
        
        res <- lme4::VarCorr(lmmFit)
        resDat <- extract.VarCorr.glmmTMB(x = res$cond, residCor = residual_cor)
        
        list(
          lmmFit = lmmFit,
          fixedEffect = fixef(lmmFit),
          VarComp = resDat,
          mgstudy = NULL # retrun a mgstudy class
        )
        
      } else{ # 分支2.1.若用戶自定義公式，則轉化爲lme4直接使用用戶的公式
        shinyjs::alert("This function is currently not available!")
      }
    }
  })     
  
  ### Run gstudy bootstrapping ----------------------------------------
  output$nCores <- renderUI({
    selectInput(inputId = "nCores",
                label = "Select number of cores for bootstrapping:",
                selected = parallel::detectCores()-1,
                choices = 1:parallel::detectCores())
  })
  nCores <- reactive(input$nCores)
  ###### --- 
  # return bootMer object
  ###### ---
  gstudyResultBoot <- eventReactive(input$runGstudyBootButton, {
    datGBoot <- datNARemoved()
    nCores <- nCores()
    datGBoot[[selectedOutcome()]] <- as.numeric(datGBoot[[selectedOutcome()]])
    datGBoot[c(input$selectedID, selectedFacet())] <- lapply(datGBoot[c(input$selectedID, selectedFacet())], as.factor)
    
    boot.gstudy <-
      lme4::bootMer(
        gstudyResult()$lmmFit,
        gstudy.forboot,
        nsim = input$nboot,
        use.u = FALSE,
        type = "parametric",
        parallel = "snow",
        ncpus = nCores
      )
    
    boot.gstudy
  })
  
  ### Run gstudy bootstrapping CI ----------------------------------------
  ###### --- 
  # Return data frame of gstudy variance components with bootstrapping CI
  ###### ---
  gstudyResultBootCI <- eventReactive(input$runGstudyBootButton, {
    gstudy_res <- gstudyResult()$VarComp # Variance-covariance
    boot_gstudy_res <- gstudyResultBoot() # bootMer object
    
    # calculate bootstrap CI
    bootCI_gstudy_res <-
      t(apply(boot_gstudy_res$t, 2, function(x) {quantile(x, probs = c(.025, .975))}))
    
    as.data.frame(cbind(gstudy_res, bootCI_gstudy_res))
  })
  
  ### Output gstudy results and bootstrapping  ----------------------------------------
  output$recommModelFixedEffectResult <- renderPrint({gstudyResult()$fixedEffect})
  output$GstudyResultPrint <- renderDT({gstudyResult()$VarComp})
  output$recommModelGStudyBootResult <- renderDT({gstudyResultBootCI()})
  
  ### buttons: gstudy results download  ----------------------------------------
  ###### --- 
  # downloadGstudyTheta: button for download theta scores
  # downloadGstudyBootResult: button for download bootstrapping results
  ###### ---
  output$downloadGstudyTheta <- downloadHandler(
    filename = "thetaEstimates.csv",
    content = \(file) {write.csv(extractTheta(gstudyResult()$lmmFit), file, row.names = FALSE)}
  )
  
  output$downloadGstudyBootResult <- downloadHandler(
    filename = paste0("gstudyBootstrapN", input$nboot, ".csv"),
    content = \(file) {write.csv(gstudyResultBootCI(), file, row.names = FALSE)}
  )
  
  ###  Pre-specifications for dstudy ----------------------------------------
  #### UI for facet and levels selection  ----------------------------------------
  observeEvent(input$runDstudyBox, {
    defaultN <- defaultN()
    
    #------------#
    # Select which facet to add sample size
    #------------#
    output$selectedFacetMenu <- renderUI({
      selectInput(inputId = "FacetDStudySelector",
                  label = "Select facet:",
                  choices = selectedFacet(),
                  multiple = FALSE)
    })
    
    #------------#
    # Select levels for target facet
    #------------#
    observeEvent(input$FacetDStudySelector, {
      selectedFacetForDstudy <- selectedFacetForDstudy()
      ###### ---
      # Facet levels slider for selected Facet
      ###### ---
      output$selectedFacetLevels <- renderUI({
        numericInput(
          inputId = "FacetValueSlider",
          label = "Enter target level: ",
          value = defaultN[selectedFacetForDstudy],
          min = 0,
          max = defaultN[selectedFacetForDstudy] * 10,
          step = 10
        )
      })
      
      ###### ---
      # Facet levels range for dstudy plot
      ###### ---
      output$selectedMultipleFacetLevels <- renderUI({
        startValue <- defaultN[selectedFacetForDstudy]
        endValue <- startValue*10
        textInput(
          inputId = "FacetValueRange",
          label = "(Optional) Enter level's range: ",
          value = glue::glue("{startValue}:{endValue}:10"),
          placeholder = "`100:200:10` is parsed as from 100 to 200 with step 10"
        )
      })
    })
    
    ## ----------------------------- ##
    ## 当按下确认Facet Level键
    ## ----------------------------- ##
    observeEvent(input$confirmFacetLevel, {
      selectedFacetForDstudy <- selectedFacetForDstudy()
      selectedFacetValue <- selectedFacetValue()
      
      updatedN[selectedFacetForDstudy] <<- selectedFacetValue
      
      ###### --- 
      # Output updated facets' levels for dstudy
      ###### ---
      output$updatedNDT <- renderDT({
        data.frame(New = updatedN, old = defaultN)
      })
    })
    
  })
  
  #### Selectors for Facet levels  ----------------------------------------
  ###### --- 
  # selectedFacetValue: Levels for certain facet
  # selectedFacetForDstudy: Facet drop-down selector for certain facet
  ###### ---
  selectedFacetValue <- reactive({input$FacetValueSlider})
  selectedFacetForDstudy <- reactive({input$FacetDStudySelector})
  
  #### button: confirm the facet levels  ----------------------------------------
  defaultN <- reactive({
    dat <- datNARemoved()
    selectedFacet <- selectedFacet()
    sapply(dat[selectedFacet], n_distinct)
  })
  
  observeEvent(input$variableSettingConfirm, {
    updatedN <<- defaultN()
  })
  
  
  
  ### Run dstudy ----------------------------------------
  ###### --- 
  # Return a dStudy-class object, which is a list containing 5 elements:
  # 1. ds.df: a dataframe with varaince components
  # 2. relvar: relative error variance
  # 3. absvar: absolute error variance
  # 4. gcoef: generalizability coefficient
  # 5. dcoef: generalizability coefficient
  ###### ---
  dstudyResult <- eventReactive(input$runDstudyButton, {
    datD <- datNARemoved()
    
    datD[[selectedOutcome()]] <- as.numeric(datD[[selectedOutcome()]])
    datD[c(input$selectedID, selectedFacet())] <- lapply(datD[c(input$selectedID, selectedFacet())], as.factor)
    
    
    if (input$mGtheory == FALSE) { # 分支1: 若為univariate gstudy
      ## gstudy results
      gstudy_res <- gstudyResult()$gstudy
      dstudy_res = dstudy(x = gstudy_res, n = updatedN, unit = unit())
    }else{# 分支2: 若為multivariate gstudy,运行mdstudy
      mgstudy_res <- gstudyResult()$mgstudy
      dstudy_res = mdstudy(x = mgstudy_res, n = updatedN, unit = unit())
    }
    
    dstudy_res
  })
  
  ### Run dstudy bootstrapping  ----------------------------------------
  ###### --- 
  # return bootstrapping iterations
  ###### ---
  dstudyResultBoot <- eventReactive(input$runDstudyBootButton, {
    datDBoot <- datNARemoved()
    datDBoot[[selectedOutcome()]] <- as.numeric(datDBoot[[selectedOutcome()]])
    datDBoot[c(input$selectedID, selectedFacet())] <- lapply(datDBoot[c(input$selectedID, selectedFacet())], as.factor)
    
    boot_dstudy <- NULL # for bootstrapping iterations
    gstudy_res <- gstudyResult()$gstudy
    boot_gstudy_res <- gstudyResultBoot()
    bootCI_gstudy_res <- gstudyResultBootCI()
    
    for (i in 1:input$nboot) {
      gstudy_res$gstudy.out[, 2] <- t(boot_gstudy_res$t)[, i]
      temp.dstudy <- dstudy.forboot(x = gstudy_res, n = updatedN, unit = unit())
      
      boot_dstudy <- rbind(
        boot_dstudy,
        c(temp.dstudy$ds.df[, 4],
          temp.dstudy$relvar,
          temp.dstudy$absvar,
          temp.dstudy$gcoef,
          temp.dstudy$dcoef
        )
      )
    }
    boot_dstudy # nboot X (vcov.n + var + coef)
  })
  
  ### Run dstudy bootstrapping CI ----------------------------------------
  dstudyResultBootCI <- eventReactive(input$runDstudyBootButton, {
    
    dstudy_res_boot <- dstudyResult() # placeholder
    
    dstudy.res.CI <- t(apply(dstudyResultBoot(), 2, \(x) {quantile(x, probs = c(.025, .975))} ))
    names(dstudy.res.CI) <- c("2.5%", "97.5%")
    
    # beautify output
    dstudy_res_boot$dcoef[2] <- dstudy.res.CI[nrow(dstudy.res.CI), 1]
    dstudy_res_boot$dcoef[3] <- dstudy.res.CI[nrow(dstudy.res.CI), 2]
    names(dstudy_res_boot$dcoef) <- c("Est", "2.5%", "97.5%")
    
    dstudy_res_boot$gcoef[2] <- dstudy.res.CI[nrow(dstudy.res.CI) - 1, 1]
    dstudy_res_boot$gcoef[3] <- dstudy.res.CI[nrow(dstudy.res.CI) - 1, 2]
    names(dstudy_res_boot$gcoef) <- c("Est", "2.5%", "97.5%")
    
    dstudy_res_boot$absvar[2] <- dstudy.res.CI[nrow(dstudy.res.CI) - 2, 1]
    dstudy_res_boot$absvar[3] <- dstudy.res.CI[nrow(dstudy.res.CI) - 2, 2]
    names(dstudy_res_boot$absvar) <- c("Est", "2.5%", "97.5%")
    
    dstudy_res_boot$relvar[2] <- dstudy.res.CI[nrow(dstudy.res.CI) - 3, 1]
    dstudy_res_boot$relvar[3] <- dstudy.res.CI[nrow(dstudy.res.CI) - 3, 2]
    names(dstudy_res_boot$relvar) <- c("Est", "2.5%", "97.5%")
    
    dstudy_res_boot$ds.df <-
      cbind(dstudy_res_boot$ds.df, dstudy.res.CI[1:(-4 + nrow(dstudy.res.CI)), ])
    
    # output
    dstudy_res_boot
  })

  
  ### Output dstudy results and bootstrapping  ----------------------------------------
  output$recommModelDStudyResult <- renderPrint({print.dStudy(dstudyResult())})
  output$recommModelDStudyBootResult <- renderPrint({dstudyResultBootCI()})
  
  ### Output path plot of dstudy ----------------------------------------
  facetLevelsRange <- reactive({input$FacetValueRange})
  
  observeEvent(input$plotDstudyCoef, {
    gstudy_res <- gstudyResult()$gstudy
    
    ###### --- 
    # g coefficient path plot across sample size
    ###### ---
    if (facetLevelsRange() != "") {
      updatedNrange = updatedN ## assign N for dstudy to updatedNrange
      parms = as.numeric(str_split(facetLevelsRange(), ":")[[1]]) # parameters for generate levels
      Levels_min = parms[1]
      Levels_max = parms[2]
      Levels_step = parms[3]
      facetLevels = seq(from = Levels_min, to = Levels_max, by = Levels_step)
      gcoefs = rep(NA, length(facetLevels))
      dcoefs = rep(NA, length(facetLevels))
      
      # loop over each level
      for (i in 1:length(facetLevels)) {
        updatedNrange[selectedFacetForDstudy()] = facetLevels[i]
        dstudy.res <- dstudy(x = gstudy_res, n = updatedNrange, unit = unit())
        gcoefs[i] <- dstudy.res[["gcoef"]]
        dcoefs[i] <- dstudy.res[["dcoef"]]
      }
      ###### ---
      # rho: generalizability coefficient
      # Phi: phi coefficient or an index of dependability.
      ###### ---
      DstudyCoefData = data.frame(N = facetLevels, rho = gcoefs, Phi = dcoefs)
      
      output$DstudyCoefPlot <- renderPlot({
        ticks = seq(0, 1, 0.02)
        labels_point = seq(0, 1, 0.1)
        ylabels = rep("", length(ticks))
        for (i in seq(ticks)) {
          if (as.character(ticks[i]) %in% as.character(labels_point)) {
            ylabels[i] = ticks[i]
          }
        }
        ggplot(DstudyCoefData) +
          geom_line(aes(x = N, y = rho), alpha = 0.4) +
          geom_text(aes(x = N, y = rho), label = "rho", parse = TRUE, size = 5) +
          geom_line(aes(x = N, y = Phi), alpha = 0.4) +
          geom_text(aes(x = N, y = Phi), label = "Phi", parse = TRUE, size = 5) +
          labs(x = "Sample Size", y = "Coefficient",
               title = paste0(selectedFacetForDstudy(),": from ", Levels_min," to ", Levels_max," with step as ", Levels_step)) +
          scale_y_continuous(breaks = ticks,
                             limits = c(0, 1),
                             expand = c(0, 0),
                             labels = ylabels) +
          theme_classic() +
          theme(
            text = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)
          )
      })
      
    }
    
  })   
  
  ### button: dstudy results downloads  ----------------------------------------
  ## dstudy results
  output$downloadDstudyResult <- downloadHandler(
    filename = "dstudyResult.csv",
    content = \(file) {write.csv(dstudyResult()$ds.df, file, row.names = FALSE)}
  )
  ## dstudy results bootstrapping
  output$downloadDstudyBootResult <- downloadHandler(
    filename = paste0("dstudyBootstrapN", input$nboot, "Result.csv"),
    content = \(file) {write.csv(dstudyResultBootCI()$ds.df, file, row.names = FALSE)}
  )
  

}

# Run the application
shinyApp(ui = ui, server = server)
