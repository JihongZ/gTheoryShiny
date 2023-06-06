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

## Include elements of html
source("tutorial_page.R")

## Source all modules
source("modules/inputFile_module.R") # Input CSV file
source("modules/LongToWide_module.R") # Long format to Wide format

## Load example data
data("Rajaratnam.2", package = "gtheory")
data("Brennan.3.2", package = "gtheory")
# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  skin = "purple",
  ## title
  dashboardHeader(title = "GtheoryShiny App"),
  ## Sidebar content -----
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar", 
      menuItem(
        "Tutorial", tabName = "tutorial", icon = icon("dashboard"),
        badgeLabel = "Ver.0.2.Beta", badgeColor = "purple"
      ),
      menuItem("Data Input", tabName = "datainput", icon = icon("th")),
      menuItem("Data Structure", tabName = "datastructure", icon = icon("table")),
      menuItem("Data Analysis", tabName = "dataanalysis", icon = icon("list-alt"))
    )
  ),
  ## Body content -----
  dashboardBody(
    tabItems(
      ### Tab Page 0: Tutorial ----
      tabItem(tabName = "tutorial", tutorial_page),
      
      ### Tag Page 1: Transform data  ----
      tabItem(tabName = "datainput",
        fluidPage(
          box(title = "Input data file", 
              status = "info", solidHeader = TRUE, width = 4, 
              switchInput(
                inputId = "fileUploadSwitch",
                value = TRUE, 
                onLabel = "Uploaded data",
                offLabel = "Example data",
                onStatus = "success",
                offStatus = "danger",
                width = "80%", 
                size = 'normal'
              ),
              conditionalPanel( # if uncheck the switch, let client select the example data
                condition = "input.fileUploadSwitch == 0", 
                radioGroupButtons(
                  inputId = "selectedExpDat",
                  label = "Example data: ",
                  choices = c("Rajaratnam.2", "Brennan.3.2"),
                  direction = "vertical"
                )
              ),
              conditionalPanel( # if check the switch, let client select self data
                condition = "input.fileUploadSwitch == 1", 
                csvFileUI("fileUpload", ""),
                prettySwitch("isLongFormat", "Long format", value = TRUE, status = "info"),
                conditionalPanel(
                  condition = "input.isLongFormat == 0",
                  hr(),
                  h4("Pivot data from wide to long: "),
                  uiOutput("nRowsSelection"),
                  uiOutput("preFixText"),
                  uiOutput("TagNamesText"),
                  ## 转换
                  actionBttn("transform", tagList(icon("rotate"), "Transform"), 
                             style = "jelly", color = "primary", size = "sm"),
                )
              ),
              ## 确定
              br(),
              hr(),
              actionBttn("dataConfirm", label = "Confirm", icon = icon("circle"), 
                         style = "jelly", color = "primary", size = "sm")
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
              pickerInput(
                "missingMethod",
                label = "4. Missing Method:",
                choices = missingMethods,
                selected = missingMethods[1]
              ),
              # show selection area for covariates
              hr(),
              uiOutput("selectedCovariates"),
              hr(),
              uiOutput("mGtheory"),
              conditionalPanel(
                condition = "input.mGtheory == 1",
                uiOutput("selectedFixedFacet"),
                uiOutput("selectedFacetWithUSComponent"), # facet with unstructured components
                verbatimTextOutput("reportFacets"),
              ),
              actionBttn("variableSettingConfirm", "Confirm", 
                         icon = icon("circle"), style = "jelly", color = "primary", size = "sm")
          ),
          tabBox(id = "modelDesign", title = tagList(icon("diagram-next"), "Design"), 
            tabPanel(title = "Formular", textOutput("recommFormular"), 
                     tags$head(tags$style("#recommFormular{color:red; font-size:14px; font-style:bold;
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
          
          column(width = 4,
            box(title = "Control Panel: ", status = "danger", solidHeader = TRUE, width = NULL, 
                textInput("selfFormular", "1. User-specified formula: ",
                          placeholder = "Default: recommeded formula"),
                pickerInput(
                  "linkFunc",
                  label = "2. Link Function:",
                  choices = c("identity", "logit", "poisson", "inverse gamma"),
                  selected = "identity"
                ),
                sliderInput(
                  inputId = "nboot",
                  label = "3. Number of bootstrap for G-study and D-study",
                  min = 100,
                  max = 1000,
                  value = 500
                ),
                uiOutput("nCores")
            ),
            
            box(title = "Gstudy Estimation", status = "info", solidHeader = TRUE, width = NULL,
                actionBttn("runGstudyButton", "Run Gstudy", style = "material-flat",
                           color = "primary", size = "sm", icon = icon('circle-play')),
                br(), 
                actionBttn("runGstudyBootButton", "Run Bootstrap", style = "material-flat", 
                           color = "primary", size = "sm", icon = icon("bootstrap")),
                br(), 
                progressBar(
                  id = "gstudybar",
                  value = 0,
                  total = 100,
                  title = "boostrapping gstudy",
                  striped = TRUE,
                  display_pct = TRUE
                ),
                ### 下载按钮
                br(),
                conditionalPanel(
                  condition = "input.runGstudyButton >=1",
                  downloadButton("downloadGstudyTheta", "Factor Score Table")
                ),
                br(), 
                conditionalPanel(
                  condition = "input.runGstudyBootButton >=1",
                  downloadButton("downloadGstudyBootResult", "Bootstrapping Result")
                )
            ),
            
            box(title = "Dstudy Estimation", status = "warning", solidHeader = TRUE, width = NULL,
                prettySwitch("runDstudyBox", label = "Run Dstudy", 
                             value = FALSE,
                             bigger = TRUE),
                conditionalPanel(condition = "input.runDstudyBox == 1",
                  h4("Dstudy："),
                  ### 选择要修改的facet和number of conditions
                  uiOutput("selectedFacetMenu"),
                  uiOutput("selectedFacetLevels"),
                  uiOutput("selectedMultipleFacetLevels"),
                  #选择factor levels
                  actionBttn(inputId = "confirmFacetLevel",
                             label = "Add facet levels", icon = icon("check"),
                             style = "material-flat", color = "primary", size = "sm"),
                  actionBttn("runDstudyButton", "Run", icon = icon('circle-play'),
                             style = "material-flat", color = "primary", size = "sm"),
                  actionBttn("runDstudyBootButton", "Run Bootstrap", icon = icon("bootstrap"),
                             style = "material-flat", color = "primary", size = "sm"),
                  actionBttn("plotDstudyCoef", "Visualize", icon = icon('image'),
                             style = "material-flat", color = "primary", size = "sm"),
                  progressBar(id = "dstudybar", 
                              value = 0,
                              total = 100,
                              title = NULL,
                              striped = TRUE,
                              display_pct = TRUE),
                  ### 下载按钮dstudy
                  conditionalPanel(
                    condition = "input.runDstudyButton >=1",
                    downloadButton("downloadDstudyResult", "Download dstudy result"),
                    downloadButton("downloadDstudyBootResult", "Download bootstrap result")
                  )
                )
            )
          ),
          
          column(width = 8,
            box(title = "Gstudy Result", width = NULL, collapsible = TRUE, collapsed = FALSE,
                solidHeader = TRUE, status = 'info', 
                #### Output UI for gstudy  ----
                conditionalPanel(
                  condition = "input.runGstudyButton >= 1",
                  h4("Fixed Effects Output："),
                  DTOutput("recommModelFixedEffectResult"),
                  h4("Random Effects Output："),
                  DTOutput("GstudyResultPrint"),
                  h4("Other Output："),
                  verbatimTextOutput("GstudyResultExtraPrint")
                ),
                # gstudy with bootstrapping SD
                conditionalPanel(
                  condition = "input.runGstudyBootButton >= 1",
                  h4("Estimate Bootstrapping SD for G-study："),
                  DTOutput("recommModelGStudyBootResult")
                )
            ),
            
            box(title = "Dstudy Result", width = NULL, collapsible = TRUE, collapsed = FALSE,
                solidHeader = TRUE, status = 'info', 
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
            
          )
          
        )
      )
    )
  ), # End of dashboardBody
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "default_mode.css")
  )
) # End of dashboardPage




# Server -----------------------------------------------------------------
server <- function(input, output, session) {

  ## Server for Page 1: Data read and transformation  ----------------------------------------
  ### Read in original data ----------------------------------------
  datRaw <- csvFileServer("fileUpload", stringsAsFactors = FALSE)
  
  ### Reactive dat -----
  dat <- eventReactive(input$dataConfirm, {
    if (input$fileUploadSwitch == 0) { # if use example data
      dat = get(input$selectedExpDat)
      dat
    }else{ # if use user-uploaded data
      if(input$transform == 1){
        datTrans()
      }else{
        datRaw()
      }
    }
  })
  
  ### Output raw data table-----
  output$rawDataTable <- renderDT({
    if (input$fileUploadSwitch == 0) {
      get(input$selectedExpDat)
    }else{
      datatable(datRaw()) |> 
        formatRound(colnames(datRaw())[sapply(datRaw(), is.numeric)], digits = 3)
    }
  })
  
  ### UI selectors for facets specifications  ----------------------------------------
  #------------#
  # A bunch of reactive values:
  ## NText: 有多少行是tag/ID
  ## preFixText: tag的前綴是什麼比如第四個item—I4，為I
  ## TagNamesText: facet的name比如：Class;Rater;Item
  #------------#
  NText = reactive({input$nRows}) # 有多少行是tag/ID
  preFixText = reactive({input$TagPreFix}) # 前缀，比如A;B;C
  TagNamesText = reactive({input$TagNames}) # facet的name比如：Class;Rater;Item
  
  observeEvent(input$isLongFormat, {
    # 按下"transform"按钮后，将原始数据转换为长数据格式:
    output$nRowsSelection <- renderUI({ # 选择多少行代表Facet Tags，每一行代表一个facet
      pickerInput(
        "nRows",
        "Select the number of rows containing facets (not including headings)",
        choices = 0:nrow(datRaw()),
        selected = 0
      )
    })
    
    # when observed client specify number of rows denoting facets, react to update TagNames and TagPrefix
    observeEvent(input$nRows, {
      number_rows_facets = NText()
      number_rows_facets <- max(1, number_rows_facets)
      
      output$preFixText <- renderUI({ # 为facets选择prefix
        textInput("TagPreFix", 
                  label = "Enter prefix of facet levels seperated by `,`（for example, `O,I`)" , 
                  value = paste0(LETTERS[1:number_rows_facets], collapse = ","))
      })
      
      output$TagNamesText <- renderUI({
        textInput("TagNames",
                  label = "Enter facet's names seperated by `,` (for example, `Occasion,Item`)",
                  value = paste0(paste0(LETTERS[1:number_rows_facets], letters[1:number_rows_facets]),
                                 collapse = ","))
      })
    })
    
  })
  

  #------------#
  # Transformation logic
  ## 若觀察到用戶點擊"transform"按鈕,判斷是否提供了tag前綴(比如I)和tag名字(比如Item)
  ## 分支（1）：若tag前綴和tag名字都提供，將tag前綴和tag名字分離。判斷用戶提供的tag行數數目
  ### 分支（1.1）: 若tag行數數目為0，默認用戶tag信息放在heading裡面。直接用tag前綴進行transform。
  ### 分支（1.2）: 若tag行數數目大於0，默認用戶tag信息不放在heading裡面，而在數據的前N行。將前N行tag合併成一個特殊tag ID(mergeID)進行transform。mergeID將不同facet的tag用下劃線進行區別。最後再將mergeID分離成好幾個facet列。每一列都冠以tagNames。
  ## 分支（2）：若tag前綴和tag名字都不提供且tag行數（NText）為0。則直接將除一個列（ID）以外的列進行transform。適用於single facet design。
  #------------#
  datTrans <- eventReactive(input$transform, {
    N <- as.numeric(NText()) # number of facets
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
  output$transDataTable <- renderDT({datatable(datTrans()) %>% formatRound('Score', 2)})

  ### Update UI for data confirm and switch to Tab page 2 -----
  observeEvent(input$dataConfirm, {
    updateActionButton(
      session,
      inputId = "dataConfirm",
      icon = icon("check")
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
      pickerInput(
        "selectedID",
        "1. ID variable:",
        choices = colnames(dat),
        selected = colnames(dat)[1]
      )
    })
    
    # 选择outcome
    output$selectedOutcome <- renderUI({
      pickerInput(
        "selectedOutcome",
        "2. Outcome variable:",
        choices = setdiff(colnames(dat), input$selectedID),
        selected = setdiff(colnames(dat), input$selectedID)[1]
      )
    })
    
    # 选择facet
    output$selectedMultipleFacets <- renderUI({
      pickerInput(
        "selectedMultipleFacets",
        "3. Facet(s):",
        choices = setdiff(colnames(dat), c(input$selectedID, input$selectedOutcome)),
        selected = setdiff(colnames(dat), c(input$selectedID, input$selectedOutcome)),
        multiple = TRUE
      )
    })
    
    # 选择covariates
    output$selectedCovariates <- renderUI({
      prettyCheckboxGroup(
        inputId = "selectedCovariates",
        label = "(Optional) Select covariate(s):",
        choices = setdiff(colnames(dat), c(input$selectedID, input$selectedOutcome)),
        selected = NULL,
        icon = icon("check"), 
        shape = "round",
        status = "primary",
        inline = TRUE,
        animation = "jelly"
      )
    })
    
    # 选择是否要进行mGtheory
    output$mGtheory <- renderUI({
      prettySwitch(inputId = "mGtheory", 
                    label = strong("(Optional) mGtheory"),
                    value = FALSE, width = "400px")
    })
    
    # mgTheory选择fixed facet
    output$selectedFixedFacet <- renderUI({
      pickerInput(
        "selectedFixedFacet",
        "5. Select fixed facet:",
        choices = selectedFacet(),
        selected = selectedFacet()[1]
      )
    })
    
    # mgTheory选择 facet with unstructured var-cov matrix
    output$selectedFacetWithUSComponent <- renderUI({
      # all random facets except fixed facet
      all_facets = c(selectedID(), selectedFacet())
      random_facets <- setdiff(all_facets, input$selectedFixedFacet)
      checkboxGroupButtons(
        "selectedFacetWithUSComponent",
        label = "6. Select facet(s) with covariances to be estimated:",
        choices = random_facets,
        selected = random_facets,
        status = "success",
        checkIcon = list(
          no = icon("circle"),
          yes = icon("check")
        )
      )
    })
    
    output$reportFacets <- renderText({
      facets_cov_text <- paste0(selectedFacetWithUSComponent(), collapse = "; ")
        
      facets_var_text <- paste0(setdiff(c(selectedID(), selectedFacet()), 
                                        c(selectedFixedFacet(), selectedFacetWithUSComponent())), 
                                collapse = "; ")
        
      glue::glue("ID: {selectedID()};\n
                 Outcome: {selectedOutcome()};\n
                 Fixed Facet: {selectedFixedFacet()};\n
                 Random Facet with covariances estimated: {facets_cov_text}; \n
                 Random Facet with only variances estimated: {facets_var_text}; \n
                 ")
    })
  })
  
  ### Reactive facets / outcomes / ID ----------------------------------------
  selectedID = reactive({input$selectedID}) ## ID
  selectedOutcome = reactive({input$selectedOutcome}) ## Outcome (i.e., Score)
  selectedFacet = reactive({input$selectedMultipleFacets}) ## Facets used for gstudy/dstudy
  selectedCovariates = reactive({input$selectedCovariates}) ## covariates for fixed effects
  selectedMissingMethod = reactive({input$missingMethod}) ## selected missing data handling method
  ## mGtheory
  selectedFixedFacet = reactive({input$selectedFixedFacet}) ## mgtheory: fixed facet
  selectedFacetWithUSComponent = reactive({input$selectedFacetWithUSComponent})
  

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
  
  ### nestedStrc: automatically detect design of data and return structure table-----
  nestedStrc <- eventReactive(input$variableSettingConfirm, {
    dat <- datNARemoved()
    selectedFacet <- c(selectedFacet(), selectedID()) # facets and ID
    
    if (length(selectedFacet) == 1) { # if single facet
      nestedStrc = data.frame(NA)
      colnames(nestedStrc) = selectedFacet
      nestedStrc
    }else{ # if multiple facets
      # Each row represents a pair of facets
      TagPairs <- as.data.frame(t(combn(selectedFacet, 2))) 
      # Placeholder for structure table
      NestedorCrossed = rep(NA, nrow(TagPairs))
      # Set two facets into "f1" and "f2"
      colnames(TagPairs) <- c("f1", "f2")
      # Compare facet with the other pair by pair
      for (pair in 1:nrow(TagPairs)) {
        whichpair = TagPairs[pair,]
        isNested <- suppressMessages(sjmisc::is_nested(dat[[whichpair[[1]]]], dat[[whichpair[[2]]]]))
        NestedorCrossed[pair] = ifelse(isNested, "Nested", "Crossed")
      }
      nestedStrc = TagPairs
      nestedStrc$NestedorCrossed = NestedorCrossed
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
  ##        1, 生成selectedOutcome() ~ (1 | selectedID()) + (1 | selectedFacet())
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
     selectedID <- selectedID()

     if (ncol(nestedStrcTable) == 1) { # 分支1:single facet

       formularText <- glue::glue("{selectedOutcome} ~ (1 |{selectedID}) + (1 |{selectedFacet})")

     }else if (ncol(nestedStrcTable) > 1) { # 分支2:multiple facets
       if (input$mGtheory == FALSE) { # 分支2.1: univariate gtheory
         for (r in 1:nrow(nestedStrcTable)) { # loop over each row
           if (nestedStrcTable[r, "NestedorCrossed"] == "Crossed") {
             formularFacets <- c(formularFacets, 
                                 glue::glue("(1 | {nestedStrcTable[r, 1:2]})"))
           }
           if (nestedStrcTable[r, "NestedorCrossed"] == "Nested") {
             tab <- table(nestedStrcTable[r, 1], nestedStrcTable[r, 2])
             nested <- !any(apply(tab, 1, \(x) sum(x != 0) > 1))
             if (nested) {
               formularFacets <-
                 c(
                   formularFacets,
                   glue::glue("(1 | {nestedStrcTable[r, 1]})"),
                   glue::glue("(1 | {nestedStrcTable[r, 2]}:{nestedStrcTable[r, 1]})")
                  )
             } else{
               formularFacets <-
                 c(formularFacets,
                   glue::glue("(1 | {nestedStrcTable[r, 2]})"),
                   glue::glue("(1 | {nestedStrcTable[r, 1]}:{nestedStrcTable[r, 2]})")
                  )
             }
           }
         }
         ## add covariates and facets into the formulate
         formularText <- paste0(c(selectedCovariates, unique(formularFacets)), collapse = " + ")
         formularText <- glue::glue("{selectedOutcome} ~ {formularText}")
         formularText
       }else{ # 分支2.2: multivariate gtheory
         fixedfacet <- selectedFixedFacet()
         randomfacets <- c(selectedID, setdiff(selectedFacet, fixedfacet))
         formularRHS <- paste0(glue::glue("us({fixedfacet} + 0 | {randomfacets})"), collapse = " + ")
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
     output$nestedStrucTable <- renderDT({nestedStrc()})

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
  # 分支1: 若為univariate gstudy，運行glmer
  # 分支2: 若為multivariate gstudy，運行lmmTMB::glmmTMB
  #------------#

  ### Run gstudy  ----------------------------------------
  #### Link Functions  ----------------------------------------
  linkFuncText <- reactive({input$linkFunc})
  
  ###### --- 
  # Return a list with three elements:
  # lmmFit: lme4 object
  # fixedEffect: vector; fixed effect
  # VarComp: matrix; variance-covariance matrix
  # gstudy: gstudy Class
  ###### ---
  gstudyResult <- eventReactive(input$runGstudyButton, {
    nFacet <- NULL
    linkFuncText <- linkFuncText()
    datG <- datNARemoved() # data used for gstudy
    ## make sure outcome as numeric and facet as factors
    datG[[selectedOutcome()]] <- as.numeric(datG[[selectedOutcome()]])
    datG[c(input$selectedID, selectedFacet())] <- lapply(datG[c(input$selectedID, selectedFacet())], as.factor)
    
    ###### --- 
    # Family selection
    # c("identity", "logit", "poisson", "inverse gamma")
    ###### ---
    if (linkFuncText == "identity") {
      linkFunc <- gaussian(link = "identity")
      # glmer = lme4::lmer
    }else if(linkFuncText == "logit"){
      linkFunc <- binomial(link = "logit")
    }else if(linkFuncText == "poisson"){
      linkFunc <- poisson(link = "log")
    }else if(linkFuncText == "inverse gamma"){
      linkFunc <- Gamma(link = "inverse")
    }else{
      linkFunc <- gaussian(link = "identity")
    }
    
    if (input$mGtheory == FALSE) { # 分支1: 若為univariate gstudy
      if (input$selfFormular == "") { # 分支1.1.若用戶沒有自定義公式
        formulaRecomm <- as.formula(gtheoryFormula())
        lmmFit <- glmer(data = datG, formula = formulaRecomm, family = linkFunc)
      } else{ # 分支1.1.若用戶自定義公式，則轉化爲lme4直接使用用戶的公式
        lmmFit <- glmer(data = datG, family = linkFunc,
                        formula = as.formula(makehardformular(input$selfFormular)))
      }
      ## Random effects
      randomEffectEstimate <- ranef(lmmFit)
      randomEffectLevel <- sapply(lapply(datG[selectedFacet()], unique), length)
      allRandomFacets <<- unlist(randomEffectLevel[selectedFacet()])
      fixedEffectEstimate <- as.data.frame(summary(lmmFit)$coefficients)
      
      # return
      list(
        lmmFit = lmmFit,
        fixedEffect = fixedEffectEstimate,
        VarComp = gstudy(lmmFit)$gstudy.out,
        gstudy = gstudy(lmmFit)
      )
      
    }else{# 分支2: 若為multivariate gstudy
      if (input$selfFormular == "") { # 分支2.1.若用戶沒有自定義公式
        formulaTxt <- gtheoryFormula()
        formulaRecomm <- as.formula(formulaTxt)
        lmmFit <- glmmTMB::glmmTMB(
          data = datG,
          formula = formulaRecomm,
          family = linkFunc,
          dispformula = ~0
        )
        ## extract residual var-cov matrix
        residuals_Person <- cbind(residuals = residuals(lmmFit, "response"),
                                  datG[c(selectedID(), selectedFacet())]) %>%
          pivot_wider(names_from = selectedFixedFacet(), 
                      values_from = residuals, names_prefix = "facet") %>%
          ungroup()
        residual_cor = cor(residuals_Person |> dplyr::select(starts_with("facet")),
                           use = "pairwise.complete.obs")
        residual_cov = cov(residuals_Person |> dplyr::select(starts_with("facet")),
                           use = "pairwise.complete.obs")
        
        ## run second time
        dat2 = datG
        dat2$Residual = residuals(lmmFit, "response")
        formulaWtResidTxt <- paste0(formulaTxt, "+ diag(", selectedFixedFacet(), " + 0 | Residual)")
        formulaRecommWtResid <- as.formula(formulaWtResidTxt)
        lmmFit <- glmmTMB::glmmTMB(
          formula = formulaRecommWtResid,
          data = dat2,
          family = linkFunc,
          dispformula =~0
        )
        
        # Extract useful information
        res <- lme4::VarCorr(lmmFit)
        resVarCor <- extract.VarCorr.glmmTMB(x = res$cond, residCor = residual_cor)
        fixedEffectEstimate <- extractFixedCoefsmG(lmmFit)
        
        ## generalizability coefficient
        g_coef <- gCoef_mGTheory(
          dat = datG[c(selectedID(), selectedFacet())],
          nDimension = n_distinct(datG[selectedFixedFacet()]),
          glmmTMBObj = lmmFit,
          residual_cov = residual_cov,
          person_ID = selectedID()
        )
        
        
        list(
          lmmFit = lmmFit,
          fixedEffect = fixedEffectEstimate,
          VarComp = resVarCor,
          g_coef = g_coef # return a mgStudy class
        )
        
      } else{ # 分支2.1.若用戶自定義公式，則轉化爲lme4直接使用用戶的公式
        shinyjs::alert("This function is currently not available!")
      }
    }
  })     
  
  ### Run gstudy bootstrapping ----------------------------------------
  output$nCores <- renderUI({
    pickerInput(inputId = "nCores",
                label = "4. Select number of cores for bootstrapping:",
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
    
    # ProgressBar: start of bootstrapping
    updateProgressBar(session = session, id = "gstudybar", 
                      value = 30, total = 100,
                      title = "In progress")
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
    # ProgressBar: end of bootstrapping
    updateProgressBar(session = session, id = "gstudybar", 
                      value = 100, total = 100,
                      title = "Finished", status = "success")
    boot.gstudy
  })
  
  ### Run gstudy bootstrapping CI ----------------------------------------
  ###### --- 
  # Return data frame of gstudy variance components with bootstrapping CI
  ###### ---
  gstudyResultBootCI <- eventReactive(input$runGstudyBootButton, {
    gstudy_res <- gstudyResult()$VarComp # Variance-covariance
    boot_gstudy_res <- gstudyResultBoot() # bootMer object
    
    # calculate bootstrap CI and bind with estimated variances (and covarinces, if mgstudy)
    if (input$mGtheory == FALSE) {
      bootCI_gstudy_res <-
        t(apply(boot_gstudy_res$t, 2, function(x) {quantile(x, probs = c(.025, .975))}))
      as.data.frame(cbind(gstudy_res, bootCI_gstudy_res))
    }else{
      # ongoing: placeholder for mgstudy boostrap
    }
  })
  
  ### Output gstudy results and bootstrapping  ----------------------------------------
  output$recommModelFixedEffectResult <- renderDT(round(gstudyResult()$fixedEffect, 3))
  output$GstudyResultPrint <- renderDT({
    res <- gstudyResult()$VarComp
    datatable(res) |> 
      formatRound(colnames(res)[sapply(res, is.numeric)], digits = 3)
  })
  output$GstudyResultExtraPrint <- renderText({
    if (input$mGtheory == TRUE) {
      glue::glue("g-coefficient: {gstudyResult()$g_coef}")
    }
  })
  output$recommModelGStudyBootResult <- renderDT({
    res <- gstudyResultBootCI()
    datatable(res) |> 
      formatRound(colnames(res)[sapply(res, is.numeric)], digits = 3)
  })
  
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
  #### UI for facet and condition selection  ----------------------------------------
  observeEvent(input$runDstudyBox, {
    defaultN <- defaultN()
    
    #------------#
    # Select which facet to add sample size
    #------------#
    output$selectedFacetMenu <- renderUI({
      pickerInput(inputId = "FacetDStudySelector",
                  label = "Select facet:",
                  choices = selectedFacet(),
                  multiple = FALSE)
    })
    
    #------------#
    # Select number of conditions for target facet
    #------------#
    observeEvent(input$FacetDStudySelector, {
      selectedFacetForDstudy <- selectedFacetForDstudy()
      ###### ---
      # Facet levels slider for selected Facet
      ###### ---
      output$selectedFacetLevels <- renderUI({
        numericInput(
          inputId = "FacetValueSlider",
          label = "Enter number of conditions: ",
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
          label = "(Optional) Enter the range of conditions: ",
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
  # selectedFacetValue: number of conditions for certain facet
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
      dstudy_res = dstudy(x = gstudy_res, n = updatedN, unit = selectedID())
    }else{# 分支2: 若為multivariate gstudy,运行mdstudy
      mgstudy_res <- gstudyResult()$mgstudy
      dstudy_res = mdstudy(x = mgstudy_res, n = updatedN, unit = selectedID())
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
    
    updateProgressBar(session = session, id = "dstudybar", 
                      value = 30, total = 100,
                      title = "In progress: 30%")
    for (i in 1:input$nboot) {
      gstudy_res$gstudy.out[, 2] <- t(boot_gstudy_res$t)[, i]
      temp.dstudy <- dstudy.forboot(x = gstudy_res, n = updatedN, unit = selectedID())
      
      boot_dstudy <- rbind(
        boot_dstudy,
        c(temp.dstudy$ds.df[, 4],
          temp.dstudy$relvar,
          temp.dstudy$absvar,
          temp.dstudy$gcoef,
          temp.dstudy$dcoef
        )
      )
      updateProgressBar(session = session, id = "dstudybar", 
                        value = 30+(70 / input$nboot * i), total = 100,
                        title = paste0("In progress: ", round(30+(70 / input$nboot * i), 2), "%"))
    }
    updateProgressBar(session = session, id = "dstudybar", 
                      value = 100, total = 100,
                      title = "Finished",
                      status = "success")
    boot_dstudy # nboot X (vcov.n + var + coef)
  })
  
  ### Run dstudy bootstrapping CI ----------------------------------------
  dstudyResultBootCI <- eventReactive(input$runDstudyBootButton, {
    
    dstudy_res_boot <- dstudyResult() # placeholder using dstudy results
    boot_iteration_raw <- dstudyResultBoot()
    
    dstudy.res.CI <- t(apply(boot_iteration_raw, 2, 
                             \(x) {quantile(x, probs = c(.025, .975))} ))
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
        dstudy.res <- dstudy(x = gstudy_res, n = updatedNrange, unit = selectedID())
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
