#
# This is a Shiny web application for g theory visualization. ]
#
# Pathway:
# 1, whether need bootstrapping sd
#
# Data:
#     1. sleepstudy:
#         (1) Reaction: average reaction time
#         (2) Days: number of days of sleep
#         (3) Subject: Subject number

#    http://shiny.rstudio.com/
#

# 【X】 1) data input应该也允许用户们上传long format的，只有当数据是wide format我们才提供transform的service
# 【X】 2）bootstrap得允许人家设置bootstrap多少次
# 【X】 3）extract出random effect可供下载
# 【X】 4）留一个说明页的tab界面用来放instruction或tutorials
# 【x】 5）测试一下其他不同design数据形式下，比如crossed和4个多重nested level，弄几个内置sample dataset可以供人直接下载mock on
# 【X】 6）加入co-variate的功能


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

# Define UI for application that draws a histogram
ui <- navbarPage(
  "gtheory Shiny App",
  
  # title
  useShinyjs(),
  
  # Tab Page 0: Tutorial ----------------------------------------
  tabPanel(
    "Tutorial",
    # Sidebar with a slider input for number of bins
    fluidPage(
      # title
      titlePanel("gTheoryShiny (ver. 0.1.0) Tutorial"),
      
      #
      h4("Example data:"),
      p(
        span("Rajaratnam.2.new.csv", style = "color:blue"),
        " is a example data with 4-way crossed/nested example. The facets include",
        strong("Subset, Item, Rater, Ocasion"),
        "; ID variable is",
        strong("Person"),
        "; Outcome variable is",
        strong("Score"),
        "."
      ),
      
      p(
        span("Rajaratnam.2.new.withCovariate.csv", style = "color:blue"),
        " is similar to Rajaratnam.2.new except two covarites added. The facets include",
        strong("Subset, Item, Rater, Ocasion"),
        ";ID variable is",
        strong("Person"),
        "; Outcome variable is",
        strong("Score"),
        "; Covariates are",
        strong("trait1 and trait2"),
        "."
      ),
      
      h4("Step 1: Load data and transformation"),
      p(
        "Click ",
        em("Data Input"),
        " tab on the top navigation tools. Chick Upload button and select the data. Check the ",
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
  
  # Tag Page 1: A Tab to transform data from wide to long -------------------
  tabPanel("Data Input",
           sidebarLayout(
             sidebarPanel(
               csvFileUI("fileUpload", h4("Choose CSV File:")),
               p(
                 "Notice: If data is wide-format, make sure the first two rows of your data file should be TAG/ID, first column should be subject ID"
               ),
               verbatimTextOutput("newNotificaion"),
               
               # pivotwiderUI("LongToWide")
               tagList(
                 h4("Transform wide format to long format:"),
                 ## 设定tag/ID前缀和标签
                 checkboxInput("isLongFormat", "Your data is long format?", TRUE),
                 conditionalPanel(
                   condition = "input.isLongFormat == 0",
                   uiOutput("nRowsSelection"),
                   uiOutput("preFixText"),
                   uiOutput("TagNamesText"),
                   ## 转换
                   actionButton("transform", "Transform")
                 )
               )
             ),
             mainPanel(
               h2("Raw Data:"),
               DTOutput("rawDataTable"),
               DTOutput("transDataTable")
             )
           )),
  
  
  # Tab Page 2: Check Data Structure ----------------------------------------
  tabPanel("Data Structure",
           # Sidebar with a slider input for number of bins
           sidebarLayout(
             sidebarPanel(
               h4("Control Panel："),
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
               # whether mGtheory is used
               hr(),
               h5("(Optional) Multivariate G-theory Setup:"),
               # tags$b(tags$span(style="color:darkgrey; font-size:18px",
               #                  "ignore if you use univariate G-theory")),
               checkboxInput("mGtheory", label = "mGtheory?", FALSE),
               ## 设定tag/ID前缀和标签
               conditionalPanel(
                 condition = "input.mGtheory == 1",
                 uiOutput("selectedFixedFacet"),
                 uiOutput("selectedSubtest")
               ),
               actionButton("variableSettingConfirm", "Confirm")
             ),
             # Show a plot of the generated distribution
             mainPanel(
               h4("Recommended default model: "),
               textOutput("recommFormular"),
               tags$head(
                 tags$style(
                   "#recommFormular{color:red; font-size:14px; font-style:bold;
            overflow-y:scroll; max-height: 50px; background: ghostwhite;}"
                 )
               ),
            h4("Structural Table:"),
            DTOutput("nestedStrucTable"),
            h4("Summary Table:"),
            DTOutput("factorNestTable"),
             )
           ),),
  
  # Tab Page 3: Check Data Analysis ----------------------------------------
  tabPanel("Data Analysis",
           sidebarLayout(
             sidebarPanel(
               h3("Formula Setting: "),
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
                 value = 200
               ),
               hr(),
               ## Page: Run gstudy ----------------------------------------
               ## 运行gstudy
               h3("Gstudy："),
               actionButton("runRecommModel", "gstudy estimate"),
               actionButton("runRecommModelBoot", "bootstrap estimate"),
               conditionalPanel(
                 condition = "input.mGtheory == 1",
                 actionButton("runmGtheoryModel", "multivariate G-theory estimate")
               ),
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
                 condition = "input.runRecommModel >=1",
                 downloadButton("downloadGstudyTheta", "Download theta estimates")
               ),
               conditionalPanel(
                 condition = "input.runRecommModel >=1",
                 downloadButton("downloadGstudyResult", "Download gstudy result")
               ),
               conditionalPanel(
                 condition = "input.runRecommModelBoot >=1",
                 downloadButton("downloadGstudyBootResult", "Download bootstrap result")
               ),
               
               
               hr(),
               ## 运行dstudy
               h4("Dstudy："),
               ### 选择要修改的facet
               uiOutput("selectedFacetMenu"),
               #选择一个facet
               uiOutput("selectedFacetLevels"),
               #选择factor levels
               conditionalPanel(
                 condition = "input.runRecommModel >= 1",
                 actionButton(inputId = "confirmFacetLevel",
                              label = "confirm facet levels")
               ),
               actionButton("runRecommModelDstudy", "dstudy estimate"),
               actionButton("runRecommModelDstudyBoot", "bootstrap estimate"),
               progressBar(id = "dstudybar",
                           value = 0,
                           total = 100),
               ### 下载按钮dstudy
               conditionalPanel(
                 condition = "input.runRecommModelDstudy >=1",
                 downloadButton("downloadDstudyResult",
                                "Download dstudy result")
               ),
               conditionalPanel(
                 condition = "input.runRecommModelDstudyBoot >=1",
                 downloadButton("downloadDstudyBootResult",
                                "Download bootstrap result")
               ),
             ),
             # Output panel: gstudy / dstudy ----
             mainPanel(
               conditionalPanel(
                 condition = "input.runRecommModel >= 1",
                 h4("Gstudy Output："),
                 h5("Fixed Effects Output："),
                 verbatimTextOutput("recommModelFixedEffectResult"),
                 h5("Random Effects Output："),
                 verbatimTextOutput("recommModelGStudyResult")
               ),
               # gstudy with bootstrapping SD
               conditionalPanel(
                 condition = "input.runRecommModelBoot >= 1",
                 h4("Estimate Bootstrapping SD for G-study："),
                 verbatimTextOutput("recommModelGStudyBootResult")
               ),
               conditionalPanel(
                 condition = "input.runRecommModelDstudy >= 1",
                 h4("Dstudy Output："),
                 p("Sample Size:"),
                 verbatimTextOutput("updatedN"),
                 p("Result:"),
                 verbatimTextOutput("recommModelDStudyResult")
               ),
               # dstudy with bootstrapping SD
               conditionalPanel(
                 condition = "input.runRecommModelDstudyBoot >= 1",
                 h4("Estimate Bootstrapping SD for D-study："),
                 verbatimTextOutput("recommModelDStudyBootResult")
               ),
             )
           ),),
  
  
  
  # End of UI ---------------------------------------------------------------
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # 上传数据
  datRaw <- csvFileServer("fileUpload", stringsAsFactors = FALSE)
  
  # 把数据转化为长数据
  # 按下"transform"按钮后，将原始数据转换为长数据格式:
  output$nRowsSelection <- renderUI({ # 选择多少行代表Facet Tags，每一行代表一个facet
    selectInput(
      "nRows",
      "How many rows represent tages of facets except headings? By default, heading reprents the only facet. For more than one facet, you should have same number of tagging rows as number of facets. Each row represents each facet.",
      choices = 0:nrow(datRaw()),
      selected = 0
    )
  })
  
  output$TagNamesText <- renderUI({
    textInput("TagNames",
              "What are names of your facets (for example, for two facets — Occasion and Item, simply type in `Item;Occasion`. Note that the sequence of names should be same as the sequence of prefix below and rows in your data. If only one facet exsit, type `Item`.)",
              "Occasion;Item")
  })
  
  output$preFixText <- renderUI({ # 为facets选择prefix
    textInput("TagPreFix", 
              "What are prefix of facet(s) to identify levels of each facet（for example, for two facets — Occasion and Item, you may want to label levels of Occasion as I1/I2/I3 and levels of Occasion as O1/O2/O3, then you can type in `I;O`. If only one facet exsit, type `I`.", 
              value = "O;I")
  })
  
  
  # 显示数据
  output$rawDataTable <- renderDT({
    datRaw()
  })
  
  # tag/ID information---------------------
  NText = reactive({
    input$nRows
  }) # 有多少行是tag/ID
  preFixText = reactive({
    input$TagPreFix
  }) # 前缀，比如A;B;C
  TagNamesText = reactive({
    input$TagNames
  }) # facet的name比如：Class;Rater;Item
  
  observeEvent(datRaw(), {dat <<- datRaw()})
    

  observeEvent(input$transform, {
    N = as.numeric(NText())
    if (preFixText() != "" & TagNamesText() != "") { #如果preFix和TagNames都提供
      TagPreFix <- str_split(preFixText(), ";")[[1]]
      TagNames <- str_split(TagNamesText(), ";")[[1]]
      # browser()
      if (N == 0) { # 如果没有facet行，默认facet藏在heading里且为single facet,将PreFix开头的转化为long，冠以TagNames
        if (length(TagPreFix) > 1) {
          shinyalert("Oops!", "Your specification of facets are not correct.", type = "error")
        }
        if (sum(str_detect(colnames(datRaw()), pattern = TagPreFix)) == 0 ) {
          shinyalert("Oops!", "Your specification of facets are not correct.", type = "error")
        }
        
        dat <<- dplyr::tibble(datRaw()) |> 
          pivot_longer(starts_with({{TagPreFix}}), names_to = TagNames, values_to = "Score")
        
      }else if (N > 0){ # 如果有facet行，将tags of facet转换为合并的ID再进行long-format转化
        tags = datRaw()[1:N, -1] # 取出levels名字
        # 将所有facets合并成特殊ID
        # browser()
        mergedID = apply(tags, 2, \(x) paste0(x, collapse = "_"))
        
        dat_FacetNameOmitted <- dplyr::tibble(datRaw()[-(1:N), ]) # 去掉了facet信息的纯数据
        colnames(dat_FacetNameOmitted) <- c("ID", mergedID)
        
        dat <<- dat_FacetNameOmitted |>
          pivot_longer(cols = any_of(as.character(mergedID)), values_to = "Score") |>
          separate(name, into = TagNames, sep = "_")
      }
      
    }else if (preFixText() == "" & TagNamesText() == "" & N == 0){
      dat <<- dplyr::tibble(datRaw()) |> 
        pivot_longer(any_of(colnames(datRaw())[-1]), names_to = "Facet", values_to = "Score")
    }else{
      # Show a modal when the button is pressed
      shinyalert("Oops!", "Your specification of facets are not correct.", type = "error")
    }
    
    # output file
    output$transDataTable <- renderDT({
      dat
    })
  })
  
  # Server for tag page 2 ----------------------------------------------------
  ## Reaction values: Facets / Covariates / Outcome ----
  
  unit = reactive({
    input$selectedID
  })
  selectedFacet = reactive({
    input$selectedMultipleFacets
  })
  selectedCovariates = reactive({
    input$selectedCovariates
  })
  selectedOutcome = reactive({
    input$selectedOutcome
  })
  selectedFixedFacet = reactive({
    input$selectedFixedFacet
  })
  selectedSubtest = reactive({
    input$selectedSubtest
  })
  
  # 选择ID
  output$selectedID <- renderUI({
    selectInput(
      "selectedID",
      "1. Which variable represents ID:",
      choices = colnames(dat),
      selected = colnames(dat)[1]
    )
  })
  
  # 选择outcome
  output$selectedOutcome <- renderUI({
    selectInput(
      "selectedOutcome",
      "2. Which variable represents outcome:",
      choices = colnames(dat),
      selected = colnames(dat)[1]
    )
  })
  
  # 选择facet
  output$selectedMultipleFacets <- renderUI({
    checkboxGroupInput(
      "selectedMultipleFacets",
      "3. Which column(s) represent facet(s):",
      choices = colnames(dat),
      selected = NULL
    )
  })
  
  # 选择covariates
  output$selectedCovariates <- renderUI({
    checkboxGroupInput(
      "selectedCovariates",
      "(Optional) Which column(s) represent covariates:",
      choices = colnames(dat),
      selected = NULL
    )
  })
  
  ## missing data server
  observeEvent(input$variableSettingConfirm, {
    method = selectedMissingMethod()
    ## Deal with missing method
    if(method == "Zero Inputation"){
      dat[[selectedOutcome()]] <<- replace(dat[[selectedOutcome()]], is.na(dat[[selectedOutcome()]]), 0)
    }else if(method == "Mean Inputation"){
      dat[[selectedOutcome()]] <<- replace(dat[[selectedOutcome()]], is.na(dat[[selectedOutcome()]]), mean(dat[[selectedOutcome()]], na.rm = TRUE))
    }else if(method == "Median Inputation"){
      dat[[selectedOutcome()]] <<- replace(dat[[selectedOutcome()]], is.na(dat[[selectedOutcome()]]), median(dat[[selectedOutcome()]], na.rm = TRUE))
    }else{
      dat <<- dat[!is.na(dat[[selectedOutcome()]]),]
    }
  })
  
  # mgTheory选择fixed facet
  output$selectedFixedFacet <- renderUI({
    checkboxGroupInput(
      "selectedFixedFacet",
      "5. Which column represent fixed facet:",
      choices = selectedFacet(),
      selected = selectedFacet()
    )
  })
  
  output$selectedSubtest <- renderUI({
    checkboxGroupInput(
      "selectedSubtest",
      "6. Which column represent subtest:",
      choices = selectedFacet(),
      selected = selectedFacet()
    )
  })
  
  ##  nestedStrc: automatically detect design of data and return structure table-----
  nestedStrc <- eventReactive(input$variableSettingConfirm, {
    if (length(selectedFacet()) == 1) { # if single facet
      nestedStrc = data.frame(NA)
      colnames(nestedStrc) = selectedFacet()
      nestedStrc
    }else{
      TagPairs <- as.data.frame(t(combn(selectedFacet(), 2))) # Each row represents a pair of facets
      colnames(TagPairs) <- c("f1", "f2")
      NestedOrCrossed = rep(NA, nrow(TagPairs))
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
  
  
  ## Formula generator for gtheory and dtheory ----
  ## ----------------------------- ##
  ## If there is only one facet, generate a formula as (1 | Person) + (1 | Facet)
  ## ----------------------------- ##
  
  gtheoryFormula <- reactive({
    nestedStrcTable <- nestedStrc() # load nested structure
    formularFacets <- NULL
    
    if (ncol(nestedStrcTable) > 1) { # 若不是single facet
      for (r in 1:nrow(nestedStrcTable)) {
        if (nestedStrcTable[r, "NestedOrCrossed"] == "Crossed") {
          formularFacets <-
            c(formularFacets,
              paste0("(1 | ", nestedStrcTable[r, 1:2], ")"))
        }
        if (nestedStrcTable[r, "NestedOrCrossed"] == "Nested") {
          tab <- table(nestedStrcTable[r, 1], nestedStrcTable[r, 2])
          nested <-
            !any(apply(tab, 1, function(x)
              sum(x != 0) > 1))
          if (nested) {
            formularFacets <-
              c(
                formularFacets,
                paste0("(1 | ", paste0(nestedStrcTable[r, 1]), ")"),
                paste0("(1 | ", input$selectedID, ":", nestedStrcTable[r, 1], ")"),
                paste0(
                  "(1 | ",
                  paste0(nestedStrcTable[r, 2], ":", nestedStrcTable[r, 1]),
                  ")"
                )
              )
          } else{
            formularFacets <-
              c(formularFacets,
                paste0("(1 | ", paste0(nestedStrcTable[r, 2]), ")"),
                paste0("(1 | ", input$selectedID, ":", nestedStrcTable[r, 2], ")"),
                paste0("(1 | ", paste0(nestedStrcTable[r, 1], ":", nestedStrcTable[r, 2]), ")"))
          }
        }
      }
      ## add covariates and facets into the formulate
      formularText <- paste0(c(selectedCovariates() , unique(formularFacets)), collapse = " + ")
      formularText <- paste0(selectedOutcome(), " ~ (1 |", input$selectedID, ") + ", formularText) 
      formularText
    }else{ # 若是single facet
      formularText <- paste0(selectedOutcome(), " ~ (1 |", input$selectedID, ") + ", "(1 |", colnames(nestedStrcTable), ")") 
    }
    
})
  
  ## mGtheory formula
  mgtheoryFormula <- reactive({
    nestedStrcTable <- nestedStrc() # load nested structure
    formularFacets <- NULL
    
    for (r in 1:nrow(nestedStrcTable)) {
      if (nestedStrcTable[r, "NestedOrCrossed"] == "Crossed") {
        formularFacets <-
          c(formularFacets,
            paste0("(1 | ", nestedStrcTable[r, 1:2], ")"))
      }
      if (nestedStrcTable[r, "NestedOrCrossed"] == "Nested") {
        tab <- table(nestedStrcTable[r, 1], nestedStrcTable[r, 2])
        nested <-
          !any(apply(tab, 1, function(x)
            sum(x != 0) > 1))
        if (nested) {
          formularFacets <-
            c(
              formularFacets,
              paste0("(1 | ", paste0(nestedStrcTable[r, 1]), ")"),
              paste0("(1 | ", input$selectedID, ":", nestedStrcTable[r, 1], ")"),
              paste0(
                "(1 | ",
                paste0(nestedStrcTable[r, 2], ":", nestedStrcTable[r, 1]),
                ")"
              )
            )
        } else{
          formularFacets <-
            c(
              formularFacets,
              paste0("(1 | ", paste0(nestedStrcTable[r, 2]), ")"),
              paste0("(1 | ", input$selectedID, ":", nestedStrcTable[r, 2], ")"),
              paste0(
                "(1 | ",
                paste0(nestedStrcTable[r, 1], ":", nestedStrcTable[r, 2]),
                ")"
              )
            )
        }
      }
    }
    ## add covariates and facets into the formulate
    
    paste0("us(", selectedFixedFacet(), " | ", selectedSubtest(), ")")
    formularText <-
      paste0(c(selectedCovariates() , unique(formularFacets)), collapse = " + ")
    
    paste0(selectedOutcome(),
           " ~ (1 |",
           input$selectedID,
           ") + ",
           formularText)
  })
  
  # Tabset2: 显示表格
  observeEvent(input$variableSettingConfirm, {
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
  
  # Running recommended model ------------------------------------------------------------------
  selectedMissingMethod = reactive({
    input$missingMethod
  })
  
  output$recommFormular <- renderText({
    makeeasyformular(gtheoryFormula())
  })
  
  gstudyResult <- eventReactive(input$runRecommModel, {
    nFacet <- NULL
    datG <- dat
    
    datG[[selectedOutcome()]] <- as.numeric(dat[[selectedOutcome()]])
    
    datG[c(input$selectedID, selectedFacet())] <-
      lapply(datG[c(input$selectedID, selectedFacet())], as.factor)
    if (input$selfFormular == "") {
      formulaRecomm <- as.formula(gtheoryFormula())
      lme4.res <<- lmer(data = datG, formula = formulaRecomm)
    } else{
      lme4.res <<-
        lmer(data = datG,
             formula = as.formula(makehardformular(input$selfFormular)))
    }
    ## fixed effect
    fixedEffectEstimate <<- fixef(lme4.res)
    ## Random effects
    randomEffectEstimate <- ranef(lme4.res)
    randomEffectLevel <- lapply(lapply(datG, unique), length)
    nFacet <<- unlist(randomEffectLevel[selectedFacet()])
    gstudy(lme4.res)
  })
  
  dstudyResult <- eventReactive(input$runRecommModelDstudy, {
    datD <- dat
    datD[[selectedOutcome()]] <-
      as.numeric(datD[[selectedOutcome()]])
    datD[c(input$selectedID, selectedFacet())] <-
      lapply(datD[c(input$selectedID, selectedFacet())], as.factor)
    
    if (input$selfFormular == "") {
      lme4.res <-
        lmer(data = datD, formula = as.formula(gtheoryFormula()))
    } else{
      lme4.res <-
        lmer(data = datD,
             formula = as.formula(makehardformular(input$selfFormular)))
    }
    gstudy.res <- gstudy(lme4.res)
    dstudy(x = gstudy.res, n = nFacet, unit = unit())
  })
  
  ## with Bootstrapping
  gstudyResultBoot <- eventReactive(input$runRecommModelBoot, {
    datGBoot <- dat
    gstudy.res <- gstudyResult()
    datGBoot[[selectedOutcome()]] <-
      as.numeric(datGBoot[[selectedOutcome()]])
    datGBoot[c(input$selectedID, selectedFacet())] <-
      lapply(datGBoot[c(input$selectedID, selectedFacet())], as.factor)
    if (input$selfFormular == "") {
      formulaRecomm <- as.formula(gtheoryFormula())
      lme4.res <- lmer(data = datGBoot, formula = formulaRecomm)
    } else{
      lme4.res <-
        lmer(data = datGBoot,
             formula = as.formula(makehardformular(input$selfFormular)))
    }
    
    boot.gstudy <<-
      lme4::bootMer(
        lme4.res,
        gstudy.forboot,
        nsim = input$nboot,
        use.u = FALSE,
        type = "parametric",
        parallel = "snow",
        ncpus = 2
      )
    
    boot.gstudy.res <-
      cbind(gstudy.res$gstudy.out, t(boot.gstudy$t))
    boot.gstudy.res
    
  })
  
  dstudyResultBoot <-
    eventReactive(input$runRecommModelDstudyBoot, {
      datDBoot <- dat
      datDBoot[[selectedOutcome()]] <-
        as.numeric(datDBoot[[selectedOutcome()]])
      datDBoot[c(input$selectedID, selectedFacet())] <-
        lapply(datDBoot[c(input$selectedID, selectedFacet())], as.factor)
      gstudy.res <- gstudyResult()
      boot.gstudy.res <- gstudyResultBoot()
      dstudy.res_boot <- dstudyResult() # placeholder
      
      boot.dstudy.res <- NULL
      for (i in 1:input$nboot) {
        temp <- gstudy.res
        temp[1]$gstudy.out[, 2] <- boot.gstudy.res[, -(1:3)][, i]
        temp.dstudy <- dstudy(temp, nFacet, unit = unit())
        
        boot.dstudy.res <- rbind(
          boot.dstudy.res,
          c(
            temp.dstudy$ds.df[, 4],
            temp.dstudy$relvar,
            temp.dstudy$absvar,
            temp.dstudy$gcoef,
            temp.dstudy$dcoef
          )
        )
      }
      dstudy.res.CI <- t(apply(t(boot.dstudy.res), 1,
                               function(x) {
                                 quantile(x, probs = c(.025, .975))
                               }))
      names(dstudy.res.CI) <- c("2.5%", "97.5%")
      
      # beautify output
      dstudy.res_boot$dcoef[2] <-
        dstudy.res.CI[nrow(dstudy.res.CI), 1]
      dstudy.res_boot$dcoef[3] <-
        dstudy.res.CI[nrow(dstudy.res.CI), 2]
      names(dstudy.res_boot$dcoef) <- c("Est", "2.5%", "97.5%")
      
      
      dstudy.res_boot$gcoef[2] <-
        dstudy.res.CI[nrow(dstudy.res.CI) - 1, 1]
      dstudy.res_boot$gcoef[3] <-
        dstudy.res.CI[nrow(dstudy.res.CI) - 1, 2]
      names(dstudy.res_boot$gcoef) <- c("Est", "2.5%", "97.5%")
      
      dstudy.res_boot$absvar[2] <-
        dstudy.res.CI[nrow(dstudy.res.CI) - 2, 1]
      dstudy.res_boot$absvar[3] <-
        dstudy.res.CI[nrow(dstudy.res.CI) - 2, 2]
      names(dstudy.res_boot$absvar) <- c("Est", "2.5%", "97.5%")
      
      dstudy.res_boot$relvar[2] <-
        dstudy.res.CI[nrow(dstudy.res.CI) - 3, 1]
      dstudy.res_boot$relvar[3] <-
        dstudy.res.CI[nrow(dstudy.res.CI) - 3, 2]
      names(dstudy.res_boot$relvar) <- c("Est", "2.5%", "97.5%")
      dstudy.res_boot$ds.df <-
        cbind(dstudy.res_boot$ds.df, dstudy.res.CI[1:(-4 + nrow(dstudy.res.CI)),])
      
      # output
      dstudy.res_boot
    })
  
  
  
  
  ###################### dstudy:显示facets的levels
  
  ###################### 打印模型结果
  observeEvent(input$runRecommModel, {
    i = 0
    output$recommModelGStudyResult <- renderPrint({
      gstudy.out <- gstudyResult()
      
      gstudy.out$gstudy.out
    })
    output$recommModelFixedEffectResult <- renderPrint({
      fixedEffectEstimate
    })
    
    ## add UI for 选择facet
    selectedFacetForLevel <-
      reactive({
        input$selectedFacetValue
      })
    output$selectedFacetMenu <- renderUI({
      allfacets <- selectedFacet()
      selectInput(inputId = "selectedFacet",
                  label = "Select the facet to change",
                  choices = allfacets)
    })
    
    observeEvent(input$selectedFacet, {
      output$selectedFacetLevels <- renderUI({
        whichfacet <<- input$selectedFacet
        whichValue <<-
          ifelse(is.null(input$selectedFacetValue),
                 nFacet[whichfacet],
                 selectedFacetForLevel())
        sliderInput(
          inputId = "selectedFacetValue",
          label = "level: ",
          min = 0,
          max = 100,
          value = whichValue
        )
      })
    })
    
    observeEvent(input$confirmFacetLevel, {
      nFacet[whichfacet] <<- whichValue
    })
    
    i = 100
    updateProgressBar(
      id = "gstudybar",
      status = "success",
      value = i,
      total = 100,
      title = paste0("Process ", i, "%")
    )
    
    ## 可下载的表格 downloadGstudyResult
    output$downloadGstudyResult <- downloadHandler(
      filename = "gstudyResult.csv",
      content = function(file) {
        write.csv(gstudyResult()$gstudy.out, file, row.names = FALSE)
      }
    )
    ## 可下载的表格 downloadGstudyTheta
    output$downloadGstudyTheta <- downloadHandler(
      filename = "thetaEstimates.csv",
      content = function(file) {
        write.csv(extractTheta(lme4.res), file, row.names = FALSE)
      }
    )
  })
  
  observeEvent(input$runRecommModelDstudy, {
    i = 0
    dstudy.out <- dstudyResult()
    output$recommModelDStudyResult <- renderPrint({
      print.dStudy(dstudy.out)
    })
    output$updatedN <- renderPrint({
      nFacet
    })
    i = 100
    updateProgressBar(
      id = "dstudybar",
      status = "success",
      value = i,
      total = 100,
      title = paste0("Process ", i, "%")
    )
    
    ## 可下载的表格 downloadGstudyResult
    colnames(dstudy.out$ds.df) <-
      c("Source", "Est.Variance", "N", "Est.(Var/N)")
    output$downloadDstudyResult <- downloadHandler(
      filename = "dstudyResult.csv",
      content = function(file) {
        write.csv(dstudy.out$ds.df, file, row.names = FALSE)
      }
    )
    
  })
  
  # observe Bootstrapping and output
  observeEvent(input$runRecommModelBoot, {
    gstudy.res_boot <- gstudyResult() # place holder
    boot.gstudy.res <- gstudyResultBoot()
    # calculate bootstrap CI
    gstudy.res.CI <-
      t(apply(t(boot.gstudy$t), 1, function(x) {
        quantile(x, probs = c(.025, .975))
      }))
    
    ## 打印表格
    gstudy.res_boot$gstudy.out <-
      cbind(gstudy.res_boot$gstudy.out, gstudy.res.CI)
    
    output$recommModelGStudyBootResult <- renderPrint({
      gstudy.res_boot$gstudy.out
    })
    
    ## 可下载的表格
    output$downloadGstudyBootResult <- downloadHandler(
      filename = paste0("gstudyBootstrapN", input$nboot, "Result.csv"),
      content = function(file) {
        write.csv(gstudy.res_boot$gstudy.out, file, row.names = FALSE)
      }
    )
    
  })
  
  observeEvent(input$runRecommModelDstudyBoot, {
    dstudy.res_boot <- dstudyResultBoot()
    colnames(dstudy.res_boot$ds.df) <-
      c("Source",
        "Est.Variance",
        "N",
        "Est.(Var/N)",
        "2.5%",
        "97.5%")
    output$recommModelDStudyBootResult <- renderPrint({
      print.dStudy(dstudy.res_boot)
    })
    
    ## 可下载的表格
    output$downloadDstudyBootResult <- downloadHandler(
      filename = paste0("dstudyBootstrapN", input$nboot, "Result.csv"),
      content = function(file) {
        write.csv(dstudy.res_boot$ds.df, file, row.names = FALSE)
      }
    )
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
