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

# data(Brennan.3.2)
# dat <- Brennan.3.2 |>
#     mutate(across(c(Task, Rater), function(x) paste0(cur_column(), x))) |>
#     rowwise() |>
#     mutate(ID = paste0(c(Task, Rater), collapse = ";")) |>
#     select(-Task, -Rater) |>
#     pivot_wider(id_cols = Person, names_from = ID, values_from = Score)
# 
# Row1 <- c(NA, as.character(sapply(str_split(colnames(dat[-1]), ";"), function(x) x[1])))
# Row2 <- c(NA, as.character(sapply(str_split(colnames(dat[-1]), ";"), function(x) x[2])))
# datWide <- rbind(
#     Row1,
#     Row2,
#     tibble(dat)
# )
# write.csv(datWide, file = "~/Documents/Projects/gTheoryShiny/Brennan.3.2Wide.csv", row.names = FALSE)

rm(list = ls())
library(shiny)
library(shinyjs)
library(shinyWidgets) # for widgets like progress bar
library(lme4) # for lme modeling
library(DT) # for data table
library(markdown) # for better markdown format
library(sjmisc) # check nested/crossed strucutre
library(tidyverse) # for better markdown format


source("advGtheoryFunctions.R")
nboot = 200
unit = "ID"

# Define UI for application that draws a histogram
ui <- navbarPage(
    "G-theory Data Explorer", # title
    useShinyjs(),
# Tag Page 1: A Tab to transform data from wide to long -------------------
    tabPanel("Data Input",
        sidebarLayout(
            sidebarPanel(
                # Copy the line below to make a file upload manager
                fileInput("file", label = h3("Choose CSV File"), accept = ".csv"),
                p("Notice: your data should have TAG/ID in first 2 rows"),
                verbatimTextOutput("newNotificaion"),
                checkboxInput("isHeaderIncluded", "你的CSV有列名字吗?", TRUE),
                checkboxInput("isIDIncluded", "Column 1 is Subject ID?", TRUE),
                hr(),
                ## 设定tag/ID前缀和标签
                uiOutput("nRows"),
                uiOutput("preFix"),
                uiOutput("TagNames"),
                
                ## 转换
                actionButton("transform", "Transform")
            ),
            mainPanel(
                h2("Raw Data:"),
                DTOutput("rawDataTable"),
                DTOutput("transDataTable")
            )
        )
    ),
    

# Tab Page 2: Check Data Structure ----------------------------------------
    tabPanel("Data Structure",
    # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                p("Control facets/outcome："),
                uiOutput("selectedMultipleFacets"),
                uiOutput("selectedOutcome"),
                # hr(),
                # p("推荐的概化理论模型为："),
                # textOutput("recommFormular"),
                # ## 运行推荐模型
                # actionButton("runRecommModel", "运行gstudy"),
                # actionButton("runRecommModelDstudy", "运行dstudy"),
            ),
            # Show a plot of the generated distribution
            mainPanel(
               h4("Structural Table"),
               DTOutput("nestedStrucTable"),
               h4("Summary Table"),
               DTOutput("factorNestTable"),
            )
        ),
    ),

# Tab Page 3: Check Data Analysis ----------------------------------------
    tabPanel("Data Analysis",
        sidebarLayout(
            sidebarPanel(
                strong("Recommended formula："),
                textOutput("recommFormular"),
                textInput("selfFormular", "User-specified formula: ", placeholder = "Default: recommeded formula"),
                
                hr(),
                ## 运行gstudy
                h3("G-study："),
                actionButton("runRecommModel", "gstudy estimate"),
                actionButton("runRecommModelBoot", "Estimate Bootstrap SD"),
                progressBar(
                  id = "gstudybar",
                  value = 0,
                  total = 100,
                  title = NULL,
                  striped = TRUE,
                  display_pct = TRUE
                ),
                
                hr(),
                ## 运行dstudy
                h3("D-study："),
                ### 选择要修改的facet
                uiOutput("selectedFacet"), #选择一个facet
                uiOutput("selectedFacetLevels"), #选择factor levels
                conditionalPanel(condition = "input.runRecommModel >= 1", 
                                 actionButton(inputId = "confirmFacetLevel", label = "Confirm")),
                hr(),
                ### 运行dstudy
                actionButton("runRecommModelDstudy", "dstudy estimate"),
                actionButton("runRecommModelDstudyBoot", "Estimate Bootstrap SD"),
                progressBar(
                  id = "dstudybar",
                  value = 0,
                  total = 100
                ),
                
            ),
            # output panel: gstudy / dstudy
            mainPanel(
                conditionalPanel(condition = "input.runRecommModel >= 1",
                                 h4("G-study Output："),
                                 verbatimTextOutput("recommModelGStudyResult")
                ),
                # gstudy with bootstrapping SD
                conditionalPanel(condition = "input.runRecommModelBoot >= 1",
                                 h4("Estimate Bootstrapping SD for G-study："),
                                 verbatimTextOutput("recommModelGStudyBootResult")
                ),
                conditionalPanel(condition = "input.runRecommModelDstudy >= 1",
                                 h4("D-study Output："),
                                 p("Sample Size:"),
                                 verbatimTextOutput("updatedN"),
                                 p("Result:"),
                                 verbatimTextOutput("recommModelDStudyResult")
                ),
                # dstudy with bootstrapping SD
                conditionalPanel(condition = "input.runRecommModelDstudyBoot >= 1",
                                 h4("Estimate Bootstrapping SD for D-study："),
                                 verbatimTextOutput("recommModelDStudyBootResult")
                ),
            )
        ),
    )

# End of UI ---------------------------------------------------------------
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
        # 隐藏转换按钮
        shinyjs::hide("transform")
        if(!is.null(input$file)) shinyjs::show("transform")
    })
    
    # 上传数据
    datRaw <- reactive({
        file <- input$file
        if(is.null(file)) {return()}
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        read.csv(file$datapath, header = input$isHeaderIncluded)
    })
    
    # You can access the value of the widget with input$file, e.g.
    output$rawDataTable <- renderDT({
        if (is.null(input$file)) {return()}
        datRaw()
    })
    
    # 按下"transform"按钮后，将原始数据转换为长数据格式
    output$nRows <- renderUI({
        if (is.null(input$file)) return()
        selectInput("nRows", "多少行是tag/ID", choices = 1:nrow(datRaw()), selected = 2)
    })
    output$preFix <- renderUI({
        if (is.null(input$file)) return()
        textInput("preFix", "tag/ID前缀（比如A;B;C）", value = "T;R")
    })
    output$TagNames <- renderUI({
        if (is.null(input$file)) return()
        textInput("TagNames", "tag/ID的column名字(比如Class;Rater;Item)", "Task;Rater")
    })
    
# Ractive values: interactively receive the tag/ID information---------------------
    NText = reactive({input$nRows}) # 有多少行是tag/ID
    preFixText = reactive({input$preFix}) # 前缀，比如A_, R_, C_
    TagNamesText = reactive({input$TagNames}) # 侧面的标签比如："Class", "Rater", "Item"        
    datLong <- reactive({
        N = as.numeric(NText())
        preFix <- str_split(preFixText(), ";")[[1]]
        TagNames <- str_split(TagNamesText(), ";")[[1]]
        file <- datRaw()
        #第N行是facets
        tags = file[1:N, ]
        # 将所有facets合并成特殊ID
        tagWLabels = apply(tags, 2, function(x) paste0(preFix, "_", x))
        mergedID = as.character(apply(tagWLabels, 2, function(x) paste0(x, collapse = ";"))[-1])
        dat <- dplyr::tibble(file[-(1:N),] )
        colnames(dat) <- c("ID", mergedID)
        dat |> 
            pivot_longer(cols = {{mergedID}}, values_to = "Score" ) |> 
            separate(name, into = TagNames, sep = ";")
    })

    observeEvent(input$transform,
        output$transDataTable <- renderDT({datLong()})
    )
    


# Sever for tag page 2 ----------------------------------------------------
    # Reactive values
    selectedFacet = reactive({input$selectedMultipleFacets})
    selectedOutcome = reactive({input$selectedOutcome})
    nestedStrc <- reactive({
        dat <- datLong()
        TagNames <- selectedFacet()
        TagPairs <- as.data.frame(t(combn(TagNames, 2)))
        colnames(TagPairs) <- c("f1", "f2")
        NestedOrCrossed = rep(NA, nrow(TagPairs))
        for (pair in 1:nrow(TagPairs)) { # pair = TagPairs[2,]
            whichpair = TagPairs[pair,]
            isNested <- sjmisc::is_nested(dat[[whichpair[[1]]]], dat[[whichpair[[2]]]])
            NestedOrCrossed[pair] = ifelse(isNested, "Nested", "Crossed")
        }
        nestedStrcTable = TagPairs
        nestedStrcTable$NestedOrCrossed = NestedOrCrossed
        nestedStrcTable
    })
    
    # 将用户输入的公式转换成gtheory认可的公式
    gtheoryFormula <- reactive({ 
        nestedStrcTable <- nestedStrc()
        formularText <- NULL
        for (r in 1:nrow(nestedStrcTable)) {
            if (nestedStrcTable[r, "NestedOrCrossed"] == "Crossed") {
                formularText <- paste0( formularText, "+",paste0("(1 | ", nestedStrcTable[r, 1:2], " )", collapse = " +"))
            }
            if (nestedStrcTable[r, "NestedOrCrossed"] == "Nested") {
                tab <- table(nestedStrcTable[r, 1], nestedStrcTable[r, 2])
                nested <- !any(apply(tab, 1, function(x) sum(x != 0) > 1))
                if (nested) { # for nest model: "Score ~ (1 | Person) + (1 | Task) + (1 | Rater:Task) + (1 | Person:Task)"
                    formularText <-
                        paste0(formularText, "+", 
                               paste0("(1 | ", paste0(nestedStrcTable[r, 1]), ")"),
                               "+",
                               paste0("(1 | ", "ID", ":", nestedStrcTable[r, 1], " )"),
                               "+",
                               paste0("(1 | ", paste0(nestedStrcTable[r, 2], ":", nestedStrcTable[r, 1]), " )")
                        )
                }else{
                    formularText <-
                        paste0(formularText, "+", 
                               paste0("(1 | ", paste0(nestedStrcTable[r, 2]), ")"),
                               "+",
                               paste0("(1 | ", "ID", ":", nestedStrcTable[r, 2], " )"),
                               "+",
                               paste0("(1 | ", paste0(nestedStrcTable[r, 1], ":", nestedStrcTable[r, 2]), " )")
                               )
                }
            }
        }
        paste0(selectedOutcome(), " ~ (1 | ID)", formularText)
    })
    
    # single variable visualization
    output$selectedMultipleFacets <- renderUI({
        checkboxGroupInput("selectedMultipleFacets",
                           "Which facets you are looking at:",
                           choices = colnames(datLong()),
                           selected = str_split(TagNamesText(), ";")[[1]]
        )
    })
    
     output$factorTable <- renderDT({
         facet  <- datLong()[, selectedFacet()]
         datLong() |> 
             group_by({{ facet }}) |> 
             summarise(
                 n = n()
             )
     })
     
     output$selectedOutcome <- renderUI({
         selectInput("selectedOutcome",
                     "Which outcome you are look at:",
                     choices = colnames(datLong()),
                     selected = colnames(datLong())[ncol(datLong())])
     })
     
     output$factorNestTable <- renderDT({
         facets  <- selectedFacet()
         outcome  <- selectedOutcome()
         datLong() |> 
             group_by(across(facets)) |> 
             summarise(
                 `Sample Size (Outcome)` = n_distinct(.data[[outcome]])
             )
     })
     
     output$nestedStrucTable <- renderDT({
         nestedStrc()
     })
     


     
# 运行推荐模型 ------------------------------------------------------------------
     output$recommFormular <- renderText({
         makeeasyformular(gtheoryFormula())
     })
     
     
     gstudyResult <- eventReactive(input$runRecommModel, {
         dat <- datLong()
         dat[[selectedOutcome()]] <- as.numeric(dat[[selectedOutcome()]])
         dat[c("ID", selectedFacet())] <- lapply(dat[c("ID", selectedFacet())], as.factor)
         if (input$selfFormular == "") {
             formulaRecomm <- as.formula(gtheoryFormula())
             lme4.res <- lmer(data = dat, formula = formulaRecomm)
         }else{
             lme4.res <- lmer(data = dat, formula = as.formula(makehardformular(input$selfFormular)))
         }
        randomEffectEstimate <- ranef(lme4.res)
        randomEffectLevel <- lapply(lapply(dat, unique), length)
        n <<- unlist(randomEffectLevel[!names(randomEffectLevel) %in% c(unit, outcome)])
        gstudy(lme4.res)
     })
     
     dstudyResult <- eventReactive(input$runRecommModelDstudy, {
         dat <- datLong()
         dat[[selectedOutcome()]] <- as.numeric(dat[[selectedOutcome()]])
         dat[c("ID", selectedFacet())] <- lapply(dat[c("ID", selectedFacet())], as.factor)
         
         
         if (input$selfFormular == "") {
           lme4.res <- lmer(data = dat, formula = as.formula(gtheoryFormula()))
         }else{
           lme4.res <- lmer(data = dat, formula = as.formula(makehardformular(input$selfFormular)))
         }
         gstudy.res <- gstudy(lme4.res)
         dstudy(x = gstudy.res, n = n, unit = unit)
     })
     ## with Bootstrapping
     gstudyResultBoot <- eventReactive(input$runRecommModelBoot, {
       dat <- datLong()
       gstudy.res <- gstudyResult()
       dat[[selectedOutcome()]] <- as.numeric(dat[[selectedOutcome()]])
       dat[c("ID", selectedFacet())] <- lapply(dat[c("ID", selectedFacet())], as.factor)
       if (input$selfFormular == "") {
         formulaRecomm <- as.formula(gtheoryFormula())
         lme4.res <- lmer(data = dat, formula = formulaRecomm)
       }else{
         lme4.res <- lmer(data = dat, formula = as.formula(makehardformular(input$selfFormular)))
       }
       
       boot.gstudy <-
         lme4::bootMer(
           lme4.res,
           gstudy.forboot,
           nsim = nboot,
           use.u = FALSE,
           type = "parametric",
           parallel = "snow",
           ncpus = 2
         ) 
       boot.gstudy.res <- cbind(gstudy.res$gstudy.out, t(boot.gstudy$t))
       boot.gstudy.res
       
     })
     
     dstudyResultBoot <- eventReactive(input$runRecommModelDstudyBoot, {
       dat <- datLong()
       dat[[selectedOutcome()]] <- as.numeric(dat[[selectedOutcome()]])
       dat[c("ID", selectedFacet())] <- lapply(dat[c("ID", selectedFacet())], as.factor)
       gstudy.res <- gstudyResult()
       dstudy.res <- dstudyResult()
       
       gstudy.res <- gstudyResult()
       boot.gstudy.res <- gstudyResultBoot()
       boot.dstudy.res <- NULL
       for(i in 1:nboot) {
         temp <- gstudy.res
         temp[1]$gstudy.out[, 2] <- boot.gstudy.res[, -(1:3)][, i]
         temp.dstudy <- dstudy(temp, n, unit)
         
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
       dstudy.res.CI<-t(apply(t(boot.dstudy.res),1,function(x){quantile(x, probs = c(.025, .975))}))
       
       
       # beautify output
       dstudy.res_boot$dcoef[2] <-
         dstudy.res.CI[nrow(dstudy.res.CI), 1]
       dstudy.res_boot$dcoef[3] <- dstudy.res.CI[nrow(dstudy.res.CI), 2]
       names(dstudy.res_boot$dcoef) <- c("Est", "2.5%", "97.5%")
       dstudy.res_boot$gcoef[2] <-
         dstudy.res.CI[nrow(dstudy.res.CI) - 1, 1]
       dstudy.res_boot$gcoef[3] <- dstudy.res.CI[nrow(dstudy.res.CI) - 1, 2]
       names(dstudy.res_boot$gcoef) <- c("Est", "2.5%", "97.5%")
       dstudy.res_boot$absvar[2] <-
         dstudy.res.CI[nrow(dstudy.res.CI) - 2, 1]
       dstudy.res_boot$absvar[3] <- dstudy.res.CI[nrow(dstudy.res.CI) - 2, 2]
       names(dstudy.res_boot$absvar) <- c("Est", "2.5%", "97.5%")
       dstudy.res_boot$relvar[2] <-
         dstudy.res.CI[nrow(dstudy.res.CI) - 3, 1]
       dstudy.res_boot$relvar[3] <- dstudy.res.CI[nrow(dstudy.res.CI) - 3, 2]
       names(dstudy.res_boot$relvar) <- c("Est", "2.5%", "97.5%")
       dstudy.res_boot$ds.df <-
         cbind(dstudy.res_boot$ds.df, dstudy.res.CI[1:(-4 + nrow(dstudy.res.CI)), ])
       dstudy.res_boot
     })
     
     
     
     ###################### dstudy:显示facets的levels
     
    
     ###################### 打印模型结果
     observeEvent(input$runRecommModel,{
         i = 0
         output$recommModelGStudyResult <- renderPrint( {
             gstudy.out <- gstudyResult()
             gstudy.out$gstudy.out
         })
         
         ## add UI for 选择facet
         selectedFacetForLevel <- reactive({input$selectedFacetValue})
         output$selectedFacet <- renderUI({
           allfacets <- selectedFacet()
           selectInput(inputId = "selectedFacetForLevel", label = "Select the facet to change",
                       choices = allfacets)
         })
         observeEvent(input$selectedFacetForLevel, {
           output$selectedFacetLevels <- renderUI({
             whichfacet <<- input$selectedFacetForLevel
             whichValue <<- ifelse(is.null(input$selectedFacetValue), n[whichfacet], selectedFacetForLevel())
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
           n[whichfacet] <<- whichValue
         })
         
         i = 100
         updateProgressBar(
           id = "gstudybar",
           status = "success",
           value = i,
           total = 100,
           title = paste0("Process ", i, "%")
         )
     }
    )
    
    observeEvent(input$runRecommModelDstudy,{
         i = 0
         output$recommModelDStudyResult <- renderPrint( {
             dstudy.out <- dstudyResult()
             print.dStudy(dstudy.out)
         })
         output$updatedN <- renderPrint( {
           n
         })
         i = 100
         updateProgressBar(
           id = "dstudybar",
           status = "success",
           value = i
         )
         # output$recommModelDStudyStats <- renderDT({
         #     dstudy.out <- dstudyResult()
         #     indices <- c("var.universe",
         #     "generalizability",
         #     "var.error.rel",
         #     "sem.rel",
         #     "see.rel",
         #     "dependability",
         #     "var.error.abs",
         #     "sem.abs",
         #     "see.abs")
         #     dt <- data.frame(do.call("rbind",dstudy.out[indices]))
         #     colnames(dt) <- "statistics"
         #     dt$statistics <- round(dt$statistics, 3)
         #     dt
         # })
     }
    )
    
    # observe Bootstrapping and output 
    observeEvent(input$runRecommModelBoot,{
      output$recommModelGStudyBootResult <- renderPrint( {
        gstudy.res_boot <- gstudyResult()
        boot.gstudy.res <- gstudyResultBoot()
        
        # calculate bootstrap CI
        gstudy.res.CI <-
          t(apply(t(boot.gstudy$t), 1, function(x) {
            quantile(x, probs = c(.025, .975))
          }))
        
        # output
        gstudy.res_boot$gstudy.out <-
          cbind(gstudy.res_boot$gstudy.out, gstudy.res.CI)
        gstudy.res_boot$gstudy.out
      })
     }
    )
    
    observeEvent(input$runRecommModelDstudyBoot,{
      i = 1
      output$recommModelDStudyBootResult <- renderPrint( {
        dstudy.res_boot <- dstudyResultBoot()
        print.dStudy(dstudy.res_boot)
      })
      i = 100
      updateProgressBar(
        id = "dstudybar",
        status = "success",
        value = i
      )
     }
    )
    
        # if(is.numeric(facet)){
        #     output$distPlot <- renderPlot({
        #         # generate bins based on input$bins from ui.R
        #         bins <- seq(min(facet), max(facet), length.out = input$bins + 1)
        #         
        #         # draw the histogram with the specified number of bins
        #         hist(x, breaks = bins, col = 'darkgray', border = 'white',
        #              xlab = 'Waiting time to next eruption (in mins)',
        #              main = 'Histogram of waiting times')
        #     })
        # }else if(is.factor(facet)){
        #     output$factorTable <- renderDataTable({
        #         sleepstudy |> 
        #             group_by({{ facet }}) |> 
        #             summarise(
        #                 n = n()
        #             )
        #     })
        # }
     
    
        
}

# Run the application 
shinyApp(ui = ui, server = server)
