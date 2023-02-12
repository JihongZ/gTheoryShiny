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
# 【X】 2）boostrap得允许人家设置bootstrap多少次
# 【X】 3）extract出random effect可供下载
# 【X】 4）留一个说明页的tab界面用来放instruction或tutorials
# 【x】 5）测试一下其他不同design数据形式下，比如crossed和4个多重nested level，弄几个内置sample dataset可以供人直接下载mock on
# 【X】 6）加入covariate的功能


rm(list = ls())
source("library_load.R")
source("advGtheoryFunctions.R")
nboot = 200


# Define UI for application that draws a histogram
ui <- navbarPage(
    "G-theory Data Explorer", # title
    useShinyjs(),
    
    # Tab Page 0: Tutorial ----------------------------------------
    tabPanel("Tutorial",
             # Sidebar with a slider input for number of bins 
             fluidPage(
               # title
               titlePanel("Tutorial"),
               
               # 
               h4("Example data:"),
               p(span("Rajaratnam.2.new.csv", style = "color:blue"),  " is a example data with 4-way crossed/nested example. The facets include", strong("Subset, Item, Rater, Ocasion"), "ID variable is", strong("Person"), ". Outcome variable is", strong("Score"), "."),
               
               h4("Step 1: Load data and transformation"),
               p("Click ", em("Data Input"), " tab on the top navigation tools. Chick Upload button and select the data. Check the ", em('long-format'), " checkbox under the Data property section."),
               
               h4("Step 2: Specify each column's type"),
               p("Click ", em("Data Structure"), " tab. Select ", strong("Person"), " for ", em("which column represents ID"), " question."),
               p("Then check ", strong("Subset, Item, Rater, Ocasion"), " for ", em("Which column(s) represent facets"), " question."),
               p("Finally select ", strong("Ocasion"), " for ", em("Which column represents outcome"), " question."),
               
               h4("Step 3: Run data analysis"),
               p("Click ", em("Data Analysis"), " tab. You will notice the recommended formula for gtheory has been given to you. You can also specify your formula in ", em("User-specified formula"), " section. Choose link function for your model (defaulty is identity link). Finally decise on how many boostrap iterations for bootstrapping standard diviation estimation."),
               
               p("Chick ", strong("gstudy estimate"), " button to print gstudy results. A button called ", strong("Download gstudy result"), " will pop up. Click that button to download the results into local machine.")
              
             )
    ),

# Tag Page 1: A Tab to transform data from wide to long -------------------
    tabPanel("Data Input",
        sidebarLayout(
            sidebarPanel(
                # Copy the line below to make a file upload manager
                fileInput("file", label = h4("Choose CSV File:"), accept = ".csv", buttonLabel = "Upload..."),
                p("Notice: If data is wide-format, make sure the first two rows of your data file should be TAG/ID, first column should be subject ID"),
                verbatimTextOutput("newNotificaion"),
                
                h4("Data property:"),
                checkboxInput("isLongFormat", "long-format", FALSE),
                checkboxInput("isHeaderIncluded", "include header", TRUE),
                
                hr(),
                h4("Wide to Long Transformation:"),
                ## 设定tag/ID前缀和标签
                conditionalPanel(condition = "input.isLongFormat == 0", 
                                 uiOutput("nRows"),
                                 uiOutput("preFix"),
                                 uiOutput("TagNames"),
                                 ## 转换
                                 actionButton("transform", "Transform")
                                 ),
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
                uiOutput("selectedID"),
                uiOutput("selectedMultipleFacets"),
                uiOutput("selectedOutcome"),
                actionButton("variableSettingConfirm", "confirm")
            ),
            # Show a plot of the generated distribution
            mainPanel(
               h4("Structural Table:"),
               DTOutput("nestedStrucTable"),
               h4("Summary Table:"),
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
                textInput("selfFormular", "User-specified formula: ", 
                          placeholder = "Default: recommeded formula"),
                selectInput("linkFunc", label = "Link Function:", 
                            choices = c("identity", "logit", "probit", "poisson", "inverse gamma"),
                            selected = "identity"),
                sliderInput(inputId = "nboot", label = "Number of bootstrap", min = 100, max = 1000, value = 200),
                
                hr(),
                ## 运行gstudy
                h3("G-study："),
                actionButton("runRecommModel", "gstudy estimate"),
                actionButton("runRecommModelBoot", "bootstrap estimate"),
                progressBar(
                  id = "gstudybar",
                  value = 0,
                  total = 100,
                  title = NULL,
                  striped = TRUE,
                  display_pct = TRUE
                ),
                ### 下载按钮
                conditionalPanel(condition = "input.runRecommModel >=1",
                                 downloadButton("downloadGstudyTheta", "Download theta estimates")
                ),
                conditionalPanel(condition = "input.runRecommModel >=1",
                                 downloadButton("downloadGstudyResult", "Download gstudy result")
                                 ),
                conditionalPanel(condition = "input.runRecommModelBoot >=1",
                                 downloadButton("downloadGstudyBootResult", "Download bootstrap result")
                ),
                
                
                hr(),
                ## 运行dstudy
                h3("D-study："),
                ### 选择要修改的facet
                uiOutput("selectedFacetMenu"), #选择一个facet
                uiOutput("selectedFacetLevels"), #选择factor levels
                conditionalPanel(condition = "input.runRecommModel >= 1", 
                                 actionButton(inputId = "confirmFacetLevel", 
                                              label = "confirm facet levels")),
                h4("D-study Estimate:"),
                actionButton("runRecommModelDstudy", "dstudy estimate"),
                actionButton("runRecommModelDstudyBoot", "bootstrap estimate"),
                progressBar(
                  id = "dstudybar",
                  value = 0,
                  total = 100
                ),
                ### 下载按钮dstudy
                conditionalPanel(condition = "input.runRecommModelDstudy >=1",
                                 downloadButton("downloadDstudyResult", 
                                                "Download dstudy result")),
                conditionalPanel(condition = "input.runRecommModelDstudyBoot >=1",
                                 downloadButton("downloadDstudyBootResult", 
                                                "Download bootstrap result")),
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
    ),



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
    
    # 显示数据
    output$rawDataTable <- renderDT({
        if (is.null(input$file)) {return()}
        datRaw()
    })
    
    # 按下"transform"按钮后，将原始数据转换为长数据格式
    output$nRows <- renderUI({
        if (is.null(input$file)) return()
      selectInput("nRows",
                  "How many rows for TAG/ID",
                  choices = 1:nrow(datRaw()),
                  selected = 2)
    })
    output$preFix <- renderUI({
        if (is.null(input$file)) return()
        textInput("preFix", "Set TAG/ID Prefix（for example, A;B;C）", value = "T;R")
    })
    output$TagNames <- renderUI({
        if (is.null(input$file)) return()
        textInput("TagNames", "tag/ID的column名字(比如Class;Rater;Item)", "Task;Rater")
    })
    
# Reactive values: interactively receive the tag/ID information---------------------
    observeEvent(input$file, {dat <<- datRaw()})
    NText = reactive({input$nRows}) # 有多少行是tag/ID
    preFixText = reactive({input$preFix}) # 前缀，比如A_, R_, C_
    TagNamesText = reactive({input$TagNames}) # 侧面的标签比如："Class", "Rater", "Item"        
    observeEvent(input$transform, {
        file <- datRaw()
        N = as.numeric(NText())
        preFix <- str_split(preFixText(), ";")[[1]]
        TagNames <- str_split(TagNamesText(), ";")[[1]]
        #第N行是facets
        tags = file[1:N, ]
        # 将所有facets合并成特殊ID
        tagWLabels = apply(tags, 2, function(x) paste0(preFix, "_", x))
        mergedID = as.character(apply(tagWLabels, 2, function(x) paste0(x, collapse = ";"))[-1])
        datTrans <- dplyr::tibble(file[-(1:N),] )
        colnames(datTrans) <- c("ID", mergedID)
        dat <<- datTrans |>
            pivot_longer(cols = {{mergedID}}, values_to = "Score" ) |>
            separate(name, into = TagNames, sep = ";")
        # output file
        output$transDataTable <- renderDT({dat})
    })

    
    
    

# Sever for tag page 2 ----------------------------------------------------
# Reactive values
    unit = reactive({input$selectedID})
    selectedFacet = reactive({input$selectedMultipleFacets})
    selectedOutcome = reactive({input$selectedOutcome})
    nestedStrc <- reactive({
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
    # 选择ID
    output$selectedID <- renderUI({
      selectInput("selectedID",
                  "Which column represents ID:",
                  choices = colnames(dat),
                  selected = colnames(dat)[1])
    })
    
    # 选择facet
    output$selectedMultipleFacets <- renderUI({
      checkboxGroupInput("selectedMultipleFacets",
                         "Which column(s) represent facets:",
                         choices = colnames(dat),
                         # str_split(TagNamesText(), ";")[[1]]
                         selected = NULL
      )
    })
    
    # 选择outcome
    output$selectedOutcome <- renderUI({
      selectInput("selectedOutcome",
                  "Which column represents outcome:",
                  choices = colnames(dat),
                  selected = colnames(dat)[ncol(dat)])
    })
    
    # 将用户输入的公式转换成gtheory认可的公式
    gtheoryFormula <- reactive({
        nestedStrcTable <- nestedStrc()
        formularPredictors <- NULL
        for (r in 1:nrow(nestedStrcTable)) {
            if (nestedStrcTable[r, "NestedOrCrossed"] == "Crossed") {
              formularPredictors <- c( formularPredictors, paste0("(1 | ", nestedStrcTable[r, 1:2], ")") )
            }
            if (nestedStrcTable[r, "NestedOrCrossed"] == "Nested") {
                tab <- table(nestedStrcTable[r, 1], nestedStrcTable[r, 2])
                nested <- !any(apply(tab, 1, function(x) sum(x != 0) > 1))
                if (nested) { 
                  formularPredictors <-
                        c(formularPredictors,
                               paste0("(1 | ", paste0(nestedStrcTable[r, 1]), ")"),
                               paste0("(1 | ", input$selectedID, ":", nestedStrcTable[r, 1], ")"),
                               paste0("(1 | ", paste0(nestedStrcTable[r, 2], ":", nestedStrcTable[r, 1]), ")")
                        )
                }else{
                  formularPredictors <-
                        c(formularPredictors,
                          paste0("(1 | ", paste0(nestedStrcTable[r, 2]), ")"),
                          paste0("(1 | ", input$selectedID, ":", nestedStrcTable[r, 2], ")"),
                          paste0("(1 | ", paste0(nestedStrcTable[r, 1], ":", nestedStrcTable[r, 2]), ")")
                          )
                }
            }
        }
        formularText <- paste0(unique(formularPredictors), collapse = " + ")
        
        paste0(selectedOutcome(), " ~ (1 |", input$selectedID, ") + ", formularText)
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

# 运行推荐模型 ------------------------------------------------------------------
     output$recommFormular <- renderText({
         makeeasyformular(gtheoryFormula())
     })


     gstudyResult <- eventReactive(input$runRecommModel, {
         nFacet <- NULL
         datG <- dat
         datG[[selectedOutcome()]] <- as.numeric(dat[[selectedOutcome()]])
         datG[c(input$selectedID, selectedFacet())] <- lapply(datG[c(input$selectedID, selectedFacet())], as.factor)
         if (input$selfFormular == "") {
             formulaRecomm <- as.formula(gtheoryFormula())
             lme4.res <<- lmer(data = datG, formula = formulaRecomm)
         }else{
             lme4.res <<- lmer(data = datG, formula = as.formula(makehardformular(input$selfFormular)))
         }
        randomEffectEstimate <- ranef(lme4.res)
        randomEffectLevel <- lapply(lapply(datG, unique), length)
        nFacet <<- unlist(randomEffectLevel[selectedFacet()])
        gstudy(lme4.res)
     })

     dstudyResult <- eventReactive(input$runRecommModelDstudy, {
         datD <- dat
         
         datD[[selectedOutcome()]] <- as.numeric(datD[[selectedOutcome()]])
         datD[c(input$selectedID, selectedFacet())] <- lapply(datD[c(input$selectedID, selectedFacet())], as.factor)

         if (input$selfFormular == "") {
           lme4.res <- lmer(data = datD, formula = as.formula(gtheoryFormula()))
         }else{
           lme4.res <- lmer(data = datD, formula = as.formula(makehardformular(input$selfFormular)))
         }
         gstudy.res <- gstudy(lme4.res)
         dstudy(x = gstudy.res, n = nFacet, unit = unit())
     })

     ## with Bootstrapping
     gstudyResultBoot <- eventReactive(input$runRecommModelBoot, {
       datGBoot <- dat
       gstudy.res <- gstudyResult()
       datGBoot[[selectedOutcome()]] <- as.numeric(datGBoot[[selectedOutcome()]])
       datGBoot[c(input$selectedID, selectedFacet())] <- lapply(datGBoot[c(input$selectedID, selectedFacet())], as.factor)
       if (input$selfFormular == "") {
         formulaRecomm <- as.formula(gtheoryFormula())
         lme4.res <- lmer(data = datGBoot, formula = formulaRecomm)
       }else{
         lme4.res <- lmer(data = datGBoot, formula = as.formula(makehardformular(input$selfFormular)))
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

       boot.gstudy.res <- cbind(gstudy.res$gstudy.out, t(boot.gstudy$t))
       boot.gstudy.res

     })

     dstudyResultBoot <- eventReactive(input$runRecommModelDstudyBoot, {
       datDBoot <- dat
       datDBoot[[selectedOutcome()]] <- as.numeric(datDBoot[[selectedOutcome()]])
       datDBoot[c(input$selectedID, selectedFacet())] <- lapply(datDBoot[c(input$selectedID, selectedFacet())], as.factor)
       gstudy.res <- gstudyResult()
       boot.gstudy.res <- gstudyResultBoot()
       dstudy.res_boot <- dstudyResult() # placeholder

       boot.dstudy.res <- NULL
       for(i in 1:input$nboot) {
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
       dstudy.res.CI<-t(apply(t(boot.dstudy.res), 1,
                              function(x){quantile(x, probs = c(.025, .975))}))
       names(dstudy.res.CI) <- c("2.5%", "97.5%")

       # beautify output
       dstudy.res_boot$dcoef[2] <-
         dstudy.res.CI[nrow(dstudy.res.CI), 1]
       dstudy.res_boot$dcoef[3] <- dstudy.res.CI[nrow(dstudy.res.CI), 2]
       names(dstudy.res_boot$dcoef) <- c("Est", "2.5%", "97.5%")


       dstudy.res_boot$gcoef[2] <-dstudy.res.CI[nrow(dstudy.res.CI) - 1, 1]
       dstudy.res_boot$gcoef[3] <- dstudy.res.CI[nrow(dstudy.res.CI) - 1, 2]
       names(dstudy.res_boot$gcoef) <- c("Est", "2.5%", "97.5%")

       dstudy.res_boot$absvar[2] <- dstudy.res.CI[nrow(dstudy.res.CI) - 2, 1]
       dstudy.res_boot$absvar[3] <- dstudy.res.CI[nrow(dstudy.res.CI) - 2, 2]
       names(dstudy.res_boot$absvar) <- c("Est", "2.5%", "97.5%")

       dstudy.res_boot$relvar[2] <-
         dstudy.res.CI[nrow(dstudy.res.CI) - 3, 1]
       dstudy.res_boot$relvar[3] <- dstudy.res.CI[nrow(dstudy.res.CI) - 3, 2]
       names(dstudy.res_boot$relvar) <- c("Est", "2.5%", "97.5%")
       dstudy.res_boot$ds.df <-
         cbind(dstudy.res_boot$ds.df, dstudy.res.CI[1:(-4 + nrow(dstudy.res.CI)), ])

       # output
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
         output$selectedFacetMenu <- renderUI({
           allfacets <- selectedFacet()
           selectInput(inputId = "selectedFacet", label = "Select the facet to change",
                       choices = allfacets)
         })
         
         observeEvent(input$selectedFacet, {
           output$selectedFacetLevels <- renderUI({
             whichfacet <<- input$selectedFacet
             whichValue <<-
               ifelse(is.null(input$selectedFacetValue), nFacet[whichfacet], selectedFacetForLevel())
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
     }
    )

    observeEvent(input$runRecommModelDstudy,{
         i = 0
         dstudy.out <- dstudyResult()
         output$recommModelDStudyResult <- renderPrint( {
             print.dStudy(dstudy.out)
         })
         output$updatedN <- renderPrint( {
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
         colnames(dstudy.out$ds.df) <- c("Source", "Est.Variance", "N", "Est.(Var/N)")
         output$downloadDstudyResult <- downloadHandler(
           filename = "dstudyResult.csv",
           content = function(file) {
             write.csv(dstudy.out$ds.df, file, row.names = FALSE)
           }
         )
         
     }
    )

    # observe Bootstrapping and output
    observeEvent(input$runRecommModelBoot,{
      
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
      
      output$recommModelGStudyBootResult <- renderPrint( {
        gstudy.res_boot$gstudy.out
      })
      
      ## 可下载的表格
      output$downloadGstudyBootResult <- downloadHandler(
        filename = paste0("gstudyBootstrapN", input$nboot, "Result.csv"),
        content = function(file) {
          write.csv(gstudy.res_boot$gstudy.out, file, row.names = FALSE)
        }
      )
      
     }
    )

    observeEvent(input$runRecommModelDstudyBoot,{
      
      dstudy.res_boot <- dstudyResultBoot()
      colnames(dstudy.res_boot$ds.df) <- c("Source", "Est.Variance", "N", "Est.(Var/N)", "2.5%", "97.5%")
      output$recommModelDStudyBootResult <- renderPrint( {
        print.dStudy(dstudy.res_boot)
      })
      
      ## 可下载的表格
      output$downloadDstudyBootResult <- downloadHandler(
        filename = paste0("dstudyBootstrapN", input$nboot, "Result.csv"),
        content = function(file) {
          write.csv(dstudy.res_boot$ds.df, file, row.names = FALSE)
        }
      )
      
     }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
