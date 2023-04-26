# UI function for long to wide panel
pivotwiderUI <- function(id){
  ns <- NS(id)
  
  tagList(
    h4("Long to Wide Transformation:"),
    ## 设定tag/ID前缀和标签
    checkboxInput(ns("isLongFormat"), "Long Format?", TRUE),
    conditionalPanel(
      condition = "input.isLongFormat == 0",
      ns = ns,
      uiOutput(ns("nRowsSelection")),
      uiOutput(ns("preFixText")),
      uiOutput(ns("TagNamesText")),
      ## 转换
      actionButton(ns("transform"), "Transform")
    )
  )
}


pivotwiderServer <- function(id, data){
    moduleServer(
      id,
      function(input, output, session){
        
        # 按下"transform"按钮后，将原始数据转换为长数据格式
        output$nRowsSelection <- renderUI({
          selectInput(
            session$ns("nRows"),
            "How many rows for TAG/ID",
            choices = 1:nrow(data),
            selected = 2
          )
        })
        
        output$preFixText <- renderUI({
          textInput(session$ns("preFix"), "Set TAG/ID Prefix（for example, A;B;C）", value = "T;R")
        })
        
        output$TagNamesText <- renderUI({
          textInput(session$ns("transform"),
                    "tag/ID的column名字(比如Class;Rater;Item)",
                    "Task;Rater")
        })
        
      }
    )
}