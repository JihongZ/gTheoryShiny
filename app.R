#
# This is a Shiny web application for g theory visualization. ]
#
# Pathway:
# 1. A long-format data is imported
# 2. Allow users to identify facets
# 3. Check relationship among facets
#
# Data:
#     1. sleepstudy: 
#         (1) Reaction: average reaction time 
#         (2) Days: number of days of sleep 
#         (3) Subject: Subject number
    
#    http://shiny.rstudio.com/
#

library(lme4)
library(shiny)
library(DT) # for data table
library(markdown)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "G-theory Data Explorer",
    
    # # Application title
    # tabPanel("single variable",
    # 
    # # Sidebar with a slider input for number of bins 
    #     sidebarLayout(
    #         sidebarPanel(
    #             selectInput("selectedFacet",
    #                         "Which facet you are look at:",
    #                         choices = colnames(sleepstudy),
    #                         selected = colnames(sleepstudy)[1]),
    #         ),
    #         # Show a plot of the generated distribution
    #         mainPanel(
    #            DTOutput("factorTable")
    #         )
    #     ),
    # ),
    
    tabPanel("Data Structure",

    # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                checkboxGroupInput("selectedMultipleFacets",
                                   "Which facets you are looking at:",
                                   choices = colnames(sleepstudy),
                                   selected = NULL
                ),
                selectInput("selectedOutcome",
                            "Which outcome you are look at:",
                            choices = colnames(sleepstudy),
                            selected = NULL)
            ),
            # Show a plot of the generated distribution
            mainPanel(
               DTOutput("factorNestTable")
            )
        ),
    ),
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # single variable visualization
     output$factorTable <- renderDT({
         facet  <- sleepstudy[, input$selectedFacet]
         sleepstudy |> 
             group_by({{ facet }}) |> 
             summarise(
                 n = n()
             )
     })
     
     output$factorNestTable <- renderDT({
         facets  <- input$selectedMultipleFacets
         outcome  <- input$selectedOutcome
         sleepstudy |> 
             group_by(across(facets)) |> 
             summarise(
                 `Sample Size (Outcome)` = n_distinct(.data[[outcome]])
             )
     })
    
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
