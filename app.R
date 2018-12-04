## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "p-value worth"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("p-value", tabName = "p-value", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "p-value",
              fluidRow(
                numericInput("pvalue", label = h5("What is your p-value?"), 
                             value = 0.05, min = 0, max = 1, step = 0.01),
          

                numericInput("beta", label = h5("Type II error rate (β) ?"), 
                             value = 0.2, min = 0, max = 1, step = 0.1),
                
                numericInput("R", label = h5("Replication rate"), 
                             value = 0.5, min = 0, max = 1, step = 0.01),
                
                hr(),
                h5("Your positive predicitve value is"),
                fluidRow(column(3, verbatimTextOutput("value")))
                
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  
  output$value <- renderPrint({ 
    ppv <- input$R * (1 - input$beta) / (input$R - input$R*input$beta + input$pvalue)
    ppv <- round (ppv, 4)
    print(ppv)
    })
  

}

shinyApp(ui, server)