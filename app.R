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
                valueBoxOutput("ppv"),
                
                numericInput("pvalue", label = h5("What is your p-value?"), 
                             value = 0.05, min = 0, max = 1, step = 0.01),
          

                numericInput("beta", label = h5("Type II error rate (β) ?"), 
                             value = 0.2, min = 0, max = 1, step = 0.1),
                
                numericInput("R", label = h5("Replication rate"), 
                             value = 0.5, min = 0, max = 1, step = 0.01)
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
  
  output$ppv <- renderValueBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    ppv <- input$R * (1 - input$beta) / (input$R - input$R*input$beta + input$pvalue)
    ppv <- round (ppv, 4)
    
    valueBox(
      value = formatC(ppv, digits = 2, format = "f"),
      subtitle = "Probablity of true positive",
      icon = icon("area-chart"),
      color = "aqua"
    )
  })

}

shinyApp(ui, server)