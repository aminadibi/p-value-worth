## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "p-Value Meter"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculator", tabName = "p-value", icon = icon("dashboard")),
      menuItem("About", tabName = "aboutTab", icon = icon("address-book", lib="font-awesome")),
      menuItem("Terms", tabName = "termsTab", icon = icon("balance-scale", lib="font-awesome"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "p-value",
              fluidRow(
                valueBoxOutput("ppv"),
                valueBoxOutput("npv"),
                
                numericInput("pvalue", label = h5("What is your p-value?"), 
                             value = 0.05, min = 0, max = 1, step = 0.01),
          

                numericInput("beta", label = h5("Type II error rate (β) ?"), 
                             value = 0.2, min = 0, max = 1, step = 0.1),
                
                numericInput("R", label = h5("Replication rate in similar studies"), 
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
    R <- input$R / (1 - input$R)
    ppv <- R * (1 - input$beta) / (R - R * input$beta + input$pvalue)
    ppv <- round (ppv, 4)
    
    valueBox(
      value = formatC(ppv, digits = 2, format = "f"),
      subtitle = "Probablity of true positive",
      icon = icon("user-plus"),
      color = "aqua"
    )
  })

  output$npv <- renderValueBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    R <- input$R / (1 - input$R)
    npv <- (1 - input$pvalue) / (R * input$beta + 1 - input$pvalue)

    valueBox(
      value = formatC(npv, digits = 2, format = "f"),
      subtitle = "Probablity of true negative",
      icon = icon("user-minus"),
      color = "red"
    )
  })
}

shinyApp(ui, server)