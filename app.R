## app.R ##
library(shinydashboard)
library(scales)
library(shinythemes)


ui <- dashboardPage(
  dashboardHeader(title = "p-meter"),
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
              includeMarkdown("intro.Rmd"),
              tags$b("Enter inputs:"),
              #valueBoxOutput("npv"),
              fluidRow(
           
                column(4, offset = 0.8, 
                  "p-value", tags$span(style="color:red", "*"), ":",       
                  sliderInput("pvalue", width = '100%', label = "", 
                             value = 0.05, min = 0, max = 0.05, step = 0.001)
                ),
              
                column(4, 
                     "Statistical Power (1-β)", tags$span(style="color:red", "*"), ":",
                     sliderInput("power", width = '100%', label = "", 
                                  value = 0.8, min = 0, max = 1, step = 0.1)

                ),
              
                column(4, 
                       "Prior probability (%)", tags$span(style="color:red", "*"), ":",
                       sliderInput("R", width = '100%', label = "", 
                                  value = 69, min = 0, max = 100, step = 1)
                )
              
              ),
              valueBoxOutput("ppv", width = 12),
              
              #includeMarkdown("intro.Rmd"),
              includeMarkdown("methods.Rmd")
      ),
      
      tabItem(tabName = "aboutTab",
              includeMarkdown("about.Rmd")
      ),
      
      tabItem(tabName = "termsTab",
              includeMarkdown("disclaimer.Rmd")
      )
    )
  )
)

server <- function(input, output) {
  
  output$ppv <- renderValueBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    R <- input$R / (100 - input$R)
    beta <- 1 - input$power
    ppv <- scales::percent(R * (1 - beta) / (R - R * beta + input$pvalue))
    #ppv <- round (ppv, 4)
    
    valueBox(
      value = formatC(ppv, digits = 1, format = "f"),
      subtitle = "Positive Predictive Value (probablity of true positive)",
      icon = icon("user-plus"),
      color = "aqua"
    )
  })

  output$npv <- renderValueBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    R <- input$R / (100 - input$R)
    beta <- 1 - input$power
    npv <- (1 - input$pvalue) / (R * beta + 1 - input$pvalue)

    valueBox(
      value = formatC(npv, digits = 2, format = "f"),
      subtitle = "Probablity of true negative",
      icon = icon("user-minus"),
      color = "red"
    )
  })
}

shinyApp(ui, server)