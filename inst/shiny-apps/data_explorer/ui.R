library(shiny)

shinyUI(fluidPage(

  titlePanel("Typeform Data Explorer"),

  sidebarLayout(
    sidebarPanel(
      h4("Step 1:  Input your API Key."),
      tags$span(style="color:darkorange",
                "Check the 'My Account' section of the ",
                tags$a(href="https://www.typeform.com/", "Typeform"), " website" ),
      textInput("api_key", "API Key: "),
      actionButton("fetch_surveys", "Get Survey List", icon = icon("refresh")),
      br(),
      tags$hr(),
      br(),
      h4("Step 2: Choose a survey to analyze."),
      selectInput("survey_name", "Survey: ", choices=list()),
      actionButton("fetch_data", "Get Survey Results", icon = icon("refresh")),
      br(),
      tags$hr(),
      br(),
      h4("Survey Statistics:"),
      htmlOutput("survey_stats")
    ),


    # Main
    mainPanel(
      h4("Step 3: Choose a question"),
      selectizeInput("question1", "Question:", choices=list(), options=list(escape=FALSE),
                     width="600px"),

      tabsetPanel(type = 'tabs',
        tabPanel("Plot", plotOutput("plot1")),
        tabPanel("Table", tableOutput("simple_table")),
        tabPanel("Associations", dataTableOutput("assoc_table"))
      )
    )

  )
))
