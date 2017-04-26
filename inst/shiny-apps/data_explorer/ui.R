# ui

shinyUI(fluidPage(

  useShinyjs(),

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
      htmlOutput("survey_stats"),
      br(),
      tags$hr(),
      br(),
      h4("Plot Options:"),
      div(style="display:inline-block", numericInput('x_axis_text', "Text Size (x): ", value=10)),
      div(style="display:inline-block", numericInput('y_axis_text', "Text Size (y): ", value=10)),
      div(style="display:inline-block", numericInput('x_axis_label', "Label Size (x): ", value=14)),
      div(style="display:inline-block", numericInput('y_axis_label', "Label Size (y): ", value=14))

    ),


    # Main
    mainPanel(
      h4("Step 3: Choose a question"),
      selectizeInput("question1", "Question:", choices=list(), options=list(escape=FALSE),
                     width="600px"),
      tabsetPanel(type = 'tabs',
        tabPanel("Plot", plotOutput("plot1", height="600px")),
        tabPanel("Table", tableOutput("simple_table")),
        tabPanel("Associations", dataTableOutput("assoc_table")),
        tabPanel("Heatmap", htmlOutput("indexes"),
                            plotlyOutput("heatmap", height="600px")),
        tabPanel("Pairwise Plot",
                 selectizeInput("question2", "Question 2:", choices=list(),
                                options=list(escape=FALSE), width="600px"),
                 selectInput("plot_style", "Plot Style: ",
                             choices = list("Bar Graph" = "bar",
                                            "Dot Plot" = "dot",
                                            "Grid Plot" = "grid"),
                             selected="bar"),
                 plotOutput("plot2", height="600px"))
      )
    )

  )
))
