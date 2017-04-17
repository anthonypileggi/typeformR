
library(shiny)
library(magrittr)
library(dplyr)
library(DT)
library(ggplot2)


# Functions
cleanTags <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  ## Reactive Values
  ##--------------------------------------------------------------------
  values <- reactiveValues()
  observeEvent(input$fetch_surveys, {
              cat("Collecting data from the Typeform API\n")
              values$survey_list <- rtypeform::get_all_typeforms(input$api_key)[['content']]
  })

  # - collect available typeform surveys (given an api_key)
  observeEvent(input$fetch_data, {
    cat("Collecting data for ", input$survey_name, "(ID = ",
        values$survey_list$id[values$survey_list$name==input$survey_name], ")\n")

    values$data <- rtypeform::get_results(api=input$api_key,
                                          uid=values$survey_list$id[values$survey_list$name==input$survey_name],
                                          completed=TRUE)

    values$data <- typeformR::cleanData(values$data)

    values$summary <- typeformR::summarizeData(values$data)

    values$n_questions <- length(values$data)
    values$n_respondents <- length(unique(do.call('c', lapply(values$data, function(x) x$results$userid))))
  })

  # -- update selectInput for `survey_list`
  observeEvent(values$survey_list, {
    updateSelectInput(session, 'survey_name',
                      choices = setNames(values$survey_list$name, cleanTags(values$survey_list$name)))
  })

  # -- update selectInput for `question1`
  observeEvent(values$data, {
    choice_list <- as.character(sapply(values$data, function(x) x$question))
    updateSelectizeInput(session, 'question1',
                         choices = setNames(choice_list, cleanTags(choice_list)),
                         options = list(escape=FALSE))
  })

  # -- update summary data and associationsfor the selected question
  observeEvent(input$question1, {

    # check requirements
    req(values$summary, values$n_questions, values$data)

    # identify the selected question
    id <- which(names(values$summary)==input$question1)
    idc <- setdiff(1:values$n_questions, id)

    # get summary statistics
    values$simple_summary <- values$summary[[id]]

    # get associations w/ all other questions
    values$assoc_summary <- data.frame(Question = names(values$data)[idc],
                                       Pvalue = sapply(idc, function(x) typeformR::associationTest(values$data, id, x))) %>%
                              dplyr::filter(!is.na(Pvalue)) %>%
                              dplyr::arrange(Pvalue)
  })

  #--------------------------------------------------------------------------


  # General survey statistics
  output$survey_stats <- renderText({
      paste0("<b>",req(input$survey_name), "<br> <br>",
             "Number of Questions: </b>", req(values$n_questions), "<br>",
             "<b> Number of Respondents: </b>", req(values$n_respondents), "<br>")
  })

  # Plot: simple descriptive statistics
  output$plot1 <- renderPlot({
    ggplot(req(values$simple_summary), aes(x=Var1, y=Pct)) +
        geom_bar(stat="identity", fill='blue', alpha=.7) +
        labs(x=NULL, y="Percent") +
        scale_y_continuous(labels=scales::percent) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
  })

  # Table: simple descriptive statistics
  output$simple_table <- renderTable({
    setNames(values$simple_summary, c("Levels", "N", "Percent"))
  })

  # Table: strongest associations
  output$assoc_table <- DT::renderDataTable({
    DT::datatable(req(values$assoc_summary),
                  escape = TRUE,
                  options = list(pageLength=10)) %>%
      DT::formatRound('Pvalue', digits=4) %>%
      DT::formatStyle("Pvalue", color = styleInterval(c(.05, .15), c("red", "orange", "black")))
  })
})
