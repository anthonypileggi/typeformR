# server

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  ## Reactive Values
  ##--------------------------------------------------------------------
  values <- reactiveValues()

  # - collect available typeform surveys (given an api_key)
  observeEvent(input$fetch_surveys, {
              cat("Collecting data from the Typeform API\n")
              values$survey_list <- rtypeform::get_all_typeforms(input$api_key)[['content']]
  })

  # - collect data for a specific survey
  observeEvent(input$fetch_data, {
    cat("Collecting data for ", input$survey_name, "(ID = ",
        values$survey_list$id[values$survey_list$name==input$survey_name], ")\n")

    values$raw_data <- rtypeform::get_results(api=input$api_key,
                                          uid=values$survey_list$id[values$survey_list$name==input$survey_name],
                                          completed=TRUE)

    values$data <- typeformR::cleanData(values$raw_data)

    values$summary <- typeformR::summarizeData(values$data)

    values$associations <- typeformR::getAssociations(values$data)

    values$n_questions <- length(values$data)
    values$n_respondents <- length(unique(do.call('c', lapply(values$data, function(x) x$results$userid))))
  })

  # -- update selectInput for `survey_list`
  observeEvent(values$survey_list, {
    updateSelectInput(session, 'survey_name',
                      choices = setNames(values$survey_list$name, cleanTags(values$survey_list$name)))
  })

  # -- update selectInput for `question1`
  observeEvent(values$raw_data, {
    choice_list <- as.character(sapply(values$data, function(x) x$question))
    updateSelectizeInput(session, 'question1',
                         choices = setNames(choice_list, cleanTags(choice_list)),
                         options = list(escape=FALSE))
    updateSelectizeInput(session, 'question2',
                        choices = setNames(choice_list, cleanTags(choice_list)),
                        options = list(escape=FALSE))
  })


  # -- store question indices --
  # store `question1` & `question2` id as a reactive value
  observeEvent(input$question1, {
    values$id1 <- which(names(req(values$summary))==req(input$question1))
  })
  observeEvent(input$question2, {
    values$id2 <- which(names(req(values$summary))==req(input$question2))
  })

  # -- reactives to trigger updates --
  # if either question index changes
  updateQuestions <- reactive({
    list(values$id1, values$id2)
  })
  # if elements of the pairwise plot change
  updatePairwisePlot <- reactive({
    list(values$id1, values$id2, input$plot_style)
  })
  # if global plot options change
  updatePlotOptions <- reactive({
    list(input$x_axis_text, input$y_axis_text, input$x_axis_label, input$y_axis_label)
  })

  # -- store plot options of reactive values (after a delay, TODO check the delay is working)
  observeEvent(updatePlotOptions(), {
      shinyjs::delay(2000, {
        values$x_axis_text <- input$x_axis_text
        values$y_axis_text <- input$y_axis_text
        values$x_axis_label <- input$x_axis_label
        values$y_axis_label <- input$y_axis_label
      })
  })

  # -- update summary data and associations for the selected question
  observeEvent(input$question1, {

    # check requirements
    req(values$summary, values$n_questions, values$data)

    # identify the selected question
    id <- values$id1
    idc <- setdiff(1:values$n_questions, id)

    # - plot: simple descriptive statiistics (question 1)
    values$plot1 <-
          ggplot(req(values$summary[[values$id1]]), aes(x=forcats::fct_reorder(Var1,Pct), y=Pct)) +
            geom_bar(stat="identity", fill='blue', alpha=.7) +
            labs(x=NULL, y="Percent") +
            scale_y_continuous(labels=scales::percent) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5))

    # get associations w/ all other questions
    values$assoc_summary <- values$associations$data %>%
                              dplyr::filter(question1 == names(values$data)[id]) %>%
                              dplyr::arrange(pvalue) %>%
                              dplyr::select(Question=question2, Pvalue=pvalue)

  })


  # -- add lines to heatmap --
  observeEvent(updateQuestions(), {

    values$associations$heatmap2 <- req(values$associations$heatmap)

    if (!is.null(values$id1)) {
      values$associations$heatmap2 <- values$associations$heatmap2 + geom_hline(yintercept=req(values$id1))
    }

    if (!is.null(values$id2)) {
      values$associations$heatmap2 <- values$associations$heatmap2 + geom_vline(xintercept=req(values$id2))
    }

  })

  # -- heatmap click event
  observe ({
    values$click <- event_data("plotly_click")
  })

  # -- update `question1` & `question2` when heatmap is clicked
  observeEvent(values$click, {
    id1 <- values$click$pointNumber[[1]][1] + 1
    id2 <- values$click$pointNumber[[1]][2] + 1
    updateSelectizeInput(session, "question1",
                         selected=names(values$data)[id1])
    updateSelectizeInput(session, "question2",
                         selected=names(values$data)[id2])
  })

  # -- update pairwise plot when either question changes --
  observeEvent(updatePairwisePlot(), {

    req(values$data, values$id1, values$id2)

    lev1 <- lev2 <- NULL
    if (is.factor(values$data[[values$id1]]$results$value)) {
      lev1 <- levels(forcats::fct_infreq(values$data[[values$id1]]$results$value))
    }
    if (is.factor(values$data[[values$id2]]$results$value)) {
      lev2 <- levels(forcats::fct_infreq(values$data[[values$id2]]$results$value))
    }
    values$plot2 <- typeformR::plotTwoQuestions(values$data, values$id1, values$id2,
                                                levels1 = lev1, levels2 = lev2,
                                                plot_style = input$plot_style)
  })

  # -- update available plot styles when questions change --
  observeEvent(updateQuestions(), {

    qtype_cat <- c('dropdown','list','yesno')
    qtype_num <- c("number", "opinionscale")

    if (!is.null(values$data) & !is.null(values$id1) & !is.null(values$id2)) {

      shinyjs::enable('plot_style')
      qtype1 <- values$data[[values$id1]]$type
      qtype2 <- values$data[[values$id2]]$type

      if (is.element(qtype1, qtype_cat) & is.element(qtype2, qtype_cat)) {
        updateSelectInput(session, 'plot_style',
                          choices = list("Bar Graph" = "bar",
                                          "Dot Plot" = "dot",
                                          "Grid Plot" = "grid"),
                          selected = "bar")
      } else if ( sum(is.element(c(qtype1,qtype2), qtype_cat)) == 1 ) {

        updateSelectInput(session, 'plot_style',
                          choices = list("Boxplot" = "boxplot",
                                         "Violin Plot" = "violin"),
                          selected = "boxplot")
      } else {
        shinyjs::disable('plot_style')
      }
    } else {
      shinyjs::disable('plot_style')
    }
  })

  #--------------------------------------------------------------------------


  # General survey statistics
  output$survey_stats <- renderText({
      paste0("<b>",req(input$survey_name), "<br> <br>",
             "Number of Questions: </b>", req(values$n_questions), "<br>",
             "<b> Number of Respondents: </b>", req(values$n_respondents), "<br>")
  })

  # Question Indices
  output$indexes <- renderText({
    paste0("<b>Queston 1:</b>  ", req(values$id1), "<br>",
           "<b>Question 2:</b> ", req(values$id2))
  })

  #---------- PLOT OUTPUTS -------------------

  # Plot: simple descriptive statistics
  output$plot1 <- renderPlot({
    values$plot1 +
      theme(axis.text.x = element_text(size=values$x_axis_text),
            axis.text.y = element_text(size=values$y_axis_text),
            axis.title.x = element_text(size=values$x_axis_label),
            axis.title.y = element_text(size=values$y_axis_label))
  })

  # Plot: pairwise
  output$plot2 <- renderPlot({
    values$plot2 +
      theme(axis.text.x = element_text(size=values$x_axis_text),
            axis.text.y = element_text(size=values$y_axis_text),
            axis.title.x = element_text(size=values$x_axis_label),
            axis.title.y = element_text(size=values$y_axis_label))
  })

  # Plot: association heatmap
  output$heatmap <- renderPlotly({
    plotly::ggplotly(req(values$associations$heatmap2), tooltip="text")
  })

  #---------- TABLE OUTPUTS -------------------

  # Table: simple descriptive statistics
  output$simple_table <- renderTable({
    setNames(values$summary[[values$id1]], c("Levels", "N", "Percent"))
  })

  # Table: strongest associations
  output$assoc_table <- DT::renderDataTable({
    DT::datatable(req(values$assoc_summary),
                  rownames=FALSE,
                  escape = FALSE,
                  options = list(pageLength=10)) %>%
      DT::formatRound('Pvalue', digits=4) %>%
      DT::formatStyle("Pvalue", color = styleInterval(c(.05, .15), c("red", "orange", "black")))
  })
})
