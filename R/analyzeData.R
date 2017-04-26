#' Compute descriptive statistics for responses to a typeform question
#' @param x Data.frame with clean typeform survey results (one question only)
#' @return A data.frame where each element is a summary of the correponding question's results
#' @details Statistics are computed based on the type of question that is being asked.
#' @export
summarizeQuestion <- function (x) {

  out <- NULL
  question_type <- x$type
  x <- x$results

  if (question_type == 'list') {
    n <- length(unique(x[['userid']]))
    out <- data.frame(table(x[['value']]), stringsAsFactors=FALSE) %>%
      dplyr::mutate(Pct = Freq/n) %>%
      dplyr::arrange(desc(Freq))
  }

  if (question_type %in% c('dropdown','yesno')) {
    out <- data.frame(table(x[['value']]), stringsAsFactors=FALSE) %>%
      dplyr::mutate(Pct = Freq/sum(Freq)) %>%
      dplyr::arrange(desc(Freq))
  }

  if (question_type == 'opinionscale') {
    out <- data.frame(table(x[['value']]), stringsAsFactors=FALSE) %>%
      dplyr::mutate(Pct = Freq/sum(Freq)) %>%
      dplyr::arrange(Var1)
  }

  if (question_type == 'number') {
    out <- data.frame(min = min(x[['value']], na.rm=TRUE),
      q1 = quantile(x[['value']], 0.25, na.rm=TRUE),
      median = quantile(x[['value']], 0.5, na.rm=TRUE),
      mean = mean(x[['value']], na.rm=TRUE),
      q3 = quantile(x[['value']], 0.75, na.rm=TRUE),
      max = max(x[['value']], na.rm=TRUE),
      nmiss = sum(is.na(x[['value']])))
  }

  if (question_type %in% c("textfield", "textarea", "email")) {
    out <- data.frame(userid=x[['userid']],
      value=x[['value']],
      stringsAsFactors=FALSE)
  }

  return(out)
}



#' Compute descriptive statistics for a list of typeform question-responses
#' @param x List of data.frames, each with clean results for a single question
#' @return A list where each element is a summary of the correponding question's results
#' @details Text fields are combined into a single data.frame as the last element (called "Text")
#' @export
summarizeData <- function (x) {

  ## summarize each individual question
  results <- lapply(x, typeformR::summarizeQuestion)

  ## deal with questions with text field responses
  if (1 == 0) {

    ## combine text fields into their own unique list element (because I don't know what to use them for yet!)
    text_id <- as.numeric(which(sapply(x, function(x) x$type %in% c("textfield", "textarea", "email"))))
    for (i in seq_along(text_id)) {
      id <- text_id[i]
      if (i==1) {
        out <- results[[id]]
      } else {
        out <- dplyr::left_join(out, results[[id]], by="userid")
      }
      names(out)[i+1] <- names(results)[id]
    }

    ## remove from summary data, append to end of summary
    results[text_id] <- NULL
    results[[length(results)+1]] <- out
    names(results)[length(results)] <- "Text"

  }

  return(results)
}


#' Merge user responses for three different questions
#' @param x List of data.frames, each with clean results for a single question
#' @param ids Vector of question indices
#' @return A list containing {data, type, question}
#'           data -- data.frame with the merged data: {userid, value1, value2}
#' @export
mergeQuestions <- function(x, ids) {

  # Merge all questions within `ids`
  for (i in seq_along(ids)) {
    if (i == 1) {
      out <- setNames(x[[ ids[i] ]][['results']], c('userid','value1'))
    } else {
      out <- out %>% dplyr::left_join(setNames(x[[ ids[i] ]][['results']], c('userid',paste0('value',i))),
        by = 'userid')
    }
  }

  # Remove rows w/ missing data
  out <- out[complete.cases(out), ]

  # Store result as a list
  list(data = out,
       type = as.character(vapply(x[ids], function(x) x$type, character(1))),
       question = as.character(vapply(x[ids], function(x) x$question, character(1))))
}



#' Create a bi-variate plot that describes the results of two questions
#' @param x List of data.frames, each with clean results for a single question
#' @param id1 Index of the first question
#' @param id2 Index of the second question
#' @param levels1 Ordered factor levels for the first question
#' @param levels2 Ordered factor levels for the second question
#' @param plot_style Plot style ("bar", "grid", "dot", "boxplot", "violin")
#' @return A plot
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' @details Question type will dictate the plot that is produced:
#' \itemize{
#'  \item{\bold{dropdown/list vs. dropdown/list} -- faceted bar graph}
#'  }
#' @export
plotTwoQuestions <- function (x, id1, id2, levels1=NULL, levels2=NULL, plot_style="bar") {

  # merge responses
  xt <- typeformR::mergeQuestions(x, ids=c(id1, id2))

  # remove levels with only 1 response
  removeSparseLevels <- function (vals) {
    vals <- as.character(vals)
    tmp <- data.frame(table(vals))
    good_levels <- as.character(tmp[ tmp[,2]>1, 1])
    vals[!(vals %in% good_levels)] <- "Other"
    vals <- factor(vals, levels=c(good_levels,"Other"))
    return(vals)
  }
  # -- perform on categorical variables only
  if ( xt$type[1] %in% c("dropdown","list","yesno") ) {
    xt$data$value1 <- removeSparseLevels(xt$data$value1)
  }
  if ( xt$type[2] %in% c("dropdown","list","yesno") ) {
    xt$data$value2 <- removeSparseLevels(xt$data$value2)
  }


  # set factor levels
  if (!is.null(levels1)) { xt$data$value1 <- factor(xt$data$value1, levels=levels1) }
  if (!is.null(levels2)) { xt$data$value2 <- factor(xt$data$value2, levels=levels2) }
  # -- and drop rows w/ missing data  (note: this may be dangerous!)
  xt$data <- xt$data[complete.cases(xt$data),]

  # bivariate plot

  # - categorical vs. categorical
  if ( sum(xt$type %in% c("dropdown","list","yesno")) == 2 ) {

    tmp <- xt$data %>% dplyr::group_by(value1) %>%
                      dplyr::mutate(respondents = dplyr::n_distinct(userid)) %>%
                      dplyr::group_by(value1, value2) %>%
                      dplyr::summarize(respondents = head(respondents,1),
                                       n = n()) %>%
                      dplyr::mutate(pct=n/respondents,
                                    name = paste0(value1," (n = ", respondents, ")"))

    if (plot_style == "bar") {

      g1 <- tmp %>% ggplot(aes(x=value2, y=pct)) +
              geom_bar(stat="identity", fill='orange') +
              labs(x=NULL, y="Percentage of Respondents",
                title=gsub("<strong>|</strong>", "", names(x)[id1]),
                subtitle=gsub("<strong>|</strong>", "", names(x)[id2])) +
              scale_y_continuous(labels=scales::percent) +
              facet_wrap(~name) +
              theme_bw() +
              theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                axis.text.x = element_text(angle=-45, hjust=0))

    } else if (plot_style == "grid") {

      g1 <- expand.grid(value1=unique(xt$data$value1),value2=unique(xt$data$value2)) %>%
              left_join(tmp, by=c('value1','value2')) %>%
              dplyr::mutate(pct = ifelse(is.na(pct), 0, pct)) %>%
              ggplot(aes(x=value1, y=value2, fill=pct)) +
              geom_tile() +
              geom_text(aes(label=ifelse(pct>0, scales::percent(round(pct,2)), "")),
                color='black') +
              labs(x=gsub("<strong>|</strong>", "", names(x)[id1]),
                y=gsub("<strong>|</strong>", "", names(x)[id2]),
                fill=NULL) +
              scale_fill_gradient(labels=scales::percent, low='white', high='lightblue') +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text.y = element_text(angle = 0, hjust = 1),
                plot.title = element_text(hjust = 0.5)) +
              guides(fill=FALSE)

    } else if (plot_style == "dot") {

      g1 <- tmp %>% ggplot() +
                      geom_point(aes(x=value1, y=value2, size=pct), color='darkred') +
                      labs(x=NULL, y=NULL, size=NULL,
                        title=gsub("<strong>|</strong>", "", names(x)[id1]),
                        subtitle=gsub("<strong>|</strong>", "", names(x)[id2])) +
                      scale_size_continuous(labels=scales::percent) +
                      theme_bw() +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1),
                            plot.title = element_text(hjust = 0.5),
                            plot.subtitle = element_text(hjust = 0.5))

    }
  }

  # - numeric vs. numeric
  if ( sum(xt$type %in% c("number","opinionscale")) == 2 ) {

    g1 <- xt$data %>% ggplot(aes(x=value1, y=value2)) +
                      geom_jitter(width=0.15, height=0.15) +
                      geom_smooth(method = "lm", se=FALSE, color='darkgreen') +
                      labs(x=xt$question[1], y=xt$question[2],
                          title = paste0("Correlation = ", round(cor(xt$data$value1, xt$data$value2),2))) +
                      theme_bw() +
                      theme(plot.title = element_text(hjust = 0.5),
                            plot.subtitle = element_text(hjust = 0.5))
  }

  # - numeric vs. categorical
  if ( sum(xt$type %in% c("dropdown","list","yesno")) == 1 &
       sum(xt$type %in% c("number","opinionscale")) == 1 ) {

    idx <- which(xt$type %in% c("dropdown","list","yesno"))
    idy <- which(xt$type %in% c("number", "opinionscale"))

    if (plot_style == "boxplot") {

      g1 <- xt$data %>% ggplot(aes_string(x=names(xt$data)[idx+1],
                                          y=names(xt$data)[idy+1])) +
                          geom_boxplot(fill='dodgerblue') +
                          labs(x=xt$question[idx], y=xt$question[idy]) +
                          theme_bw() +
                          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                plot.title = element_text(hjust = 0.5),
                                plot.subtitle = element_text(hjust = 0.5))
    } else if (plot_style == "violin") {

        g1 <- xt$data %>% ggplot(aes_string(x=names(xt$data)[idx+1],
                                            y=names(xt$data)[idy+1])) +
                            geom_violin(fill='dodgerblue') +
                            labs(x=xt$question[idx], y=xt$question[idy]) +
                            theme_bw() +
                            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                  plot.title = element_text(hjust = 0.5),
                                  plot.subtitle = element_text(hjust = 0.5))
    }
  }

  g1
}



#' Test for an association between two survey questions
#' @param x A list where each element represents a single question
#' @param id1 Index of the first question
#' @param id2 Index of the second question
#' @return P-value for the test of association
#' @export
associationTest <- function (x, id1, id2) {

  ## Merge two questions
  x <- typeformR::mergeQuestions(x, c(id1, id2))

  ## Question type
  qtype_cat <- c('dropdown','list','yesno')
  qtype_num <- c("number", "opinionscale")

  ## Test for an association
  ## - two category = chi-square test
  ## - one category, one numeric = anova test
  ## - two numeric = Pearson correlation test  (<-- still need to add)
  if ( length(unique(x$data$value1))<=1 | length(unique(x$data$value2))<=1 ) {

    pval <- NA

  } else if ( sum(x$type %in% qtype_cat) == 2 ) {

    pval <- chisq.test(x$data$value1, x$data$value2)$p.value

  } else if ( sum(x$type %in% qtype_num)==1 & sum(x$type %in% qtype_cat)==1 ) {

    x_id <- which(x$type %in% qtype_cat) + 1
    y_id <- which(x$type %in% qtype_num) + 1
    pval <- anova(lm(x$data[,y_id] ~ x$data[,x_id]))[1,5]

  } else if ( sum(x$type %in% qtype_num) == 2 ) {

    pval <- cor.test(x$data$value1,x$data$value2)$p.value

  } else {

    pval <- NA

  }

  return(pval)
}



#' Test associations between all survey questions
#' @param data A data.frame returned from typeformR::cleanData()
#' @return A list that includes:
#'           data -- data.frame with:  {question1, question1, pvalue}
#'           plot -- association heatmap
#' @export
getAssociations <- function (data) {

  n <- length(data)
  assoc_summary <- NULL
  for (id in 1:n) {
    cat(id, "\n")

    idc <- setdiff(1:n, id)

    tmp <- data.frame(question1 = rep(names(data)[id], n-1),
                      question2 = names(data)[idc],
                      pvalue = sapply(idc,
                                      function(x) suppressWarnings(typeformR::associationTest(data, id, x))))

    if (is.null(assoc_summary)) {
      assoc_summary <- tmp
    } else {
      assoc_summary <- rbind(assoc_summary, tmp)
    }
  }

  assoc_summary$question1 <- factor(assoc_summary$question1, levels=names(data))
  assoc_summary$question2 <- factor(assoc_summary$question2, levels=names(data))

  # -- stats
  stats <- data.frame(n_pairs = nrow(assoc_summary),
                      n_missing = sum(is.na(assoc_summary$pvalue)),
                      n_sig_001 = sum(assoc_summary$pvalue < .001, na.rm=TRUE),
                      n_sig_01 = sum(assoc_summary$pvalue < .01, na.rm=TRUE),
                      n_sig_05 = sum(assoc_summary$pvalue < .05, na.rm=TRUE))

  # -- heatmap
  g1 <- assoc_summary %>%
          ggplot(aes(x=question1, y=question2, fill=pvalue)) +
            geom_tile(aes(text=sprintf("%s<br>%s<br>%s",
                                      question1, question2,
                                      sprintf('%0.3f',round(pvalue,3))))) +
            scale_fill_gradient(low="red", high="white") +
            labs(x="Question 1", y="Question 2", fill="P-value") +
            theme_bw() +
            theme(axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5))

  # -- P-value vs. [# of pairs]
  g2 <- assoc_summary %>%
          dplyr::arrange(pvalue) %>%
          dplyr::mutate(count=row_number()) %>%
          ggplot(aes(x=count, y=ifelse(is.na(pvalue), 1, pvalue),
                     color=ifelse(is.na(pvalue),'a','b'))) +
            geom_line() +
            scale_color_manual(values=c("a"="gray", "b"="black"), guide=FALSE) +
            labs(x="# of Question Pairs", y="P-value") +
            theme_bw()

  list(data = assoc_summary,
       stats = stats,
       heatmap = g1,
       plot = g2)
}
