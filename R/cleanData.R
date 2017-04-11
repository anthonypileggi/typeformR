#' Clean & prepare data obtained from the Typeform API
#' @param x Survey data from the typeform api (via ifixitR::getTypeformData(name))
#' @return A list where each element corresponds to survey data for a single question
#' @export
cleanData <- function (x) {

  ## - make a list, with an element for each unique question
  out <- data.frame(userid = x[['responses']][['token']],
    x[['responses']][['answers']],
    stringsAsFactors = FALSE) %>%
    tidyr::gather(id, value, -userid) %>%
    dplyr::left_join(x[['questions']], by="id") %>%
    dplyr::mutate(field_id = sapply(id, function(x) typeformR:::parseID(x)[['id']]),
                  choice_id = sapply(id, function(x) typeformR:::parseID(x)[['choice']]),
                  question_type = sapply(id, function(x) typeformR:::parseID(x)[['type']]))


  # - one list per question
  out <- split(out, out[['question']])

  # - convert `value` based on the `question_type` variable
  out <- lapply(out, typeformR:::convertValues)

  # - reorder to match original survey order (ignore high-level groups)
  out <- typeformR:::reorderQuestions(out, x[['questions']])

  # - compress data
  out <- lapply(out, typeformR:::compressData)

  return (out)
}


#' Get survey data from Typeform (using the API and rtypeform R package)
#' @param name Survey name
#' @return list containing: {stats, questions, responses, response}
getData <- function (api_key=NULL, survey_id=NULL) {

  # Get the requested survey data
  if (!is.null(api_key) & !is.null(survey_id)) {
    rtypeform::get_results(survey_id, api_key, completed=TRUE)
  }

}



#' Parse a Typeform Question ID into its individual components
#' @param x Typeform question ID (as a character)
#' @return list containing: {type, field_id, choice}
parseID <- function(x) {
  tmp <- strsplit(x, "_")[[1]]
  n <- length(tmp)
  list(type = tmp[1],
    field_id = tmp[2],
    choice = ifelse(n<3, NA, paste0(tmp[3:n], collapse="_")))
}


#' Convert response values to the appropriate type (based on question variant)
#' @param x Results from one typeform question (data.frame)
#' @return Original data.frame with the freshly converted `value` column
convertValues <- function(x) {

  if (x[['question_type']][1] %in% c('number', 'opinionscale')) {
    x[['value']] <- sapply(x[['value']], as.numeric)
  }

  if (x[['question_type']][1] %in% c('dropdown','list')) {
    x[['value']] <- sapply(x[['value']], as.factor)
  }

  if (x[['question_type']][1] %in% c('yesno')) {
    x[['value']] <- sapply(x[['value']], function(x) factor(x, levels=0:1, labels=c("No","Yes")))
  }

  if (x[['question_type']][1] %in% c('textarea','textfield','email')) {
    x[['value']] <- sapply(x[['value']], as.character)
  }

  return(x)
}



#' Put a list of question-responses in the order they were presented
#' @param x List with typeform survey results (one question/element)
#' @param questions data.frame with questions (i.e., ifixitR::getTypeformData()[['questions']])
#' @return Input list in the correct order
reorderQuestions <- function (x, questions) {

  question_order <- questions %>%
    dplyr::mutate(lag = dplyr::lag(field_id),
                  new = as.numeric(lag != field_id),
                  num = cumsum(ifelse(is.na(new), 0, new)) + 1) %>%
    dplyr::group_by(num, field_id, question) %>%
    dplyr::summarize() %>%
    dplyr::ungroup()

  question_order <- data.frame(new_num = 1:length(x),
    question = names(x),
    stringsAsFactors=FALSE) %>%
    dplyr::left_join(question_order, by = "question") %>%
    dplyr::arrange(num) %>%
    dplyr::select(new_num) %>% .[[1]]

  x[question_order]
}


#' Compress questions (to remove duplicate data)
#' @param x Results from one typeform question (data.frame)
#' @return Compressed data in list form
compressData <- function(x) {
  list(question = as.character(x$question[1]),
    type = as.character(x$question_type[1]),
    group = as.character(x$group[1]),
    results = x[,c("userid","value")])
}