# global for typeformR shiny app

library(shiny)
library(shinyjs)
library(magrittr)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)



# Functions
cleanTags <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}