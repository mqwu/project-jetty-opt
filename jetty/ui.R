 
library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(DT)


shinyUI(fluidPage(
 
  source("external/header.R",local=T)$value, br(),
  source("external/sidebar.R",local=T)$value,
  source("external/main.R",local=T)$value

))

