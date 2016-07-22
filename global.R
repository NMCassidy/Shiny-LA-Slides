library(shiny)
library(dplyr)
library(readr)
library(ggplot2) 
library(reshape2)
library(plotly)
library(stringr)
library(shinythemes)

HiLoTen <- function(x, number = 10){
  top_ten<-head(x, n = number)
  bot_ten<-tail(x, n = number)
  top_bot_df<-rbind(top_ten, bot_ten)
  return(top_bot_df)
}

#read data from csv
emAdDta <- read.csv("Q:/Shiny LA Slides/dataset.csv", stringsAsFactors = FALSE)[2:6]

#labels data
lablsDZ <- read_csv("Q:/Shiny LA Slides/DZlabels.csv")
lablsIG <- read_csv("Q:/Shiny LA Slides/IGlabels.csv")