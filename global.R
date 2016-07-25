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
emAdDta <- read.csv("dataset.csv", stringsAsFactors = FALSE)[2:6]

#labels data
lablsDZ <- read_csv("DZlabels.csv")[2:3]
lablsIG <- read_csv("IGlabels.csv")[2:4]