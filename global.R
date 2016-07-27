library(shiny)
library(dplyr)
library(readr)
library(ggplot2) 
library(reshape2)
library(plotly)
library(stringr)
library(shinythemes)
library(RSQLite)

HiLoTen <- function(x, number = 10){
  if(nrow(x)>= 20){
  top_ten<-head(x, n = number)
  bot_ten<-tail(x, n = number)
  } else{
  top_ten<-head(x, n = (nrow(x)/2))
  bot_ten<-tail(x, n = (nrow(x)/2))  
  }
  top_bot_df<-rbind(top_ten, bot_ten)
  return(top_bot_df)
}

#read data from database
dd <- src_sqlite("dataset.db")
emAdDta <- collect(tbl(dd, "actual"))

#labels data
lablsDZ <- read_csv("DZlabels.csv")[2:3]
lablsIG <- read_csv("IGlabels.csv")[2:4]