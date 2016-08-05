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
  x <- na.omit(x)
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
emAdDta <- as.data.frame(emAdDta)
emAdDta$value <- as.numeric(emAdDta$value)

#labels data
lablsDZ <- collect(tbl(dd, "dzlabels"))
lablsDZ <- as.data.frame(lablsDZ)
lablsIG <- collect(tbl(dd, "iglabels"))
lablsIG <- as.data.frame(lablsIG)