library(shiny)
library(dplyr)
library(readr)
library(ggplot2) 
library(ggthemes)
library(reshape2)
library(plotly)
library(stringr)
library(RSQLite)
library(DT)

Sys.setenv("plotly_username" = "NMCassidy")
Sys.setenv("plotly_api_key" = "xwk9zuxumf")

# High and Low function - sorts top and bottom ten values
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

#labels data from database
lablsDZ <- collect(tbl(dd, "dzlabels"))
lablsDZ <- as.data.frame(lablsDZ)
lablsIG <- collect(tbl(dd, "iglabels"))
lablsIG <- as.data.frame(lablsIG)

#read SIMD 16 data
dat16 <- readRDS("SIMD16.rds")
dat16$ReferenceArea <- as.character(dat16$ReferenceArea)

#read Incomes Data
datinc <- readRDS("IncomeData.rds")
colnames(datinc)[1] <- "ReferenceArea"
