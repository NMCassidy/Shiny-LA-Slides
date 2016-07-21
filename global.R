library(pacman)
p_load(shiny, dplyr, readr, ggplot2, reshape2, plotly, SPARQL, stringr)

HiLoTen <- function(x, number = 10){
  top_ten<-head(x, n = number)
  bot_ten<-tail(x, n = number)
  top_bot_df<-rbind(top_ten, bot_ten)
  return(top_bot_df)
}
##SPARQL for geographic information
endpoint <- "http://statistics.gov.scot/sparql"
queryDZ <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?Council ?DataZ
WHERE {
?s <http://statistics.data.gov.uk/def/statistical-entity#code> <http://statistics.gov.scot/id/statistical-entity/S01>.
?s <http://statistics.gov.scot/def/hierarchy/best-fit#council-area> ?cn.
?cn rdfs:label ?Council.
?s rdfs:label ?DataZ.
?s <http://statistics.data.gov.uk/def/statistical-geography#status> 'Archive'  
} ORDER BY (?DataZ)"

lablsDZ <- SPARQL(endpoint, queryDZ)$results

queryIG <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?Council ?IntZ ?code
WHERE {
?s <http://statistics.data.gov.uk/def/statistical-entity#code> <http://statistics.gov.scot/id/statistical-entity/S02>.
?s <http://statistics.data.gov.uk/def/statistical-geography#parentcode> ?cn.
?s <http://statistics.data.gov.uk/def/statistical-geography#status> 'Archive'.
?cn rdfs:label ?Council.
?s rdfs:label ?IntZ.   
?s <http://www.w3.org/2004/02/skos/core#notation> ?code
}"
lablsIG <- SPARQL(endpoint, queryIG)$results
for(i in 1:nrow(lablsIG)){
  lablsIG$code[i] <- str_sub(lablsIG$code[i], start = 2, end = 10)
}

#read data from csv
emAdDta <- read.csv("C:/Users/nickm/Google Drive/IS Work/Shiny LA Slides/dataset.csv", stringsAsFactors = FALSE)[2:6]
