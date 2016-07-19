library(pacman)
p_load(shiny, dplyr, readr, ggplot2, reshape2, plotly, SPARQL)

HiLoTen <- function(x, number = 10){
  top_ten<-head(x, n = number)
  bot_ten<-tail(x, n = number)
  top_bot_df<-rbind(top_ten, bot_ten)
  return(top_bot_df)
}

emAdDta <- read_csv("http://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhospital-admissions&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fratio&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fyear%2F2012&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FadmissionType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fadmission-type%2Femergency&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fgender=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fgender%2Fall", skip = 9)

emAdDta[emAdDta$`http://purl.org/linked-data/sdmx/2009/dimension#refArea` == "http://statistics.gov.scot/id/statistical-geography/S92000003",1] <- "Scotland"
emAdDta[grep("http://statistics.gov.scot/id/statistical-geography/S16", emAdDta$`http://purl.org/linked-data/sdmx/2009/dimension#refArea`),1] <- "Constituency"
emAdDta[grep("http://statistics.gov.scot/id/statistical-geography/S13", emAdDta$`http://purl.org/linked-data/sdmx/2009/dimension#refArea`),1] <- "Electoral Wards"
emAdDta[grep("http://statistics.gov.scot/id/statistical-geography/S12", emAdDta$`http://purl.org/linked-data/sdmx/2009/dimension#refArea`),1] <- "Council"
emAdDta[grep("http://statistics.gov.scot/id/statistical-geography/S08", emAdDta$`http://purl.org/linked-data/sdmx/2009/dimension#refArea`),1] <- "Health Board"
emAdDta[grep("http://statistics.gov.scot/id/statistical-geography/S06", emAdDta$`http://purl.org/linked-data/sdmx/2009/dimension#refArea`),1] <- "Regeneration Outcome Areas"
emAdDta[grep("http://statistics.gov.scot/id/statistical-geography/S05", emAdDta$`http://purl.org/linked-data/sdmx/2009/dimension#refArea`),1] <- "Regeneration Outcome Areas - CPP"
emAdDta[grep("http://statistics.gov.scot/id/statistical-geography/S03", emAdDta$`http://purl.org/linked-data/sdmx/2009/dimension#refArea`),1] <- "CHPs"
emAdDta[grep("http://statistics.gov.scot/id/statistical-geography/S02", emAdDta$`http://purl.org/linked-data/sdmx/2009/dimension#refArea`),1] <- "Intermediate Geography"
emAdDta[grep("http://statistics.gov.scot/id/statistical-geography/S01", emAdDta$`http://purl.org/linked-data/sdmx/2009/dimension#refArea`),1] <- "Data Zone"

rowsKeep <- c("Scotland", "Council", "Intermediate Geography","Data Zone")
colnames(emAdDta)[1] <- "Area"
emAdDta_cln <- emAdDta[emAdDta$Area %in% rowsKeep,]
emAdDta_mlt <- melt(emAdDta, id.vars = 1:2)

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

SELECT ?Council ?IntZ
WHERE {
?s <http://statistics.data.gov.uk/def/statistical-entity#code> <http://statistics.gov.scot/id/statistical-entity/S02>.
?s <http://statistics.data.gov.uk/def/statistical-geography#parentcode> ?cn.
?cn rdfs:label ?Council.
?s rdfs:label ?IntZ.   
}"
lablsIG <- SPARQL(endpoint, queryIG)$results

emAdDta_mlt$variable <- as.character(emAdDta_mlt$variable)
