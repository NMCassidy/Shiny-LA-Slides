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

queryDestinations <- "PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/concept#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX void: <http://rdfs.org/ns/void#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?label ?area ?ratio
WHERE {
  ?s qb:dataSet <http://statistics.gov.scot/data/school-leaver-destinations-followup>.
  ?s <http://statistics.gov.scot/def/dimension/populationGroup> <http://statistics.gov.scot/def/concept/population-group/all>.
  ?s <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> <http://reference.data.gov.uk/id/government-year/2012-2013>.
  ?s <http://statistics.gov.scot/def/dimension/schoolLeaverDestination> <http://statistics.gov.scot/def/concept/school-leaver-destination/positive>.
  ?s <http://statistics.gov.scot/def/measure-properties/ratio> ?ratio.
  ?s <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?a.
  ?a rdfs:label ?area.
  ?a <http://statistics.data.gov.uk/def/statistical-entity#code> ?e.
  ?e rdfs:label ?label
}"
Dests <- SPARQL(endpoint, queryDestinations)$results

rowsKeep <- c("Country", "Council Areas", "Intermediate Zones","Data Zone")
Dests_cln <- Dests[Dests$label %in% rowsKeep,]
Dests_mlt <- melt(Dests_cln, id.vars = 1:2)