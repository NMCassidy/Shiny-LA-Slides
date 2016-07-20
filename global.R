library(pacman)
p_load(shiny, dplyr, readr, ggplot2, reshape2, plotly, SPARQL, stringr)

HiLoTen <- function(x, number = 10){
  top_ten<-head(x, n = number)
  bot_ten<-tail(x, n = number)
  top_bot_df<-rbind(top_ten, bot_ten)
  return(top_bot_df)
}
#Sparql for data 
endpoint <- "http://statistics.gov.scot/sparql"

emAdQry <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX qb: <http://purl.org/linked-data/cube#>

SELECT ?ReferenceArea ?code ?value ?variable
WHERE {
  ?s qb:dataSet <http://statistics.gov.scot/data/hospital-admissions>.
  ?s <http://purl.org/linked-data/cube#measureType> <http://statistics.gov.scot/def/measure-properties/ratio>.
  ?s <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> <http://reference.data.gov.uk/id/year/2012>.
  ?s <http://statistics.gov.scot/def/dimension/gender> <http://statistics.gov.scot/def/concept/gender/all>.
  ?s <http://statistics.gov.scot/def/dimension/admissionType> <http://statistics.gov.scot/def/concept/admission-type/emergency>.
  ?s <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?a.
  ?a rdfs:label ?ReferenceArea.
  ?a skos:notation ?code.
  ?s <http://statistics.gov.scot/def/measure-properties/ratio> ?value.
  ?s <http://statistics.gov.scot/def/dimension/age> ?ind.
  ?ind rdfs:label ?variable.
}"
emAdDta <- SPARQL(endpoint, emAdQry)$results
#Tidy up code variable
for(i in 1:nrow(emAdDta)){
emAdDta$code[i] <- str_sub(emAdDta$code[i], start = 2, end = 10)
}
emAdDta[[5]] <- 1:nrow(emAdDta)
#New variable with the geography type
emAdDta[emAdDta$code == "S92000003",5] <- "Scotland"
emAdDta[grep("S16", emAdDta$code),5] <- "Constituency"
emAdDta[grep("S13", emAdDta$code),5] <- "Electoral Wards"
emAdDta[grep("S12", emAdDta$code),5] <- "Council"
emAdDta[grep("S08", emAdDta$code),5] <- "Health Board"
emAdDta[grep("S06", emAdDta$code),5] <- "Regeneration Outcome Areas"
emAdDta[grep("S05", emAdDta$code),5] <- "Regeneration Outcome Areas - CPP"
emAdDta[grep("S03", emAdDta$code),5] <- "CHPs"
emAdDta[grep("S02", emAdDta$code),5] <- "Intermediate Geography"
emAdDta[grep("S01", emAdDta$code),5] <- "Data Zone"
colnames(emAdDta)[5] <- "Area"
rowsKeep <- c("Scotland", "Council", "Intermediate Geography","Data Zone")
emAdDta <- emAdDta[emAdDta$Area %in% rowsKeep,]
##SPARQL for geographic information
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
emAdDta$variable <- as.character(emAdDta$variable)

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