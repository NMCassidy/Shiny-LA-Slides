library(SPARQL)
#Sparql for data 
endpoint <- "http://statistics.gov.scot/sparql"

emAdQry <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX qb: <http://purl.org/linked-data/cube#>

SELECT ?ReferenceArea ?code ?value ?variable ?Area
WHERE {
?s qb:dataSet <http://statistics.gov.scot/data/hospital-admissions>.
?s <http://purl.org/linked-data/cube#measureType> <http://statistics.gov.scot/def/measure-properties/ratio>.
?s <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> <http://reference.data.gov.uk/id/year/2012>.
?s <http://statistics.gov.scot/def/dimension/gender> <http://statistics.gov.scot/def/concept/gender/all>.
?s <http://statistics.gov.scot/def/dimension/admissionType> <http://statistics.gov.scot/def/concept/admission-type/emergency>.
?s <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?a.
?a rdfs:label ?ReferenceArea.
?a <http://statistics.data.gov.uk/def/statistical-entity#code> ?e.
?e rdfs:label ?Area.
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
rowsKeep <- c("Country", "Council Areas", "Intermediate Zones","Data Zones")
emAdDta <- emAdDta[emAdDta$Area %in% rowsKeep,]

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

SELECT  ?ReferenceArea ?code ?value ?Area
WHERE {
?s qb:dataSet <http://statistics.gov.scot/data/school-leaver-destinations-followup>.
?s <http://statistics.gov.scot/def/dimension/populationGroup> <http://statistics.gov.scot/def/concept/population-group/all>.
?s <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> <http://reference.data.gov.uk/id/government-year/2012-2013>.
?s <http://statistics.gov.scot/def/dimension/schoolLeaverDestination> <http://statistics.gov.scot/def/concept/school-leaver-destination/positive>.
?s <http://statistics.gov.scot/def/measure-properties/ratio> ?value.
?s <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?a.
?a rdfs:label ?ReferenceArea.
?a skos:notation ?code.
?a <http://statistics.data.gov.uk/def/statistical-entity#code> ?e.
?e rdfs:label ?Area
}"
Dests <- SPARQL(endpoint, queryDestinations)$results

Dests <- Dests[Dests$Area %in% rowsKeep,]
Dests$variable <- rep("Positive Leaver Destinations (Pct)", nrow(Dests))
for(i in 1:nrow(Dests)){
  Dests$code[i] <- str_sub(Dests$code[i], start = 2, end = 10)
}
Dests <- Dests[c(1,2,3,5,4)]
emAdDta <- rbind(emAdDta, Dests)

write.csv(emAdDta, "C:/Users/nickm/Google Drive/IS Work/Shiny LA Slides/dataset.csv")
