library(SPARQL)
library(readr)
library(reshape2)
library(stringr)
library(dplyr)
#Sparql for data 
endpoint <- "http://statistics.gov.scot/sparql"
#Emergency Admissions
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
emAdDta[emAdDta$variable == "All", 4] <- "Emergency Admission Rate per 10,000"
emAdDta[emAdDta$variable == "65 And Over", 4] <- "Emergency Admission Rate per 10,000, Over 65s"

#School destinations
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
#Job Seeker's Allowance
JSAQry <- "PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/concept#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX void: <http://rdfs.org/ns/void#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?ReferenceArea ?code ?value ?Area
WHERE {
?s qb:dataSet <http://statistics.gov.scot/data/job-seekers-allowance>.
?s <http://purl.org/linked-data/cube#measureType> <http://statistics.gov.scot/def/measure-properties/ratio>.
?s <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> <http://reference.data.gov.uk/id/quarter/2012-Q4>.
?s <http://statistics.gov.scot/def/dimension/gender> <http://statistics.gov.scot/def/concept/gender/all>.
?s <http://statistics.gov.scot/def/measure-properties/ratio> ?value.
?s <http://statistics.gov.scot/def/dimension/age> <http://statistics.gov.scot/def/concept/age/16-64>.
?s <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?a.
?a rdfs:label ?ReferenceArea.
?a skos:notation ?code.
?a <http://statistics.data.gov.uk/def/statistical-entity#code> ?e.
?e rdfs:label ?Area
}"

JSADta <- SPARQL(endpoint, JSAQry)$results
JSADta$variable <- rep("JSA Claimant Pct, 16-64", nrow(JSADta))
for(i in 1:nrow(JSADta)){
  JSADta$code[i] <- str_sub(JSADta$code[i], start = 2, end = 10)
}
JSADta <- JSADta[c(1,2,3,5,4)]
JSADta <- JSADta[JSADta$Area %in% rowsKeep,]
#School Attainment
AttainmentQry <- "PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/concept#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX void: <http://rdfs.org/ns/void#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?ReferenceArea ?code ?value ?Area
WHERE {
?s qb:dataSet <http://statistics.gov.scot/data/pupil-attainment>.
?s <http://purl.org/linked-data/cube#measureType> <http://statistics.gov.scot/def/measure-properties/ratio>.
?s <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> <http://reference.data.gov.uk/id/government-year/2012-2013>.
?s <http://statistics.gov.scot/def/dimension/gender> <http://statistics.gov.scot/def/concept/gender/all>.
?s <http://statistics.gov.scot/def/measure-properties/ratio> ?value.
?s <http://statistics.gov.scot/def/dimension/scqfLevel> <http://statistics.gov.scot/def/concept/scqf-level/4-and-above>.
?s <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?a.
?a rdfs:label ?ReferenceArea.
?a skos:notation ?code.
?a <http://statistics.data.gov.uk/def/statistical-entity#code> ?e.
?e rdfs:label ?Area
}"

AttainDta <- SPARQL(endpoint, AttainmentQry)$results
AttainDta$variable <- rep("Pct of S4 Pupils Gaining 5 SCQF level 4 and above awards", nrow(AttainDta))
for(i in 1:nrow(AttainDta)){
  AttainDta$code[i] <- str_sub(AttainDta$code[i], start = 2, end = 10)
}
AttainDta <-AttainDta[c(1,2,3,5,4)]
AttainDta <- AttainDta[AttainDta$Area %in% rowsKeep,]

#Get some data zone only data and tidy it up for merging
DZdta<-readRDS("C:/users/cassidy.nicholas/OndeDrive-IS/Shiny LA Slides/DZdataset")
cols <- c("datazone_2001","Percentage of the population income deprived 2011", "Percentage of the population employment deprived 2011","SIMD ranking 2012")
DZdta <- DZdta[cols]
DZdta <- melt(DZdta, id.vars = c("datazone_2001"))
DZdta$Area <- rep("Data Zones", nrow(DZdta))
DZdta$code <- DZdta$datazone_2001  
colnames(DZdta)[1] <- "ReferenceArea"
DZdta <-DZdta[c(1,5,3,2,4)]

TSdta <- read_csv("C:/users/cassidy.nicholas/OneDrive - IS/Data/TariffScores.csv")
TSdta <- melt(TSdta)
TSdta$code <- TSdta$datazone
TSdta$Area <- rep("Data Zones", nrow(TSdta))
TSdta <- TSdta[c(1,4,3,2,5)]
colnames(TSdta)[1] <- "ReferenceArea"

emAdDta <- rbind(emAdDta, Dests, JSADta, AttainDta, DZdta, TSdta)

library(readstata13)
LAD <- read.dta13("S:/G - Governance & Performance Mngmt/Research Team/Local Authority Slides/Local Authority Slides - January 2015 Update/Data/LAdata.dta")
colskeep <- c("laname", "lacode", "csincdeprived_2011", "csempdeprived_2011", "edsqass4avgts_201213", "edsqass5avgts_201213")
LADDIE <- LAD[colskeep]
LADDIE <- melt(LADDIE, idvars = c("laname", "lacode"))
LADDIE$Area <- rep("Council Areas", nrow(LADDIE))
LADDIE[nrow(LADDIE)+1, ] <- c("Scotland", "S92000003","csincdeprived_2011", 13, "Country")
LADDIE[nrow(LADDIE)+1, ] <- c("Scotland", "S92000003","csempdeprived_2011", 13, "Country")
LADDIE[nrow(LADDIE)+1, ] <- c("Scotland", "S92000003","edsqass4avgts_201213", 193, "Country")
LADDIE[nrow(LADDIE)+1, ] <- c("Scotland", "S92000003","edsqass5avgts_201213", 356, "Country")
LADDIE <- LADDIE[c(1,2,4,3,5)]
colnames(LADDIE) <- colnames(emAdDta)
LADDIE$variable <- as.character(LADDIE$variable)
LADDIE[LADDIE$variable == "csincdeprived_2011", 4] <-"Percentage of the population income deprived 2011"
LADDIE[LADDIE$variable == "csempdeprived_2011", 4] <-"Percentage of the population employment deprived 2011"
LADDIE[LADDIE$variable == "edsqass4avgts_201213", 4] <-"S4 Average Tariff Score 2012/13"
LADDIE[LADDIE$variable == "edsqass5avgts_201213", 4] <-"S5 Average Tariff Score2012/13"
emAdDta <- rbind(emAdDta, LADDIE)
emAdDta[emAdDta$ReferenceArea=="Edinburgh, City of",1] <- "City of Edinburgh"
emAdDta[emAdDta$ReferenceArea=="Eilean Siar",1] <- "Comhairle nan Eilean Siar"
write.csv(emAdDta, "C:/users/cassidy.nicholas/OneDrive - IS/Shiny LA Slides/dataset.csv")

##SPARQL for geographic information
endpoint <- "http://statistics.gov.scot/sparql"
queryDZ <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?Council ?DataZ
WHERE {
?s <http://statistics.data.gov.uk/def/statistical-entity#code> <http://statistics.gov.scot/id/statistical-entity/S01>.
?s <http://statistics.gov.scot/def/hierarchy/best-fit#council-area> ?cn.
?cn rdfs:label ?Council.
?s rdfs:label ?DataZ.
} ORDER BY (?DataZ)"

lablsDZ <- SPARQL(endpoint, queryDZ)$results
write.csv(lablsDZ, "C:/users/cassidy.nicholas/OneDrive - IS/Shiny LA Slides/DZlabels.csv")

queryIG <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?Council ?IntZ ?code
WHERE {
?s <http://statistics.data.gov.uk/def/statistical-entity#code> <http://statistics.gov.scot/id/statistical-entity/S02>.
?s <http://statistics.data.gov.uk/def/statistical-geography#parentcode> ?cn.
?cn rdfs:label ?Council.
?s rdfs:label ?IntZ.   
?s <http://www.w3.org/2004/02/skos/core#notation> ?code
}"
lablsIG <- SPARQL(endpoint, queryIG)$results
for(i in 1:nrow(lablsIG)){
  lablsIG$code[i] <- str_sub(lablsIG$code[i], start = 2, end = 10)
}
write.csv(lablsIG, "C:/users/cassidy.nicholas/OneDrive - IS/Shiny LA Slides/IGlabels.csv")

hincQry <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?area ?value
WHERE {
?s ?p <http://statistics.gov.scot/data/local-level-average-household-income-estimates-2014>.
?s <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> <http://reference.data.gov.uk/id/year/2014>.
?s  <http://statistics.gov.scot/def/measure-properties/median> ?value.
?s <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?ar.
?ar rdfs:label ?area.
}"
IncDta <- SPARQL(endpoint, hincQry)$result

IncDta <- left_join(IncDta, lablsDZ, by = c("area" = "DataZ"))
write_rds(IncDta, "C:/users/cassidy.nicholas/OneDrive - IS/Shiny LA Slides/IncomeData.rds")
