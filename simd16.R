##First Analysis of SIMD16 Data
library(pacman)
p_load(ggplot2, readxl, dplyr, reshape2)
#data source - http://www.gov.scot/Topics/Statistics/SIMD
dta <- read_excel("Q:/Data/SIMD16/SIMD16Indicators.xlsx", sheet = 3,
                 col_names = TRUE)

#rename Eilean Siar
dta[dta$Council_area %in% "Na h-Eileanan an Iar", 3] <- "Comhairle nan Eilean Siar"

#Calculate some Council figures
dtaCnc <- dta %>% group_by(Council_area) %>%
  summarise_at(vars(matches("Total_population|Working_age_population|Income_count|Employment_count")), funs(sum)) 
dtaCnc$IncRat <- round(dtaCnc$Income_count/dtaCnc$Total_population, 2)
dtaCnc$EmpRat <- round(dtaCnc$Employment_count/dtaCnc$Working_age_population,2)
dtaCNCM <- dta %>% group_by(Council_area) %>% na.omit() %>%
  summarise_at(vars(matches("EMERG|Attendance|Attainment|NEET|HESA")), funs(mean))
#bind and get rid of extra df
dtaCnc <- left_join(dtaCnc, dtaCNCM, by = "Council_area")
rm(dtaCNCM)

#Get Lookups for IG and Council codes - also from http://www.gov.scot/Topics/Statistics/SIMD
codeDta <- read_excel("Q:/Data/SIMD16/DZLookup.xlsx", sheet = 1,
                      col_names = TRUE)
dta <- left_join(dta, codeDta[c(1,18)], by = c("Data_Zone" = "DZ"))
#get figures for int geos
dtaIG <- dta %>% group_by(IZcode) %>%
  summarise_at(vars(matches("Total_population|Working_age_population|Income_count|Employment_count")), funs(sum))
dtaIG$IncRat <- round(dtaIG$Income_count/dtaIG$Total_population, 2)
dtaIG$EmpRat <- round(dtaIG$Employment_count/dtaIG$Working_age_population, 2)
dtaIGM <- dta %>% group_by(IZcode) %>% na.omit() %>%
  summarise_at(vars(matches("EMERG|Attendance|Attainment|NEET|HESA")), funs(mean))
#bind & drop
dtaIG <- left_join(dtaIG, dtaIGM, by = "IZcode")
rm(dtaIGM)

#Scotland Values
dtaScot <- dta %>% summarise_at(vars(matches("Total_population|Working_age_population|Income_count|Employment_count")), funs(sum))
dtaScot$IncRat <- round(dtaScot$Income_count/dtaScot$Total_population, 2)
dtaScot$EmpRat <- round(dtaScot$Employment_count/dtaScot$Working_age_population, 2)
dtaScotM <- dta %>% na.omit() %>%
  summarise_at(vars(matches("EMERG|Attendance|Attainment|NEET|HESA")), funs(mean))
dtaScot <- cbind("Scotland", dtaScot, dtaScotM)
colnames(dtaScot)[1] <-"ReferenceArea"
rm(dtaScotM)
dtaScot$EMERG <- 100

#filter interesting data
dtaDZ <- select(dta, matches("Data_Zone|Council_area|Income_rate|Employment_rate|EMERG|Attendance|Attainment|NEET|HESA"))

#merge codes with data
dtaIG <- unique(left_join(dtaIG, codeDta[18:19], by = "IZcode"))
dtaCnc <- unique(left_join(dtaCnc, codeDta[20:21], by = c("Council_area" = "LAname")))
rm(codeDta)

#Add area type
dtaCnc$Area <- "Council Areas"
dtaDZ$Area <- "Data Zones"
dtaIG$Area <- "Intermediate Zones"

#tidy and melt
dtaCnc <- select(dtaCnc, -matches("population|count"))
dtaCncMlt <- melt(dtaCnc, id.vars = c("Council_area", "LAcode", "Area"))
dtaDZ <- select(dtaDZ, -matches("population|count"))
dtaDZMlt <- melt(dtaDZ, id.vars = c("Data_Zone", "Area", "Council_area"))
dtaDZMlt$code <- dtaDZMlt$Data_Zone
dtaIG <- select(dtaIG, -matches("population|count"))
dtaIGMlt <- melt(dtaIG, id.vars = c("IZname", "IZcode", "Area"))
dtaIGMlt <- unique(left_join(dtaIGMlt, dta[c(3,37)], by = "IZcode"))
dtaScot <- select(dtaScot, -matches("population|count"))
dtaScotMlt <- melt(dtaScot, id.vars = "ReferenceArea")
dtaScotMlt$Area <- "Country"
dtaScotMlt$code <- "S92000003"
dtaScotMlt$Council_area <- NA
#final clean and merge
dtaCncMlt <- dtaCncMlt %>% rename(ReferenceArea = Council_area) %>%
  rename(code = LAcode) %>% mutate(Council_area = ReferenceArea)
dtaCncMlt <- dtaCncMlt[c(1,2,5,4,3,6)] 
dtaDZMlt <- dtaDZMlt %>% rename(ReferenceArea = Data_Zone)
dtaDZMlt <- dtaDZMlt[c(1,6,5,4,2,3)]
dtaIGMlt <- dtaIGMlt %>% rename(ReferenceArea = IZname) %>%
  rename(code = IZcode)
dtaIGMlt <- dtaIGMlt[c(1,2,5,4,3,6)]
dtaScotMlt <-dtaScotMlt[c(1,5,3,2,4,6)]
#Bind into one long dataset
allDta <- rbind(dtaScotMlt, dtaCncMlt, dtaIGMlt, dtaDZMlt)
rm(dtaScotMlt, dtaCncMlt, dtaIGMlt, dtaDZMlt, dtaScot, dtaCnc, dtaIG, dtaDZ)

allDta$value <- round(allDta$value, 3)
allDta$variable <- as.character(allDta$variable)
#Multiply percentages
allDta[which(allDta$variable== "IncRat" |allDta$variable == "Income_rate"|
               allDta$variable == "EmpRat"|allDta$variable == "Employment_rate"|
               allDta$variable == "Attendance"|allDta$variable == "NEET"|
               allDta$variable == "HESA"), 3] <- allDta[which(allDta$variable== "IncRat" |allDta$variable == "Income_rate"|
                         allDta$variable == "EmpRat"|allDta$variable == "Employment_rate"|
                         allDta$variable == "Attendance"|allDta$variable == "NEET"|
                         allDta$variable == "HESA"), 3]*100
#rename the variables
allDta[which(allDta$variable == "IncRat" | allDta$variable == "Income_rate"), 4] <- "Percentage of the population income deprived 2016"
allDta[which(allDta$variable %in% "EmpRat"| allDta$variable == "Employment_rate"), 4] <- "Percentage of the population employment deprived 2016"
allDta[which(allDta$variable == "EMERG"), 4] <- "Emergency stays in hospital, standardised ratio"
allDta[which(allDta$variable == "Attendance"), 4] <- "School pupil attendance percentage"
allDta[which(allDta$variable == "Attainment"), 4] <- "School pupil attainment score"
allDta[which(allDta$variable == "NEET"), 4] <- "Percentage 16-19 not in full time education, employment or training"
allDta[which(allDta$variable == "HESA"), 4] <- "Percentage 17-21 entering full time education"

#Add on the SIMD rankings - http://www.gov.scot/Resource/0050/00504759.xlsx
simdDta <- read_excel("Q:/Data/SIMD16/SIMD16.xlsx",
                      sheet = 2, col_names = TRUE)
simdDta <- simdDta[c(1,3,6)]
simdDta$Area <- "Data Zones"
simdDta$variable <- "SIMD 16 Ranking"
simdDta$code <- simdDta$Data_Zone
colnames(simdDta)[c(1,3)] <- c("ReferenceArea", "value")
simdDta <- simdDta[c(1,6,3,5,4,2)]
allDta <- rbind(allDta, simdDta)

#rename council column
colnames(allDta)[6] <- "Council"

#Save
write.csv(allDta, file = "Q:/Shiny LA Slides/SIMD16.csv")
saveRDS(allDta, file = "Q:/Shiny LA Slides/SIMD16.rds")

#Quick fix because I don't want to run the whole thing
#dta <- readRDS("Q:/Shiny LA Slides/SIMD16.rds")
#dta[dta$Area == "Data Zones 2011", 5] <- "Data Zones"
#dta[dta$Area == "Intermediate Zones 2011", 5] <- "Intermediate Zones"

#Quick fix because I don't want to run the whole thing
#dta <- readRDS("Q:/Shiny LA Slides/SIMD16.rds")
#dta$ReferenceArea <- as.character(dta$ReferenceArea)
#dta[dta$Council %in% "Na h-Eileanan an Iar", 6] <- "Comhairle nan Eilean Siar"
#dta[dta$ReferenceArea == "Na h-Eileanan an Iar", 1] <- "Comhairle nan Eilean Siar"
