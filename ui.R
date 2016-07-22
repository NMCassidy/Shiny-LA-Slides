

shinyUI(fluidPage(
  titlePanel("Local Authority Slides"),
  sidebarPanel(
    selectInput("Area", "Select Area Type", c("Council Areas", "Intermediate Zones","Data Zones")),
    selectInput("Cncl", "Select Council Area", sort(unique(emAdDta[emAdDta$Area == "Council Areas", 1]))),
    uiOutput("Ind"),
    textInput("Ttl", "Insert a Plot Title", value = ""),
    conditionalPanel(condition = "input.Area != 'Council'", 
                     selectInput("graphType", "Select Data", c("All", "Top/Bottom Ten")))
  ),
  mainPanel(plotlyOutput("barplot"), height = "400px")
))