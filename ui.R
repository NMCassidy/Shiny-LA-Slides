

shinyUI(fluidPage(
  titlePanel("Local Authority Slides"),
  mainPanel(plotlyOutput("barplot"), height = "1200px", width = "100%"),
  hr(),
  fluidRow(
    column(4,selectInput("Area", "Select Area Type", c("Council Areas", "Intermediate Zones","Data Zones"))),
    column(4,selectInput("Cncl", "Select Council Area", sort(unique(emAdDta[emAdDta$Area == "Council Areas", 1]))),
    uiOutput("Ind")),
    column(4,textInput("Ttl", "Insert a Plot Title", value = ""),
    conditionalPanel(condition = "input.Area != 'Council'", 
                     selectInput("graphType", "Select Data", c("All", "Top/Bottom Ten")))))
  )
)