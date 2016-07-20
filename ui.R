

shinyUI(fluidPage(
  titlePanel("Local Authority Slides"),
  sidebarPanel(
    selectInput("Area", "Select Area Type", c("Council", "Intermediate Geography","Data Zone")),
    selectInput("Cncl", "Select Council Area", unique(emAdDta[emAdDta$Area == "Council", 1])),
    selectInput("Ind", "Select Indicator to Graph", unique(emAdDta$variable)),
    conditionalPanel(condition = "input.Area != 'Council'", 
                     selectInput("graphType", "Select Data", c("All", "Top/Bottom Ten")))
  ),
  mainPanel(plotlyOutput("barplot"), height = 20)
))