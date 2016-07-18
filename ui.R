

shinyUI(fluidPage(
  titlePanel("Local Authority Slides"),
  sidebarPanel(
    selectInput("Area", "Select Area Type", c("Council", "Intermediate Geography","Data Zone")),
    selectInput("Cncl", "Select Council Area", unique(emAdDta_mlt[emAdDta_mlt$Area == "Council",2])),
    selectInput("Ind", "Select Indicator to Graph", unique(emAdDta_mlt$variable))
  ),
  mainPanel(plotlyOutput("barplot"))
))