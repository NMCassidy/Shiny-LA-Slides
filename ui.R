shinyUI(fluidPage(theme = shinytheme("readable"),
  titlePanel("Local Authority Slides"),
  sidebarPanel(
    selectInput("Area", "Select Area Type", c("Council Areas", "Intermediate Zones","Data Zones")),
    uiOutput("graphType"),
    selectInput("Cncl", "Select Council Area", sort(unique(emAdDta[emAdDta$Area == "Council Areas", 1]))),
    uiOutput("Ind"),
    textInput("Ttl", "Insert a Plot Title", value = ""), 
    downloadButton("dlAllData", "Download All Indicator Data"),
    downloadButton("dlPlot", "Download Plot"),
    width = 3),
    
  mainPanel(tabsetPanel(tabPanel("Plot",plotlyOutput("brpltRnd"), height = "1200px"),
              tabPanel("Data Explorer", DT::dataTableOutput("dataExp"))), width = 9)
  )
)