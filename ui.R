shinyUI(navbarPage("Local Authority Slides", theme = "bootstrap.css",
  
    tabPanel("Plot",
    sidebarLayout(
        sidebarPanel(
    selectInput("SIMD", "Select SIMD Data to Examine", c("SIMD 2012", "SIMD 2016", "Incomes Data")),
    uiOutput("AreaType"),
    uiOutput("graphType"),
    selectInput("Cncl", "Select Council Area", sort(unique(emAdDta[emAdDta$Area == "Council Areas", 1]))),
    uiOutput("Ind"),
    textInput("Ttl", "Insert a Plot Title", value = ""), 
    downloadButton("dlAllData", "Download All Indicator Data"),
    br(),
    br(),
    downloadButton("dlPlot", "Download Plot"), width = 3),
  mainPanel(plotlyOutput("brpltRnd"),width = 9)
  )),
  
  tabPanel("Data Explorer",
  mainPanel(DT::dataTableOutput("dataExp"), width = 12)
    )
  )
)