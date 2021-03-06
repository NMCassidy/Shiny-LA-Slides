shinyServer(function(input, output, session){
  
# Reactive UI for selecting indicators
  output$Ind <- renderUI({
    fltDta <- data()
    selectInput("Ind", "Select Indicator to Graph", sort(unique(fltDta$variable)),
                selected = input$Ind)
  })
  output$AreaType <- renderUI({
      if(input$SIMD != "Incomes Data"){
        selectInput("Area", "Select Area Type", c("Council Areas", "Intermediate Zones","Data Zones"))
      } else{
        selectInput("Area", "Select Area Type", c("Data Zones"))
      }
    })
# Ability to switch between all data or top and bottom ten values  
  output$graphType <- renderUI({
    if(input$Area != "Council Areas"){
      selectInput("graphType", "Select Data", c("All", "Top/Bottom Ten"))
    }
  })
# Reactive funtions to filter data based on area type and all data or top and bottom ten  
  data <- reactive({
    if(input$SIMD == "SIMD 2012"){
    clnDta <- filter(emAdDta, Area == input$Area)
    
    if(input$Area == "Data Zones"){
      clnDta <- left_join(clnDta, lablsDZ, by = c("ReferenceArea" = "DataZ")) %>%
        filter(., Council == input$Cncl)
    }else if(input$Area == "Intermediate Zones"){
      clnDta <- left_join(clnDta, lablsIG, by = c("code" = "code")) %>%
        filter(., Council == input$Cncl)
    }else{
      clnDta <- clnDta
    }
    }else if(input$SIMD == "SIMD 2016"){
      clnDta <- filter(dat16, Area == input$Area)
      
      if(input$Area == "Data Zones"){
        clnDta <- filter(clnDta, Council == input$Cncl)
      }else if(input$Area == "Intermediate Zones"){
        clnDta <- filter(clnDta, Council == input$Cncl)
      }else{
        clnDta <- clnDta
      }
    } else if(input$SIMD == "Incomes Data"){
      clnDta <- datinc
    }
  })
  
  data2 <- reactive({
      if(input$Area =="Council Areas"){
        dt <- data()
        dt <- filter(dt, variable == input$Ind) 
      }else if(input$graphType == "Top/Bottom Ten"){
        dt <- data()
        dt <- filter(dt, variable == input$Ind)
        dt <- dt[order(dt$value),]
        dt <- HiLoTen(dt)
      } else{
        dt <- data()
        dt <- filter(dt, variable == input$Ind)
      }
  })
# Filter all indicator data for download  
   indicatorDta <- reactive({
     if(input$SIMD == "SIMD 2012"){
     dt <- filter(emAdDta, variable == input$Ind) 
     } else if(input$SIMD == "SIMD 2016"){
       dt <- filter(dat16, variable == input$Ind)
     }
     
      })
# Get value for Scotland and Council in order to make horizontal line  
  scotVal <- reactive({
    if(input$SIMD == "SIMD 2012"){
      dt <- filter(emAdDta, variable == input$Ind)
    SVal <- dt[dt$Area =="Country", 3]
    }else if(input$SIMD == "SIMD 2016"){
      dt <- filter(dat16, variable == input$Ind)
      SVal <- dt[dt$Area =="Country", 3]
    }
    })
  cnclVal <- reactive({
    if(input$SIMD == "SIMD 2012"){
    dt <- filter(emAdDta, Area == "Council Areas" & variable == input$Ind)
      CVal <- dt[dt$`ReferenceArea` == input$Cncl, 3]
    } else if(input$SIMD == "SIMD 2016"){
      dt <- filter(dat16, Area == "Council Areas" & variable == input$Ind)
      CVal <- dt[dt$`ReferenceArea` == input$Cncl, 3]
    }
      })
#Produce barplot  
  brplt <- function(){
    dta <- data2()
    if(input$Area != "Council Areas"){
      scotVal <- scotVal()
      if(length(scotVal) != 0){
        p <- ggplot(data = dta) +
          geom_bar(aes(x = reorder(ReferenceArea, value), y = value, text = paste("Area:", `ReferenceArea`)), fill = "black",stat = "identity")+
          geom_hline(yintercept = cnclVal(), colour = "green4")+
          geom_hline(yintercept = scotVal + 0.00000001, colour = "red")+
          xlab("")+
          ylab("")+
          ggtitle(input$Ttl)+
        #  scale_y_continuous(breaks = sort(c(seq(round(min(dta$value)), max(dta$value), length.out = 5), scotVal(), cnclVal())), labels = )+
          scale_x_discrete(label = abbreviate)+
      #    geom_text(aes(x =length(`ReferenceArea`)/3.5, y = scotVal(), label = paste("Scotland:", as.character(scotVal()))),colour = "red", nudge_y = (scotVal()/11))+
      #    geom_text(aes(x =length(`ReferenceArea`)/7, y = cnclVal(), label = paste("Council:", as.character(cnclVal()))),colour = "palegreen4", check_overlap = TRUE,nudge_y = -(cnclVal()/10))+
          theme_hc()+
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.line.y = element_line(colour = "black", "size = 0.6"),
                axis.line.x = element_line(colour = "black", "size = 0.6"))
        a <- list()
        a[[1]] <- list(
          x = nrow(dta)/13,
          y = scotVal(),
          text = paste("Scotland:", scotVal()),
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 6,
          ax = 40,
          ay = -40,
          colour = "red"
        )
        a[[2]] <- list(
          x = nrow(dta)/4,
          y = cnclVal(),
          text = paste0(as.character(input$Cncl),": ", cnclVal()),
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 6,
          ax = 40,
          ay = -40,
          colour = "green"
        )
      } else{
        p <- ggplot(data = dta) +
          geom_bar(aes(x = reorder(ReferenceArea, value), y = value, text = paste("Area:", `ReferenceArea`)), fill = "black",stat = "identity")+
          xlab("")+
          ylab("")+
          ggtitle(input$Ttl)+
          scale_x_discrete(label = abbreviate)+
          theme_hc()+
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.line.y = element_line(colour = "black", "size = 0.6"),
                axis.line.x = element_line(colour = "black", "size = 0.6"))
        a = NULL
      } 
    } else{
      rarara <- dta[order(dta$value), ]
      nmbr <- match(input$Cncl, rarara$ReferenceArea) 
      clrs <- c(rep("black", nmbr-1), "blue", rep("black", (32 - nmbr)))
      p <- ggplot(data = dta) +
        geom_bar(aes(x = reorder(`ReferenceArea`, value), y = value, text = paste("Council:", `ReferenceArea`)),stat = "identity", fill = clrs)+
        geom_hline(yintercept = scotVal(), colour = "red", aes(text = paste("Scotland Average:", scotVal())))+
        xlab("Council")+
        ylab("")+
        ggtitle(input$Ttl)+
      #  geom_text(aes(x =length(`ReferenceArea`)/4, y = scotVal(), label = paste("Scotland:", as.character(scotVal()))), colour = "red", nudge_y = (scotVal()/11))+
        theme_hc()+
        scale_fill_manual(values = clrs)+
        guides(fill = FALSE)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line.y = element_line(colour = "black", "size = 0.6"),
              axis.line.x = element_line(colour = "black", "size = 0.6"))
      a <- list()
      a[[1]] <- list(
        x = nrow(dta)/10,
        y = scotVal(),
        text = paste("Scotland:", scotVal()),
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 6,
        ax = 40,
        ay = -40,
        colour = "red"
      )
    }
      pp <- ggplotly(p, tooltip = c("text", "y"), height = 750, width = 920) %>% layout(autosize = FALSE,
                                                             annotations = a)
      return(pp)
  }
  
  output$brpltRnd <- renderPlotly({
    brplt()
  })
# Create data explorer 
  output$dataExp <- DT::renderDataTable({
    dta <- data2()[c("ReferenceArea", "variable", "value")]
    datatable(dta, extensions = c("FixedColumns", "ColReorder", "KeyTable"),
    options = list(colReorder = TRUE, fixedColumns = list(leftColumns = 1), keys = TRUE),
    colnames = c("Area", "Indicator", "Value")) %>%
      formatStyle("value", fontWeight = "bold")
  })
# Download dataset
  output$dlAllData <- downloadHandler(
    filename = function() {paste(as.character(input$Ind), ".csv", sep = "")},
    content = function(file) {
      write.csv(indicatorDta(), file)
    }
  )
# Download plot    
  output$dlPlot <- downloadHandler(
    filename = function() {paste(as.character(input$Ind), ".png", sep ="")},
    content = function(file){
      plotly_IMAGE(x = brplt(), out_file = file, format = "png")
    }
  )
})