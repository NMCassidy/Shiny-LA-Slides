shinyServer(function(input, output){
  
  data <- reactive({
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
  })
  
  data2 <- reactive({
    if(input$graphType == "All"){
    dt <- data()
    dt <- filter(dt, variable == input$Ind)
    }else if(input$Area != "Council Areas"){
      dt <- data()
      dt <- filter(dt, variable == input$Ind)
      dt <- dt[order(dt$value),]
      dt <- HiLoTen(dt)
    }
  })
  
  output$Ind <- renderUI({
    fltDta <- data()
    selectInput("Ind", "Select Indicator to Graph", sort(unique(fltDta$variable)))
  })
  
  scotVal <- reactive({
    dt <- filter(emAdDta, variable == input$Ind)
    SVal <- dt[dt$Area =="Country", 3]
  })
  cnclVal <- reactive({
    dt <- filter(emAdDta, Area == "Council Areas" & variable == input$Ind)
      CVal <- dt[dt$`ReferenceArea` == input$Cncl, 3]
      })
  
  output$barplot <- renderPlotly({
    dta <- data2()
    if(input$Area != "Council Areas"){
      scotVal <- scotVal()
      if(length(scotVal) != 0){
        p <- ggplot(data = dta) +
          geom_bar(aes(x = reorder(ReferenceArea, value), y = value, text = paste("Area:", `ReferenceArea`)), fill = "black",stat = "identity")+
          geom_hline(yintercept = cnclVal(), colour = "green4")+
          geom_hline(yintercept = scotVal(), colour = "red")+
          xlab("")+
          ylab("")+
          ggtitle(input$Ttl)+
          scale_x_discrete(label = abbreviate)+
          geom_text(aes(x =length(`ReferenceArea`)/4, y = scotVal(), label = paste("Scotland:", as.character(scotVal()))),colour = "red", nudge_y = (scotVal()/11))+
          geom_text(aes(x =length(`ReferenceArea`)/4.5, y = cnclVal(), label = paste("Council:", as.character(cnclVal()))),colour = "green4", nudge_y = -(cnclVal()/10))+
          theme_bw()+
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
      } else{
        p <- ggplot(data = dta) +
          geom_bar(aes(x = reorder(ReferenceArea, value), y = value, text = paste("Area:", `ReferenceArea`)), fill = "black",stat = "identity")+
          xlab("")+
          ylab("")+
          ggtitle(input$Ttl)+
          scale_x_discrete(label = abbreviate)+
          theme_bw()+
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
      } 
    } else{
      rarara <- dta[order(dta$value), ]
      nmbr <- match(input$Cncl, rarara$ReferenceArea) 
      clrs <- c(rep("black", nmbr-1), "blue", rep("black", (32 - nmbr)))
      p <- ggplot(data = dta) +
        geom_bar(aes(x = reorder(`ReferenceArea`, value), y = value, text = paste("Council:", `ReferenceArea`)),stat = "identity", fill = clrs)+
        geom_hline(yintercept = scotVal(), colour = "red")+
        xlab("Council")+
        ylab("")+
        ggtitle(input$Ttl)+
        geom_text(aes(x =length(`ReferenceArea`)/4, y = scotVal(), label = paste("Scotland:", as.character(scotVal()))), colour = "red", nudge_y = (scotVal()/11))+
        theme_bw()+
        scale_fill_manual(values = clrs)+
        guides(fill = FALSE)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
    pp <- ggplotly(p, tooltip = c("text", "y"))
    pp
  })
  
#  output$barplot <- renderPlotly({
#    return(plotInput())
#  })
})