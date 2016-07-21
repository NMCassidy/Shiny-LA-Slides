shinyServer(function(input, output){
  
  data <- reactive({
    clnDta <- filter(emAdDta, Area == input$Area)
    
    if(input$Area == "Data Zone"){
      clnDta <- left_join(emAdDta, lablsDZ, by = c("ReferenceArea" = "DataZ")) %>%
        filter(., Council == input$Cncl)
    }else if(input$Area == "Intermediate Geography"){
      clnDta <- left_join(emAdDta, lablsIG, by = c("code" = "code")) %>%
        filter(., Council == input$Cncl)
    }else{
      clnDta <- clnDta
    }
  })
  
  data2 <- reactive({
    if(input$graphType == "All"){
    dt <- data()
    dt <- filter(dt, variable == input$Ind)
    }else if(input$Area != "Council"){
      dt <- data()
      dt <- filter(dt, variable == input$Ind)
      dt <- dt[order(dt$value),]
      dt <- HiLoTen(dt)
    }
  })
  
  scotVal <- reactive({
    dt <- filter(emAdDta, variable == input$Ind)
    SVal <- dt[dt$Area =="Scotland", 3]
  })
  
  output$barplot <- renderPlotly({
    dta <- data2()
    if(input$Area != "Council"){
      p <- ggplot(data = dta) +
      geom_bar(aes(x = reorder(ReferenceArea, value), y = value), colour = "black",stat = "identity")+
      geom_hline(yintercept = scotVal(), colour = "red")+
      xlab("")+
      ylab("")+
      geom_text(aes(x =length(`ReferenceArea`)/4, y = scotVal(), label = paste("Scotland", as.character(scotVal()))),colour = "red", nudge_y = (scotVal()/11))+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    } else{
      rarara <- dta[order(dta$value), ]
      nmbr <- match(input$Cncl, rarara$ReferenceArea) 
      clrs <- c(rep("black", nmbr-1), "blue", rep("black", (32 - nmbr)))
      p <- ggplot(data = dta) +
        geom_bar(aes(x = reorder(`ReferenceArea`, value), y = value, text = paste("Council:", `ReferenceArea`)),stat = "identity", fill = clrs)+
        geom_hline(yintercept = scotVal(), colour = "red")+
        xlab("Council")+
        ylab("")+
        geom_text(aes(x =length(`ReferenceArea`)/4, y = scotVal(), label = paste("Scotland", as.character(scotVal()))), colour = "red", nudge_y = (scotVal()/11))+
        theme_bw()+
        scale_fill_manual(values = clrs)+
        guides(fill = FALSE)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    }
    pp <- ggplotly(p)
    pp
  })
  
#  output$barplot <- renderPlotly({
#    return(plotInput())
#  })
})