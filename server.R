shinyServer(function(input, output){
  
  data <- reactive({
    clnDta <- filter(emAdDta_mlt, Area == input$Area)
    
    if(input$Area == "Data Zone"){
      clnDta <- left_join(emAdDta_mlt, lablsDZ, by = c("Reference Area" = "DataZ")) %>%
        filter(., Council == input$Cncl)
    }else if(input$Area == "Intermediate Geography"){
      clnDta <- left_join(emAdDta_mlt, lablsIG, by = c("Reference Area" = "IntZ")) %>%
        filter(., Council == input$Cncl)
    }else{
      clnDta <- clnDta
    }
  })
  
  data2 <- reactive({
    dt <- data()
    dt <- filter(dt, variable == input$Ind)
  })
  scotVal <- reactive({
    dt <- filter(emAdDta_mlt, variable == input$Ind)
    SVal <- dt[dt$Area =="Scotland", 4]
  })
  
  output$barplot <- renderPlotly({
    dta <- data2()
    p <- ggplot(data = dta) +
      geom_bar(aes(x = reorder(`Reference Area`, value), y = value),stat = "identity")+
      geom_hline(yintercept = scotVal())+
      xlab("Area")+
      geom_text(aes(x =8, y = scotVal(), label = paste("Scotland", as.character(scotVal()))), nudge_y = (scotVal()/11))+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    
    pp <- ggplotly(p)
    pp
  })
  
#  output$barplot <- renderPlotly({
#    return(plotInput())
#  })
})