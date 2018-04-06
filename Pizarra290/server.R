



#*********************
## Shiny app ####
#*********************
shinyServer ( function ( input , output ) {
  
  # Render box - Primas emitidas
  output$PrEmitida_Box <- renderInfoBox({
    features <- c(03030, 03999, 05005, 14999, 18999, 12998)
    gr_ramo <- input$grupo_ramos
    meses_rank <- input$periodo
    serie <- ifelse(input$horizonte=="mensual", m_serie.290, serie.290)
    ranking <- ranking.290(serie.290 = serie, gr_ramo = gr_ramo, features = features, periods = meses_rank)
    s <- sum(ranking[[1]][[7]])
    
    infoBox(
      "Progress", paste0(s + input$count, "MM"), icon = icon("bar-chart"),
      color = "green"
    )
  })
  
  
  
  output$tabset1Selected <- renderText({
    input$tabset1
  })
  
  output$moreControls <- renderUI({
    if(input$tabset1=="Tab1"){
      sliderInput("slider1", input$tabset1 , 1, 10000, 5)
    } else{
      sliderInput("slider2", input$tabset1, 1, 10, 1)
    }
  })
  
  
  output$Preload = DT::renderDataTable({
    info.loaded()
  })
  
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider1)]
    hist(data)
  })
  
  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider2)]
    hist(data)
  })
  
  output$plotly1 <-renderPlotly({
    p<-ggplot(data = Evol, aes(x = Producto, y = Total, fill = TipoRes)) +
      geom_bar(stat="identity", position=position_dodge()) 
    ggplot(p)
  })
  
  #*****************************
  ### Mensajes en el header ####
  #*****************************
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    messageData<-data.frame(from=c("camilo","Diana"), message=c("soy yo", "es ella"))
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  #*****************************
  ### Sidebar Menu ####
  #*****************************
  ## Notese que logramos incluir un slider dentro de la lista desplegable
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", tabName = "menuServer", icon = icon("calendar"), startExpanded = FALSE,
               menuSubItem("subItem", tabName = "subItem"),
               sliderInput("slider", "Number of observations:", 1, 100, 50),
               radioButtons("TipoInf", label = h5("Tipo de Informe: "), choices = c("Total", "Por producto"),
                            selected = "Total", width = validateCssUnit("60%")),
               conditionalPanel(
                 condition = "input.TipoInf == 'Por producto'",
                 selectInput("Products", label = h5("Seleccione productos:"), multiple = T, 
                             choices = c("opc1","opc2"), selected = "opc1", width = validateCssUnit("85%"))
               ),
               menuSubItem("subItem2", tabName = "subItem2"),
               menuSubItem("subItem3", tabName = "subItem3"),
               menuSubItem("subItem4", tabName = "subItem4"))
    )
  })
})