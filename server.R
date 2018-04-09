

#*********************
## Shiny app ####
#*********************
shinyServer ( function ( input , output ) {
  
  #**************************************
  # Render box - Primas emitidas ####
  #**************************************
  output$PrEmitida_Box <- renderInfoBox({
    features <- c(03030, 03999, 05005, 14999, 18999, 12998)
    gr_ramo <- input$grupo_ramos
    meses_rank <- input$periodo
    
    # Selección de información a usar
    if(input$horizonte=="mensual"){
      serie <- readRDS("./results/m_serie.290(features).rds")
      } else{
            serie <- readRDS("./results/serie.290(features).rds")
            }
    print(names(serie))
    
    # Genera información
    ranking <- ranking.290(serie = serie, gr_ramo = gr_ramo, features = features, periods = meses_rank)
    s <- sum(ranking[[1]][[7]])
    
    # Genera infobox
    infoBox(
      "Primas Emitidas", paste0(round(s/1000000, 0), " MM"), subtitle=paste0(input$periodo, " ", input$horizonte),
      icon = icon("industry"), color = "green"
    )
  })
  
  #**************************************
  # Render box - Primas devengadas ####
  #**************************************
  output$PrDevengada_Box <- renderInfoBox({
    features <- c(03030, 03999, 05005, 14999, 18999, 12998)
    gr_ramo <- input$grupo_ramos
    meses_rank <- input$periodo
    
    # Selección de información a usar
    if(input$horizonte=="mensual"){
      serie <- readRDS("./results/m_serie.290(features).rds")
    } else{
      serie <- readRDS("./results/serie.290(features).rds")
    }
    print(names(serie))
    
    # Genera información
    ranking <- ranking.290(serie = serie, gr_ramo = gr_ramo, features = features, periods = meses_rank)
    s <- sum(ranking[[1]][[3]])
    
    # Genera infobox
    infoBox(
      "Primas Devengadas", paste0(round(s/1000000, 0), " MM"), subtitle=paste0(input$periodo, " ", input$horizonte), 
      icon = icon("sellsy"), color = "orange"
    )
  })
  
  #**************************************
  # Render box - resultado Técnico ####
  #**************************************
  output$ResTecnicio_Box <- renderInfoBox({
    features <- c(03030, 03999, 05005, 14999, 18999, 12998)
    gr_ramo <- input$grupo_ramos
    meses_rank <- input$periodo
    
    # Selección de información a usar
    if(input$horizonte=="mensual"){
      serie <- readRDS("./results/m_serie.290(features).rds")
    } else{
      serie <- readRDS("./results/serie.290(features).rds")
    }
    print(names(serie))
    
    # Genera información
    ranking <- ranking.290(serie = serie, gr_ramo = gr_ramo, features = features, periods = meses_rank)
    s <- sum(ranking[[1]][[4]])
    
    # Genera infobox
    infoBox(
      "Resultado Técnico", paste0(round(s/1000000, 0), " MM"), subtitle=paste0(input$periodo, " ", input$horizonte),
      icon = icon("balance-scale"), color = "blue"
    )
  })
  
  #*************************************
  # Gráfico pastel - Pr Emitidas ####
  #*************************************
  output$pie_Emitidas <- renderPlotly({
    features <- c(03030, 03999, 05005, 14999, 18999, 12998)
    gr_ramo <- input$grupo_ramos
    meses_rank <- input$periodo
    
    # Selección de información a usar
    if(input$horizonte=="mensual"){
      serie <- readRDS("./results/m_serie.290(features).rds")
    } else{
      serie <- readRDS("./results/serie.290(features).rds")
    }
    
    # Genera información
    ranking <- ranking.290(serie = serie, gr_ramo = gr_ramo, features = features, periods = meses_rank)
    
    # Genera gráfico
    p <- ranking[[1]] %>% 
         arrange(desc(`Prima Emitida Directa`)) %>%
         mutate(Primas=round(`Prima Emitida Directa`/1000000,0)) %>%
         plot_ly(labels = ~Compania, values = ~Primas) %>%
         add_pie(hole = 0.6) %>%
         layout(title = "Primas Emitidas \n Participación por compañías",  showlegend = F,
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    #chart_link = api_create(p, filename="pie-donut")
    #chart_link    
    p
  })
  
  #*************************************
  # Gráfico pastel - Pr Devengadas ####
  #*************************************
  output$pie_Devengadas <- renderPlotly({
    features <- c(03030, 03999, 05005, 14999, 18999, 12998)
    gr_ramo <- input$grupo_ramos
    meses_rank <- input$periodo
    
    # Selección de información a usar
    if(input$horizonte=="mensual"){
      serie <- readRDS("./results/m_serie.290(features).rds")
    } else{
      serie <- readRDS("./results/serie.290(features).rds")
    }
    
    # Genera información
    ranking <- ranking.290(serie = serie, gr_ramo = gr_ramo, features = features, periods = meses_rank)
    
    # Genera gráfico
    p <- ranking[[1]] %>% 
      arrange(desc(`PRIMAS DEVENGADAS`)) %>%
      mutate(Primas=round(`PRIMAS DEVENGADAS`/1000000,0)) %>%
      plot_ly(labels = ~Compania, values = ~Primas) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Primas Devengadas \n Participación por compañías",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    #chart_link = api_create(p, filename="pie-donut")
    #chart_link    
    p
    
  })
  
  #*************************************
  # Gráfico pastel - Res. Técnico ####
  #*************************************
  output$pie_ResTec <- renderPlotly({
    features <- c(03030, 03999, 05005, 14999, 18999, 12998)
    gr_ramo <- input$grupo_ramos
    meses_rank <- input$periodo
    
    # Selección de información a usar
    if(input$horizonte=="mensual"){
      serie <- readRDS("./results/m_serie.290(features).rds")
    } else{
      serie <- readRDS("./results/serie.290(features).rds")
    }
    
    # Genera información
    ranking <- ranking.290(serie = serie, gr_ramo = gr_ramo, features = features, periods = meses_rank)
    
    # Genera gráfico
    p <- ranking[[1]] %>% 
      arrange(desc(`RESULTADO TECNICO`)) %>%
      mutate(ResTec=round(`RESULTADO TECNICO`/1000000,0)) %>%
      plot_ly(labels = ~Compania, values = ~ResTec) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Resultado Técnico \n Participación por compañías",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    #chart_link = api_create(p, filename="pie-donut")
    #chart_link    
    p
    
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