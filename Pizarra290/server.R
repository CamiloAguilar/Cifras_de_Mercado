options(scipen=999)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)


shinyServer ( function ( input , output ) {
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$moreControls <- renderUI({
    if(input$tabset1=="Información precargada"){
      sliderInput("slider1", input$tabset1 , 1, 10, 5)
    } else{
      sliderInput("slider2", input$tabset1, 1, 1000, 1)
    }
  })
  
  
  output$Preload <- renderUI({
    df<-as.data.frame(info.loaded())
    if(is.null(df)){
      "No hay información disponible aún."
    } else{
      DT::renderDataTable(df)
    }
  })
  
  #output$Preload <- DT::renderDataTable({
  #  df<-info.loaded()
  #  })
  
  
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