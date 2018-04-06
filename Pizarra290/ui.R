library(shinydashboard)
library(plotly)
options(scipen=999)
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
source("fun.R")

#*********************
## Load data ####
#*********************
load("./results/serie.290(features).RData")
load("./results/m_serie.290(features).RData")
load("./results/years.RData")
Ramos_grupos <- readRDS("./results/Ramos_grupos.rds")



#*****************************************
## 1. Header ####
#*****************************************
header <- dashboardHeader(
  title = h4("Pizarra F290"),
  dropdownMenuOutput("messageMenu")
)


#*****************************************
## 2. Sidebar ####
#*****************************************
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("home")),
    menuItem("Generador", tabName = "generador", icon = icon("wrench"), badgeLabel = "nuevo", 
             badgeColor = "green"),
    
    ## Menu generado en el server.R
    ## Se puede generar un menu que se active unicamente si se cumple una condicion deseada
    sidebarMenuOutput("menu")
  )
)

#*****************************************
## 3. Body ####
#*****************************************
body <- dashboardBody(
  tabItems(
    ## C1. First tab content ####
    #*****************************
    tabItem(tabName = "Home",
            fluidRow(
              tabBox(
                title = tagList(shiny::icon("gear"), "Parámetros para análisis"),
                selected = "Nivel del análisis", width=9,
                
                tabPanel("Nivel del análisis", "Seleccione...",
                         selectInput("grupo_ramos", "Nivel del análisis",
                                     choices = names(Ramos_grupos), selected = "Ramos_Objetivo"
                         )
                ),
                
                tabPanel("Horizonte", "Seleccione el horizonte de tiempo...",
                         radioButtons("horizonte", "Horizonte", 
                                      choices = list("mensual" = 1, "anual" = 2), selected = 1
                         )
                ),
                
                tabPanel("Periodo", "Seleccione el periodo a analizar...",
                         selectInput("periodo", "Periodo", choices = periodos(serie.290), 
                                     selected = periodos(serie.290)[length(periodos(serie.290))]
                         )
                )
                
              ) ## Final de tabBox
            ),
            
            fluidRow(
              valueBoxOutput("PrEmitida_Box")
            ),
            
            tabBox(
              title = tagList(shiny::icon("gear"), "Info status"),
              selected = "Data",
              
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "data_panel", 
              height = "400px", width=9,
              
              tabPanel("Data", 
                       "El sistema tiene precargada la siguiente información: \n"
                       #DT::dataTableOutput("Preload")
              ),
              
              tabPanel("Actualizar", 
                       "Presione para actualizar información"
                       #br(), br(),
                       #submitButton("  Actualizar", icon("refresh"))
              )
            )
          ),
    
    ## C2. Second tab content ####
    #*****************************
    tabItem(tabName = "generador",
            fluidRow(
              tabBox(
                title = tagList(shiny::icon("gear"), "Info 2"),
                selected = "Tab2",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "250px",
                tabPanel("Tab1", "First tab content"),
                tabPanel("Tab2", "Tab content 2")
              ),
              
              box(
                title = "Controls",
                width=3,
                collapsible = T, collapsed = F,
                uiOutput("moreControls")
              )
            ),
            
            fluidRow(
              tabBox(
                # Title can include an icon
                title = tagList(shiny::icon("gear"), "tabBox status"),
                tabPanel("Tab1",
                         "Currently selected tab from first box:",
                         verbatimTextOutput("tabset1Selected")
                ),
                tabPanel("Tab2", "Tab content 2")
              )
            )
    ),
    
    ## C3. 3th tab content ####
    #*****************************
    tabItem(tabName = "menuServer",
            h2("Generado desde el server")
    )
    )
)


#*****************************************
## 4. Compilation ####
#*****************************************
dashboardPage(header, sidebar, body)


