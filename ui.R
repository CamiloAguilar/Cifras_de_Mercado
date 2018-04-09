library(shinydashboard)
options(scipen=999)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
source("fun.R")

#*********************
## Load data ####
#*********************


#load("./results/serie.290(features).RData")
#load("./results/m_serie.290(features).RData")
years <- readRDS("./results/years.rds")
serie.290 <- readRDS("./results/serie.290(features).rds")
m_serie.290 <- readRDS("./results/m_serie.290(features).rds")
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
                         selectInput("horizonte", "Horizonte", 
                                      choices = list("mensual", "anual"), selected = "mensual"
                         )
                ),
                
                tabPanel("Periodo", "Seleccione el periodo a analizar...",
                         selectInput("periodo", "Periodo", choices = periodos(), 
                                     selected = periodos()[length(periodos())]
                         )
                )
                
              ) ## Final de tabBox
            ), ## Final fluidRow 1
            
            fluidRow(
              valueBoxOutput("PrEmitida_Box"),
              valueBoxOutput("PrDevengada_Box"),
              valueBoxOutput("ResTecnicio_Box")
            ), ## Final fluidRow 2
            
            tabBox(
              title = "Participación del mercado",
              selected = "Primas Emitidas",
              
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "part_mercado", 
              height = "400px", width=12,
              
              tabPanel("Primas Emitidas", 
                       plotlyOutput("pie_Emitidas")
              ),
              
              tabPanel("Primas Devengadas", 
                       plotlyOutput("pie_Devengadas")
              ),
              
              tabPanel("Resultado Técnico",
                       plotlyOutput("pie_ResTec")
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


