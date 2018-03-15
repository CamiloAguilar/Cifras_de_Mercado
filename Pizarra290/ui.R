library(shinydashboard)
library(plotly)
source("fun.R")

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
            tabBox(
              title = tagList(shiny::icon("gear"), "Info status"),
              selected = "Buscar información nueva",
              
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabset1", height = "400px", width=9,
              
              tabPanel("Data", 
                       "El sistema tiene precargada la siguiente información: \n",
                       uiOutput("Preload")
              ),
              
              tabPanel("Actualizar", 
                       "Presione para actualizar información",
                       br(), br(),
                       submitButton("  Actualizar", icon("refresh"))
              )
            )
          ),
    
    ## C2. Second tab content ####
    #*****************************
    tabItem(tabName = "generador",
            fluidRow(
              tabBox(
                title = tagList(shiny::icon("gear"), "Info 2"),
                selected = "Buscar información nueva",
                
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "400px", width=9,
                
                tabPanel("Información precargada", 
                         "El sistema tiene precargada la siguiente información: "
                         #DT::dataTableOutput("Preload")
                ),
                
                tabPanel("Buscar información nueva", 
                         "Presione para actualizar información"
                         #plotOutput("plot1")
                )
              ),
              
              box(
                title = "Controls",
                width=3,
                collapsible = T, collapsed = F,
                uiOutput("moreControls")
              )
            )
    ),
    
    ## C3. Second tab content ####
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


