#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(highcharter)

header <- 
    dashboardPage(
        dashboardHeader(title = "Recuperaciones",
                        disable = FALSE,
                        titleWidth = 200),
        dashboardSidebar(
            width = 200,
            sidebarMenu(

                menuItem('Main Dashboard',tabName = 'dashboard',icon = icon('dashboard'),
                         badgeColor = "green"),
                
                menuItem("Gestores",tabName = 'portfolio_intel_1', icon = icon('globe'))#,
                
                #menuItem("Abogados", tabName = 'portfolio_intel_2', icon = icon('barcode'))
            )
            
            
        ),
        dashboardBody(
            
            tabItems(
                
                tabItem(tabName = 'dashboard',
                        h1(paste("Recuperacion")), ## Colocar el monto recuperado
                        fluidRow(
                            valueBoxOutput("BoxGeneral", width = 3),
                            valueBoxOutput("BoxVencido", width = 3),
                            valueBoxOutput("BoxJudicial", width = 3),
                            valueBoxOutput("BoxCastigado",width = 3)
                        ),
                        
                        ## Grafico de lineas (Time series plot)
                        
                        h2(paste("Evolucion de recuperaciones")),
                        fluidRow(
                            column(width = 6, h3("Recuperacion por cartera"), align = 'center'), highchartOutput('LineaGeneral')
                            
                        ),
                        
                        h2(paste("Recuperacion por coordinador")),
                        p("Las tasa de recuperacion se halla con el pago capital. Los valores pueden ordenarse a gusto."),
                        fluidRow(dataTableOutput('Coordinadores')), #cambiar el nombre
                        
                        
                        h2(paste("Top 20 clientes recuperados")),
                        fluidRow(dataTableOutput('Principales_clientes_recuperados')),
                        
                        #div(id = 'message_to_show_more',
                        #    tags$hr(),
                        #    tags$h3("Clike on the 'Show more details' button to display more information"),
                        #    actionButton("btn_show_more",
                        #                 paste('Show more details'),
                        #                 icon = icon('chevron-circle-down')))
                        
                ),
                
                tabItem(tabName = 'portfolio_intel_1',
                        h1(paste("Gestores")),
                        h2(paste("Recuperaciones Obtenidas")),
                        fluidRow(
                            column(width = 12, h3("Recuperaciones"), align = 'center', highchartOutput('RecuperacionesGestores'))
                        ),
                        
                        h2(paste("Detalle de pagos")),
                        p("Muestra la recuperacion general, tasa y clientes recuperados de cada uno de los gestores"),
                        fluidRow(dataTableOutput("Recuperaciones_detalle_gestores")),
                        
                        
                        h2(paste("Seguimiento de actividades")),
                        fluidRow(
                            column(width = 12, h3("Actividades"), align = "center", highchartOutput("ActividadesGestores"))
                        ),
                        
                        
                        
                        h2(paste("Detalle de clientes / Actividades")),
                        p("Base de datos que contiene informacion de los clientes de acuerdo a las actividades realizadas"),
                        fluidRow(dataTableOutput("Clientes_gestores"))
                        
                )
                
                
                
                
                
            )#tabItems
        )
    )







