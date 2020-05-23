#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# 1. create the app and modify it
# 2. create a copy of the script and modified to be able to upload it to personal projects


server <- function(input, output){
    
    output$BoxGeneral <- renderValueBox({
        valueBox(subtitle = 'Recuperaciones',
                 paste('S/',TotalCollection, "MM"),
                 #icon = icon('export',lib = 'glyphicon'),
                 color = 'green'
        )
    })
    
    output$BoxVencido <- renderValueBox({
        valueBox(subtitle = 'Recuperacion Vencido',
                 paste('S/',After_due_collections,"MM"),
                 color = 'blue')
    })
    
    output$BoxJudicial <- renderValueBox({
        valueBox(subtitle = 'Recuperacion Judicial',
                 paste('S/',Sued_collections,'MM'),
                 color = 'purple')
    })
    
    output$BoxCastigado <- renderValueBox({
        valueBox(subtitle = 'Recuperacion Castigado',
                 paste('S/',Castigated_collections,'MM'),
                 color = 'orange')
    })
    
    output$LineaGeneral <- renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series(list(name = 'Total', data = round(minicollection$RECUPERACION_ACUMULADA,0),color = 'green', marker = list(symbol = 'circle')),
                      list(name = 'Vencido', data = round(minicollection_after_due$RECUPERACION_ACUMULADA,0), color = 'blue',marker = list(symbol = 'triangle')),
                      list(name = 'Judicial', data = round(minicollection_sued$RECUPERACION_ACUMULADA,0), color = 'purple',marker = list(symbol = 'square')),
                      list(name = 'Castigado', data = round(minicollection_castigated$RECUPERACION_ACUMULADA,0), color = 'orange',marker = list(symbol = 'circle'))) %>%
            hc_xAxis(categories = minicollection$DIA_PAGO) %>%
            hc_yAxis(title = list(text = 'S/ millones'))
        
    })
    
    
    
    output$Coordinadores <- renderDataTable({
        datatable(collection_by_manager,
                  rownames = F,
                  extensions = 'Buttons',
                  options = list(dom = 'Bt',
                                 buttons = c('copy','csv','excel','pdf','print'),
                                 scrollX = TRUE),
                  colnames = c('Gestionador','Recuperacion de capital','Cartera Total','Tasa','Clientes con pagos','Cartera de clientes','Tasa de clientes')) 
        
    })
    
    
    output$Principales_clientes_recuperados <- renderDataTable({
        datatable(top_20_clients,
                  rownames = F,
                  extensions = c('Buttons','Scroller'),
                  options = list(dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','csv','excel','pdf','print'),
                                 pagelength = 5, lengthMenu = list(c(5,10,-1),c('5','10','All'))),
                  
                  colnames = c('Cliente','Capital','Intereses','Recuperacion total',
                               'Tipo de cliente'))
    })
    
    
    output$RecuperacionesGestores <- renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formatAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'column') %>%
            hc_xAxis(categories = collectors_collection_as_bar$GESTOR, labels = list(rotation = -90, step = 1)) %>%
            hc_add_series(data = collectors_collection_as_bar$Pagos_obtenidos, showInLegend = F)
        
    })
    
    
    
    output$Recuperaciones_detalle_gestores <- renderDataTable({
        
        datatable(collectors_collection,
                  rownames = F,
                  extensions = c('Buttons','Scroller'),
                  options = list(dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','csv','excel','pdf','print'),
                                 pagelength = 10, lengthMenu = list(c(10,25,100,-1),c('10','25','100','All'))),
                  
                  colnames = c('Gestor','Cartera','Recuperacion de capital','Tasa de recuperacion',
                               'Pagos obtenidos','Clientes recuperados'))
    })
    
    
    
    
    
    output$ActividadesGestores <- renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formatAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'column') %>%
            hc_xAxis(categories = collectors_activities$GESTOR, labels = list(rotation = -90, step = 1)) %>%
            hc_add_series(name = "En negociacion", data = collectors_activities$`En negociaciones`) %>%
            hc_add_series(name = "Por castigar", data = collectors_activities$`Por castigar`) %>%
            hc_add_series(name = "Por demandar", data = collectors_activities$`Por demandar`) %>%
            hc_add_series(name = "Por pagar", data = collectors_activities$`Por pagar`) %>%
            hc_add_series(name = "Por refinanciar", data = collectors_activities$`Por refinanciar`)
        
        
        
    })
    
    
    output$Clientes_gestores <- renderDataTable({
        datatable(clients_to_managers_intervention,
                  rownames = F,
                  extensions = c('Buttons','Scroller'),
                  options = list(dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','csv','excel','pdf','print'),
                                 pagelength = 10, lengthMenu = list(c(10,25,100,-1),c('10','25','100','All'))),
                  colnames = c('Credito','DNI/RUC','Cliente','Saldo','Gestor','Accion de recuperacion'))
        # add telephone number
    })
    
    
    
    
    
}
