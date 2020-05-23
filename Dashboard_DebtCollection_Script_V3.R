# Libraries used

library(readxl)
library(openxlsx)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Month and year to evaluate

this_year <- '2020'

this_month <- '01'

# Loading collectors information and selecting important variables

debt_collectors_path <- 'D:/01_BDD/05GestoresReinsercion/02HojaRuta'

collectors_info_backup <- read.xlsx(paste(debt_collectors_path,'/',this_year,this_month,'_','HojaRuta.xlsx',sep = ''))

collectors_info <- collectors_info_backup %>%
  select(NRO_CREDITO,FACTOR_IMPAGO,ACCION_RECUPERACION,FECHA_VISITA_1) %>%
  mutate(NRO_CREDITO = as.character(NRO_CREDITO)) %>%
  filter(nchar(FECHA_VISITA_1) == 5) %>%
  mutate(FECHA_VISITA_1 = as.Date(as.numeric(FECHA_VISITA_1), origin = '1899-12-30')) %>%
  filter(!is.na(ACCION_RECUPERACION) & !is.na(FACTOR_IMPAGO)) %>%
  filter(month(as.POSIXct(FECHA_VISITA_1, format = '%d,%m,%Y')) == as.numeric(this_month)) %>%
  mutate(ACCION_RECUPERACION = ifelse(
    ACCION_RECUPERACION %in% c('b) Pagara','d) Amortizara'),
    'Por pagar',
    ifelse(
      ACCION_RECUPERACION == 'j) Por Refinanciar',
      'Por refinanciar',
      ifelse(
        ACCION_RECUPERACION == 'n) Por castigar',
        'Por castigar',
        ifelse(
          ACCION_RECUPERACION == 'o) A cobranza judicial',
          'Por demandar',
          'En negociaciones'
          )
        )
      )
    )
  )

# Checking the info before with general database

general_database_path <- 'D:/01_BDD/01DataMaestra/TXT_FILES' #path

# generating a flag to grep the needed file
temp_grep <- 
  ifelse(
    this_month == '01',
    paste(as.numeric(this_year)-1,'-','12',sep = ''),
    paste(this_year,'-',ifelse(as.numeric(this_month)-1 < 10,
                               paste('0',as.numeric(this_month)-1,sep = ''),
                               as.numeric(this_month)-1),sep = '')
  )


files_data_general <- list.files(general_database_path,pattern = '.xlsx',full.names = T)

# File to grab

x <- grep(temp_grep,files_data_general)

general_data_base_backup <- read.xlsx(files_data_general[x],cols = c(1,6,8,9,27,33,35,41)) #loading the general database

general_data_base <- general_data_base_backup %>%
  filter(SITUACION_V2 %in% c('VENCIDO','JUDICIAL','CASTIGADO')) #filtering just what we need

## Collectors info cleaning

collectors_info <- left_join(collectors_info,general_data_base[,c(1,5,6)] ,by = 'NRO_CREDITO')

collectors_info <- collectors_info %>%
  filter(SITUACION_V2 %in% c('CASTIGADO','VENCIDO') &
           Tipo_Cred_2 == '01 Minorista')


# Creating some fictional debt collectors names and assign them a portfolio

debt_collectors <- paste('Collector','_',1:42,sep = '')

collectors_activities <- c('Por pagar','Por refinanciar','Por demandar','Por castigar') # narrowing their activities

general_data_base <- general_data_base %>%
  mutate(GESTOR = ifelse(
    SITUACION_V2 %in% c('VENCIDO','CASTIGADO') & Tipo_Cred_2 == '01 Minorista', # collectors only visit these type of clients
    debt_collectors,
    NA
    )
  )


# Merging the collectors activities with the general database since we want...
#...to compare all clients vs reached clients

general_data_base <- left_join(general_data_base,collectors_info[,c(1,2,3,4)], by = 'NRO_CREDITO')

general_data_base <- general_data_base %>%
  mutate(ACCION_RECUPERACION = ifelse(
   is.na(GESTOR),
   NA,
   ACCION_RECUPERACION
    )
  )

# Attorneys follow the same process (replicate if needed)


# Debt collections in the current month

collections_path <- 'D:/01_BDD/03Pagos/PagosDiarios'

collections_backup <- read.xlsx(paste(collections_path,'/',this_year,this_month,'Pagos.xlsx',sep = '')) #keep this data structure...
#...from SQL grab creditID, payment day, principal, interest, total payment. If total payment == 0 do not considered it

collections <- collections_backup %>%
  select(NumeroCredito,DiaPago,Mes.Fecha.Valor,Capital,Intereses,PagoTotal) %>%
  rename(NRO_CREDITO = NumeroCredito, DIA_PAGO = DiaPago, MES = Mes.Fecha.Valor,
         CAPITAL = Capital, INTERESES = Intereses, PAGO_TOTAL = PagoTotal) %>%
  mutate(NRO_CREDITO = as.character(NRO_CREDITO)) %>%
  filter(PAGO_TOTAL != 0)

# Merge collections with general database


general_data_base <- left_join(general_data_base,collections, by = 'NRO_CREDITO') # this will have more records

general_data_base <- general_data_base %>%
  mutate(SALDO = ifelse(
    duplicated(NRO_CREDITO), # we dont want to double sum the principal
    0,
    SALDO
    )
  )

## Freeing memory

rm(list = ls()[!(ls() == 'general_data_base')])

##########################################################################
##########################################################################

# Boxes in dashboard first tab

TotalCollection <- round(sum(general_data_base$PAGO_TOTAL,na.rm = TRUE)/(10^6),2)

After_due_collections <- general_data_base %>%
  filter(SITUACION_V2 == 'VENCIDO') %>%
  summarize(Collection = round(sum(PAGO_TOTAL,na.rm = TRUE)/(10^6),2))

After_due_collections <- After_due_collections$Collection

Sued_collections <- general_data_base %>%
  filter(SITUACION_V2 == 'JUDICIAL') %>%
  summarize(Collection = round(sum(PAGO_TOTAL,na.rm = TRUE)/(10^6),2))

Sued_collections <- Sued_collections$Collection

Castigated_collections <- general_data_base %>%
  filter(SITUACION_V2 == 'CASTIGADO') %>%
  summarize(Collection = round(sum(PAGO_TOTAL,na.rm = TRUE)/(10^6),2))

Castigated_collections <- Castigated_collections$Collection

# Collections by day based on portfolio segmentation

# Total collection

minicollection <- general_data_base %>%
  filter(!is.na(DIA_PAGO)) %>%
  mutate(DIA_PAGO = ifelse(
    DIA_PAGO < 10,
    paste('0',DIA_PAGO,sep = ''),
    DIA_PAGO
  )) %>%
  group_by(DIA_PAGO) %>%
  summarize(RECUPERACION = sum(PAGO_TOTAL,na.rm = TRUE)) %>%
  mutate(RECUPERACION_ACUMULADA = cumsum(RECUPERACION))

# since this will give all the days with payments we need to grab all possibe days. For this we create a flag

flag_complete <- data.frame(DIA_PAGO = minicollection$DIA_PAGO, RECUPERACION_ACUMULADA_FLAG = 0)


## Sued collections

minicollection_sued <- general_data_base %>%
  filter(!is.na(DIA_PAGO) & SITUACION_V2 == 'JUDICIAL') %>%
  mutate(DIA_PAGO = ifelse(
    DIA_PAGO < 10,
    paste('0',DIA_PAGO,sep = ''),
    DIA_PAGO
  )) %>%
  group_by(DIA_PAGO) %>%
  summarize(RECUPERACION = sum(PAGO_TOTAL,na.rm = TRUE))


minicollection_sued <- right_join(minicollection_sued,flag_complete, by = 'DIA_PAGO')

minicollection_sued <- minicollection_sued %>%
  select(DIA_PAGO,RECUPERACION) %>%
  mutate(RECUPERACION = ifelse(is.na(RECUPERACION),0,RECUPERACION)) %>%
  mutate(RECUPERACION_ACUMULADA = cumsum(RECUPERACION))


## After due collections

minicollection_after_due <- general_data_base %>%
  filter(!is.na(DIA_PAGO) & SITUACION_V2 == 'VENCIDO') %>%
  mutate(DIA_PAGO = ifelse(
    DIA_PAGO < 10,
    paste('0',DIA_PAGO,sep = ''),
    DIA_PAGO
  )) %>%
  group_by(DIA_PAGO) %>%
  summarize(RECUPERACION = sum(PAGO_TOTAL,na.rm = TRUE))

minicollection_after_due <- right_join(minicollection_after_due,flag_complete, by = 'DIA_PAGO')

minicollection_after_due <- minicollection_after_due %>%
  select(DIA_PAGO,RECUPERACION) %>%
  mutate(RECUPERACION = ifelse(is.na(RECUPERACION),0,RECUPERACION)) %>%
  mutate(RECUPERACION_ACUMULADA = cumsum(RECUPERACION))

## Castigated collections

minicollection_castigated <- general_data_base %>%
  filter(!is.na(DIA_PAGO) & SITUACION_V2 == 'CASTIGADO') %>%
  mutate(DIA_PAGO = ifelse(
    DIA_PAGO < 10,
    paste('0',DIA_PAGO,sep = ''),
    DIA_PAGO
  )) %>%
  group_by(DIA_PAGO) %>%
  summarize(RECUPERACION = sum(PAGO_TOTAL,na.rm = TRUE))

minicollection_castigated <- right_join(minicollection_castigated,flag_complete, by = 'DIA_PAGO')

minicollection_castigated <- minicollection_castigated %>%
  select(DIA_PAGO,RECUPERACION) %>%
  mutate(RECUPERACION = ifelse(is.na(RECUPERACION),0,RECUPERACION)) %>%
  mutate(RECUPERACION_ACUMULADA = cumsum(RECUPERACION))


## Collection by manager

collection_by_manager <- general_data_base %>%
  group_by(Gestionador) %>%
  summarize(Recuperacion = sum(CAPITAL,na.rm = TRUE),
            
            Cartera = sum(SALDO),
            
            Tasa =  round((sum(CAPITAL,na.rm = TRUE)/sum(SALDO))*100,2),
            
            Clientes_Recuperados = n_distinct(NRO_DOC[!is.na(PAGO_TOTAL)]),
            
            Cartera_Clientes = n_distinct(NRO_DOC),
            
            Tasa_clientes = (n_distinct(NRO_DOC[!is.na(PAGO_TOTAL)])/n_distinct(NRO_DOC))*100)

managers_total_collection <- data.frame(Gestionador = 'TOTAL',
                                 Recuperacion = sum(collection_by_manager$Recuperacion),
                                 Cartera = sum(collection_by_manager$Cartera),
                                 Tasa = sum(collection_by_manager$Recuperacion)/sum(collection_by_manager$Cartera),
                                 Clientes_Recuperados = sum(collection_by_manager$Clientes_Recuperados),
                                 Cartera_Clientes = sum(collection_by_manager$Cartera_Clientes),
                                 Tasa_clientes = sum(collection_by_manager$Clientes_Recuperados)/sum(collection_by_manager$Cartera_Clientes))

collection_by_manager <- rbind.fill(collection_by_manager,managers_total_collection)
rm(managers_total_collection)

# Formating collections by manager. To be used in datatable function

collection_by_manager <- collection_by_manager %>%
  mutate(Recuperacion = format(round(Recuperacion,0), big.mark = ","),
         Cartera = format(round(Cartera,0), big.mark = ","),
         Tasa = paste(round(Tasa,2),"%",sep = ""),
         Clientes_Recuperados = format(Clientes_Recuperados, big.mark = ","),
         Cartera_Clientes = format(Cartera_Clientes, big.mark = ","),
         Tasa_clientes = paste(round(Tasa_clientes,2),"%",sep = ""))

# Top 20 collections

clients_collected <- general_data_base %>%
  filter(!is.na(PAGO_TOTAL)) %>%
  group_by(NRO_DOC) %>%
  summarize(Recuperacion_capital = sum(CAPITAL,na.rm = TRUE),
            Recuperacion_interes = sum(INTERESES, na.rm = TRUE),
            Recuperacion_total = sum(PAGO_TOTAL,na.rm = TRUE)) %>%
  arrange(-Recuperacion_total)

flag_clients_name <- general_data_base %>%
  select(NRO_DOC,NOMBRE_CLIENTE,Tipo_Cred_2) %>%
  filter(!duplicated(NRO_DOC))

clients_collected <- inner_join(clients_collected,flag_clients_name, by = 'NRO_DOC')

top_20_clients <- clients_collected %>%
  select(NOMBRE_CLIENTE,Recuperacion_capital,Recuperacion_interes,Recuperacion_total,Tipo_Cred_2) %>%
  top_n(20,Recuperacion_total) %>%
  mutate(Recuperacion_capital = format(round(Recuperacion_capital,0), big.mark = ","),
         Recuperacion_interes = format(round(Recuperacion_interes,0), big.mark = ","),
         Recuperacion_total = format(round(Recuperacion_total,0), big.mark = ","),
         Tipo_Cred_2 = substr(Tipo_Cred_2,4,length(Tipo_Cred_2)))

## Debt collectors activities


collectors_activities <- general_data_base %>%
  filter(!duplicated(NRO_CREDITO)) %>%
  filter(SITUACION_V2 %in% c('VENCIDO','CASTIGADO') & Tipo_Cred_2 == '01 Minorista') %>%
  filter(ACCION_RECUPERACION %in% c('Por pagar','En negociaciones', 'Por demandar','Por castigar','Por refinanciar')) %>%
  group_by(GESTOR,ACCION_RECUPERACION) %>%
  summarize(Cantidad_creditos = n())

# from long to wide in order to use highchart package

collectors_activities <- spread(collectors_activities,ACCION_RECUPERACION,Cantidad_creditos)
collectors_activities[is.na(collectors_activities)] <- 0

# Clients from debt collectors to managers to intervine. Managers could use this info to collect the rest.

clients_to_managers_intervention <- general_data_base %>%
  filter(!is.na(PAGO_TOTAL)) %>% # we are not considering the clients that we have already collected!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  filter(!duplicated(NRO_CREDITO)) %>%
  filter(SITUACION_V2 %in% c('VENCIDO','CASTIGADO') & Tipo_Cred_2 == '01 Minorista') %>%
  filter(ACCION_RECUPERACION %in% c('Por pagar','En negociaciones', 'Por demandar','Por castigar','Por refinanciar')) %>%
  select(NRO_CREDITO,NRO_DOC,NOMBRE_CLIENTE,SALDO,GESTOR,ACCION_RECUPERACION) %>%
  mutate(SALDO = format(round(SALDO,0),big.mark = ","))


# Debt collector's collections bar chart

collectors_collection_as_bar <- general_data_base %>%
  filter(SITUACION_V2 %in% c('VENCIDO','CASTIGADO') & Tipo_Cred_2 == '01 Minorista') %>%
  group_by(GESTOR) %>%
  summarize(Pagos_obtenidos = round(sum(PAGO_TOTAL,na.rm = TRUE),0)) %>%
  arrange(-Pagos_obtenidos)
  

# Debt collector's collection as datatable

collectors_collection <- general_data_base %>%
  filter(SITUACION_V2 %in% c('VENCIDO','CASTIGADO') & Tipo_Cred_2 == '01 Minorista') %>%
  group_by(GESTOR) %>%
  summarize(Cartera = format(round(sum(SALDO),0),big.mark = ","),
            Recuperacion_capital = format(round(sum(CAPITAL,na.rm = TRUE),0), big.mark = ","),
            TasaRecuperacion = paste(round(sum(CAPITAL,na.rm = TRUE)/sum(SALDO),2)*100,"%",sep = ""),
            Pagos_obtenidos = round(sum(PAGO_TOTAL,na.rm = TRUE)),
            Clientes_recuperados = n_distinct(NRO_DOC[!is.na(PAGO_TOTAL)])) %>%
  arrange(-Pagos_obtenidos) %>%
  mutate(Pagos_obtenidos = format(Pagos_obtenidos, big.mark = ","))

# Saving environment

save.image(file = 'D:/DashboardApp/myEnvironment.RData')

