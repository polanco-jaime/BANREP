########################################################################
########################      Version 0.5       ########################
#######################         Banrep          ########################
########################################################################

#
########################    Calling libraries    ########################
########################################################################
packageList<-c("olapR", "foreign", "tidyverse", "haven","beepr", 'dplyr', 'readr', 'devtools')
# devtools::install_github("apache/arrow/r")

for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}


devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/functions.R") ## it deppends of devtools library

########################    Example of runing variable     ########################
###################################################################################

source("credentials.R", echo=TRUE)
################################# requried data


######################################################
####### Running loop
 

################################# requried data


### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
EPS_CODE = '[Administradoras].[Codigo de Administradora]'
SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]'

### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[ValorIndicador]'
AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[Año]'
AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'


connection_string = cnnstr_rips
olapCnn<-olapR::OlapConnection(connection_string)
olapR::explore(olapCnn)

from_olap_catalog <- 'CU - Morbilidad_ASIS'

#####################
# ITERATIONS <- read_delim("ITERATIONS.csv",
#                        ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),
#                        trim_ws = TRUE)

iterated <- read_delim("iterated.csv", 
                         ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE) 
# iterated = ITERATIONS
for (i in 1:nrow(iterated)) {
  
  EPS =as.character(iterated[i,1])
  VAR_INTERES = as.character(iterated[i,2])
  TYPE_USER =  as.character(iterated[i,3])
  SQL  = " SELECT * FROM iterated WHERE 
        CASE WHEN EPS = '%s'  AND 
        VAR_OF_INTEREST = '%s'  AND
        TYPE_OF_USER   = '%s'  THEN 1 ELSE  0 END = 0  " 
  
  iterated = sqldf::sqldf( sprintf(SQL, EPS,VAR_INTERES,TYPE_USER  ) )
  
   
  
  
  mdx = query_cube_mdx(AXIS0 = AXIS0, AXIS1 = AXIS1, AXIS2 = AXIS2, 
                       TYPE_USER= TYPE_USER,
                       SEGREGATION_VAR_INTERES=SEGREGATION_VAR_INTERES ,
                       VAR_INTERES=VAR_INTERES,
                       SEGREGATION_EPS_CODE=SEGREGATION_EPS_CODE  ,
                       EPS=EPS,
                       cube = from_olap_catalog )
  
  print( "The query is ready!")
  
  tryCatch( {
    tempo = execue_query_mdx(mdx =mdx , 
                             connection_string = connection_string,
                             EPS=EPS,
                             VAR_INTERES=VAR_INTERES,
                             TYPE_USER=  TYPE_USER ,  olapCnn = olapCnn)
    print(sprintf("The EPS with code: %s, var. of interes: %s, and of type user: %s had beed downloaded", EPS, VAR_INTERES, TYPE_USER ))
    csv_name = paste0('tables_from_cube/',EPS, '_',VAR_INTERES, '_',TYPE_USER,'.csv' )
    
    
    
    write_csv2(tempo, csv_name ,  na = '')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
  print( "ok!")
  write_csv2(iterated, 'iterated.csv' ,  na = '')
  Sys.sleep(1)
  gc()
}
 
 
#######################################
# [Municipio de Ocurrencia].[Geografia].[Departamento]
# [Municipio de Ocurrencia].[Municipio]
# [Municipio de Residencia].[Municipio].[Municipio]
# [Municipio de Ocurrencia].[Municipio].[Municipio]
#  [Régimen Salud].[Tipo Régimen].&[C - CONTRIBUTIVO]
# [Indicador].[Indicador].&[MUERTE FETOINFANTIL]
# [Tiempo].[Ano].[Ano]



### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
EPS_CODE = '[Administradoras].[Codigo de Administradora]'
SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]'

### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Indicador].[Indicador]'
AXIS1 <- '[Tiempo].[Ano].[Ano]'
AXIS2 <- '[Municipio de Ocurrencia].[Municipio].[Municipio]'


CAUSA = c('MUERTE FETOINFANTIL',
  'PORCENTAJE DE NACIDOS VIVOS A TÉRMINO CON BAJO PESO AL NACER',
  'PORCENTAJE DE NACIDOS VIVOS CON BAJO PESO AL NACER',
  'PORCENTAJE DE NACIDOS VIVOS CON CUATRO O MAS CONSULTAS DE CONTROL PRENATAL',
  'PORCENTAJE DE NACIDOS VIVOS DE MUJERES ENTRE 15 A 18 AÑOS  DONDE EL PADRE ES MAYOR 4 O MÁS AÑOS DE EDAD',
  'PORCENTAJE DE NACIDOS VIVOS DE MUJERES MENORES DE 15 AÑOS  DONDE EL PADRE ES MAYOR 4 O MÁS AÑOS DE EDAD',
  'PORCENTAJE DE PARTOS ATENDIDOS POR PERSONAL CALIFICADO',
  'PORCENTAJE DE PARTOS INSTITUCIONALES',
  'PORCENTAJE DE PARTOS POR CESAREA',
  'PORPORCIÓN DE NACIDOS VIVOS HIJOS DE MUJERES MENORES DE 14 AÑOS',
  'PROMEDIO DE CONTROLES PRENATALES',
  'PROPORCIÓN DE NACIDOS VIVOS HIJOS DE MUJERES DE 14 A 17 AÑOS',
  'PROPORCIÓN DE NACIDOS VIVOS HIJOS DE MUJERES DE 18 A 26 AÑOS',
  'RAZÓN DE MORTALIDAD MATERNA A 1 AÑO',
  'RAZÓN DE MORTALIDAD MATERNA A 42 DIAS',
  'TASA DE FECUNDIDAD ESPECÍFICA EN MUJERES DE 10 A 14 AÑOS',
  'TASA DE FECUNDIDAD ESPECÍFICA EN MUJERES DE 10 A 19 AÑOS',
  'TASA DE FECUNDIDAD ESPECÍFICA EN MUJERES DE 15 A 19 AÑOS',
  'TASA DE MORTALIDAD EN LA NIÑEZ (MENORES DE 5 AÑOS DE EDAD)',
  'TASA DE MORTALIDAD EN MENORES DE UN AÑO DE EDAD',
  'TASA DE MORTALIDAD EN NIÑOS MENORES DE 5 AÑOS POR ENFERMEDAD DIARREICA AGUDA',
  'TASA DE MORTALIDAD EN NIÑOS MENORES DE 5 AÑOS POR INFECCIÓN RESPIRATORIA AGUDA',
  'TASA DE MORTALIDAD FETAL',
  'TASA DE MORTALIDAD GENERAL',
  'TASA DE MORTALIDAD NEONATAL',
  'TASA DE MORTALIDAD NEONATAL TARDÍA',
  'TASA DE MORTALIDAD NEONATAL TEMPRANA',
  'TASA DE MORTALIDAD PERINATAL',
  'TASA DE MORTALIDAD POR DESNUTRICIÓN EN MENORES DE 5 AÑOS',
  'TASA DE MORTALIDAD POST NEONATAL'
)
EPS_CODE <- read_delim("codigo_entidad_regimen.csv", ",", escape_double = FALSE, trim_ws = TRUE)
eapb_list <- EPS_CODE[['codigo']] #[2:10]

#   ITERATIONS = data.frame()
#   for (k  in 1:length(eapb_list )  ) {
#     # k=1
#     EPS = as.character(eapb_list[[k]] )
#     print(sprintf("The information for the EPS with code: %s will be downloaded", EPS))
#     for (l in CAUSA) {
#       VAR_INTERES <-  l
#       
#       for (m in TIPO_USUARIO) {
#         print(sprintf("The EPS with code: %s, var. of interes: %s, and of type user: %s had beed downloaded", EPS, VAR_INTERES, m ))
#         TEMPO = data.frame('EPS' = EPS, 'VAR_OF_INTEREST' = VAR_INTERES, 'TYPE_OF_USER' = m )
#         
#         ITERATIONS = rbind(TEMPO, ITERATIONS)
#       }
#       
#       
#     }
#     
#   }
#   write.csv2(ITERATIONS, "ITERATIONS.csv", row.names = F)
# }
# git add .
# git commit -m "loading"
# git push


