packageList<-c("readxl","olapR", "foreign", "tidyverse", "haven","beepr", 'dplyr', 'readr', 'devtools', 'sqldf')
# devtools::install_github("apache/arrow/r")

for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}
source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/credentials.R")

#####################################################################
#
#####################################################################

devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/functions.R") 


metadata_Estadisticas_Vitales <-  c('EEVV - Defunciones' , 'EEVV - Nacimientos')
# ITERATIONS  


path_input = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/input/"
# EPS diccionario
diccionario_codigos_eps <- read_excel(paste0(path_input, "diccionario_codigos_eps.xlsx" ) )

A= data.frame("year" =c(2009:2021)  ) # data.frame("EPS_CODE" = unique(diccionario_codigos_eps$codigo))
B =  data.frame('GBD'= c('A. Accidentes',
                         'A. Enfermedades infecciosas y parasitarias',
                         'A. Tumores malignos',
                         'B. Infecciones respiratorias',
                         'B. Lesiones intencionales',
                         'B. Otros tumores',
                         'C. Causas maternas',
                         'C. Diabetes mellitus',
                         'D. Ciertas afecciones originadas en el período perinatal',
                         'D. Otras enfermedades endocrinas, metabolicas, hematologicas e immunológicas',
                         'E. Deficiencias de la nutrición',
                         'E. Trastornos mentales y enfermedades del sistema nervioso',
                         'Eventos (lesiones) de intención no determinada',
                         'F. Enfermedades de los órganos de los sentidos',
                         'G. Enfermedades cardiovasculares',
                         'H. Enfermedades respiratorias',
                         'I. Enfermedades digestivas',
                         'J. Enfermedades del sistema genito-urinario',
                         'K. Enfermedades de la piel',
                         'L. Enfermedades del sistema músculo esquelético',
                         'M. Anomalías congénitas',
                         'N. Enfermedades dentales',
                         'No Definido'))


INTERATION = sqldf(" SELECT * FROM A INNER JOIN B ON 1=1  ")

### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[Número de Defunciones]'
AXIS1 <- '[Administradora].[Administradora]'
AXIS2 <- '[Geografía de Defuncion].[Municipio]'

i = 1
INTERATION[i,]
var1 = as.character(INTERATION[i,1] )
var2 = as.character(INTERATION[i,2] )

where_filter


# query_olap <- function(AXIS0,AXIS1, AXIS2,from_olap_catalog,where_filter   ){
  
  mdx<- "SELECT  {%s} ON AXIS(0),
                               {%s.MEMBERS} ON AXIS(1), 
                               {%s.MEMBERS} ON AXIS(2) 
                        FROM [%s]
                        WHERE (%s) "
  mdx<- sprintf(mdx, AXIS0, AXIS1, AXIS2, from_olap_catalog, where_filter )
  
#   return(mdx)
# }

cnnstr = 
mdx  = 
var1 = 
var2 = 
  
# run_query_olap <- function(cnnstr, mdx,var1,  var2 ){
  olapCnn<-olapR::OlapConnection(cnnstr)
  temp <- olapR::execute2D(olapCnn, mdx)
  temp$var1 = var1
  temp$var2 = var2
  return(mdx)
# }

 
from_olap_catalog =  metadata_Estadisticas_Vitales[1]
filter_string =  '[Fecha de Defunción].[Año].&[%s], [Lista GBD].[Nivel2].&[%s]'


A = query_olap_est_vital(AXIS0=AXIS0,AXIS1=AXIS1, AXIS2= AXIS2,
                     from_olap_catalog = from_olap_catalog,
                     filter_string = filter_string,  
                     var1 = as.character(INTERATION[i,1] ) ,
                     var2 = as.character(INTERATION[i,1] ) , cnnstr = cnnstr_Estadisticas_Vitales  )
