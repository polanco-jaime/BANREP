packageList<-c("readxl","olapR", "foreign", "tidyverse", "haven","beepr", 'dplyr', 'readr', 'devtools')
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
olapCnn<-olapR::OlapConnection(cnnstr_Estadisticas_Vitales)

metadata_Estadisticas_Vitales <-  c('EEVV - Defunciones' , 'EEVV - Nacimientos')

# ITERATIONS  aca voy
path_input = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/input/"
# EPS diccionario
diccionario_codigos_eps <- read_excel(paste0(path_input, "diccionario_codigos_eps.xlsx" ) )
A = data.frame("fechas"= (2009:2022))
B= 
### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[Número de Defunciones]'
AXIS1 <- '[Fecha de Defunción].[Año]'
AXIS2 <- '[Geografía de Defuncion].[Municipio]'
fechas = (2009:2022)[1]
Filtro = sprintf( '[Fecha de Defunción].[Año]&[%s]' , fechas)
Filtro = paste0( sprintf( '[Administradora].[Administradora].&[%s]' , eapb) )
where_filter <- '(%s.&[%s] , %s.&[%s]  ) '
where_filter <- sprintf(where_filter , Filtro, condition )
from_olap_catalog =  metadata_Estadisticas_Vitales[1]

mdx<- "SELECT  {%s} ON AXIS(0),
                               {%s.MEMBERS} ON AXIS(1), 
                               {%s.MEMBERS} ON AXIS(2) 
                        FROM [%s]
                        WHERE %s "

mdx<- sprintf(mdx, AXIS0, AXIS1, AXIS2, from_olap_catalog, where_filter )