packageList<-c("readxl","olapR", "foreign", "tidyverse", "haven","beepr", 'dplyr', 'readr', 'devtools', 'sqldf')
# devtools::install_github("apache/arrow/r")

for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}
source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/Scripts/credentials.R")

#####################################################################
#
#####################################################################

# devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/functions.R") 


path_input = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/input/"
# EPS diccionario
diccionario_codigos_eps <- read_excel(paste0(path_input, "diccionario_codigos_eps.xlsx" ) )


library(olapR)
cnnstr = cnnstr_indicadores
olapCnn<-olapR::OlapConnection(cnnstr)
olapR::explore(olapCnn)
metadata = 'Cu - Indicadores'


A= data.frame("year" =c(2009:2021)  ) # data.frame("EPS_CODE" = unique(diccionario_codigos_eps$codigo))
B =  data.frame('GBD'= c('MUERTE FETOINFANTIL',
                         'PORCENTAJE DE NACIDOS VIVOS A TÉRMINO CON BAJO PESO AL NACER',
                         'PORCENTAJE DE NACIDOS VIVOS CON BAJO PESO AL NACER',
                         'PORCENTAJE DE NACIDOS VIVOS CON CUATRO O MAS CONSULTAS DE CONTROL PRENATAL',
                         'PORCENTAJE DE NACIDOS VIVOS DE MUJERES ENTRE 15 A 18 AÑOS DONDE EL PADRE ES MAYOR 4 O MÁS AÑOS DE EDAD',
                         'PORCENTAJE DE NACIDOS VIVOS DE MUJERES MENORES DE 15 AÑOS DONDE EL PADRE ES MAYOR 4 O MÁS AÑOS DE EDAD',
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
                         'TASA DE MORTALIDAD POST NEONATAL'))


INTERATION = sqldf(" SELECT * FROM A INNER JOIN B ON 1=1  ")

### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[ValorIndicador] ,	[Measures].[Denominador]	, [Measures].[Numerador]'
AXIS1 <- '[Administradora].[Administradora]'
AXIS2 <- '[Geografía de Defuncion].[Municipio]'

i = 1
INTERATION[i,]
var1 = as.character(INTERATION[i,1] )
var2 = as.character(INTERATION[i,2] )



