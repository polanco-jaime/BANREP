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


### SET THE GRADE OF INTEREST 
CAUSA = c('Condiciones transmisibles y nutricionales'	, 'Enfermedades no transmisibles'	, 'Lesiones'	,'Signos y sintomas mal definidos'	)

TIPO_USUARIO <- c('1 - CONTRIBUTIVO'	,'2 - SUBSIDIADO'	, '3 - VINCULADO'	,'4 - PARTICULAR'	,'5 - OTRO'	, '6 - DESPLAZADO CON AFILIACIÓN A RÉGIMEN CONTRIBUTIVO'	,
                  '7 - DESPLAZADO CON AFILIACIÓN A RÉGIMEN SUBSIDIADO'	,'8 - DESPLAZADO NO ASEGURADO O VINCULADO'	) # [Tipo Usuario].[Tipo Usuario].&[1 - CONTRIBUTIVO]

# taking the list of the entire eps

EPS_CODE <- read_delim("codigo_entidad_regimen.csv", ",", escape_double = FALSE, trim_ws = TRUE)


eapb_list <- EPS_CODE[['codigo']][0:10]
from_olap_catalog <- 'CU - Morbilidad_ASIS'


### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
EPS_CODE = '[Administradoras].[Codigo de Administradora]'
SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]'

### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[ValorIndicador]'
AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[Año]'
AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'
# 
# ### the cube used
# from_olap_catalog = 'CU - Morbilidad_ASIS'
# 
# mdx = query_cube_mdx(AXIS0 = AXIS0, AXIS1 = AXIS1, AXIS2 = AXIS2, TYPE_USER=TYPE_USER,
#                      SEGREGATION_VAR_INTERES=SEGREGATION_VAR_INTERES ,
#                      VAR_INTERES=VAR_INTERES, 
#                      SEGREGATION_EPS_CODE=SEGREGATION_EPS_CODE  , 
#                      EPS=EPS,  
#                      cube = from_olap_catalog )
# 
# 
# mdx
# 
# tempo = execue_query_mdx(mdx =mdx ,
#                          connection_string = connection_string, 
#                          EPS=EPS,
#                          VAR_INTERES=VAR_INTERES, 
#                          TYPE_USER= TYPE_USER  )


 
for (k  in 1:length(eapb_list )  ) {
  k=1
  EPS = as.character(eapb_list[k] ) 
  for (l in CAUSA) {
    VAR_INTERES <-  l
    
    for (m in TIPO_USUARIO) {
      TYPE_USER <- m
       
      mdx = query_cube_mdx(AXIS0 = AXIS0, AXIS1 = AXIS1, AXIS2 = AXIS2, TYPE_USER=TYPE_USER,
                           SEGREGATION_VAR_INTERES=SEGREGATION_VAR_INTERES ,
                           VAR_INTERES=VAR_INTERES,
                           SEGREGATION_EPS_CODE=SEGREGATION_EPS_CODE  ,
                           EPS=EPS,
                           cube = from_olap_catalog )
      Sys.sleep(5)
      tempo = execue_query_mdx(mdx =mdx ,
                               connection_string = connection_string,
                               EPS=EPS,
                               VAR_INTERES=VAR_INTERES,
                               TYPE_USER= TYPE_USER  )
      
      warning(paste0('the eps: ' ,EPS,  " have finished", 'with data of ', GRAN_CAUSA, TYPE_USER) )
      csv_name = paste0('tables_from_cube/',EPS, '_',GRAN_CAUSA, '_',TYPE_USER,'.csv' )
      write.csv(tempo3, csv_name , row.names = F, sep = '|')
      
    }
    
    
  }
  
}

























connection_string = "Provider=MSOLAP;Data Source=cubos.sispro.gov.co;Password=u4_gu41n14;
            Persist Security Info=True;
            User ID=sispro.local\\UA_Guainia;
            Initial Catalog=SGD_ReportesRIPS;
            Data Source=cubos.sispro.gov.co"
#### variables for looping
VAR_INTERES = 'Condiciones transmisibles y nutricionales' ### Looped variable
EPS = "RES014"   ### Looped variable

### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
EPS_CODE = '[Administradoras].[Codigo de Administradora]'
SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]' 
TYPE_USER = '1 - CONTRIBUTIVO'
### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[ValorIndicador]'
AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[Año]'
AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'

### the cube used
from_olap_catalog = 'CU - Morbilidad_ASIS'

mdx = query_cube_mdx(AXIS0 = AXIS0, AXIS1 = AXIS1, AXIS2 = AXIS2, TYPE_USER=TYPE_USER,
                     SEGREGATION_VAR_INTERES=SEGREGATION_VAR_INTERES ,
                     VAR_INTERES=VAR_INTERES, 
                     SEGREGATION_EPS_CODE=SEGREGATION_EPS_CODE  , 
                     EPS=EPS,  
                     cube = from_olap_catalog )


mdx

tempo = execue_query_mdx(mdx =mdx ,
                         connection_string = connection_string, 
                         EPS=EPS,
                         VAR_INTERES=VAR_INTERES, 
                         TYPE_USER= TYPE_USER  )
