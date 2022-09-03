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
library(readr)

################################# requried data


### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
EPS_CODE = '[Administradoras].[Codigo de Administradora]'
SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]'

### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[ValorIndicador]'
AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[Año]'
AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'

from_olap_catalog <- 'CU - Morbilidad_ASIS'
connection_string = cnnstr_rips
ITERATIONS <- read_delim("ITERATIONS.csv", 
                         ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE)
ITERATIONS <- subset(ITERATIONS, ITERATIONS$TYPE_OF_USER == '1 - CONTRIBUTIVO' )

for (i in 1:nrow(ITERATIONS)) {
  # i=1000
  EPS = as.character(ITERATIONS[i,1])
  VAR_INTERES = as.character(ITERATIONS[i,2])
  TYPE_USER = as.character(ITERATIONS[i,3])
  mdx = query_cube_mdx(AXIS0 = AXIS0, AXIS1 = AXIS1, AXIS2 = AXIS2, 
                       TYPE_USER= TYPE_USER,
                       SEGREGATION_VAR_INTERES=SEGREGATION_VAR_INTERES ,
                       VAR_INTERES=VAR_INTERES,
                       SEGREGATION_EPS_CODE=SEGREGATION_EPS_CODE  ,
                       EPS=EPS,
                       cube = from_olap_catalog )
  
  print( "The query is ready!")
  olapR::is.Query(mdx)
  
  tryCatch( {
    tempo = execue_query_mdx(mdx =mdx , 
                             connection_string = connection_string,
                             EPS=EPS,
                             VAR_INTERES=VAR_INTERES,
                             TYPE_USER=  TYPE_USER  )
    print(sprintf("The EPS with code: %s, var. of interes: %s, and of type user: %s had beed downloaded", EPS, VAR_INTERES, TYPE_USER ))
    csv_name = paste0('tables_from_cube/',EPS, '_',VAR_INTERES, '_',TYPE_USER,'.csv' )
    write.csv(tempo, csv_name , row.names = F, sep = '|')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
  print( "ok!")
  Sys.sleep(1)
  gc()
}
 
 
 








