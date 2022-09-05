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

from_olap_catalog <- 'CU - Morbilidad_ASIS'
connection_string = cnnstr_rips
olapCnn<-olapR::OlapConnection(connection_string)
olapR::explore(olapCnn)

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
 
 
 





# git add .
# git commit -m "loading"
# git push


