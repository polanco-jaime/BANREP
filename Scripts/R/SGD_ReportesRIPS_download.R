# VAR_INTERES = 'Condiciones transmisibles y nutricionales' ### Looped variable
# EPS = "RES014"   ### Looped variable
# TYPE_USER = '1 - CONTRIBUTIVO'  ### Looped variable
packageList<-c("olapR", "foreign", "tidyverse", "haven","beepr", 'dplyr', 'readr', 'devtools')
# devtools::install_github("apache/arrow/r")

for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}
source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/credentials.R")


devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/functions.R") 

### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
EPS_CODE = '[Administradoras].[Codigo de Administradora]'
SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]'

### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[ValorIndicador], [Measures].[Número de Personas] ,[Measures].[Número de Atenciones]'
AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[Mes]'
AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'
# [Administradoras].[Codigo de Administradora].&[05000]
# [Tiempo].[Año - Semestre - Trimestre - Mes].[Año]&[%s] 

# Step 1 Make query filter by 


# Step 2


# Step 3


# Step 4

################################################################################
query_rips_mdx <- function(  AXIS0 ,AXIS1 ,AXIS2 ,
                             Filter 
                             EPS=NULL, 
                             cube ){
  
   
  
    if (isTRUE(  is.na(TYPE_USER) ) ) {
      where_filter <- '(%s.&[%s] , %s.&[%s]  ) '
      where_filter <- sprintf(where_filter , SEGREGATION_VAR_INTERES, VAR_INTERES, SEGREGATION_EPS_CODE  , EPS )
    } else{
      where_filter <- '(  %s.&[%s], %s.&[%s] , [Tipo Usuario].[Tipo Usuario].&[%s]
                                         ) '
      where_filter <- sprintf(where_filter   ,SEGREGATION_VAR_INTERES, VAR_INTERES,SEGREGATION_EPS_CODE  , EPS, TYPE_USER )
    }
    
    mdx<- "SELECT  {%s} ON AXIS(0),
                               {%s.MEMBERS} ON AXIS(1), 
                               {%s.MEMBERS} ON AXIS(2) 
                        FROM [%s]
                        WHERE %s "
    
    mdx<- sprintf(mdx, AXIS0, AXIS1, AXIS2, from_olap_catalog, where_filter ) 
  
  return(mdx)
  
}











library(olapR)
connection_string = cnnstr_rips
olapCnn<-olapR::OlapConnection(connection_string)
olapR::explore(olapCnn)

from_olap_catalog <- 'CU - Morbilidad_ASIS'

#####################
# ITERATIONS <- read_delim("ITERATIONS.csv",
#                        ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),
#                        trim_ws = TRUE)
# iterated = ITERATIONS
iterated <- read_delim("iterated.csv", 
                       ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                       trim_ws = TRUE) 
iterated = ITERATIONS
iterated <- subset(iterated, iterated$EPS == 'EPS017')
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



iterated = ITERATIONS
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

