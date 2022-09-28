# VAR_INTERES = 'Condiciones transmisibles y nutricionales' ### Looped variable
# EPS = "RES014"   ### Looped variable
# TYPE_USER = '1 - CONTRIBUTIVO'  ### Looped variable
source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/credentials.R")
# ### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
EPS_CODE = '[Administradoras].[Codigo de Administradora]'
SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]'

### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[ValorIndicador]'
AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[All]'
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





# #####################
if(1==1){

  ### SET THE GRADE OF INTEREST
  CAUSA = c('Condiciones transmisibles y nutricionales'
            , 'Enfermedades no transmisibles'	, 'Lesiones'	,'Signos y sintomas mal definidos'
  )

  TIPO_USUARIO <- c('1 - CONTRIBUTIVO'	,'2 - SUBSIDIADO'
                    # , '3 - VINCULADO'	,'4 - PARTICULAR'	,'5 - OTRO'	, '6 - DESPLAZADO CON AFILIACIÓN A RÉGIMEN CONTRIBUTIVO'	,
                    # '7 - DESPLAZADO CON AFILIACIÓN A RÉGIMEN SUBSIDIADO'	,'8 - DESPLAZADO NO ASEGURADO O VINCULADO'
  ) # [Tipo Usuario].[Tipo Usuario].&[1 - CONTRIBUTIVO]

  # taking the list of the entire eps

  EPS_CODE <-  read_excel(paste0(path_input, "diccionario_codigos_eps.xlsx" ) ) #<- read_delim("codigo_entidad_regimen.csv", ",", escape_double = FALSE, trim_ws = TRUE)

  connection_string = cnnstr_rips
  eapb_list <- EPS_CODE[['codigo']] #[2:10]
  from_olap_catalog <- 'CU - Morbilidad_ASIS'

  ################################# requried data


  ### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
  SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
  EPS_CODE = '[Administradoras].[Codigo de Administradora]'
  SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]'

  ### Mandatory variables to get observations a municipalities's level.
  AXIS0 <- '[Measures].[ValorIndicador]'
  AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[All], '
  AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'



  ITERATIONS = data.frame()
  for (k  in 1:length(eapb_list )  ) {
    # k=1
    EPS = as.character(eapb_list[[k]] )
    print(sprintf("The information for the EPS with code: %s will be downloaded", EPS))
    for (l in CAUSA) {
      VAR_INTERES <-  l

      for (m in TIPO_USUARIO) {
        print(sprintf("The EPS with code: %s, var. of interes: %s, and of type user: %s had beed downloaded", EPS, VAR_INTERES, m ))
        TEMPO = data.frame('EPS' = EPS, 'VAR_OF_INTEREST' = VAR_INTERES, 'TYPE_OF_USER' = m )

        ITERATIONS = rbind(TEMPO, ITERATIONS)
      }


    }

  }
   write.csv2(ITERATIONS, "ITERATIONS.csv", row.names = F)
}


### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
EPS_CODE = '[Administradoras].[Codigo de Administradora]'
SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]'

### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[ValorIndicador]'
AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[All]'
AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'

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