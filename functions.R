########################################################################
######################## Version 0.5            ########################
####################### Polanco-Jimenez, Jaime  ########################
########################################################################
############ Making easy query function ############
query_cube_mdx <- function(  AXIS0 ,AXIS1 ,AXIS2 ,
                         TYPE_USER=NULL,
                         SEGREGATION_VAR_INTERES='' ,
                         VAR_INTERES=NULL, 
                         SEGREGATION_EPS_CODE=NULL  , 
                         EPS=NULL, 
                         cube ){
  
      if ( isTRUE( SEGREGATION_VAR_INTERES=='' ) ) {
                mdx<- "SELECT   {%s} ON AXIS(0),
                                     {%s.MEMBERS} ON AXIS(1),
                                     {%s.MEMBERS} ON AXIS(2)
                              FROM [%s]   
                              "
                mdx<- sprintf(mdx, AXIS0, AXIS1, AXIS2, from_olap_catalog, AXIS0 )} else{
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
          
          mdx<- sprintf(mdx, AXIS0, AXIS1, AXIS2, from_olap_catalog, where_filter )  }
  return(mdx)
  
}



 
 

#Running query and cleaning empty observations
execue_query_mdx <- function(mdx,connection_string, EPS,VAR_INTERES, TYPE_USER, olapCnn  ){
  
  
  tempo3 <- olapR::execute2D(olapCnn, mdx)
  tempo3 = subset(tempo3, is.na(tempo3[[3]]) == F)
  if (nrow(tempo3) == 0) {
    print((sprintf("The EPS with code: %s, var. of interes: %s, and of type user: %s doesnt have info ", EPS, VAR_INTERES, TYPE_USER )))
  }else{
    print(paste0("the number of observations are " , nrow(tempo3)) )
    gc()
    tempo3[[2]] < - as.numeric(tempo3[[2]])
    tempo3 = subset(tempo3, tempo3[[2]] >= 2009 &  tempo3[[2]]  <=2022)
    tempo3 = subset(tempo3, is.na(tempo3[[3]]) == F)
    colnames(tempo3) <- c('CITY', 'YEAR', 'VALUE')
    tempo3$EPS <- EPS
    tempo3$VAR_INTERES <- as.character(VAR_INTERES)
    tempo3$TYPE_USER <- as.character(TYPE_USER)
    return(tempo3)
  }
  

}



#############
warning("The functions created for Banrep have been loaded")
print("The functions created for Banrep have been loaded")


########################    Example of runing variable     ########################
###################################################################################
# VAR_INTERES = 'Condiciones transmisibles y nutricionales' ### Looped variable
# EPS = "RES014"   ### Looped variable
# TYPE_USER = '1 - CONTRIBUTIVO'  ### Looped variable

# ### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
# SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
# EPS_CODE = '[Administradoras].[Codigo de Administradora]'
# SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]' 

# ### Mandatory variables to get observations a municipalities's level.
# AXIS0 <- '[Measures].[ValorIndicador]'
# AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[Año]'
# AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'
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





# #####################
# if(1==1){
#   
#   ### SET THE GRADE OF INTEREST
#   CAUSA = c('Condiciones transmisibles y nutricionales'
#             , 'Enfermedades no transmisibles'	, 'Lesiones'	,'Signos y sintomas mal definidos'
#   )
#   
#   TIPO_USUARIO <- c('1 - CONTRIBUTIVO'	,'2 - SUBSIDIADO'
#                     , '3 - VINCULADO'	,'4 - PARTICULAR'	,'5 - OTRO'	, '6 - DESPLAZADO CON AFILIACIÓN A RÉGIMEN CONTRIBUTIVO'	,
#                     '7 - DESPLAZADO CON AFILIACIÓN A RÉGIMEN SUBSIDIADO'	,'8 - DESPLAZADO NO ASEGURADO O VINCULADO'
#   ) # [Tipo Usuario].[Tipo Usuario].&[1 - CONTRIBUTIVO]
#   
#   # taking the list of the entire eps
#   
#   EPS_CODE <- read_delim("codigo_entidad_regimen.csv", ",", escape_double = FALSE, trim_ws = TRUE)
#   
#   connection_string = cnnstr_rips
#   eapb_list <- EPS_CODE[['codigo']] #[2:10]
#   from_olap_catalog <- 'CU - Morbilidad_ASIS'
#   
#   ################################# requried data
#   
#   
#   ### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
#   SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
#   EPS_CODE = '[Administradoras].[Codigo de Administradora]'
#   SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]'
#   
#   ### Mandatory variables to get observations a municipalities's level.
#   AXIS0 <- '[Measures].[ValorIndicador]'
#   AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[Año]'
#   AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'
#   
#   
#   
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