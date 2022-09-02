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
                mdx<- "SELECT NON EMPTY {%s} ON AXIS(0),
                                     {%s.MEMBERS} ON AXIS(1),
                                     {%s.MEMBERS} ON AXIS(2)
                              FROM [%s]   
                              "
                mdx<- sprintf(mdx, AXIS0, AXIS1, AXIS2, from_olap_catalog, AXIS0 )} else{
          if (is.na(TYPE_USER)) {
            where_filter <- '(%s.&[%s] , %s.&[%s]  ) '
            where_filter <- sprintf(where_filter , SEGREGATION_VAR_INTERES, VAR_INTERES, SEGREGATION_EPS_CODE  , EPS )
          } else{
            where_filter <- '(  %s.&[%s], %s.&[%s] , [Tipo Usuario].[Tipo Usuario].&[%s]
                                         ) '
            where_filter <- sprintf(where_filter   ,SEGREGATION_VAR_INTERES, VAR_INTERES,SEGREGATION_EPS_CODE  , EPS, TYPE_USER )
          }
          
          mdx<- "SELECT NON EMPTY {%s} ON AXIS(0),
                               {%s.MEMBERS} ON AXIS(1), 
                               {%s.MEMBERS} ON AXIS(2) 
                        FROM [%s]
                        WHERE %s "
          
          mdx<- sprintf(mdx, AXIS0, AXIS1, AXIS2, from_olap_catalog, where_filter )  }
  return(mdx)
  
}


#Running query and cleaning empty observations
execue_query_mdx <- function(mdx,connection_string, EPS,VAR_INTERES, TYPE_USER  ){
  olapCnn<-olapR::OlapConnection(connection_string)
  tempo3 <- olapR::execute2D(olapCnn, mdx)
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