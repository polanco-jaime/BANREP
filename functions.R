#################################
EPS = "RES014"
SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
EPS_CODE = '[Administradoras].[Codigo de Administradora]'
SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]'
AXIS0 <- '[Measures].[ValorIndicador]'
AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[Año]'
AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'
from_olap_catalog = 'CU - Morbilidad_ASIS'
TYPE_USER = '1 - CONTRIBUTIVO'
VAR_INTERES = 'Condiciones transmisibles y nutricionales'
query_cube_mdx(AXIS0 = AXIS0, 
               AXIS1 = AXIS1,
               AXIS2 = AXIS2,
               cube = from_olap_catalog,
               )

mdx = query_cube_mdx(AXIS0 = AXIS0, 
               AXIS1 = AXIS1,
               AXIS2 = AXIS2,
               TYPE_USER=TYPE_USER,
               SEGREGATION_VAR_INTERES=SEGREGATION_VAR_INTERES ,
               VAR_INTERES=VAR_INTERES, 
               SEGREGATION_EPS_CODE=SEGREGATION_EPS_CODE  , 
               EPS=EPS,  
               cube = from_olap_catalog
)

execue_query_mdx <- function(mdx, )}{
  olapCnn<-olapR::OlapConnection(cnnstr)
  tempo3 <- olapR::execute2D(olapCnn, mdx)
  tempo3[[2]] < - as.numeric(tempo3[[2]])
  tempo3 = subset(tempo3, tempo3[[2]] >= 2009 &  tempo3[[2]]  <=2022)
  colnames(tempo3) <- c('CITY', 'YEAR', 'VALUE')
  tempo3$EPS <- EPS
  tempo3$GRAN_CAUSA <- as.character(GRAN_CAUSA)
  tempo3$TYPE_USER <- as.character(TYPE_USER)
}



execute2D(olapCnn, mdx)
 
 

query_cube_mdx <- function(  AXIS0 ,AXIS1 ,AXIS2 ,
                         TYPE_USER=NULL,
                         SEGREGATION_VAR_INTERES='' ,
                         VAR_INTERES=NULL, 
                         SEGREGATION_EPS_CODE=NULL  , 
                         EPS=NULL, 
                         cube ){
  
      if ( isTRUE( SEGREGATION_VAR_INTERES=='' ) ) {
                mdx<- "SELECT {%s} ON AXIS(0),
                                     {%s.MEMBERS} ON AXIS(1),
                                     {%s.MEMBERS} ON AXIS(2)
                              FROM [%s]
                              "
                mdx<- sprintf(mdx, AXIS0, AXIS1, AXIS2, from_olap_catalog )} else{
          if (is.na(TYPE_USER)) {
            where_filter <- '(%s.&[%s] , %s.&[%s]  ) '
            where_filter <- sprintf(where_filter , SEGREGATION_VAR_INTERES, VAR_INTERES, SEGREGATION_EPS_CODE  , EPS )
          } else{
            where_filter <- '(%s.&[%s], %s.&[%s] , [Tipo Usuario].[Tipo Usuario].&[%s]
                                         ) '
            where_filter <- sprintf(where_filter   ,SEGREGATION_VAR_INTERES, VAR_INTERES,SEGREGATION_EPS_CODE  , EPS, TYPE_USER )
          }
          
          mdx<- "SELECT {%s} ON AXIS(0),
                               {%s.MEMBERS} ON AXIS(1), 
                               {%s.MEMBERS} ON AXIS(2) 
                        FROM [%s]
                        WHERE %s "
          
          mdx<- sprintf(mdx, AXIS0, AXIS1, AXIS2, from_olap_catalog, where_filter )  }
  return(mdx)
  
}


