EPS_CODE = '[Administradoras].[Codigo de Administradora]'
SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]'
AXIS0 <- '[Measures].[ValorIndicador]'
AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[Año]'
AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'
from_olap_catalog = ''
 

query_cube_mdx(AXIS0 = AXIS0, 
               AXIS1 = AXIS1,
               AXIS2 = AXIS2,
               cube = from_olap_catalog,
               )



AXIS0 <- '[Measures].[ValorIndicador]'
AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[Año]'
AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'
where_filter <- '([Causas de Morbilidad].[Gran Causa].&[%s] ,
                             [Administradoras].[Codigo de Administradora].&[%s]
                             , [Tipo Usuario].[Tipo Usuario].&[%s]
                             ) '
where_filter <- sprintf(where_filter   ,GRAN_CAUSA, EPS, TYPE_USER )
mdx<- "SELECT {%s} ON AXIS(0),
                   {%s.MEMBERS} ON AXIS(1), 
                   {%s.MEMBERS} ON AXIS(2) 
            FROM [%s]
            WHERE %s "

mdx<- sprintf(mdx, AXIS0, AXIS1, AXIS2, from_olap_catalog, where_filter ) 




query_cube_mdx <- function(  AXIS0 ,AXIS1 ,AXIS2 ,
                         TYPE_USER=NULL,
                         SEGREGATION_VAR_INTERES=NULL,
                         VAR_INTERES=NULL, 
                         SEGREGATION_EPS_CODE=NULL  , 
                         EPS=NULL, 
                         cube ){
  
  if (is.na(SEGREGATION_VAR_INTERES) ) {
    # mdx<- "SELECT {%s} ON AXIS(0), 
    #                      {%s.MEMBERS} ON AXIS(1), 
    #                      {%s.MEMBERS} ON AXIS(2) 
    #               FROM [%s]
    #               "
    # mdx<- sprintf(mdx, AXIS0, AXIS1, AXIS2, from_olap_catalog ) 
    print("toma el primer")
  } else{
    print("ASDHKJASD")
          # if (is.na(TYPE_USER)) {
          #   where_filter <- '(%s.&[%s] , %s.&[%s]  ) '
          #   where_filter <- sprintf(where_filter , SEGREGATION_VAR_INTERES, VAR_INTERES, SEGREGATION_EPS_CODE  , EPS )
          # } else{
          #   where_filter <- '(%s.&[%s], %s.&[%s] , [Tipo Usuario].[Tipo Usuario].&[%s]
          #                                ) '
          #   where_filter <- sprintf(where_filter   ,SEGREGATION_VAR_INTERES, VAR_INTERES,SEGREGATION_EPS_CODE  , EPS, TYPE_USER )
          # }
          # 
          # mdx<- "SELECT {%s} ON AXIS(0),
          #                      {%s.MEMBERS} ON AXIS(1), 
          #                      {%s.MEMBERS} ON AXIS(2) 
          #               FROM [%s]
          #               WHERE %s "
          # 
          # mdx<- sprintf(mdx, AXIS0, AXIS1, AXIS2, from_olap_catalog, where_filter ) 
  }
  # return(mdx)
  
}
