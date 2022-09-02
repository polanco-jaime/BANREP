

execue_query_mdx <- function(mdx,connection_string, EPS,VAR_INTERES, TYPE_USER  ){
  olapCnn<-olapR::OlapConnection(connection_string)
  tempo3 <- olapR::execute2D(olapCnn, mdx)
  tempo3[[2]] < - as.numeric(tempo3[[2]])
  tempo3 = subset(tempo3, tempo3[[2]] >= 2009 &  tempo3[[2]]  <=2022)
  tempo3 = subset(tempo3, is.na(tempo3[[3]]) == F)
  colnames(tempo3) <- c('CITY', 'YEAR', 'VALUE')
  tempo3$EPS <- EPS
  tempo3$VAR_INTERES <- as.character(VAR_INTERES)
  tempo3$TYPE_USER <- as.character(TYPE_USER)
  return(tempo3)
}



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


