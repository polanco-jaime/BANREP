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



########################    Example of runing variable     ########################
###################################################################################

homogenizacion_eps <- function(tabla,Nombre_eps,Regimen_salud  ) {
  tabla$eps = tabla[[Nombre_eps]]
  tabla$regimen_ = tabla[[Regimen_salud]]
  
  tabla = sqldf("
            SELECT  *,
            CASE 
                --#ASOCIACIÓN INDÍGENA DEL CAUCA
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%AIC%' AND regimen_ LIKE '%CONT%'  THEN 'EPSIC3'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%AIC%' AND regimen_ LIKE '%SUB%'  THEN 'EPSI03'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%AIC%' THEN 'EPSI03'  
                --# ALIANSALUD ENTIDAD PROMOTORA DE SALUD
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%ALIANSALUD%' AND regimen_ LIKE '%CONT%'  THEN 'EPS001'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%ALIANSALUD%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS01'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%ALIANSALUD%' AND regimen_ IS NULL THEN 'EPSS01'  
                --# ASOCIACIÓN MUTUAL BARRIOS UNIDOS DE QUIBDÓ
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%AMBUQ%' AND regimen_ LIKE '%CONT%'  THEN 'ESSC76'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%AMBUQ%' AND regimen_ LIKE '%SUB%'  THEN 'ESS076'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%AMBUQ%' THEN 'ESS076'  
                --# 	ANASWAYUU
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%WAYUU%' AND regimen_ LIKE '%CONT%'  THEN 'EPSIC4'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%WAYUU%' AND regimen_ LIKE '%SUB%'  THEN 'EPSI04'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%WAYUU%' THEN 'EPSI04'
                --# ASOCIACIÓN MUTUAL LA ESPERANZA ASMET
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%ASMET%' AND regimen_ LIKE '%CONT%'  THEN 'ESSC62'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%ASMET%' AND regimen_ LIKE '%SUB%'  THEN 'ESS062'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%ASMET%' THEN 'ESS062'  
                --# CAJA DE COMPENSACION FAMILIAR CAFAM
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAFAM%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC18'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAFAM%' AND regimen_ LIKE '%SUB%'  THEN 'CCF018'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAFAM%' THEN 'CCF018'  
                --#CAFESALUD   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAFESALUD%' AND regimen_ LIKE '%CONT%'  THEN 'EPS003'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAFESALUD%' AND regimen_ LIKE '%SUB%'  THEN 'EPSM03'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAFESALUD%' THEN 'EPSM03'  
                --# CAJA DE DE COMPENSACION FAMILIAR  CAJACOPI ATLANTICO
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAJACOPI%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC55'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAJACOPI%' AND regimen_ LIKE '%SUB%'  THEN 'CCF055'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAJACOPI%' THEN 'CCF055'  
                --#CAPITAL SALUD   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAPITAL %' AND regimen_ LIKE '%CONT%'  THEN 'EPSC34'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAPITAL %' AND regimen_ LIKE '%SUB%'  THEN 'EPSS34'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAPITAL %' THEN 'EPSS34'  
                --#CAPRECOM    
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%NUEVA EPS (INCLUYE CAPRECOM)%'    THEN 'EPSS41'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAPRECOM%' AND regimen_ LIKE '%CONT%'  THEN 'EPS020'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAPRECOM%' AND regimen_ LIKE '%SUB%'  THEN 'EPSC20'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAPRECOM%' THEN 'EPSC20'  
                --#CAPRESOCA   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAPRESOCA%' AND regimen_ LIKE '%CONT%'  THEN 'EPSC25'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAPRESOCA%' AND regimen_ LIKE '%SUB%'  THEN 'EPS025'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CAPRESOCA%' THEN 'EPS025'  
                --#CCF DE NARINO
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CCF DE NARI%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC27'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CCF DE NARI%' AND regimen_ LIKE '%SUB%'  THEN 'CCF027'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CCF DE NARI%' THEN 'CCF027'   
                --#CCF DEL CHOCO    CAJA DE COMPENSACIÓN FAMILIAR DEL CHOCÓ 
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CCF DEL CHOCO%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC20'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFACHOC%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC20'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CCF DEL CHOCO%' AND regimen_ LIKE '%SUB%'  THEN 'CCF102'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFACHOC%' AND regimen_ LIKE '%SUB%'  THEN 'CCF102'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CCF DEL CHOCO%' THEN 'CCF102' 
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFACHOC%' THEN 'CCF102' 
                --#COLSUBSIDIO   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COLSUBSIDIO%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC10'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COLSUBSIDIO%' AND regimen_ LIKE '%SUB%'  THEN 'CCF010'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COLSUBSIDIO%' THEN 'CCF010' 
                --#CAJA DE COMPENSACIÓN FAMILIAR DE BOYACÁ COMFABOY   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFABOY%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC09'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFABOY%' AND regimen_ LIKE '%SUB%'  THEN 'CCF009'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFABOY%' THEN 'CCF009'            
                --#NUEVA EPS
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%NUEVA EPS%' AND regimen_ LIKE '%CONT%'  THEN 'EPS041'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%NUEVA EPS%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS41'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%NUEVA EPS%' THEN 'EPSS41' 
                --#	CAJA DE COMPENSACIÓN FAMILIAR DE CUNDINAMARCA COMFACUNDI
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFACUNDI%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC53'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFACUNDI%' AND regimen_ LIKE '%SUB%'  THEN 'CCF053'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFACUNDI%' THEN 'CCF053' 
                --#COMFACOR   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFACOR%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC15'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFACOR%' AND regimen_ LIKE '%SUB%'  THEN 'CCF015'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFACOR%' THEN 'CCF015' 
                --#COMFAMILIAR CARTAGENA   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR CART%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC07'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR CART%' AND regimen_ LIKE '%SUB%'  THEN 'CCF007'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR CART%' THEN 'CCF007' 
                --#COMFAMILIAR CHOCO   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR CHOCO%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC20'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR CHOCO%' AND regimen_ LIKE '%SUB%'  THEN 'CCF102'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR CHOCO%' THEN 'CCF102' 
                --#COMFAMILIAR GUAJIRA   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR GUAJIRA%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC23'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR GUAJIRA%' AND regimen_ LIKE '%SUB%'  THEN 'CCF023'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR GUAJIRA%' THEN 'CCF023' 
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIARGUAJIRA%' THEN 'CCF023' 
                --#COMFAMILIAR HUILA   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR HUILA%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC24'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR HUILA%' AND regimen_ LIKE '%SUB%'  THEN 'CCF024'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR HUILA%' THEN 'CCF024' 
                --#COMFAMILIAR NAR   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR NAR%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC27'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR NAR%' AND regimen_ LIKE '%SUB%'  THEN 'CCF027'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAMILIAR NAR%' THEN 'CCF027' 
                --#COMFAORIENTE   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAORIENTE%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC50'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAORIENTE%' AND regimen_ LIKE '%SUB%'  THEN 'CCF050'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFAORIENTE%' THEN 'CCF050' 
                --#COMFASUCRE   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFASUCRE%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC33'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFASUCRE%' AND regimen_ LIKE '%SUB%'  THEN 'CCF033'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFASUCRE%' THEN 'CCF033' 
                --#COMFENALCO ANTIOQUIA   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFENALCO ANTIOQUIA%' AND regimen_ LIKE '%CONT%'  THEN 'CCFC02'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFENALCO ANTIOQUIA%' AND regimen_ LIKE '%SUB%'  THEN 'CCF002'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFENALCO ANTIOQUIA%' THEN 'CCF002' 
                --#COMFENALCO VAL   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFENALCO VAL%' AND regimen_ LIKE '%CONT%'  THEN 'EPS012'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFENALCO VAL%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS12'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFENALCO VAL%' THEN 'EPSS12' 
                --#COMPARTA   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMPARTA%' AND regimen_ LIKE '%CONT%'  THEN 'ESSC33'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMPARTA%' AND regimen_ LIKE '%SUB%'  THEN 'ESS133'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMPARTA%' THEN 'ESS133' 
                --#COMPENSAR   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMPENSAR%' AND regimen_ LIKE '%CONT%'  THEN 'EPS008'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMPENSAR%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS08'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMPENSAR%' THEN 'EPSS08' 
                --#CONVIDA   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CONVIDA%' AND regimen_ LIKE '%CONT%'  THEN 'EPSC22'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CONVIDA%' AND regimen_ LIKE '%SUB%'  THEN 'EPS022'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CONVIDA%' THEN 'EPS022' 
                --#COOMEVA   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COOMEVA%' AND regimen_ LIKE '%CONT%'  THEN 'EPS016'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COOMEVA%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS16'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COOMEVA%' THEN 'EPSS16' 
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CONMEVA%' THEN 'EPSS16' 
                --#COOSALUD   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COOSALUD%' AND regimen_ LIKE '%CONT%'  THEN 'EPS042'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COOSALUD%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS42'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COOSALUD%' THEN 'EPSS42' 
                --#CRUZ BLANCA   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CRUZ BLANCA%' AND regimen_ LIKE '%CONT%'  THEN 'EPS023'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CRUZ BLANCA%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS23'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%CRUZ BLANCA%' THEN 'EPSS23' 
                --#DUSAKAW   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%DUSAKAW%' AND regimen_ LIKE '%CONT%'  THEN 'EPSIC1'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%DUSAKAW%' AND regimen_ LIKE '%SUB%'  THEN 'EPSI01'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%DUSAKAW%' THEN 'EPSI01' 
                --#ECOOPSOS   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%ECOOPSOS%' AND regimen_ LIKE '%CONT%'  THEN 'ESSC91'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%ECOOPSOS%' AND regimen_ LIKE '%SUB%'  THEN 'ESS091'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%ECOOPSOS%' THEN 'ESS091' 
                --#EMDISALUD   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%EMDISALUD%' AND regimen_ LIKE '%CONT%'  THEN 'ESSC02'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%EMDISALUD%' AND regimen_ LIKE '%SUB%'  THEN 'ESS002'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%EMDISALUD%' THEN 'ESS002' 
                --#EMSSANAR   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%EMSSANAR%' AND regimen_ LIKE '%CONT%'  THEN 'ESSC18'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%EMSSANAR%' AND regimen_ LIKE '%SUB%'  THEN 'ESS118'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%EMSSANAR%' THEN 'ESS118' 
                --#SANITAS   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SANITAS%' AND regimen_ LIKE '%CONT%'  THEN 'EPS005'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SANITAS%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS05'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SANITAS%' THEN 'EPSS05' 
                --#SOS   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '% SOS%' AND regimen_ LIKE '%CONT%'  THEN 'EPS018'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '% SOS%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS18'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '% SOS%' THEN 'EPSS18' 
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE 'SOS%' THEN 'EPSS18' 
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE 'SOS' THEN 'EPSS18'         
                --#SURA   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SURA%' AND regimen_ LIKE '%CONT%'  THEN 'EPS010'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SURA%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS10'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SURA%' THEN 'EPSS10' 
                --#CCF045    COMFANORTE
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFANORTE%' AND regimen_ LIKE '%CONT%'  THEN 'CCF045'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFANORTE%' AND regimen_ LIKE '%SUB%'  THEN 'CCF045'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%COMFANORTE%' THEN 'CCF045' 
                --#FAMISANAR   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%FAMISANAR%' AND regimen_ LIKE '%CONT%'  THEN 'EPS017'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%FAMISANAR%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS17'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%FAMISANAR%' THEN 'EPSS17' 
                --#FERROCARRILES   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%FERROCARRILES%' THEN 'EAS027' 
                --#GOLDEN    
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%GOLDEN%' AND regimen_ LIKE '%CONT%'  THEN 'EPS039'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%GOLDEN%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS39'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%GOLDEN%' THEN 'EPSS39' 
                --#HUMANA VIVIR    
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%HUMANA VIVIR%' AND regimen_ LIKE '%CONT%'  THEN 'EPS014'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%HUMANA VIVIR%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS14'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%HUMANA VIVIR%' THEN 'EPSS14' 
                --#MALLAMAS   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%MALLAMAS%' AND regimen_ LIKE '%CONT%'  THEN 'EPSIC5'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%MALLAMAS%' AND regimen_ LIKE '%SUB%'  THEN 'EPSI05'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%MALLAMAS%' THEN 'EPSI05' 
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%MUTUAL%' THEN 'ESS115' 
                --#MANEXKA   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%MANEXKA%' AND regimen_ LIKE '%CONT%'  THEN 'EPSIC2'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%MANEXKA%' AND regimen_ LIKE '%SUB%'  THEN 'EPSI02'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%MANEXKA%' THEN 'EPSI02' 
                --#PIJAOS   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%PIJAOS%' AND regimen_ LIKE '%CONT%'  THEN 'EPSIC6'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%PIJAOS%' AND regimen_ LIKE '%SUB%'  THEN 'EPSI06'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%PIJAOS%' THEN 'EPSI06' 
                --#TOTAL   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%TOTAL%' AND regimen_ LIKE '%CONT%'  THEN 'EPS002'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%TOTAL%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS02'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%TOTAL%' THEN 'EPSS02'
                --#SALUDVIDA   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SALUDVIDA%' AND regimen_ LIKE '%CONT%'  THEN 'EPSC33'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SALUDVIDA%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS33'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SALUDVIDA%' THEN 'EPSS33' 
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SALUDIVA%' THEN 'EPSS33' 
                --#SAVIA    
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SAVIA%' AND regimen_ LIKE '%CONT%'  THEN 'EPS040'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SAVIA%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS40'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SAVIA%' THEN 'EPSS40' 
                --#MEDIMAS    
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%MEDIMAS%' AND regimen_ LIKE '%CONT%'  THEN 'EPS044'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%MEDIMAS%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS45'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%MEDIMAS%' THEN 'EPS044' 
                --#SALUDCOOP    
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SALUDCOOP%' AND regimen_ LIKE '%CONT%'  THEN 'EPS013'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SALUDCOOP%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS13'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SALUDCOOP%' THEN 'EPSS13' 
                --#SOLSALUD    
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SOLSALUD%' AND regimen_ LIKE '%CONT%'  THEN 'EPS026'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SOLSALUD%' AND regimen_ LIKE '%SUB%'  THEN 'EPSS26'
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SOLSALUD%' THEN 'EPSS26' 
                
                --#SELVASALUD   
                WHEN REPLACE(UPPER(eps), '.', '')  LIKE '%SELVASALUD%' THEN 'EPS031' 
                
                ELSE NULL  
            END AS  EPS_CODE
            FROM tabla
            ORDER BY 1
             ")
  return(tabla)
}


query_olap <- function(AXIS0,AXIS1, AXIS2,from_olap_catalog,where_filter   ){
  
  mdx<- "SELECT  {%s} ON AXIS(0),
                               {%s.MEMBERS} ON AXIS(1) , 
                               {%s.MEMBERS} ON AXIS(2) 
                        FROM [%s]
                        WHERE (%s) "
  mdx<- sprintf(mdx, AXIS0, AXIS1,  AXIS2, from_olap_catalog, where_filter )
  return(mdx)
}

run_query_olap <- function(cnnstr, mdx,var1,  var2 ){
  olapCnn<-olapR::OlapConnection(cnnstr)
  temp <- olapR::execute2D(olapCnn, mdx)
  temp <- subset(temp, is.na(temp[[3]])==F  )
  temp$var1 = var1
  temp$var2 = var2
  return(temp)
}



correction_morbilidad <- function(TABLA , VARIABLE ){
  SQL = "
          SELECT CASE WHEN years = 2022 THEN %s/7 
              WHEN years  != 2022 THEN   %s/12
              ELSE null END AS %s FROM %s 
        "
  return( sqldf(sprintf(SQL, VARIABLE, VARIABLE, VARIABLE, TABLA )) )
}

table_wo_na <- function(base , cols_number ){
  base <- base[ ,cols_number ]
  base <- na.omit(base)
  return(base)
}

## function
na_by_cols <- function(base  ){
  library(dplyr)
  library(purrr)
  library(tidyr)
  filas = nrow(base)
  base <- base %>% map_df(function(x) sum(is.na(x))/filas)
  # %>% # gather(feature, num_nulls) #%>%   print(n = 208)
  return(base)
}



frac_nulls_eps <- function(df, column_name){
  Nulos = data.frame()
  for (i  in unique(df[[column_name]]) ) {
    temp = subset(df, df[[column_name]] ==i)
    nulos = na_by_cols(temp)
    nulos =  as.data.frame(t(nulos))
    colnames(nulos) = nulos[1,]
    nulos  = nulos[-1,]
    nulos$EPS_ =  i
    # temp = subset(temp, is.na(temp$years) == F  )
    rownames(nulos) <- NULL
    Nulos = rbind(nulos, Nulos)
    
  }
  return(Nulos)
}


### drop space and some characters from a string
drop_characters = function(string){
  library(stringi)
  string=  gsub( gsub(string, pattern = ' ', replacement =  '_'), pattern = '[.]', replacement = '_')
  string=  gsub(string, pattern = '__', replacement =  '_')
  string=  gsub(string, pattern = '__', replacement =  '_')
  string=  gsub(string, pattern = '-', replacement =  '_')
  string=  gsub(string, pattern = '[()]', replacement =  '')
  string=  gsub(string, pattern = ',', replacement =  '')
  string= stri_trans_general(string,"Latin-ASCII")
  return(string)
}

drop_characters("D_Otras_enfermedades_endocrinas,_metabolicas,_hematologicas_e_immunologicas")

############################################# 
eps_homog <- function(Tabla , CODIGO__EPS ){
  
  if ('CODIGO__EPS' %in% colnames(Tabla)) {
    colnames(Tabla['CODIGO__EPS'] ) = 'CODIGO__EPS_'
    Tabla$CODIGO__EPS = Tabla[[CODIGO__EPS]]
  }else{
    Tabla$CODIGO__EPS = Tabla[[CODIGO__EPS]]
  }
  
  
  library(sqldf)
  
  if ('eps_status' %in% colnames(Tabla)) {
    colnames(Tabla['eps_status'] ) = 'eps__status'
  }
  if ('homo_code_eps' %in% colnames(Tabla)) {
    colnames(Tabla['homo_code_eps'] ) = 'homo__code_eps'
  }
  
  query =  "
  SELECT * , 
  CASE WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF001'	THEN 'CCF001'	 									
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF002'	THEN 'CCF002'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC02'	THEN 'CCF002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF002' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF007'	THEN 'CCF007'	WHEN   REPLACE(CODIGO__EPS, ' ', '') = 	'CCFC07'	THEN 'CCF007' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF007' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF007' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF009'	THEN 'CCF009'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC09'	THEN 'CCF009' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF009' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF009' 
       -- WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF010'	THEN 'CCF010'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC10'	THEN 'CCF010' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF010' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF010' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF015'	THEN 'CCF015'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC15'	THEN 'CCF015' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF015' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF015' 
       -- WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF018'	THEN 'CCF018'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC18'	THEN 'CCF018' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF018' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF018' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF023'	THEN 'CCF023'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC23'	THEN 'CCF023' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF023' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF023' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF024'	THEN 'CCF024'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC24'	THEN 'CCF024' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF024' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF024' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF027'	THEN 'CCF027'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC27'	THEN 'CCF027' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF027' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF027' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF028'	THEN 'CCF028'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF028' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF028' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF028' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF029'	THEN 'CCF029'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF029' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF029' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF029' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF031'	THEN 'CCF031'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF031' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF031' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF031' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF032'	THEN 'CCF032'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF032' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF032' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF032' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF033'	THEN 'CCF033'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC33'	THEN 'CCF033' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF033' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF033' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF035'	THEN 'CCF035'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF035' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF035' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF035' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF037'	THEN 'CCF037'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF037' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF037' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF037' 
        --WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF040'	THEN 'CCF040'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF040' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF040' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF040' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF045'	THEN 'CCF045'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF045' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF045' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF045' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF050'	THEN 'CCF050'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC50'	THEN 'CCF050' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF050' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF050' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF053'	THEN 'CCF053'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC53'	THEN 'CCF053' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF053' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF053' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF054'	THEN 'CCF054'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF054' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF054' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF054' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF055'	THEN 'CCF055'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC55'	THEN 'CCF055' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF055' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF055' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF102'	THEN 'CCF102'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC20'	THEN 'CCF102' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF102' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF102' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF103'	THEN 'CCF103'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF103' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF103' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF103' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EAS016'	THEN 'EAS016'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EAS016' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EAS016' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EAS016' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EAS027'	THEN 'EAS027'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EAS027' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EAS027' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EAS027' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS001'	THEN 'EPS001'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS01'	THEN 'EPS001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS001' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS002'	THEN 'EPS002'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS02'	THEN 'EPS002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS002' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS003'	THEN 'EPS003'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS03'	THEN 'EPS003' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =		'EPSM03'	THEN 'EPS003' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSC03'	THEN 'EPS003' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS005'	THEN 'EPS005'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS05'	THEN 'EPS005' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS005' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS005' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS006'	THEN 'EPS006'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS006' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS006' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS006' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS008'	THEN 'EPS008'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS08'	THEN 'EPS008' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS008' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS008' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS009'	THEN 'EPS009'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS009' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS009' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS009' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS010'	THEN 'EPS010'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS10'	THEN 'EPS010' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS010' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS010' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS012'	THEN 'EPS012'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS12'	THEN 'EPS012' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS012' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS012' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS013'	THEN 'EPS013'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS13'	THEN 'EPS013' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS013' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS013' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS014'	THEN 'EPS014'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS14'	THEN 'EPS014' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS014' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS014' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS015'	THEN 'EPS015'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS015' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS015' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS015' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS016'	THEN 'EPS016'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS16'	THEN 'EPS016' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS016' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS016' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS017'	THEN 'EPS017'	
              WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS17'	THEN 'EPS017' 	
              WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCF010'	THEN 'EPS017' 
              WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC10'	THEN 'EPS017' 
              WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCF018'	THEN 'EPS017' 
              WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC18'	THEN 'EPS017' 
              
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS018'	THEN 'EPS018'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS18'	THEN 'EPS018' 	  WHEN   REPLACE(CODIGO__EPS, ' ', '') =		'CCF040'	THEN 'EPS018' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS018' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS020'	THEN 'EPS020'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSC20'	THEN 'EPS020' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS020' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS020' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS022'	THEN 'EPS022'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSC22'	THEN 'EPS022' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS022' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS022' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS023'	THEN 'EPS023'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS23'	THEN 'EPS023' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS023' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS023' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS025'	THEN 'EPS025'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSC25'	THEN 'EPS025' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS025' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS025' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS026'	THEN 'EPS026'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS26'	THEN 'EPS026' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS026' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS026' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS028'	THEN 'EPS028'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS028' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS028' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS028' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS030'	THEN 'EPS030'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS030' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS030' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS030' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS031'	THEN 'EPS031'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS031' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS031' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS031' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS033'	THEN 'EPS033'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSC33'	THEN 'EPS033' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =		'EPSS33'	THEN 'EPS033' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSM33'	THEN 'EPS033' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS034'	THEN 'EPS034'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS034' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS034' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS034' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS035'	THEN 'EPS035'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS035' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS035' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS035' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS037'	THEN 'EPS037'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS37'	THEN 'EPS037' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =		'EPS041'	THEN 'EPS037' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS41'	THEN 'EPS037' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS038'	THEN 'EPS038'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS038' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS038' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS038' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS039'	THEN 'EPS039'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS39'	THEN 'EPS039' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS039' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS039' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS042'	THEN 'EPS042'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS42'	THEN 'EPS042' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS042' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS042' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS044'	THEN 'EPS044'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS44'	THEN 'EPS044' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =		'EPS045'	THEN 'EPS044' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS45'	THEN 'EPS044' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS046'	THEN 'EPS046'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS46'	THEN 'EPS046' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS046' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS046' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS048'	THEN 'EPS048'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS48'	THEN 'EPS048' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS048' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS048' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSI01'	THEN 'EPSI01'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSIC1'	THEN 'EPSI01' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSI01' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSI01' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSI02'	THEN 'EPSI02'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSIC2'	THEN 'EPSI02' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSI02' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSI02' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSI03'	THEN 'EPSI03'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSIC3'	THEN 'EPSI03' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSI03' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSI03' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSI04'	THEN 'EPSI04'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSIC4'	THEN 'EPSI04' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSI04' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSI04' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSI05'	THEN 'EPSI05'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSIC5'	THEN 'EPSI05' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSI05' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSI05' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSI06'	THEN 'EPSI06'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSIC6'	THEN 'EPSI06' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSI06' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSI06' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSS34'	THEN 'EPSS34'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSC34'	THEN 'EPSS34' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSS34' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSS34' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSS40'	THEN 'EPSS40'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPS040'	THEN 'EPSS40' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSS40' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSS40' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS002'	THEN 'ESS002'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC02'	THEN 'ESS002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS002' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS024'	THEN 'ESS024'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC24'	THEN 'ESS024' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS024' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS024' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS062'	THEN 'ESS062'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC62'	THEN 'ESS062' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS062' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS062' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS068'	THEN 'ESS068'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS068' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS068' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS068' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS076'	THEN 'ESS076'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC76'	THEN 'ESS076' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS076' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS076' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS091'	THEN 'ESS091'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC91'	THEN 'ESS091' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS091' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS091' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS115'	THEN 'ESS115'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS115' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS115' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS115' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS118'	THEN 'ESS118'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC18'	THEN 'ESS118' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS118' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS118' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS133'	THEN 'ESS133'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC33'	THEN 'ESS133' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS133' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS133' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS207'	THEN 'ESS207'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC07'	THEN 'ESS207' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS207' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS207' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'FMS001'	THEN 'FMS001'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'FMS001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'FMS001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'FMS001' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'POL001'	THEN 'POL001'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'POL001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'POL001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'POL001' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES002'	THEN 'RES002'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES002' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES004'	THEN 'RES004'	 WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'REMG01'	THEN 'RES004' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES004' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES004' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES005'	THEN 'RES005'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES005' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES005' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES005' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES006'	THEN 'RES006'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES006' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES006' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES006' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES007'	THEN 'RES007'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES007' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES007' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES007' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES009'	THEN 'RES009'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES009' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES009' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES009' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES010'	THEN 'RES010'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES010' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES010' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES010' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES011'	THEN 'RES011'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES011' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES011' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES011' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES012'	THEN 'RES012'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES012' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES012' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES012' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES013'	THEN 'RES013'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES013' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES013' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES013' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES014'	THEN 'RES014'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES014' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES014' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES014' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'UT-001'	THEN 'UT-001'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'UT-001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-001' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'UT-002'	THEN 'UT-002'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'UT-002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-002' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'UT-003'	THEN 'UT-003'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-003' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'UT-003' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-003' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'UT-004'	THEN 'UT-004'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-004' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'UT-004' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-004' 
        ELSE 'NO DEFINIDO' END  AS homo_code_eps 
             
FROM Tabla
  "
  Tabla <- sqldf::sqldf(query)
  Tabla <- dplyr::select(Tabla, -'CODIGO__EPS')
  return( Tabla )
}
#
eps_quiebra <- function(Tabla,CODIGO__EPS ){
  ################################################
  if ('CODIGO__EPS' %in% colnames(Tabla)) {
    colnames(Tabla['CODIGO__EPS'] ) = 'CODIGO__EPS_'
    Tabla$CODIGO__EPS = Tabla[[CODIGO__EPS]]
  }else{
    Tabla$CODIGO__EPS = Tabla[[CODIGO__EPS]]
  }
  
  library(sqldf)
  ################################################
  if ('eps__status' %in% colnames(Tabla)) {
    colnames(Tabla['eps__status'] ) = 'eps_status_'
  }
  
  if ('eps_status' %in% colnames(Tabla)) {
    colnames(Tabla['eps_status'] ) = 'eps__status_'
  }
  
  ################################################
  
  query = " SELECT * ,
  CASE WHEN UPPER(
                  CODIGO__EPS) LIKE  '%EPS001%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS01%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSI04%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSIC4%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSIC1%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSI03%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSIC3%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSI01%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESS076%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESSC76%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESS118%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESSC18%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESS115%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESS062%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESSC62%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS048%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS48%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESS207%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESSC07%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESS068%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS03%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS003%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSM03%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF007%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC07%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF018%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC18%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC50%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF050%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF001%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF002%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC02%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF035%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF009%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC09%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF015%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC15%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF053%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC53%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF054%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF037%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF028%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF032%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF023%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC23%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF027%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC27%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF033%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC33%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF103%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF102%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC20%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF024%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC24%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF045%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF055%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC55%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF031%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%UT-001%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS028%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS34%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSC34%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS020%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSC20%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS025%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSC25%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF010%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCFC10%' THEN '0'
                 -- WHEN UPPER(CODIGO__EPS) LIKE  '%CCF040%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%CCF029%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%UT-002%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS012%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS12%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS008%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS08%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%UT-004%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%UT-003%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS016%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS16%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESS133%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESSC33%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS042%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS42%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESS024%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESSC24%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS023%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS23%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%RES002%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESS002%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESSC02%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EAS016%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESS091%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%ESSC91%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS030%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS022%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSC22%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS017%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS17%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS013%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS13%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS005%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS05%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS009%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS018%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS18%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS010%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS10%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EAS027%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%FMS001%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS46%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS046%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS039%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS39%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS14%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS014%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%RES006%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS006%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS037%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS041%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%RES004%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSI05%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSIC5%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSI02%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSIC2%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS044%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS44%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS045%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS45%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS038%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS41%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS37%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSI06%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSIC6%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%POL001%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS035%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS015%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS002%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS02%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS034%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS33%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSC33%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS033%' THEN '1'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS40%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS040%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS031%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPSS26%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%EPS026%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%RES011%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%RES005%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%RES010%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%RES009%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%RES012%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%RES013%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%RES014%' THEN '0'
                  WHEN UPPER(CODIGO__EPS) LIKE  '%RES007%' THEN '0'
                  ELSE NULL END eps_status
                  FROM Tabla
  "  
  Tabla <- sqldf::sqldf(query)
  Tabla <- dplyr::select(Tabla, -'CODIGO__EPS')
  return( Tabla )
}
#############################################




backwardElimination <- function(tabla, Y = "", sl = 0.05) {
  
  # Y = 'eps_status'
  var = Y
  tabla$Y = tabla[[Y]]
  tabla = dplyr::select(tabla, -var)
   
  
  numVars = ncol(tabla)
  
  for (i in c(1:numVars)){
    tryCatch( {
    tabla_wo_na <- na.omit(tabla)
    regressor = lm(data = tabla_wo_na, formula =  Y ~ .)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = names(which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar))
      tabla = tabla %>% dplyr::select(-j)
    }
    numVars = numVars - 1
    print(summary(regressor))
    }, error=function(e){cat("ERROR CATCH: ",conditionMessage(e), "\n")}  )
  }
  
  return(  list('regresion' = summary(regressor) ,  
                'columnas' = names(regressor$coefficients)[2: length(names(regressor$coefficients))] ))
}
######################################################
####  Aggregate funciton ###

aggregate_function = function(Tabla, 
                              aggregate = 'SUM',
                              cols_to_agg, 
                              group_by) {
  
  BASE = "SELECT "
  Base = ''
  for (i in group_by) {
    Base = paste0(Base,' ' ,i , ', ') 
  }
  BASE = paste0(BASE,Base ) 
  
  Body = ''
  for (i in cols_to_agg) {
    Body =  paste0(Body, aggregate,'(',i, ') as ', i , ' , '  )
  }
  Body =  substr(Body,1,  nchar(Body)-2 )
  
  TAIL_query = ''
  for (i in group_by) {
    TAIL_query = paste0(TAIL_query,' ' ,i , ', ') 
  }
  TAIL_query =  substr(TAIL_query,1,  nchar(TAIL_query)-2 )
  
  SQL_query = paste0(BASE,Body , ' FROM Tabla GROUP BY ' , TAIL_query)
  print(SQL_query)
  return(sqldf::sqldf(SQL_query) )


}

## Last contribution
warning("The functions created for Banrep have been loaded")
print("The functions created for Banrep have been loaded")
