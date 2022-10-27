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
  base <- base %>%
  map_df(function(x) sum(is.na(x))/filas) %>%
  gather(feature, num_nulls) #%>%   print(n = 208)
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

eps_from_code_to_name_n_status <- function(Tabla, eps_code ){
  library(sqldf)
  Tabla$CODIGO_de_eps = Tabla[[eps_code]]
  query =  "SELECT *, CASE WHEN 
                UPPER(CODIGO_de_eps)  LIKE  '%EPS001%' THEN 'ALIANSALUD ENTIDAD PROMOTORA DE SALUD SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS01%' THEN 'ALIANSALUD ENTIDAD PROMOTORA DE SALUD SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSI04%' THEN 'ANASWAYUU'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSIC4%' THEN 'ANASWAYUU'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSIC1%' THEN 'ASOCIACIÓN INDÍGENA DEL CESAR Y LA GUAJIRA DUSAKAWI-CM'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSI03%' THEN 'ASOCIACIÓN INDÍGENA DEL CAUCA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSIC3%' THEN 'ASOCIACIÓN INDÍGENA DEL CAUCA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSI01%' THEN 'ASOCIACIÓN INDÍGENA DEL CESAR Y LA GUAJIRA  DUSAKAWI'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS076%' THEN 'ASOCIACIÓN MUTUAL BARRIOS UNIDOS DE QUIBDÓ ESS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC76%' THEN 'ASOCIACIÓN MUTUAL BARRIOS UNIDOS DE QUIBDÓ ESS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS118%' THEN 'ASOCIACIÓN MUTUAL EMPRESA SOLIDARIA DE SALUD DE NARIÑO ESS EMSSANAR ESS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC18%' THEN 'ASOCIACIÓN MUTUAL EMPRESA SOLIDARIA DE SALUD DE NARIÑO ESS EMSSANAR ESS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS115%' THEN 'ASOCIACIÓN MUTUAL ESS MALLAMAS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS062%' THEN 'ASOCIACIÓN MUTUAL LA ESPERANZA ASMET  SALUD'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC62%' THEN 'ASOCIACIÓN MUTUAL LA ESPERANZA ASMET  SALUD-CM'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS048%' THEN 'ASOCIACION MUTUAL SER EMPRESA SOLIDARIA DE SALUD EPS-S MUTUAL SER EPS-S'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS48%' THEN 'ASOCIACION MUTUAL SER EMPRESA SOLIDARIA DE SALUD EPS-S MUTUAL SER EPS-S'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS207%' THEN 'ASOCIACIÓN MUTUAL SER EMPRESA SOLIDARIA DE SALUD ESS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC07%' THEN 'ASOCIACIÓN MUTUAL SER EMPRESA SOLIDARIA DE SALUD ESS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS068%' THEN 'ASOCIACIÓN SOLIDARIA DE SALUD DE ASTREA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS03%' THEN 'CAFESALUD'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS003%' THEN 'CAFESALUD'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSM03%' THEN 'CAFESALUD'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF007%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR  DE CARTAGENA \"COMFAMILIAR CARTAGENA\"'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC07%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR  DE CARTAGENA \"COMFAMILIAR CARTAGENA\"'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF018%' THEN 'CAJA DE COMPENSACION FAMILIAR CAFAM'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC18%' THEN 'CAJA DE COMPENSACION FAMILIAR CAFAM'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC50%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR CCF DEL ORIENTE COLOMBIANO - COMFAORIENTE -CM'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF050%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR CCF DEL ORIENTE COLOMBIANO - COMFAORIENTE -CM'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF001%' THEN 'CAJA DE COMPENSACION FAMILIAR COMFAMILIAR  CAMACOLCOMFAMILIAR CAMACOL'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF002%' THEN 'CAJA DE COMPENSACION FAMILIAR DE ANTIOQUIA  COMFAMA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC02%' THEN 'CAJA DE COMPENSACION FAMILIAR DE ANTIOQUIA  COMFAMA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF035%' THEN 'CAJA DE COMPENSACION FAMILIAR DE BARRANCABERMEJA CAFABA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF009%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DE BOYACÁ COMFABOY'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC09%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DE BOYACÁ COMFABOY'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF015%' THEN 'CAJA DE COMPENSACION FAMILIAR DE CORDOBA  COMFACOR'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC15%' THEN 'CAJA DE COMPENSACION FAMILIAR DE CORDOBA  COMFACOR'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF053%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DE CUNDINAMARCA COMFACUNDI'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC53%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DE CUNDINAMARCA COMFACUNDI'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF054%' THEN 'CAJA DE COMPENSACION FAMILIAR DE FENALCO  COMFENALCO  - CUNDINAMARCA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF037%' THEN 'CAJA DE COMPENSACION FAMILIAR DE FENALCO  DE TOLIMA  COMFENALCO'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF028%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DE FENALCO \"COMFENALCO QUINDIO\"'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF032%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DE FENALCO SECCIONAL DE SANTANDER'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF023%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DE LA GUAJIRA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC23%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DE LA GUAJIRA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF027%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DE NARIÑO \"COMFAMILIAR NARIÑO\"'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC27%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DE NARIÑO \"COMFAMILIAR NARIÑO\"'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF033%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DE SUCRE'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC33%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DE SUCRE'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF103%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DEL CAQUETÁ - COMFACA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF102%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DEL CHOCÓ COMFACHOCO'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC20%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DEL CHOCÓ COMFACHOCO'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF024%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DEL HUILA \"COMFAMILIAR\"'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC24%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DEL HUILA \"COMFAMILIAR\"'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF045%' THEN 'CAJA DE COMPENSACIÓN FAMILIAR DEL NORTE DE SANTANDER  \"COMFANORTE\"'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF055%' THEN 'CAJA DE DE COMPENSACION FAMILIAR  CAJACOPI ATLANTICO'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC55%' THEN 'CAJA DE DE COMPENSACION FAMILIAR  CAJACOPI ATLANTICO'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF031%' THEN 'CAJA SANTANDEREANA DE SUBSIDIO FAMILIAR \"CAJASAN\"'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%UT-001%' THEN 'CAJASALUD  ARS  UT'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS028%' THEN 'CALISALUD   EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS34%' THEN 'CAPITAL SALUD'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSC34%' THEN 'CAPITAL SALUD'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS020%' THEN 'CAPRECOM'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSC20%' THEN 'CAPRECOM'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS025%' THEN 'CAPRESOCA  EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSC25%' THEN 'CAPRESOCA  EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF010%' THEN 'COLSUBSIDIO'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC10%' THEN 'COLSUBSIDIO'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF040%' THEN 'COMFACARTAGO'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF029%' THEN 'COMFAMILIAR RISARALDA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%UT-002%' THEN 'COMFAMILIARES EN SALUD UT'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS012%' THEN 'COMFENALCO  VALLE  EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS12%' THEN 'COMFENALCO  VALLE  EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS008%' THEN 'COMPENSAR   EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS08%' THEN 'COMPENSAR   EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%UT-004%' THEN 'CONVENIO CAMACOL CONFAMA UT'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%UT-003%' THEN 'CONVENIO COMFENALCO UT'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS016%' THEN 'COOMEVA   EPS  SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS16%' THEN 'COOMEVA   EPS  SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS133%' THEN 'COOPERATIVA DE SALUD COMUNITARIA-COMPARTA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC33%' THEN 'COOPERATIVA DE SALUD COMUNITARIA-COMPARTA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS042%' THEN 'COOPERATIVA DE SALUD Y DESARROLLO INTEGRAL ZONA SUR ORIENTAL DE CARTAGENA - COOSALUD'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS42%' THEN 'COOPERATIVA DE SALUD Y DESARROLLO INTEGRAL ZONA SUR ORIENTAL DE CARTAGENA - COOSALUD'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS024%' THEN 'COOPERATIVA DE SALUD Y DESARROLLO INTEGRAL ZONA SUR ORIENTAL DE CARTAGENA LTDA COOSALUD ESS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC24%' THEN 'COOPERATIVA DE SALUD Y DESARROLLO INTEGRAL ZONA SUR ORIENTAL DE CARTAGENA LTDA COOSALUD ESS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS023%' THEN 'CRUZ BLANCA  EPS SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS23%' THEN 'CRUZ BLANCA  EPS SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES002%' THEN 'ECOPETROL'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS002%' THEN 'EMPRESA MUTUAL PARA EL DESARROLLO INTEGRAL  DE LA SALUD ESS EMDISALUD ESS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC02%' THEN 'EMPRESA MUTUAL PARA EL DESARROLLO INTEGRAL  DE LA SALUD ESS EMDISALUD ESS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EAS016%' THEN 'EMPRESAS PUBLICAS DE MEDELLIN-DEPARTAMENTO MEDICO'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS091%' THEN 'ENTIDAD COOPERATIVA SOLDE SALUD DEL NORTE DE SOACHA ECOOPSOS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC91%' THEN 'ENTIDAD COOPERATIVA SOLDE SALUD DEL NORTE DE SOACHA ECOOPSOS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS030%' THEN 'EPS  CONDOR  SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS022%' THEN 'EPS  CONVIDA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSC22%' THEN 'EPS  CONVIDA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS017%' THEN 'EPS  FAMISANAR  LTDA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS17%' THEN 'EPS  FAMISANAR  LTDA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS013%' THEN 'EPS  SALUDCOOP'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS13%' THEN 'EPS  SALUDCOOP'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS005%' THEN 'EPS  SANITAS  SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS05%' THEN 'EPS  SANITAS  SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS009%' THEN 'EPS PROGRAMA COMFENALCO ANTIOQUIA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS018%' THEN 'EPS SERVICIO OCCIDENTAL DE SALUD  SA - EPS SOS SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS18%' THEN 'EPS SERVICIO OCCIDENTAL DE SALUD  SA - EPS SOS SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS010%' THEN 'EPS Y MEDICINA PREPAGADA SURAMERICANA SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS10%' THEN 'EPS Y MEDICINA PREPAGADA SURAMERICANA SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EAS027%' THEN 'FONDO DE PASIVO SOCIAL DE LOS FERROCARRILES NALES'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%FMS001%' THEN 'FUERZAS MILITARES'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS46%' THEN 'FUNDACIÓN SALUD MIA - CM'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS046%' THEN 'FUNDACIÓN SALUD MIA - CM'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS039%' THEN 'GOLDEN GROUP SA EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS39%' THEN 'GOLDEN GROUP SA EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS14%' THEN 'HUMANA VIVIR'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS014%' THEN 'HUMANA VIVIR'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES006%' THEN 'INPEC'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS006%' THEN 'INSTITUTO  DE  SEGUROS SOCIALES  EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS037%' THEN 'LA NUEVA EPS SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS041%' THEN 'LA NUEVA EPS SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES004%' THEN 'MAGISTERIO'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSI05%' THEN 'MALLAMAS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSIC5%' THEN 'MALLAMAS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSI02%' THEN 'MANEXKA EPSI'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSIC2%' THEN 'MANEXKA EPSI'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS044%' THEN 'MEDIMAS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS44%' THEN 'MEDIMAS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS045%' THEN 'MEDIMAS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS45%' THEN 'MEDIMAS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS038%' THEN 'MULTIMEDICAS SALUD CON CALIDAD EPS SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS41%' THEN 'NUEVA EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS37%' THEN 'NUEVA EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSI06%' THEN 'PIJAOS SALUD EPSI'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSIC6%' THEN 'PIJAOS SALUD EPSI'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%POL001%' THEN 'POLICIA NACIONAL'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS035%' THEN 'RED SALUD ATENCION HUMANA EPS SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS015%' THEN 'SALUD  COLPATRIA  EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS002%' THEN 'SALUD  TOTAL  SA  EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS02%' THEN 'SALUD  TOTAL  SA  EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS034%' THEN 'SALUDCOLOMBIA EPS SA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS33%' THEN 'SALUDVIDA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSC33%' THEN 'SALUDVIDA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS033%' THEN 'SALUDVIDA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS40%' THEN 'SAVIA SALUD EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS040%' THEN 'SAVIA SALUD EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS031%' THEN 'SELVASALUD  SA  EPS'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS26%' THEN 'SOLSALUD'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS026%' THEN 'SOLSALUD'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES011%' THEN 'UANTIOQUIA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES005%' THEN 'UATLANTICO'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES010%' THEN 'UCARTAGENA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES009%' THEN 'UCAUCA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES012%' THEN 'UCORDOBA'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES013%' THEN 'UNARIÑO'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES014%' THEN 'UPTC'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES007%' THEN 'UVALLE'
                ELSE NULL END  as eps_name, 
      CASE WHEN UPPER(
                CODIGO_de_eps) LIKE  '%EPS001%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS01%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSI04%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSIC4%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSIC1%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSI03%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSIC3%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSI01%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS076%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC76%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS118%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC18%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS115%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS062%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC62%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS048%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS48%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS207%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC07%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS068%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS03%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS003%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSM03%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF007%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC07%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF018%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC18%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC50%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF050%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF001%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF002%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC02%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF035%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF009%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC09%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF015%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC15%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF053%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC53%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF054%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF037%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF028%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF032%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF023%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC23%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF027%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC27%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF033%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC33%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF103%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF102%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC20%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF024%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC24%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF045%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF055%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC55%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF031%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%UT-001%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS028%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS34%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSC34%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS020%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSC20%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS025%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSC25%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF010%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCFC10%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF040%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%CCF029%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%UT-002%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS012%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS12%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS008%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS08%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%UT-004%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%UT-003%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS016%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS16%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS133%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC33%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS042%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS42%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS024%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC24%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS023%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS23%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES002%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS002%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC02%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EAS016%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESS091%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%ESSC91%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS030%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS022%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSC22%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS017%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS17%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS013%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS13%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS005%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS05%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS009%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS018%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS18%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS010%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS10%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EAS027%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%FMS001%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS46%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS046%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS039%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS39%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS14%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS014%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES006%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS006%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS037%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS041%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES004%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSI05%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSIC5%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSI02%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSIC2%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS044%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS44%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS045%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS45%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS038%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS41%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS37%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSI06%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSIC6%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%POL001%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS035%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS015%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS002%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS02%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS034%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS33%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSC33%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS033%' THEN '1'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS40%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS040%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS031%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPSS26%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%EPS026%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES011%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES005%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES010%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES009%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES012%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES013%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES014%' THEN '0'
                WHEN UPPER(CODIGO_de_eps) LIKE  '%RES007%' THEN '0'
                ELSE NULL END eps_status
                FROM Tabla
"
  Tabla <- sqldf::sqldf(query)
  Tabla <- dplyr::select(Tabla, -'CODIGO_de_eps')
  return(Tabla)
}




rm(backwardElimination)

backwardElimination <- function(tabla, Y = "", sl = 0.05) {
  # tabla =  Table_index[, c(2,4:16,36:48)]
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


 