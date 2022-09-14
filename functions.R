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