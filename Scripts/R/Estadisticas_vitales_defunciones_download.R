packageList<-c("readxl","olapR", "foreign", "tidyverse", "haven","beepr", 'dplyr', 'readr', 'devtools', 'sqldf', 'tidyr')
# devtools::install_github("apache/arrow/r")

for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}
source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/Scripts/credentials.R")

#####################################################################
#
#####################################################################

devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/Scripts/functions.R") 

'cubos.sispro.gov.co SGD_Registro_Estadisticas_Vitales EEVV - Defunciones'
metadata <-  c('EEVV - Defunciones' , 'EEVV - Nacimientos')
# ITERATIONS  


path_input = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/input/"
# EPS diccionario
# diccionario_codigos_eps <- 
#read_excel(paste0(path_input, "diccionario_codigos_eps.xlsx" ) )
if(1==1){
  A= data.frame("Grupo_etario" =c('De 0 a antes de 1 año', 'De 01 a 05 años', 'De 06 a 09 años', 'De 10 a 14 años', 'De 15 a 18 años', 'De 19 a 26 años', 
                                  'De 27 a 44 años', 'De 45 a 59 años', 'De 60 y más', 'No Definido', 'No Reportado')  ) # 
  B=data.frame("month" =c(1:12)  ) 
  C = data.frame( 'EPS' = c( 'CCF028 - COMFENALCO QUINDIO', 'CCF029 - COMFAMILIAR RISARALDA', 'CCF030 - CAJASAI', 
                                      'CCF031 - CAJASAN', 'CCF032 - COMFENALCO SANTANDER', 'CCF033 - COMFASUCRE', 'CCF035 - CAFABA',
                                      'CCF037 - COMFENALCO', 'CCF040 - COMFACARTAGO', 'CCF045 - FAMISALUD COMFANORTE A.R.S',
                                      'CCF049 - COMFAORIENTE', 'CCF050 - CENTRO ORIENTE MOVILIDAD SUBSIDIADO EPS', 'CCF053 - COMFACUNDI',
                                      'CCF054 - CCF FENALCO', 'CCF055 - CAJACOPI', 'CCF101 - COLSUBSIDIO', 'CCF102 - COMFACHOCO', 
                                      'CCF103 - COMFACA', 'CCFC02 - CAJA DE COMPENSACION FAMILIAR DE ANTIOQU', 
                                      'CCFC07 - COMFAMILIAR CARTAGENA EPS-CCF DE CARTAGE', 'CCFC09 - COMFABOY EPS - CCF DE BOYACA',
                                      'CCFC10 - CAJA COLOMBIANA DE SUBSIDIO FAMILIAR COL', 'CCFC15 - COMFACOR EPS- CCF DE CORDOBA', 
                                      'CCFC18 - CAJA DE COMPENSACION FAMILIAR CAFAM EPS', 'CCFC20 - COMFACHOCO - CCF DEL CHOCO',
                                      'CCFC23 - COMFAMILIAR DE LA GUAJIRA EPS-CCF', 'CCFC24 - COMFAMILIAR HUILA EPS -CCF', 
                                      'CCFC27 - COMFAMILIAR NARI&#209;O EPS -CCF', 'CCFC33 - COMFASUCRE EPS - CCF DE SUCRE', 
                                      'CCFC50 - CAJA DE COMPENSACION FAMILIAR DEL ORIENT', 'CCFC53 - COMFACUNDI - CCF DE CUNDINAMARCA', 
                                      'CCFC55 - CAJACOPI ATLANTICO - CCF', 'EAS016 - EPM', 'EAS027 - FERROCARRILES NALES', 
                                      'EMP002 - EMP CAFESALUD', 'EMP023 - COLSANITAS S.A. COMPAÑÍA DE MEDICINA PRE', 'EPIO06 - EPIO06', 
                                      'EPS001 - ALIANSALUD', 'EPS002 - SALUD TOTAL', 'EPS003 - CAFESALUD', 'EPS005 - SANITAS', 
                                      'EPS006 - ISS', 'EPS008 - COMPENSAR', 'EPS009 - COMFENALCO ANTIOQUIA', 'EPS010 - EPS SURA', 
                                      'EPS012 - COMFENALCO VALLE', 'EPS013 - SALUDCOOP', 'EPS014 - HUMANA VIVIR', 'EPS015 - COLPATRIA', 
                                      'EPS016 - COOMEVA', 'EPS017 - FAMISANAR', 'EPS018 - SOS', 'EPS020 - CAPRECOM', 'EPS022 - CONVIDA', 
                                      'EPS023 - CRUZ BLANCA', 'EPS025 - CAPRESOCA', 'EPS026 - SOLSALUD', 'EPS028 - CALISALUD', 
                                      'EPS030 - CONDOR', 'EPS031 - SELVASALUD', 'EPS033 - SALUDVIDA', 'EPS034 - SALUDCOLOMBIA',
                                      'EPS035 - RED SALUD', 'EPS037 - NUEVA EPS', 'EPS038 - MULTI MEDICAS', 'EPS039 - GOLDEN GROUP',
                                      'EPS040 - SAVIA SALUD EPS', 'EPS041 - NUEVA EPS SA', 'EPS042 - EMPRESA SOLIDARIA DE SALUD Y DESARROLLO',
                                      'EPS044 - MEDIMAS Contributivo', 'EPS045 - MEDIMAS Movilidad', 'EPS046 - Fundación SALUD MIA_CONTRIBUTIVO', 
                                      'EPS048 - MUTUAL SER', 'EPSC03 - CAFESALUD E.P.S. S.A.-CM', 'EPSC20 - CAJA DE PREVISION SOCIAL DE COMUNICACION', 'EPSC22 - ENTIDAD ADMINISTRADORA DE REGIMEN SUBSI', 
                                      'EPSC25 - CAPRESOCA EPS', 'EPSC33 - SALUDVIDA S.A. E.P.S.-CM', 'EPSC34 - CAPITAL SALUD EPSS S A S',
                                      'EPSI01 - DUSAKAWI E.P.S.I.', 'EPSI02 - MANEXKA', 'EPSI03 - AIC-EPSI-I', 'EPSI04 - ANASWAYUU', 
                                      'EPSI05 - MALLAMAS', 'EPSI06 - PIJAOSALUD EPSI', 'EPSIC1 - ASOCIACION DE CABILDOS INDIGENAS DEL CES',
                                      'EPSIC2 - ASOCIACION DE CABILDOS DEL RESGUARDO IND', 'EPSIC3 - ASOCIACION DE INDIGENAS DEL CAUCA A I C',
                                      'EPSIC4 - ENTIDAD PROMOTORA DE SALUD INDIGENA ANA', 'EPSIC5 - ENTIDAD PROMOTORA DE SALUD MALLAMAS EP',
                                      'EPSIC6 - ENTIDAD PROMOTORA DE SALUD PIJAOS SALUD', 'EPSM03 - CAFESALUD EPSS SA',
                                      'EPSM33 - SALUDVIDA SA ENTIDAD PROMOTORA DE SALUD', 'EPSS01 - ALIANSALUD ENTIDAD PROMOTORA DE SALUD SA',
                                      'EPSS02 - SALUD TOTAL', 'EPSS03 - CAFESALUD', 'EPSS05 - ENTIDAD PROMOTORA DE SALUD SANITAS SA',
                                      'EPSS08 - COMPENSAR ENTIDAD PROMOTORA DE SALUD', 'EPSS09 - COMFENALCO ANTIOQUIA', 'EPSS10 - SURA EPS', 
                                      'EPSS12 - COMFENALCO VALLE EPS', 'EPSS13 - ENTIDAD PROMOTORA DE SALUD ORGANISMO COO', 'EPSS14 - HUMANA VIVIR', 
                                      'EPSS15 - SALUD COLPATRIA EPS', 'EPSS16 - COOMEVA EPS SA', 'EPSS17 - EPS FAMISANAR LTDA', 'EPSS18 - ENTIDAD PROMOTORA DE SALUD SERVICIO OCCI',
                                      'EPSS23 - CRUZ BLANCA ENTIDAD PROMOTORA DE SALUD S', 'EPSS26 - SOLSALUD', 'EPSS33 - SALUDVIDA', 'EPSS34 - CAPITAL SALUD',
                                      'EPSS37 - NUEVA EPS SA - CM', 'EPSS39 - GOLDEN GROUP SA ENTIDAD PROMOTORA DE SAL', 'EPSS40 - Alianza Medellin -savia Salud', 
                                      'EPSS41 - NUEVA EPS SA', 'EPSS42 - EMPRESA SOLIDARIA DE SALUD Y DESARROLLO', 'EPSS44 - MEDIMAS Movilidad', 'EPSS45 - MEDIMAS Subsidiado',
                                      'EPSS46 - Fundación SALUD MIA_SUBSIDIADO', 'EPSS48 - MUTUAL SER', 'ESS002 - EMDISALUD', 'ESS024 - COOSALUD E.S.S.', 'ESS062 - ASMET SALUD',
                                      'ESS068 - ASTREA', 'ESS076 - AMBUQ', 'ESS091 - ECOOPSOS', 'ESS118 - EMSSANAR E.S.S.', 'ESS133 - COMPARTA', 'ESS164 - PIJAOSALUD EPSE',
                                      'ESS207 - MUTUAL SER', 'ESS208 - ANAS WAYUU', 'ESSC02 - EMPRESA MUTUAL PARA EL DESARROLLO INTEGR', 
                                      'ESSC07 - ASOCIACION MUTUAL SER EMPRESA SOLIDARIA', 'ESSC18 - ASOCIACION MUTUAL EMPRESA SOLIDARIA DE S', 
                                      'ESSC24 - COOSALUD ESS COOPERATIVA DE SALUD Y DE', 'ESSC33 - COOP. DE SALUD COMUNITARIA COMPARTA',
                                      'ESSC62 - ASOCIACION MUTUAL LA ESPERANZA ASMET S', 'ESSC76 - ASOCIACION MUTUAL BARRIOS UNIDOS DE QUIB',
                                      'ESSC91 - ENTIDAD COOPERATIVA SOLIDARIA DE SALUD', 'ESSI06 - ESSI06', 'NO REPORTADO - NO REPORTADO',
                                      'RES001 - POLICIA NACIONAL', 'RES002 - ECOPETROL', 'RES003 - FUERZAS MILITARES', 'RES004 - MAGISTERIO', 'RES005 - UATLANTICO', 
                                      'RES006 - CAPRUIS', 'RES007 - UVALLE', 'RES008 - UNISALUD', 'RES009 - UCAUCA', 'RES010 - UCARTAGENA', 'RES011 - UANTIOQUIA', 
                                      'RES012 - UCORDOBA', 'RES013 - UNARIÑO', 
                                      'RES014 - UPTC', 'UT-001 - UT-001', 'UT-002 - UT-002', 'UT-003 - UT-003', 'UT-004 - UT-004' )  )
  
  # D =  data.frame('CIE_10'= c('C01 - CIERTAS ENFERMEDADES INFECCIOSAS Y PARASITARIAS',
  #                             'C02 - TUMORES',
  #                             'C03 - ENFERMEDADES DE LA SANGRE Y DE LOS ORGANOS HEMATOPOYETICOS, Y CIERTOS TRASTORNOS QUE AFECTAN EL MECANISMO DE LA INMUNIDAD', 
  #                             'C04 - ENFERMEDADES ENDOCRINAS, NUTRICIONALES Y METABOLICAS', 
  #                             'C05 - TRASTORNOS MENTALES Y DEL COMPORTAMIENTO',
  #                             'C06 - ENFERMEDADES DEL SISTEMA NERVIOSO',
  #                             'C07 - ENFERMEDADES DEL OJO Y SUS ANEXOS', 
  #                             'C08 - ENFERMEDADES DEL OIDO Y DE LA APOFISIS MASTOIDES', 
  #                             'C09 - ENFERMEDADES DEL SISTEMA CIRCULATORIO', 
  #                             'C10 - ENFERMEDADES DEL SISTEMA RESPIRATORIO',
  #                             'C11 - ENFERMEDADES DEL SISTEMA DIGESTIVO', 
  #                             'C12 - ENFERMEDADES DE LA PIEL Y DEL TEJIDO SUBCUTANEO', 
  #                             'C13 - ENFERMEDADES DEL SISTEMA OSTEOMUSCULAR Y DEL TEJIDO CONJUNTIVO', 
  #                             'C14 - ENFERMEDADS DEL SISTEMA GENITOURINARIO',
  #                             'C15 - EMBARAZO, PARTO Y PUERPERIO',
  #                             'C16 - CIERTAS AFECCIONES ORIGINALES EN EL PERIODO PERINATAL',
  #                             'C17 - MALFORMACIONES CONGENITAS, DEFORMIDADES Y ANOMALIAS CROSOMICAS', 
  #                             'C18 - SINTOMAS, SIGNOS Y HALLAZGOS ANORMALES CLINICOS Y DE LABORATORIO, NO CLASIFICADOS EN OTRA PARTE',
  #                             'C19 - TRAUMATISMOS, ENVENENAMIENTOS Y ALGUNAS OTRAS CONSECUENCIAS DE CAUSA EXTERNAS',
  #                             'C20 - CAUSAS EXTERNAS DE MORBILIDAD Y DE MORTALIDAD',
  #                             'C21 - FACTORES QUE INFLUYEN EN EL ESTADO DE SALUD Y CONTACTO CON LOS SERVICIOS DE SALUD',
  #                             'C22 - CODIGOS PARA PROPOSITOS ESPECIALES', 
  #                             'CIE9 - SIN CAPITULO',
  #                             'NO DEFINIDO',
  #                             'NO REPORTADO'))
 
  # B= data.frame("Options" = c("1 - SI","2 - NO"))
  INTERATION = sqldf(" SELECT * FROM A 
                            INNER JOIN B ON 1=1
                            INNER JOIN C ON 1=1
                     --       INNER JOIN D ON 1=1 
                     ")
  INTERATION = na.omit(INTERATION)
}

### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[Número de Defunciones]'
AXIS1 <- "[Fecha de Defunción].[Año]" #  '[Administradora].[Administradora]' ,
AXIS2 <- '[Geografía de Defuncion].[Municipio]'
segregacion = '[Fecha de Defunción].[Mes].&[%s],[Persona].[Grupo Etáreo - Etapas Ciclo Vital].&[%s],[Administradora].[Administradora].&[%s] ' #, [Causa Directa de Muerte].[Capítulo].&[%s]
tabla = data.frame() 

run_query_olap <- function(cnnstr, mdx,var1,  var2 , var3 ){
  olapCnn<-olapR::OlapConnection(cnnstr)
  temp <- olapR::execute2D(olapCnn, mdx)
  # temp <- subset(temp, is.na(temp[[3]])==F  )
  temp$var1 = var1
  temp$var2 = var2
  temp$var3 = var3
  # temp$var4 = var4
  return(temp)
}
query_olap <- function(AXIS0,AXIS1, AXIS2,from_olap_catalog,where_filter   ){
  if (AXIS2=="") {
    mdx<- "SELECT  {%s} ON AXIS(0),
                               {%s.MEMBERS} ON AXIS(1)  
                        FROM [%s]
                        WHERE (%s) "
    mdx<- sprintf(mdx, AXIS0, AXIS1,  AXIS2, from_olap_catalog, where_filter )  
  }else{
    mdx<- "SELECT  {%s} ON AXIS(0),
                               {%s.MEMBERS} ON AXIS(1) , 
                               {%s.MEMBERS} ON AXIS(2) 
                        FROM [%s]
                        WHERE (%s) "
    mdx<- sprintf(mdx, AXIS0, AXIS1,  AXIS2, from_olap_catalog, where_filter )
  }
  
  return(mdx)
}

for (i in  9712:10500 ) { #1:nrow(INTERATION)
  start_time <- Sys.time()
  var1 = as.character((INTERATION[[2]][i] )[1])
  var2 = as.character( (INTERATION[[1]][i] )[1] )
  var3 = as.character( (INTERATION[[3]][i] )[1] )
  # var4 = as.character( (INTERATION[[4]][i] )[1] )
  print("/------------------------------------------/")
  print("/----------Its a new iteration-------------/")
  print( paste0('The iteration: ', i , " is: '", var1, "',  ", var2, "',  '",var3,"'" ))
  print("/------------------------------------------/")
  
  mdx<- "SELECT  {[Measures].[Número de Defunciones]} ON AXIS(0),
                               {[Fecha de Defunción].[Año].MEMBERS} ON AXIS(1) , 
                               {[Geografía de Defuncion].[Municipio].MEMBERS} ON AXIS(2) 
                        FROM [EEVV - Defunciones]
        WHERE ([Fecha de Defunción].[Mes].&[%s],[Persona].[Grupo Etáreo - Salud].&[%s],[Administradora].[Administradora].&[%s]) "
  mdx<- sprintf(mdx, var1, var2, var3)
  
  tryCatch( {
    olapCnn<-olapR::OlapConnection(cnnstr_Estadisticas_Vitales)
    temp <- olapR::execute2D(olapCnn, mdx)
    temp$var1 = var1
    temp$var2 = var2
    temp$var3 = var3
    colnames(temp) = c( 'Municipio', 'Anio','Defunciones', 'Mes','GrupoEtario', 'EPS' )
    temp = subset(temp, is.na(temp$Defunciones)==F )
    temp = subset(temp, as.numeric(temp$Anio) >= 2016 )
    
    print("/------------------------------------------/")
    print(sprintf("the number of rows of the temp panel are: %s", nrow(temp) ) )
    print("/------------------------------------------/")
    tabla = rbind(tabla, temp)
    print(sprintf("the number of rows of the full panel are: %s", nrow(tabla) ) )
  }, error=function(e){cat("ERROR CATCH: ",conditionMessage(e), "\n")}
  
  
  
  )
  end_time <- Sys.time()
  print(end_time-start_time)
}

path_output = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/"

write.csv2(tabla, paste0(path_output, 'def1.csv'), row.names = F)
tabla= gbd_level2

################################33
#
########################################

Defuncion_Materna_1y = data.frame() 

for (i in 1:nrow(INTERATION) ) {
  var1 = as.character((INTERATION[[1]][i] )[1])
  var2 = as.character( (INTERATION[[2]][i] )[1] )
  print(var1)
  print(var2)
  from_olap_catalog <- metadata[1]
  Filter = ('[Administradora].[Administradora].&[%s],[Mortalidad Materna].[Defunción Materna 1 año].&[%s]')
  where_filter <- sprintf(Filter, var1, var2)
  print(where_filter)
  
  mdx  = query_olap(AXIS0=AXIS0,AXIS1=AXIS1, AXIS2= AXIS2,
                    from_olap_catalog = from_olap_catalog, where_filter =where_filter  )
  
  
  tryCatch( {
    temp  = run_query_olap(cnnstr  = cnnstr_Estadisticas_Vitales, 
                           mdx  = mdx,var1 = var1,  
                           var2  = var2 )
    colnames(temp)[3] = 'Defuncion_Materna_1_anio'
    Defuncion_Materna_1y = rbind(Defuncion_Materna_1y, temp)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
}
write.csv2(Defuncion_Materna_1y, paste0(path_output, 'Defuncion_Materna_1y'), row.names = F)
#########################################################################################
## Transforming data
#########################################################################################
colnames(Defuncion_Materna) = c("Municipio", "Anio", "Defuncion_Materna_42_dias_value" ,  "EPS" , "Defuncion_Materna_42_dias" )
DEF_MAT_SI = sqldf::sqldf("
              -- DEF MATERNA: SI
             WITH 
             DEF_MAT_SI AS  (
             SELECT  substr(Municipio, 0, 6) DIVIPOLA, 
             substr(Municipio, 9, length(Municipio)) Muni  , Anio ,
             Defuncion_Materna_42_dias_value as def_mat_42d_si, EPS ,
             substr(EPS, 0, instr(EPS,' - ') ) AS EPS_CODE
             FROM Defuncion_Materna 
             WHERE  Defuncion_Materna_42_dias  LIKE '%SI%'
             AND Municipio IS NOT NULL AND ANIO IS NOT NULL
             AND EPS NOT  LIKE '%NO DEF%'
             )  
            SELECT * FROM DEF_MAT_SI  
             ")

DEF_MAT_NO = sqldf::sqldf("
              -- DEF MATERNA: SI
             WITH 
              
             DEF_MAT_NO AS  (
             SELECT  substr(Municipio, 0, 6) DIVIPOLA, 
             substr(Municipio, 9, length(Municipio)) Muni  , Anio ,
             Defuncion_Materna_42_dias_value as def_mat_42d_no , EPS ,
             substr(EPS, 0, instr(EPS,' - ') ) AS EPS_CODE
             FROM Defuncion_Materna 
             WHERE  Defuncion_Materna_42_dias  NOT LIKE '%SI%'
             AND Municipio IS NOT NULL AND ANIO IS NOT NULL
             AND EPS NOT  LIKE '%NO DEF%'
             )  
            SELECT * FROM DEF_MAT_NO  
             ")
ALL_DIVIPOLA_EPS = sqldf::sqldf("
              WITH
             ALL_DIVIPOLA_EPS AS (
             SELECT DISTINCT substr(Municipio, 0, 6) DIVIPOLA_,  
             substr(EPS, 0, instr(EPS,' - ') ) AS EPS_CODE_ , ANIO AS ANIO_
             FROM Defuncion_Materna 
             WHERE   Municipio IS NOT NULL AND ANIO IS NOT NULL
 
             )
            SELECT * FROM ALL_DIVIPOLA_EPS  
             ")

temp = sqldf::sqldf(" 
            SELECT c.* , def_mat_42d_no, def_mat_42d_si
            FROM ALL_DIVIPOLA_EPS  c
            LEFT JOIN DEF_MAT_NO A
            ON DIVIPOLA_= A.DIVIPOLA AND Anio_ = A.Anio AND EPS_CODE_ = A.EPS_CODE
            LEFT JOIN DEF_MAT_SI B
            ON DIVIPOLA_= B.DIVIPOLA AND Anio_ = B.Anio AND EPS_CODE_ = B.EPS_CODE
             ")
#############################################
#Defuncion_Materna_1y
#############################################
colnames(Defuncion_Materna_1y) = c("Municipio", "Anio", "def_mat_1y_value" ,  "EPS" , "def_mat_1y" )


DEF_MAT_SI = sqldf::sqldf("
              -- DEF MATERNA: SI
             WITH 
             DEF_MAT_SI AS  (
             SELECT  substr(Municipio, 0, 6) DIVIPOLA, 
             substr(Municipio, 9, length(Municipio)) Muni  , Anio ,
             def_mat_1y_value as def_mat_1y_si, EPS ,
             substr(EPS, 0, instr(EPS,' - ') ) AS EPS_CODE
             FROM Defuncion_Materna_1y 
             WHERE  def_mat_1y  LIKE '%SI%'
             AND Municipio IS NOT NULL AND ANIO IS NOT NULL
             AND EPS NOT  LIKE '%NO DEF%'
             )  
            SELECT * FROM DEF_MAT_SI  
             ")

DEF_MAT_NO = sqldf::sqldf("
              -- DEF MATERNA: SI
             WITH 
              
             DEF_MAT_NO AS  (
             SELECT  substr(Municipio, 0, 6) DIVIPOLA, 
             substr(Municipio, 9, length(Municipio)) Muni  , Anio ,
             def_mat_1y_value as def_mat_1y_no , EPS ,
             substr(EPS, 0, instr(EPS,' - ') ) AS EPS_CODE
             FROM Defuncion_Materna_1y 
             WHERE  def_mat_1y  NOT LIKE '%SI%'
             AND Municipio IS NOT NULL AND ANIO IS NOT NULL
             AND EPS NOT  LIKE '%NO DEF%'
             )  
            SELECT * FROM DEF_MAT_NO  
             ")
ALL_DIVIPOLA_EPS = sqldf::sqldf("
              WITH
             ALL_DIVIPOLA_EPS AS (
             SELECT DISTINCT substr(Municipio, 0, 6) DIVIPOLA_,  
             substr(EPS, 0, instr(EPS,' - ') ) AS EPS_CODE_ , ANIO AS ANIO_
             FROM Defuncion_Materna_1y 
             WHERE   Municipio IS NOT NULL AND ANIO IS NOT NULL
             )
             SELECT * FROM ALL_DIVIPOLA_EPS  
             ")

temp = sqldf::sqldf(" 
            SELECT c.* ,  def_mat_1y_si, def_mat_1y_no
            FROM temp  c
            LEFT JOIN DEF_MAT_NO A
            ON DIVIPOLA_= A.DIVIPOLA AND Anio_ = A.Anio AND EPS_CODE_ = A.EPS_CODE
            LEFT JOIN DEF_MAT_SI B
            ON DIVIPOLA_= B.DIVIPOLA AND Anio_ = B.Anio AND EPS_CODE_ = B.EPS_CODE
             ")

write.csv2(temp, paste0(path_output, 'Defuncion_Materna.csv'), row.names = F)
################################################################################################33



library(readr)
gbd_level2 <- read_delim("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/gbd_level2.csv", 
                         ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE)
colnames(gbd_level2) = c('Municipio', 'fecha_def',  'value' , 'EPS','gbd_level2')

A = sqldf::sqldf("
             SELECT MUNICIPIO|| ' - ' ||fecha_def|| ' - ' ||EPS ID, 
                 gbd_level2,  value FROM gbd_level2") 
library(tidyr)
?assign
#pivot the data frame into a long format
A = reshape(A,timevar  = 'gbd_level2',
            idvar = "ID",
            direction = "wide")  

Variables = c()
for (i in unique(gbd_level2$gbd_level2)) {
  assign( drop_characters(i ),
          subset(gbd_level2, gbd_level2 == i )  ,
          envir = .GlobalEnv )
  Variables= append(drop_characters(i ), Variables)
}
colnames(Defuncion_Materna)
#######################################
library(sqldf)

library(readr)
Defuncion_Materna <- read_delim("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/Defuncion_Materna.csv", 
                                delim = ";", escape_double = FALSE, col_types = cols(ANIO_ = col_double()), 
                                trim_ws = TRUE)

ALL_DIVIPOLA_EPS = sqldf::sqldf("
              WITH
             ALL_DIVIPOLA_EPS AS (
             SELECT DISTINCT substr(Municipio, 0, 6) DIVIPOLA_,  
             substr(EPS, 0, instr(EPS,' - ') ) AS EPS_CODE_ , fecha_def AS ANIO_
             FROM gbd_level2 
             WHERE   Municipio IS NOT NULL AND fecha_def IS NOT NULL
             ),
             all_div as (SELECT * FROM ALL_DIVIPOLA_EPS WHERE  EPS_CODE_ != '0')
             SELECT A.*, B.def_mat_42d_no, 
             def_mat_42d_si, def_mat_1y_si, def_mat_1y_no
             FROM all_div A
             LEFT JOIN Defuncion_Materna B
             ON A.ANIO_ = B.ANIO_ 
              AND A.DIVIPOLA_ = B.DIVIPOLA_   
              AND A.EPS_CODE_ = B.EPS_CODE_  
             ") 
EST_VITALES = ALL_DIVIPOLA_EPS
for (i  in Variables) {
  print("-------------------")
  print( i )
  sql = "WITH 
        tabla AS  (
          SELECT  substr(Municipio, 0, 6) DIVIPOLA, 
          substr(Municipio, 9, length(Municipio)) Muni  , fecha_def as Anio ,
          gbd_level2 as %s, EPS ,
          substr(EPS, 0, instr(EPS,' - ') ) AS EPS_CODE, VALUE
          FROM %s 
          WHERE 
          Municipio IS NOT NULL 
          AND fecha_def IS NOT NULL
          AND EPS != '0 - NO DEFINIDO'
        )  
        SELECT A.*, B.value as %s FROM EST_VITALES A
              LEFT JOIN tabla B
              ON A.ANIO_ = B.ANIO
              AND A.DIVIPOLA_ = B.DIVIPOLA  
              AND EPS_CODE_ = EPS_CODE
          "
  sql = sprintf(sql, i, i,i)
  EST_VITALES= sqldf(sql)
}
EST_VITALES = sqldf::sqldf("SELECT DIVIPOLA_||' - '|| EPS_CODE_ as ID,
                                * FROM EST_VITALES")


lista = c(5,6,7,8:31)  
for (i in lista) {
  EST_VITALES[[i]] = ifelse(is.na(EST_VITALES[[i]]) == T, 0 , EST_VITALES[[i]] )
}
write.csv2(EST_VITALES, paste0(path_output, 'EST_VITALES.csv'), row.names = F)



