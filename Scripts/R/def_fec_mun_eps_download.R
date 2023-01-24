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
  INTERATION = sqldf(" SELECT * FROM B
                            INNER JOIN C ON 1=1
                     
                     ")
  INTERATION = na.omit(INTERATION)
}
# write.csv2(INTERATION, paste0(path_input, 'iter.csv'))
### Mandatory variables to get observations a municipalities's level.

library(readr)
iter <- read_delim("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/input/iter.csv", 
                   ";", escape_double = FALSE,
                   col_types = cols(X1 = col_skip()),   
                   locale = locale(encoding = "ISO-8859-1") )
as.character(iter[1500,2]) ==  as.character(INTERATION[1500,2])
AXIS0 <- '[Measures].[Número de Defunciones]'
AXIS1 <- "[Fecha de Defunción].[Año]" #  '[Administradora].[Administradora]' ,
AXIS2 <- '[Geografía de Defuncion].[Municipio]'
segregacion = '[Fecha de Defunción].[Mes].&[%s],[Administradora].[Administradora].&[%s] ' #, [Causa Directa de Muerte].[Capítulo].&[%s]
tabla = data.frame() 

run_query_olap <- function(cnnstr, mdx,var1,  var2  ){
  olapCnn<-olapR::OlapConnection(cnnstr)
  temp <- olapR::execute2D(olapCnn, mdx)
  # temp <- subset(temp, is.na(temp[[3]])==F  )
  temp$var1 = var1
  temp$var2 = var2
  
  # temp$var4 = var4
  return(temp)
}

for (i in 130:1000 ) {
  start_time <- Sys.time()
  var1 = as.character((INTERATION[[1]][i] )[1])
  var2 = as.character( (INTERATION[[2]][i] )[1] )
  
  # var4 = as.character( (INTERATION[[4]][i] )[1] )
  print("/------------------------------------------/")
  print("/----------Its a new iteration-------------/")
  print( paste0('The iteration: ', i , " is: '", var1, "',  ", var2, "'",  "'" ))
  print("/------------------------------------------/")
  
  mdx<- "SELECT  {[Measures].[Número de Defunciones]} ON AXIS(0),
                               {[Fecha de Defunción].[Año].MEMBERS} ON AXIS(1) , 
                               {[Geografía de Defuncion].[Municipio].MEMBERS} ON AXIS(2) 
                        FROM [EEVV - Defunciones]
        WHERE ([Fecha de Defunción].[Mes].&[%s],[Administradora].[Administradora].&[%s]) "
  mdx<- sprintf(mdx, var1, var2)
  
  tryCatch( {
    olapCnn<-olapR::OlapConnection(cnnstr_Estadisticas_Vitales)
    temp <- olapR::execute2D(olapCnn, mdx)
    temp$var1 = var1
    temp$var2 = var2
    
    colnames(temp) = c( 'Municipio', 'Anio','Defunciones', 'Mes', 'EPS' )
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

write.csv2(tabla, paste0(path_output, 'def_fec_mun_eps_1.csv'), row.names = F)

