packageList<-c("readxl","olapR", "foreign", "tidyverse", "haven","beepr", 'dplyr', 'readr', 'devtools', 'sqldf')
# devtools::install_github("apache/arrow/r")

for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}
source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/credentials.R")

#####################################################################
#
#####################################################################

devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/functions.R") 


metadata <-  c('EEVV - Defunciones' , 'EEVV - Nacimientos')
# ITERATIONS  


path_input = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/input/"
# EPS diccionario
# diccionario_codigos_eps <- 
  #read_excel(paste0(path_input, "diccionario_codigos_eps.xlsx" ) )
if(1==1){
A= data.frame("year" =c(2009:2021)  ) # 
A = data.frame( 'EPS_CODE' = c('0 - NO DEFINIDO',  '14-1 - COLSEGUROS',       '14-17 - VIDA ALFA S.A.', '14-18 - LIBERTY', '14-19 - SEGUROS DEL ESTADO',
      '14-23 - POSITIVA', '14-25 - COLMENA',      '14-26 - BBVA SEGUROS', '14-27 - CENTRAL SEGUROS',
      '14-28 - SURAMERICANA', '14-4 - COLPATRIA', '14-5 - AGRICOLA SEGUROS',
      '14-7 - BOLIVAR', '14-8 - AURORA', '25-10 - ISS RIESGOS', 'CCF001 - COMFAMILIAR CAMACOL', 'CCF002 - CONFAMA',
      'CCF007 - COMFAMILIAR CARTAGENA', 'CCF009 - COMFABOY',       'CCF015 - COMFACOR', 'CCF018 - CAFAM',
      'CCF023 - COMFAGUAJIRA', 'CCF024 - COMFAMILIAR HUILA', 'CCF027 - COMFAMILIAR NARIÑO',
      'CCF028 - COMFENALCO QUINDIO', 'CCF029 - COMFAMILIAR RISARALDA',
      'CCF030 - CAJASAI', 'CCF031 - CAJASAN', 'CCF032 - COMFENALCO SANTANDER',
      'CCF033 - COMFASUCRE', 'CCF035 -  CAFABA', 'CCF037 - COMFENALCO', 'CCF040 - COMFACARTAGO',
      'CCF045 - FAMISALUD COMFANORTE A.R.S', 'CCF049 - COMFAORIENTE', 'CCF050 - CENTRO ORIENTE MOVILIDAD SUBSIDIADO EPS',
      'CCF053 - COMFACUNDI', 'CCF054 - CCF FENALCO', 'CCF055 - CAJACOPI', 'CCF101 - COLSUBSIDIO', 'CCF102 - COMFACHOCO',
      'CCF103 - COMFACA', 'CCFC02 - CAJA DE COMPENSACION FAMILIAR DE ANTIOQU',  'CCFC07 - COMFAMILIAR CARTAGENA EPS-CCF DE CARTAGE',
      'CCFC09 - COMFABOY EPS - CCF DE BOYACA', 'CCFC10 - CAJA COLOMBIANA DE SUBSIDIO FAMILIAR COL', 'CCFC15 - COMFACOR EPS- CCF DE CORDOBA',
      'CCFC18 - CAJA DE COMPENSACION FAMILIAR CAFAM EPS', 'CCFC20 - COMFACHOCO - CCF DEL CHOCO', 'CCFC23 - COMFAMILIAR DE LA GUAJIRA EPS-CCF',
      'CCFC24 - COMFAMILIAR HUILA EPS -CCF', 'CCFC27 - COMFAMILIAR NARI&#209;O EPS -CCF',
      'CCFC33 - COMFASUCRE EPS - CCF DE SUCRE',       'CCFC50 - CAJA DE COMPENSACION FAMILIAR DEL ORIENT',
      'CCFC53 - COMFACUNDI - CCF DE CUNDINAMARCA', 'CCFC55 - CAJACOPI ATLANTICO - CCF',
      'EAS016 - EPM',  'EAS027 - FERROCARRILES NALES', 'EMP002 - EMP CAFESALUD',       'EMP023 - COLSANITAS S.A. COMPAÑÍA DE MEDICINA PRE',
      'EPIO06 - EPIO06', 'EPS001 - ALIANSALUD',      'EPS002 - SALUD TOTAL', 'EPS003 - CAFESALUD',
      'EPS005 - SANITAS', 'EPS006 - ISS', 'EPS008 - COMPENSAR', 'EPS009 - COMFENALCO ANTIOQUIA',
      'EPS010 - EPS SURA', 'EPS012 - COMFENALCO VALLE', 'EPS013 - SALUDCOOP', 'EPS014 - HUMANA VIVIR',
      'EPS015 - COLPATRIA', 'EPS016 - COOMEVA', 'EPS017 - FAMISANAR', 'EPS018 - SOS',
      'EPS020 - CAPRECOM', 'EPS022 - CONVIDA', 'EPS023 - CRUZ BLANCA', 'EPS025 - CAPRESOCA',
      'EPS026 - SOLSALUD', 'EPS028 - CALISALUD',       'EPS030 - CONDOR', 'EPS031 - SELVASALUD',
      'EPS033 - SALUDVIDA',       'EPS034 - SALUDCOLOMBIA',  'EPS035 - RED SALUD', 'EPS037 - NUEVA EPS',
      'EPS038 - MULTI MEDICAS', 'EPS039 - GOLDEN GROUP', 'EPS040 - SAVIA SALUD EPS',  'EPS041 - NUEVA EPS SA',
      'EPS044 - MEDIMAS Contributivo', 'EPS045 - MEDIMAS Movilidad', 'EPS046 - Fundación SALUD MIA_CONTRIBUTIVO',       'EPSC03 - CAFESALUD  E.P.S.  S.A.-CM',
      'EPSC20 - CAJA DE PREVISION SOCIAL DE COMUNICACION', 'EPSC22 - ENTIDAD ADMINISTRADORA  DE REGIMEN SUBSI',
      'EPSC25 - CAPRESOCA EPS', 'EPSC33 - SALUDVIDA  S.A.  E.P.S.-CM', 'EPSC34 - CAPITAL SALUD EPSS S A S', 'EPSI01 - DUSAKAWI E.P.S.I.',
      'EPSI02 - MANEXKA', 'EPSI03 -  AIC-EPSI-I', 'EPSI04 - ANASWAYUU', 'EPSI05 - MALLAMAS', 'EPSI06 - PIJAOSALUD EPSI',
      'EPSIC1 - ASOCIACION DE CABILDOS INDIGENAS DEL CES', 'EPSIC2 - ASOCIACION DE CABILDOS DEL RESGUARDO IND',
      'EPSIC3 - ASOCIACION DE INDIGENAS DEL CAUCA  A I C', 'EPSIC4 - ENTIDAD PROMOTORA DE SALUD INDIGENA  ANA',
      'EPSIC5 - ENTIDAD PROMOTORA DE SALUD  MALLAMAS  EP', 'EPSIC6 - ENTIDAD PROMOTORA DE SALUD  PIJAOS SALUD',
      'EPSM03 - CAFESALUD EPSS SA', 'EPSM33 - SALUDVIDA SA ENTIDAD PROMOTORA DE SALUD',
      'EPSS01 - ALIANSALUD ENTIDAD PROMOTORA DE SALUD SA', 'EPSS02 - SALUD TOTAL', 'EPSS03 - CAFESALUD',
      'EPSS05 - ENTIDAD PROMOTORA DE SALUD SANITAS  SA', 'EPSS08 - COMPENSAR ENTIDAD PROMOTORA DE SALUD',
      'EPSS09 - COMFENALCO ANTIOQUIA', 'EPSS10 - SURA EPS', 'EPSS12 - COMFENALCO VALLE EPS',
      'EPSS13 - ENTIDAD PROMOTORA DE SALUD ORGANISMO COO',  'EPSS14 - HUMANA VIVIR', 'EPSS15 - SALUD COLPATRIA EPS',
      'EPSS16 - COOMEVA EPS  SA', 'EPSS17 - EPS  FAMISANAR LTDA', 'EPSS18 - ENTIDAD PROMOTORA DE SALUD SERVICIO OCCI',
      'EPSS23 - CRUZ BLANCA ENTIDAD PROMOTORA DE SALUD S', 'EPSS26 - SOLSALUD', 'EPSS33 - SALUDVIDA',
      'EPSS34 - CAPITAL SALUD', 'EPSS37 - NUEVA EPS SA - CM', 'EPSS39 - GOLDEN GROUP SA ENTIDAD PROMOTORA DE SAL',
      'EPSS40 - Alianza Medellin -savia Salud', 'EPSS41 - NUEVA EPS SA', 'EPSS44 - MEDIMAS Movilidad',       'EPSS45 - MEDIMAS Subsidiado',
      'EPSS46 - Fundación SALUD MIA_SUBSIDIADO', 'ESS002 - EMDISALUD',      'ESS024 - COOSALUD E.S.S.',
      'ESS062 - ASMET SALUD', 'ESS068 - ASTREA', 'ESS076 - AMBUQ',       'ESS091 -  ECOOPSOS',
      'ESS118 - EMSSANAR E.S.S.','ESS133 - COMPARTA',
      'ESS164 - PIJAOSALUD EPSE', 'ESS207 - MUTUAL SER', 'ESS208 - ANAS WAYUU',
      'ESSC02 - EMPRESA MUTUAL PARA EL DESARROLLO INTEGR', 'ESSC07 - ASOCIACION MUTUAL SER EMPRESA SOLIDARIA',
      'ESSC18 - ASOCIACION MUTUAL EMPRESA SOLIDARIA DE S', 'ESSC24 - COOSALUD ESS  COOPERATIVA DE SALUD Y  DE',
      'ESSC33 - COOP. DE SALUD COMUNITARIA  COMPARTA', 'ESSC62 - ASOCIACION MUTUAL LA ESPERANZA  ASMET  S',
      'ESSC76 - ASOCIACION MUTUAL BARRIOS UNIDOS DE QUIB', 'ESSC91 - ENTIDAD COOPERATIVA SOLIDARIA DE SALUD',
      'ESSI06 - ESSI06', 'NO REPORTADO - NO REPORTADO',       'RES001 - POLICIA NACIONAL',
      'RES002 - ECOPETROL', 'RES003 - FUERZAS MILITARES', 'RES004 - MAGISTERIO','RES005 - UATLANTICO',  'RES006 - CAPRUIS', 'RES007 - UVALLE', 'RES008 - UNISALUD', 'RES009 - UCAUCA', 'RES010 - UCARTAGENA', 'RES011 - UANTIOQUIA',
      'RES012 - UCORDOBA', 'RES013 - UNARIÑO',
      'RES014 - UPTC', 'UT-001 - UT-001', 'UT-002 - UT-002', 'UT-003 - UT-003','UT-004 - UT-004')  )
  #data.frame("EPS_CODE" = unique(diccionario_codigos_eps$codigo))
# B =  data.frame('GBD'= c('A. Accidentes',
#                          'A. Enfermedades infecciosas y parasitarias',
#                          'A. Tumores malignos',
#                          'B. Infecciones respiratorias',
#                          'B. Lesiones intencionales',
#                          'B. Otros tumores',
#                          'C. Causas maternas',
#                          'C. Diabetes mellitus',
#                          'D. Ciertas afecciones originadas en el período perinatal',
#                          'D. Otras enfermedades endocrinas, metabolicas, hematologicas e immunológicas',
#                          'E. Deficiencias de la nutrición',
#                          'E. Trastornos mentales y enfermedades del sistema nervioso',
#                          'Eventos (lesiones) de intención no determinada',
#                          'F. Enfermedades de los órganos de los sentidos',
#                          'G. Enfermedades cardiovasculares',
#                          'H. Enfermedades respiratorias',
#                          'I. Enfermedades digestivas',
#                          'J. Enfermedades del sistema genito-urinario',
#                          'K. Enfermedades de la piel',
#                          'L. Enfermedades del sistema músculo esquelético',
#                          'M. Anomalías congénitas',
#                          'N. Enfermedades dentales',
#                          'No Definido'))
# 
# B = data.frame("GBD_3" = c('',
#                            '01. Accidentes de tráfico /a',
#                            '01. Artritis reumatoide',
#                            '01. Bajo peso al nacimiento y prematurez',
#                            '01. Defectos de la pared abdominal',
#                            '01. Depresión unipolar mayor',
#                            '01. Desnutrición calórico protéica',
#                            '01. Enfermedad cardíaca reumática',
#                            '01. Enfermedad pulmonar obstructiva crónica',
#                            '01. Glaucoma',
#                            '01. Hemorragia obstétrica',
#                            '01. Infecciones respiratorias agudas bajas',
#                            '01. Lesiones autoinfligidas intencionalmente (suicidios)',
#                            '01. Nefritis y nefrosis',
#                            '01. Tuberculosis',
#                            '01. Tumor maligno de la boca y orofaringe',
#                            '01. Ulcera péptica',
#                            '02. Agresiones (homicidios)',
#                            '02. Anencefalia y malformaciones similares',
#                            '02. Asfixia y trauma al nacimiento',
#                            '02. Asma',
#                            '02. Cirrosis y otras enfermedades crónicas del hígado',
#                            '02. Deficiencia de yodo',
#                            '02. Enfermedad periodontal',
#                            '02. Enfermedades de transmisión sexual excluyendo VIH/SIDA',
#                            '02. Enfermedades hipertensivas',
#                            '02. Hipertrofia prostática benigna',
#                            '02. Infección puerperal',
#                            '02. Infecciones respiratorias agudas altas',
#                            '02. Intoxicaciones',
#                            '02. Osteoartritis',
#                            '02. Trastorno bipolar',
#                            '02. Tumor maligno del esófago',
#                            '03. Apendicitis',
#                            '03. Atresia anorectal',
#                            '03. Caídas accidentales',
#                            '03. Deficiencia de Vitamina A',
#                            '03. Edema proteinuria y trastornos hipertensivos en el embarazo',
#                            '03. Enfermedades isquémicas del corazón',
#                            '03. Esquizofrenia',
#                            '03. Gota',
#                            '03. Guerra y conflictos armados',
#                            '03. Otitis media',
#                            '03. Tumor maligno del estómago',
#                            '03. VIH/SIDA',
#                            '04. Anemia ferropénica',
#                            '04. Enfermedad cerebrovascular',
#                            '04. Enfermedades infecciosas intestinales',
#                            '04. Epilepsia',
#                            '04. Espondilopatías y otras dorsopatías',
#                            '04. Exposición al fuego, humo y llamas',
#                            '04. Labio leporino',
#                            '04. Parto obstruido',
#                            '04. Tumor maligno del colon y recto',
#                            '05. Aborto',
#                            '05. Ahogamiento y sumersión accidentales',
#                            '05. Enfermedades inflamatorias del corazón (exc. Fiebre reumática)',
#                            '05. Enfermedades prevenibles por vacunación',
#                            '05. Paladar hendido',
#                            '05. Trastorno por consumo de alcohol',
#                            '05. Tumor maligno del hígado',
#                            '06. Alzheimer y otras demencias',
#                            '06. Atresia esofágica',
#                            '06. Meningitis',
#                            '06. Otros accidentes',
#                            '06. Tumor maligno del páncreas',
#                            '07. Agenesia renal',
#                            '07. Enfermedad de Parkinson',
#                            '07. Hepatitis B',
#                            '07. Tumor maligno de tráquea, bronquios y pulmón',
#                            '07a. Hepatitis C',
#                            '08. Esclerosis múltiple',
#                            '08. Melanoma y otros tumores malignos de la piel',
#                            '08. Paludismo (malaria)',
#                            '08. Síndrome de Down',
#                            '09. Enfermedades tropicales',
#                            '09. Malformaciones congénitas del corazón',
#                            '09. Trastornos por consumo de drogas',
#                            '09. Tumor maligno de la mama',
#                            '10. Espina bífida',
#                            '10. Lepra',
#                            '10. Tumor maligno del cuello del útero',
#                            '11. Dengue',
#                            '11. Tumor maligno del útero',
#                            '12. Tumor maligno del ovario',
#                            '13. Tumor maligno de la próstata',
#                            '14. Infestaciones intestinales por nemátodos',
#                            '14. Migraña',
#                            '14. Tumor maligno de la vejiga',
#                            '15. Linfomas y mieloma múltiple',
#                            '15. Retraso mental',
#                            '16. Leucemia',
#                            'No Definido',
#                            'Otras causas maternas',
#                            'Otras causas perinatales',
#                            'Otras enfermedades cardiovasculares',
#                            'Otras enfermedades de la boca',
#                            'Otras enfermedades del sistema genito-urinario',
#                            'Otras enfermedades digestivas',
#                            'Otras enfermedades infecciosas',
#                            'Otras enfermedades musculoesqueléticas',
#                            'Otras enfermedades respiratorias',
#                            'Otras lesiones intencionales',
#                            'Otras malformaciones congénitas',
#                            'Otros trastornos de los órganos de los sentidos',
#                            'Otros trastornos neuropsiquiátricos',
#                            'Otros trastornos nutricionales',
#                            'Otros tumores malignos' ))

B= data.frame("Options" = c("1 - SI","2 - NO"))
}

INTERATION = sqldf(" SELECT * FROM A INNER JOIN B ON 1=1  ")
INTERATION = na.omit(INTERATION)
### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[Número de Defunciones]'
AXIS1 <- "[Fecha de Defunción].[Año]" #  '[Administradora].[Administradora]'
AXIS2 <- '[Geografía de Defuncion].[Municipio]'
Defuncion_Materna = data.frame() 

for (i in 1:nrow(INTERATION) ) {
  var1 = as.character((INTERATION[[1]][i] )[1])
  var2 = as.character( (INTERATION[[2]][i] )[1] )
  print(var1)
  print(var2)
  from_olap_catalog <- metadata[1]
  Filter = ('[Administradora].[Administradora].&[%s],[Mortalidad Materna].[Defunción Materna 42 días].&[%s]')
  where_filter <- sprintf(Filter, var1, var2)
  print(where_filter)
  
  mdx  = query_olap(AXIS0=AXIS0,AXIS1=AXIS1, AXIS2= AXIS2,
                    from_olap_catalog = from_olap_catalog, where_filter =where_filter  )
  
  
  tryCatch( {
    temp  = run_query_olap(cnnstr  = cnnstr_Estadisticas_Vitales, 
                           mdx  = mdx,var1 = var1,  
                           var2  = var2 )
    colnames(temp)[3] = 'Defuncion_Materna_42_dias'
    Defuncion_Materna = rbind(Defuncion_Materna, temp)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
}
path_output = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/"

write.csv2(Defuncion_Materna, paste0(path_output, 'Defuncion_Materna_42_dias.csv'), row.names = F)

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

