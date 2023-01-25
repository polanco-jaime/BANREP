global_path = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/Tutelas EPS/"
setwd(global_path)
#dir.create(file.path(mainDir, subDir))

#load("C:/Users/USER/Desktop/DID roads/did_roads/did_roads.RData")
lista = c('readr','readxl','sqldf','plyr', 
          'did' , 'arrow',  'plyr', 'ggplot2',
          'dplyr','fixest' , 'gargle' , 'stringr'
          #, 'bigrquery' 
)
for (i in 1:length(lista) ) {
  if(lista[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(lista[i])
  }
  lapply(lista[i], library, character.only = TRUE)
}
##################
temp = list.files('./',pattern="*.parquet")
tutelas = data.frame()
for (i in temp) {
  tempo= arrow::read_parquet(i)
  tutelas = rbind(tempo, tutelas)
}
# tutelas = tutelas[, c(2:9)]

colnames(tutelas)[1] = 'radicacion_expediente'
colnames(tutelas)[6] = 'fecha_radicacion'
tutelas$longitud = nchar(tutelas$radicacion_expediente)

tutelas =  sqldf::sqldf(" SELECT 
          REPLACE( substr(radicacion_expediente, 
                instr(radicacion_expediente,' '),
                longitud   ) ,  ' ' , '') rad_expediente,  *
                        FROM tutelas
                       ")
tutelas=sqldf("SELECT *, substr(corte, 0,5) year    FROM tutelas")

tutelas = sqldf("SELECT * FROM tutelas 
                WHERE Demandado NOT LIKE '%INPEC%'
                ")

source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/Scripts/homog_eps_function.R")

Tabla = eps_homog_nombre(Tabla = tutelas , eps_nombre = 'Demandado' )

Tabla = eps_validator(Tabla = Tabla , eps_nombre = "Demandado")

Tabla= sqldf("SELECT *, 
              case  
                    WHEN fecha_radicacion like 'Ene%' THEN year||'-01-01'  
                    when fecha_radicacion like 'Feb%' THEN year||'-02-01'  
                    when fecha_radicacion like 'Mar%' THEN year||'-03-01'  
                    when fecha_radicacion like 'Abr%' THEN year||'-04-01'  
                    when fecha_radicacion like 'May%' THEN year||'-05-01'  
                    when fecha_radicacion like 'Jun%' THEN year||'-06-01'  
                    when fecha_radicacion like 'Jul%' THEN year||'-07-01'  
                    when fecha_radicacion like 'Ago%' THEN year||'-08-01'  
                    when fecha_radicacion like 'Sep%' THEN year||'-09-01'  
                    when fecha_radicacion like 'Oct%' THEN year||'-10-01'  
                    when fecha_radicacion like 'Nov%' THEN year||'-11-01'  
                    when fecha_radicacion like 'Dic%' THEN year||'-12-01'        
      Else null end Fecha_corte
      FROM Tabla")
rm(tutelas)
###################################################################
columnas  = c('rad_expediente', 'Demandante', 'Demandado', "Primera Instancia" , "Segunda Instancia", 'homo_code_eps', 'Fecha_corte', 'year'   )
Tabla= Tabla[, columnas]
write.csv2(Tabla, 'Tutelas_eps.csv' , row.names = F )
length(unique(Tabla$rad_expediente))
Tabla = sqldf("select distinct * from Tabla")
rm(TAbla)
Tabla$expediente =  Tabla$rad_expediente
Tabla$rad_expediente = 1
devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/Scripts/functions.R") 
Agregado_mensual = aggregate_function(Tabla = Tabla, 
                   aggregate = 'sum',
                   cols_to_agg = 'rad_expediente', 
                   group_by = c('homo_code_eps', 'Fecha_corte', 'year'))

Agregado_mensual = subset(Agregado_mensual, Agregado_mensual$year != '2022')

Agregado_anual = aggregate_function(Tabla = Tabla, 
                                      aggregate = 'sum',
                                      cols_to_agg = 'rad_expediente', 
                                      group_by = c('homo_code_eps',  'year'))

Agregado_anual = subset(Agregado_anual, Agregado_anual$year != '2022')

###############################################

path_output = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/"

library(readxl)
if(1==1){
  asegurados_Tipo_regimen <- read_excel(paste0(path_output, "asegurados_caracteristicas.xlsx" ) , 
                                        sheet = "Tipo_regimen")
  asegurados_com_indigena <- read_excel(paste0(path_output, "asegurados_caracteristicas.xlsx" ) , 
                                        sheet = "com_indigena")
  
  asegurados_genero <- read_excel(paste0(path_output, "asegurados_caracteristicas.xlsx" ) , 
                                  sheet = "genero")
  asegurados_etarios <- read_excel(paste0(path_output, "asegurados_etarios.xlsx" ) )  
  
  asegurados <- read_excel(paste0(path_output, "asegurados.xlsx" ), 
                           col_types = c("text", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "skip", "skip", "skip", "skip"))
  
  library(readxl)
  fecha_inicio <- read_excel(paste0(path_output, "fecha_inicio_actividades_administradoras.xlsx" ) ,
                             sheet = "eapb", col_types = c("text", 
                                                           "text", "numeric", "text", "numeric", 
                                                           "numeric", "date", "text"))
  
  fecha_inicio = data.frame('fec_inact' = c(
    '2016' ,  '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2015' , '2015' , '2014' , '2016' , '2014' , '2014' , '2014' , 
    '2014' , '2014' , '2014' , '2016' , '2017' , '2014' , '2016' , '2016' , '2014' , '2014' , '2014' , '2014' , '2016' , '2014' , 
    '2014' , '2014' , '2014' , '2016' , '2016' , '2016' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , 
    '2016' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , 
    '2017' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2016' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , 
    '2014' , '2014' , '2016' , '2014' , '2014' , '2014' , '2014' , '2015' , '2015' , '2014' , '2014' , '2014' , '2014' , '2014' , 
    '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , 
    '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , 
    '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , 
    '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , 
    '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , 
    '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , 
    '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , 
    '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2016' , '2016' , '2014' , '2014' , '2014' , 
    '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , 
    '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2016' , '2014' , '2016' , 
    '2014' , '2017' , '2014' , '2015' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , '2014' , 
    '2016' , '2016' , '2016' , '2016' , '2016' , '2018' , '2017' , '2017' ) 
    ,
    
    'eps' = c(
      '37177' , '37024' , 'ESS208' , 'EMP024' , 'CCF017' , 'SAP034' , 'SAP031' , 'ESS177' , 'EPS103' , 'ESS061' , 'ESS107' , 'PEC005' , 'PEC002' , 'EPS027' , 
      'PEC006' , '14-26' , '13-41' , 'CCF018' , 'EPS003' , 'EMP002' , 'CCF101' , 'UT-004' , 'EAS009' , 'EAS017' , 'EAS019' , 'REUE02' , 'EAS021' , 'EAS020' , 
      'EAS003' , 'EAS008' , 'EAS018' , 'UT-001' , 'EPS028' , 'EPS020' , 'CCF004' , 'CCF016' , 'CCF039' , 'CCF030' , 'CCF020' , 'CCF012' , 'CCF014' , 'CCF013' , 
      'CCF019' , 'CCF038' , 'CCF021' , 'CCF031' , 'CCF010' , 'CCF008' , 'CCF028' , 'CCF032' , 'CCF025' , 'CCF026' , 'CCF034' , 'CCF036' , '13-21' , 'EMP017' , 
      'CCF002' , '37147' , '37238' , '13-42' , '37117' , '37209' , '13-18' , 'EPS030' , '14-14' , 'EAS013' , '05000' , 'REFM01' , 'REPN01' , 'EAS006' , 
      'EAS014' , 'EAS005' , 'EPS029' , 'SAP030' , 'SAP008' , 'SAP026' , 'EAS029' , 'ESS115' , 'ESS164' , 'EPS129' , 'EPS130' , 'ESS122' , 'ESA016' , 'ESE113' , 
      'ESS119' , 'ESS080' , 'ESS084' , 'ESS083' , 'ESS098' , 'ESS170' , 'ESS069' , 'ESS100' , 'ESS073' , 'ESS112' , 'ESS074' , 'ESS101' , 'ESS077' , 'ESS057' , 
      'ESS175' , 'ESS060' , 'ESS114' , 'ESS178' , 'ESS017' , 'ESS071' , 'ESS066' ,'ESS019' , 'ESS051' , 'ESS047' , 'ESS179' , 'ESS187' , 'ESS056' , 'ESS012' , 
      'ESS054' , 'ESS016' , 'ESS113' , 'ESS070' , 'ESS127' , 'ESS121' , 'ESS153' , 'ESS126' , 'ESS028' , 'ESS136' , 'ESS035' , 'ESS130' , 'ESS120' , 'ESS131' , 
      'ESS128' , 'ESS095' , 'ESS193' , 'ESS140' , 'ESS143' , 'ESS144' , 'ESS205' , 'ESS191' , 'ESS196' , 'ESS151' , 'ESS186' , 'ESS195' , 'ESS200' , 'ESS105' , 
      'ESS147' , 'ESS162' , 'ESS145' , 'ESS092' , 'ESS154' , 'ESS094' , 'ESS146' , 'ESS090' , 'ESS025' , 'ESS029' , 'ESS129' , 'ESS034' , 'ESS166' , 'ESS123' , 
      'ESS110' , 'ESS015' , 'ESS109' , 'ESS021' , 'ESS132' , 'ESS046' , 'ESS163' , 'ESS093' , 'ESS088' , 'ESS096' , 'ESS032' , 'ESS085' , 'ESS192' , 'ESS201' , 
      'ESS139' , 'ESS138' , 'ESS152' , 'ESS086' , 'ESS008' , 'ESS157' , 'ESS042' , 'ESS044' , 'ESS045' , 'ESS043' , 'ESS041' , 'ESS159' , 'ESS161' , 'ESS174' ,  
      'ESS087' , 'ESS165' , 'ESS137' , 'ESS158' , 'ESS134' , 'PEC004' , 'REMG01' , 'EAS001' , 'EAS028' , 'EMP012' , 'EPS014' , 'RES006' , 'EAS022' , '36906' , 
      '13-22' , '13-33' , 'EMP015' , 'EPS032' , '14-15' , '14-16' , 'REUE07' , 'EPS035' , 'EPS019' , '13-15' , 'SAP035' , '13-30' , '13-25' , '13-27' , 
      '14-17' , 'SAP032' , 'REUE08' , 'EAS024' , 'EAS011' , 'EAS012' , 'SAP027' , 'ESS203' , 'REUE03' , 'REUE06' , 'REUE10' , 'EPS007' , 'UT-003' , 'REUE01' , 
      'EMP022' , 'EPSS03' , 'EPSS26' , 'ESS182' , 'ESS188' , 'CCP053' , 'EPS133' , 'ARS003' , '14-1' , '14-4' , '14-5' , '14-7' , '14-8' , '25-10' , 'CCFC18' , 
      'EPSMO3' , 'CCFC10' , 'EPSC20' , 'EPSS39' , 'EPS104' , 'EPSC03' , 'EPSM03' ) )
  
  fecha_inicio = eps_homog (Tabla =  fecha_inicio  , CODIGO__EPS = "eps" ) 
  fecha_inicio$fec_inact = as.integer(fecha_inicio$fec_inact)
  fecha_inicio = aggregate_function(aggregate = 'min',
                                    cols_to_agg = colnames(fecha_inicio)[1]  ,
                                    group_by = colnames(fecha_inicio)[3] ,
                                    Tabla = fecha_inicio)
  
  fech_liqui = data.frame( "EPS" = c('EPS003', 'EPS013'	,  'EPS020' ,  'EPS024' , 'EPS035' ,  'EPS015' ,   'EPS014' ,   'EPS009' ,  'EPSC03' ,  'CCFC10' , 'CCFC18'),
                           "fec_liqu" = c('2017' , '2015', '2015' , '2009' , '2011' , '2013' , '2013' ,  '2015' , '2017' , '2015' , '2015')
  )
  fech_liqui = eps_homog (Tabla =  fech_liqui  , CODIGO__EPS = "EPS" ) 
  fech_liqui$fec_liqu = as.integer(fech_liqui$fec_liqu)
  fech_liqui = aggregate_function(aggregate = 'min',
                                  cols_to_agg = colnames(fech_liqui)[2]  ,
                                  group_by = colnames(fech_liqui)[3] ,
                                  Tabla = fech_liqui)
  
  fecha_inicio$fec_inact = as.character(fecha_inicio$fec_inact)
  fech_liqui$fec_liqu = as.character(fech_liqui$fec_liqu)
  library(tidyr)
  
  asegurados = asegurados %>% tidyr::pivot_longer(!eps, names_to = "Years", values_to = "Asegurados")         
  asegurados_Tipo_regimen$year = as.character(asegurados_Tipo_regimen$year)
  asegurados_genero$year = as.character(asegurados_genero$year)
  asegurados_com_indigena$year = as.character(asegurados_com_indigena$year)
  asegurados_etarios$year= as.character(asegurados_etarios$year)
  
  
  asegurados = sqldf::sqldf("
                 SELECT  A.*,B.TOTAL,  CONTRIBUTIVO , EXCEPCION , SUBSIDIADO   ,
                 FEMENINO, MASCULINO , COMUNIDADES_INDIGENAS,
                 De_0_a_9 , De_10_a_19 , De_20_a_29 , De_30_a_39, 
                 De_40_a_49 , De_50_a_59 , De_60_a_69 ,
                 De_70_a_79 , Mayor_De_80
                 FROM asegurados A
                 LEFT JOIN asegurados_Tipo_regimen B 
                 ON TRIM(EPS)  = TRIM(B.COD_EPS)  AND TRIM(YEARS) = TRIM(B.YEAR)
                 LEFT JOIN asegurados_genero C
                 ON TRIM(EPS)  = TRIM(C.COD_ENTI)  AND TRIM(YEARS) = TRIM(C.YEAR)
                 LEFT JOIN asegurados_com_indigena D
                 ON TRIM(EPS)  = TRIM(D.COD_ENT)  AND TRIM(YEARS) = TRIM(D.YEAR)
                 LEFT JOIN asegurados_etarios E
                 ON TRIM(EPS)  = TRIM(E.Cod_entidad)  AND TRIM(YEARS) = TRIM(E.YEAR)
                 
     
                 ")
  
  asegurados = eps_homog (Tabla = asegurados , CODIGO__EPS = "eps" )
  
  asegurados$Asegurados = as.integer(asegurados$Asegurados)
  asegurados = subset(asegurados, asegurados$Years != '2009')
  asegurados = subset(asegurados, asegurados$Years != '2010')
  asegurados = subset(asegurados, asegurados$Years != '2011')
  asegurados = subset(asegurados, asegurados$Years != '2021')
  asegurados = subset(asegurados, asegurados$Years != '2022')
  asegurados = aggregate_function(aggregate = 'sum',
                                  cols_to_agg = colnames(asegurados)[3:19]  ,
                                  group_by = colnames(asegurados[,c(2, 20)]) ,
                                  Tabla = asegurados)
  
  #RES006
  # CCF010
  #EPS044 
  asegurados = sqldf::sqldf("
                 SELECT fec_inact, fec_liqu, A.* 
                 FROM asegurados A 
                 LEFT JOIN fecha_inicio F
                 ON TRIM(A.homo_code_eps)  = TRIM(F.homo_code_eps)
                 LEFT JOIN fech_liqui G
                 ON TRIM(A.homo_code_eps)  = TRIM(G.homo_code_eps)   
                 ")
  
  ## Dropping full nulls
  
  # EPS ppara omitir RES011 , FMS001 , POL001  ,ESS115 
  asegurados_ = data.frame()
  eps_ = unique(asegurados$homo_code_eps)
  
  ####### A este punto tenemos 96 EPS.
  length(eps_)
  
  for (i in eps_) {
    temp = subset(asegurados, asegurados$homo_code_eps == i)
    temp = (na_by_cols(temp))
    temp$homo_code_eps = i
    asegurados_ = rbind(asegurados_, temp)
  }
  eps_nulls =  unique(subset(asegurados_, asegurados_$Asegurados <= 0.8))$homo_code_eps
  asegurados = subset(asegurados,  asegurados$homo_code_eps   %in%  eps_nulls  )
  
  
  
  eps_ = unique(asegurados$homo_code_eps)
  length(eps_)
  ####### A este punto tenemos 91 EPS, 5 eps se pierden pues tienen mas del 80% de nulos.    
  # Si tengo valores de asegurados totales pero no tengo valor en ese campo, entonces es 0
  for (i in colnames(asegurados)[5:20] ) {
    asegurados[[i]] = ifelse(is.na(asegurados[[5]] )  ==F & is.na(asegurados[[i]]) ==T,  0 , asegurados[[i]] )
  }
  # Acá dejo todo en fracciones segun el total de datos
  for (i in colnames(asegurados)[7:21]) {
    asegurados[[i]] = as.numeric( as.numeric(asegurados[[i]] ) / as.numeric( asegurados[[6]]) )
  }
  
  ### Reviso la proporcion de nulos que tiene cada una de las 91 EPS
  
  asegurados_ = data.frame()
  eps_ = unique(asegurados$homo_code_eps)
  
  ####### A este punto tenemos 88 EPS.
  length(eps_)
  
  for (i in eps_) {
    temp = subset(asegurados, asegurados$homo_code_eps == i)
    temp = (na_by_cols(temp))
    temp$homo_code_eps = i
    asegurados_ = rbind(asegurados_, temp)
  }
  
  # 16 EPSs presentan mas del 22% de los registros nulos, de los cuales los mas criticos son 12 que presentan valores nulos dificiles de imputar
  
  eps_nulls =  unique(subset(asegurados_, asegurados_$Asegurados  > 0 ))$homo_code_eps
  dropped  =  subset(asegurados,  asegurados$homo_code_eps   %in%  eps_nulls  )
  asegurados  =  subset(asegurados,  !asegurados$homo_code_eps   %in%  eps_nulls  )
  eps_asegurados = unique(asegurados$homo_code_eps)
  
 
}  
glimpse(asegurados)
glimpse(Agregado_anual)
Agregado_anual$rad_expediente = as.numeric(Agregado_anual$rad_expediente)
asegurados = asegurados[,3:6]
asegurados = sqldf("
                   SELECT A.* , ifnull(rad_expediente, 0)  as total_tutelas, 
                   (ifnull(rad_expediente, 0) / (Total/1000) ) tutelas_x1000_afiliado
                   FROM asegurados A
                   LEFT JOIN Agregado_anual B
                   on A.homo_code_eps = B.homo_code_eps  
                   and A.years = B.year
                   ")
# asegurados = asegurados[, c(1:4,20,21)]
getwd()
write.csv(Tabla, 'C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/tutelas_eps_2000_2022.csv')

library(readr)
Tabla = read.csv('C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/tutelas_eps_2000_2022.csv')
colnames(Tabla)
library(sqldf)
sqldf("SELECT  year, count( expediente) Total 
          FROM Tabla group by 1
      ")
write.csv(asegurados, 'C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/tutelas_x1000_asegurados.csv')
sum(asegurados$total_tutelas)

write.csv2(Agregado_mensual, 'C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/tutelas_mensuales.csv')
# del 2012 al 2020 se encontraron 596655 tutelas.