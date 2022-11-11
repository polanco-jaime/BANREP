# Calling libraries
options(scipen=999)
packageList<-c("readxl", "readr", "tidyverse", "sqldf", 'tidyr',
               "beepr", 'dplyr', 'readr', 'devtools', 'haven',
               'reshape2', 'rlist')
for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}

devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/Scripts/functions.R") 
#################################################
# Reading tables used for index
#################################################
path_output = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/"

# if(1==1){
#####################################################################
## CARACTERISTICAS ASEGURADOS
 
##############                           
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


#################################
# EN 1995 BAJO UNA ALIANZA ESTRATEGIA ENTRE CAFAM Y COLSUBSIDIO SE CREA FAMISANAR EPS 
# EPS COLSUBSIDIO CCF010,CCFC10, CAFAM CCF018, CCFC18  -> PERTENECEN A EPS017 , EPSS17 , EPS017
#################################
# En 1995 se fundó EPS SERVICIO OCCIDENTAL DE SALUD S.O.S. (EPSS18 y EPS018), con el esfuerzo conjunto de Comfandi,
# Comfamiliar Caldas, Comfamiliar Pereira, Comfaunión Palmira, 
# Comfamiliar Tuluá, Comfamiliar Cartago  CCF040, 
# Comfamiliar Buga, Comsocial, Comfamar Buenaventura y Comfamiliar La Dorada. 
# Estas entidades aportaron no sólo los recursos económicos necesarios para la creación de S.O.S, 
# sino también toda su experiencia, conocimiento, vocación de servicio, 
# además de las IPS que en cada región atenderían a los afiliados y familiares.

################################# ¿UNIMOS MEDIMAS Y CAFESALUD? NO TIENE SENTIDO TENERLAS JUNTAS PERO TAMPOCO SEPARADAS
# MEDIMAS EPS044 EMPEZO OPERACIONES EN EL AÑO 2017 CON PACIENTES HEREDADOS DE CAFESALUD 
################################
asegurados_ = data.frame()
eps_ = unique(asegurados$homo_code_eps)

####### A este punto tenemos 72 EPS.
length(eps_)

for (i in eps_) {
  temp = subset(asegurados, asegurados$homo_code_eps == i)
  temp = (na_by_cols(temp))
  temp$homo_code_eps = i
  asegurados_ = rbind(asegurados_, temp)
}
}    
############################################
######################################################################
############################################
# sE AJUSTA LA TABLA A NIVEL MUNICIPIO
if (1==1) {
  Table_index = read.csv2(paste0(path_output, "Table_index.csv" ) )
  
  Table_index =  Table_index[ c(3:30, 32:37)]
  Table_index = eps_homog (Tabla = Table_index , CODIGO__EPS = "EPS_CODE_" )
  # Table_index = eps_quiebra (Tabla = Table_index , CODIGO__EPS = "EPS_CODE_" )
  for (i in colnames(Table_index[,c(2:32)])) {
    Table_index[[i]] =  as.numeric(Table_index[[i]])
  }
  get_inf(Table_index)
  Table_index = subset(Table_index, Table_index$ANIO_ != '2009')
  Table_index = subset(Table_index, Table_index$ANIO_ != '2010')
  Table_index = subset(Table_index, Table_index$ANIO_ != '2011')
  Table_index = subset(Table_index, Table_index$ANIO_ != '2021')
  Table_index = subset(Table_index, Table_index$ANIO_ != '2022')
  get_inf(Table_index)
  
  Table_index = aggregate_function(aggregate = 'sum',
                                   cols_to_agg = colnames(Table_index[,c(2:32)]) ,
                                   group_by = colnames(Table_index[,c(35,1)]) ,
                                   Tabla = Table_index)
  eps_ = unique(Table_index$homo_code_eps)
  #Hasta este punto tengo 68 EPS
  # Seleccionamos las eps que poseen informacion de sus caracteristicas. 
  Table_index = subset(Table_index, eps_asegurados  %in% Table_index$homo_code_eps)
  eps_ = unique(Table_index$homo_code_eps)
 
  print(paste0("Las eps a este punto son: ", length(unique(Table_index$homo_code_eps))))
  # "Las eps a este punto son: 68"
  ###############################################################################
  # glimpse(asegurados$Years)
  asegurados$Years = as.integer(asegurados$Years)
  #####
  
  Table_index = sqldf::sqldf("
             SELECT B.years, B.homo_code_eps as code_eps , A.* , 
             Total as Asegurados ,  CONTRIBUTIVO, 
             EXCEPCION , SUBSIDIADO, FEMENINO , MASCULINO ,
             COMUNIDADES_INDIGENAS , De_0_a_9 , De_10_a_19  , De_20_a_29 , De_30_a_39  ,
              De_40_a_49 , De_50_a_59 , De_60_a_69 , De_70_a_79  , Mayor_De_80 
             FROM asegurados B 
             LEFT JOIN Table_index A
             ON Years = ANIO_
             AND TRIM(A.homo_code_eps)   = TRIM(B.homo_code_eps)
             ")
  eps_ = unique(Table_index$code_eps)
  print(paste0("Las eps a este punto son: ", length(unique(Table_index$homo_code_eps))))
  #  "Las eps a este punto son: 54"
  Table_index$homo_code_eps = ifelse( is.na(Table_index$homo_code_eps), Table_index$code_eps, Table_index$homo_code_eps )
  Table_index$ANIO_ = ifelse( is.na(Table_index$ANIO_), Table_index$Years, Table_index$ANIO_ )
  Table_index = Table_index[ , c(3:51)]
  
  Table_index[[4]] = 1000*Table_index[[4]]/ (Table_index[[4]] + Table_index[[3]] )
  Table_index[[5]] = 1000*Table_index[[5]]/ (Table_index[[5]] + Table_index[[6]] )
  Table_index = Table_index[, c(1,2,4,5,8:49)]
    # Table_index_ = Table_index
  # Table_index = Table_index_
  ### De la base original sacamos la fraccion por numero de afiliados y de esto empezamos a hacer el fill
  for (i in colnames(Table_index)[5:26]) {
    
    Table_index[[i]] = as.numeric(Table_index[[i]])*100000 / as.numeric(Table_index[[34]])
  }
  for (i in colnames(Table_index)[27:30]) {
    
    Table_index[[i]] = (as.numeric(Table_index[[i]]/12)  / as.numeric(Table_index[[34]]))/10000
  }
  Table_index = subset(Table_index, Table_index$homo_code_eps %in% eps_asegurados) 
  print(paste0("Las eps a este punto son: ", length(unique(Table_index$homo_code_eps))))
  #  "Las eps a este punto son: 54"
}



 
 


table(is.infinite( Table_index[[i]]   ))

print(paste0("Las eps a este punto son: ", length(unique(Table_index$homo_code_eps))))
"Las eps a este punto son: 72"
# git status
# git add .
# git commit -m "2022-11-09"
# git push
# git status
############################################################################ 
#SE UNEN LAS DEMAS TABLAS
if (1==1) {
   OP_MED_ODO <- read_csv(paste0(path_output, "OP_MED_ODO.csv" ) )
   
   OP_MED_ODO = eps_homog (Tabla = OP_MED_ODO , CODIGO__EPS = "COD_EPS" )
   
   OP_MED_ODO = aggregate_function(aggregate = 'sum',
                                   cols_to_agg = colnames(OP_MED_ODO[,c(3,4)]) ,
                                   group_by = colnames(OP_MED_ODO[,c(2,5)]) ,
                                   Tabla = OP_MED_ODO)
   OP_MED_ODO$YEAR = as.integer(OP_MED_ODO$YEAR)
   OP_MED_ODO = sqldf::sqldf("SELECT * FROM OP_MED_ODO WHERE YEAR BETWEEN 2012 AND 2020")
   
   
   OP_MED_ODO = subset(OP_MED_ODO, OP_MED_ODO$homo_code_eps %in% eps_asegurados) 
   
   Table_index = sqldf::sqldf("
             SELECT A.*,OP_MEDI_GENERAL, OP_ODO_GENERAL
             FROM Table_index A
             LEFT JOIN OP_MED_ODO B
             ON ANIO_ = YEAR
             AND TRIM(A.homo_code_eps)   = TRIM(B.homo_code_eps)
             ")
   
   ########### RIPS Atenciones
   
   RIPS <- read_excel(paste0(path_output, "RIPS.xlsx" ) )
   RIPS <- sqldf::sqldf("
             SELECT  * , substr(EPS, 0, instr(EPS,' - ') ) AS EPS_CODE FROM RIPS
             ")
   RIPS = eps_homog (Tabla = RIPS , CODIGO__EPS = "EPS_CODE" )
   
   RIPS = aggregate_function(aggregate = 'sum',
                             cols_to_agg = colnames(RIPS[,c(2:8,10:16)]) ,
                             group_by = colnames(RIPS[,c(9,18)]) ,
                             Tabla = RIPS)
   #EPS001, EPS002
   RIPS$ANIO_ = as.integer(RIPS$ANIO_)
   glimpse(RIPS)
   RIPS = sqldf::sqldf("SELECT * FROM RIPS WHERE anio_ BETWEEN 2012 AND 2020")
   
   RIPS = subset(RIPS, RIPS$homo_code_eps %in% eps_asegurados) 
   
   
   for (i in 1:length(colnames(RIPS))) {
     print(table(is.infinite( RIPS[[i]]   )))
   }
   
   
   Table_index = sqldf::sqldf("
             SELECT A.*,
              Conteo_de_Prestadores, Costo_Consulta, Costo_Procedimiento, Neto_A_Pagar_Consulta, 
              Numero_de_Atenciones , Numero_Dias_Estancia , Valor_Cuota_Moderadora , 
             Atx_Conteo_de_Prestadores , Costo_Consulta_xAt , Costo_Procedimiento_xAt ,
             Neto_A_Pagar_Consulta_xAt, -- Numero_de_Atenciones_xAt, 
             Numero_Dias_Estancia_xAt, Valor_Cuota_Moderadora_xAt
             
             FROM Table_index A
             LEFT JOIN RIPS B
             ON B.ANIO_ = A.ANIO_
             AND TRIM(A.homo_code_eps)   = TRIM(B.homo_code_eps)
            
             ")
   #
   0.2420089862
   15739867384.3895
   
   for (i in colnames(Table_index)[52:58]) {
     
     Table_index[[i]] = as.numeric(Table_index[[i]]) / as.numeric(Table_index[[34]])
   }
   
   
   #####
   library(haven)
   time_sensitive_death_rate <- read_dta(paste0(path_output, "New/time_sensitive_death_rate.dta" ) )
   time_sensitive_death_rate = eps_homog (Tabla = time_sensitive_death_rate , CODIGO__EPS = "cod_eapb" )
   
   time_sensitive_death_rate = aggregate_function(aggregate = 'AVG',
                                                  cols_to_agg = colnames(time_sensitive_death_rate)[4] ,
                                                  group_by = colnames(time_sensitive_death_rate[,c(2,5)]) ,
                                                  Tabla = time_sensitive_death_rate)
   
   
   
   time_sensitive_death_rate = sqldf::sqldf("SELECT * FROM time_sensitive_death_rate WHERE YEAR BETWEEN 2012 AND 2020")
   time_sensitive_death_rate = subset(time_sensitive_death_rate, time_sensitive_death_rate$homo_code_eps %in% eps_asegurados) 
   time_sensitive_death_rate$year = as.integer(time_sensitive_death_rate$year)
   time_sensitive_death_rate$deaths_percentage_group
   
   Table_index = sqldf::sqldf("
             SELECT A.*, deaths_percentage_group
             
             FROM Table_index A
             LEFT JOIN time_sensitive_death_rate B
             ON B.YEAR = A.ANIO_
             AND TRIM(A.homo_code_eps)   = TRIM(B.homo_code_eps)
            
             ")
 }
print(paste0("Las eps a este punto son: ", length(unique(Table_index$homo_code_eps))))
"Las eps a este punto son: 72"


###############################################
### Missing treatment

# Table_index1 = Table_index

Table_index_ = data.frame()
eps_ = unique(Table_index$homo_code_eps)

for (i in eps_) {
  temp = subset(Table_index, Table_index$homo_code_eps == i)
  temp = (na_by_cols(temp))
  temp$homo_code_eps = i
  Table_index_ = rbind(Table_index_, temp)
}

Table_index = subset(Table_index, 
                     ! Table_index$homo_code_eps %in% eps_qual_full_null )

summary(Table_index)
eps_qual_full_null = Table_index_ %>% subset(def_mat_42d_si==1)
eps_qual_full_null = unique(eps_qual_full_null$homo_code_eps)

Table_index = subset(Table_index, ! Table_index$homo_code_eps %in% eps_qual_full_null )
print(paste0("Las eps a este punto son: ", length(unique(Table_index$homo_code_eps))))
# >> "Las eps a este punto son: 53"
# 6 eps presentan mas de 50% de missing
# 
 
Table_index = Table_index[, c(1:62)]
var_calidad = c(62,3:30, 47:48,60)
colnames(Table_index) [c(32,34:46,56)] 
summary(Table_index[[56]]) 
for (i in var_calidad) {
  print("/------------------------------------------/")
  
  Y_var = colnames(Table_index)[i]
  print(paste0("El modelo de la variable " , Y_var, " se creara"))
  X =  Table_index[,c(i,32,34:46,56 )] #,57,58,59,61
  X[[1]] = ifelse(X[[1]] == Inf, NA, X[[1]])
  X[[17]] = ifelse(X[[17]] == Inf, NA, X[[17]])
  X[[18]] = ifelse(X[[18]] == Inf, NA, X[[18]])
  X[[19]] = ifelse(X[[19]] == Inf, NA, X[[19]])
  
  X= na.omit(X)
  
  colnames(X)[1] = 'Y'
  
  tryCatch( {
    mod = lm(data = X , Y~.)
    Table_index$Pred= predict(mod, Table_index)
    colnames(Table_index)[ncol(Table_index)] = paste0(Y_var,'_hat')
  
  },
    error=function(e){ cat("ERROR CATCH: ",conditionMessage(e), "\n")}
  )

}

    
#######################################################
Table_index_ = data.frame()
eps_ = unique(Table_index$homo_code_eps)

for (i in eps_) {
  temp = subset(Table_index, Table_index$homo_code_eps == i)
  temp = (na_by_cols(temp))
  temp$homo_code_eps = i
  Table_index_ = rbind(Table_index_, temp)
}




NUMERICAS = c(3:93)
for (i  in NUMERICAS) {
   
  Table_index[[i]]  = as.numeric(Table_index[[i]]  )
  
}


# }  
###########################################################################
# PCA 1
data.frame(colnames(Table_index))
columnas_salud = c(62,3:30, 47:48,61)
table_1 <- table_wo_na (base = Table_index, cols_number = columnas_salud)
colnames(na.omit(Table_index[, columnas_salud ] ))
pca1=prcomp(table_1  ,   scale. = T,  rank. =1)
summary(pca1)

Table_index   = cbind(Table_index , predict(pca1, Table_index) )
colnames(Table_index)[ncol(Table_index)] = 'PCA_main_var'

###################################################################
# PCA 2
# Las mismas variables asociadas estan regresadas por las caracteristicas de la eps

columnas_modelos = c(61, 63:93 )
pca2=prcomp( na.omit(Table_index[, columnas_modelos ] ) ,   scale. = T,  rank. =1)
colnames(na.omit(Table_index[, columnas_modelos ] ))
summary(pca2)
Table_index   = cbind(Table_index , predict(pca2, Table_index) )
colnames(Table_index)[ncol(Table_index)] = 'PCA_main_hat_var'

######################################################

hist(Table_index[['PCA_main_var']], main = colnames(Table_index)['PCA_main_var'] )
hist(Table_index[['PCA_main_hat_var']], main = colnames(Table_index)['PCA_main_hat_var'] )
Tabla_corr = na.omit(Table_index[,c(63,62,94,95)])

cor(Tabla_corr[['PCA_main_var']],Tabla_corr[['PCA_main_hat_var']] )
colnames(Tabla_corr)
corr = cor(Tabla_corr[['deaths_percentage_group']],Tabla_corr[['PCA_main_hat_var']] )
print(paste0( " la relacion entre muertes y PCA2 es: ", corr ))
corr = cor(Tabla_corr[['PCA_main_var']],Tabla_corr[['deaths_percentage_group']] )
print(paste0( " la relacion entre muertes y PCA1 es: ", corr ))
 
corr = cor(Tabla_corr[['deaths_percentage_group_hat']],Tabla_corr[['deaths_percentage_group']] )
print(paste0( " la relacion entre muertes y PCA1 es: ", corr ))

colnames(Table_index)

#######
write.csv2(Table_index, paste0(path_output, "quality_eps_data.csv" ) , row.names = F)
#Table_index <- read.csv(paste0(path_output, "quality_eps_data.csv" ), sep = ';' )

