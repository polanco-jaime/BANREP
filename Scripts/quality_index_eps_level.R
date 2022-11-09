# Calling libraries
options(scipen=999)
packageList<-c("readxl", "readr", "tidyverse", "sqldf", 'tidyr',
               "beepr", 'dplyr', 'readr', 'devtools', 'haven',
               'reshape2')
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
}    
### Reviso la proporcion de nulos que tiene cada una de las 91 EPS

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

# 17 EPSs presentan mas del 22% de los registros nulos, de los cuales los mas criticos son 12 que presentan valores nulos dificiles de imputar

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

############################################

for (i in colnames(asegurados)[5:20]) {
  asegurados[[i]] = ifelse(is.na(asegurados[[5]] )  ==T & asegurados[[3]] <= asegurados[[2]] , 0,   asegurados[[i]]    )
}

asegurados = subset(asegurados,  asegurados$homo_code_eps   %in%   unique(subset(asegurados_, asegurados_$Asegurados <= 0.5))$homo_code_eps )
#CCF029
asegurados$homo_code_eps   %in%   unique(subset(asegurados_, asegurados_$Asegurados <= 0.5))$homo_code_eps
######################################################################



Table_index = read.csv2(paste0(path_output, "Table_index.csv" ) )
Table_index =  Table_index[ c(3:30, 32:37)]
Table_index = eps_homog (Tabla = Table_index , CODIGO__EPS = "EPS_CODE_" )
Table_index = eps_quiebra (Tabla = Table_index , CODIGO__EPS = "EPS_CODE_" )
for (i in colnames(Table_index[,c(2:32)])) {
  Table_index[[i]] =  as.numeric(Table_index[[i]])
}
Table_index = subset(Table_index, Table_index$ANIO_ != '2009')
Table_index = subset(Table_index, Table_index$ANIO_ != '2010')
Table_index = subset(Table_index, Table_index$ANIO_ != '2011')
Table_index = subset(Table_index, Table_index$ANIO_ != '2021')
Table_index = subset(Table_index, Table_index$ANIO_ != '2022')
Table_index = aggregate_function(aggregate = 'sum',
                                 cols_to_agg = colnames(Table_index[,c(2:32)]) ,
                                 group_by = colnames(Table_index[,c(35,36,1)]) ,
                                 Tabla = Table_index)


Table_index = sqldf::sqldf("
             SELECT fec_inact, A.* 
             FROM Table_index A 
             LEFT JOIN fecha_inicio F
             ON TRIM(A.homo_code_eps)  = TRIM(F.homo_code_eps)   
             ")

#EPS044
for (i in colnames(Table_index)[5:35]) {
  Table_index[[i]] = ifelse( Table_index[[1]] >= Table_index[[4]] , 0,   Table_index[[i]]    )
}

Table_index_ = data.frame()
eps_ = unique(Table_index$homo_code_eps)
 
for (i in eps_) {
  temp = subset(Table_index, Table_index$homo_code_eps == i)
  temp = (na_by_cols(temp))
  Table_index_ = rbind(Table_index_, temp)
}







#####

Table_index = sqldf::sqldf("
             SELECT A.* , Asegurados FROM Table_index A
             LEFT JOIN asegurados B
             ON Years = ANIO_
             AND TRIM(eps)   = TRIM(EPS_CODE_)
             ")
#####
OP_MED_ODO <- read_csv(paste0(path_output, "OP_MED_ODO.csv" ) )
colnames(OP_MED_ODO)
Table_index = sqldf::sqldf("
             SELECT A.*,OP_MEDI_GENERAL, OP_ODO_GENERAL
             FROM Table_index A
             LEFT JOIN OP_MED_ODO B
             ON ANIO_ = YEAR
             AND TRIM(EPS_CODE_)   = TRIM(COD_EPS)
             ")
########### RIPS Atenciones

RIPS <- read_excel(paste0(path_output, "RIPS.xlsx" ) )
#

RIPS <- sqldf::sqldf("
             SELECT  * , substr(EPS, 0, instr(EPS,' - ') ) AS EPS_CODE FROM RIPS
             ")

colnames(RIPS)
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
             AND TRIM(EPS_CODE_)   = TRIM(EPS_CODE)
            
             ")

 
###############################################

NUMERICAS = c(7:32)
for (i  in NUMERICAS) {
   
  Table_index[[i]]  = Table_index[[i]] / (Table_index[[35]]/10000)
  
}

Table_index[[5]]  = 100*Table_index[[5]] / ( Table_index[[5]] + Table_index[[4]]  )
Table_index[[6]]  = 100*Table_index[[6]] / ( Table_index[[6]] + Table_index[[4]]  )


# }  
###########################################################################
# Crear de las variables de RIPS la proporcion segun numero de afiliados ( Columna 33 de Table_index)

for(i in c(7:32,37:42)){
  Table_index[[i]] <- Table_index[[i]]/Table_index$Asegurados
}
#Crear tasas de la informacion de RIPS agregada  (No la que esta con atx)
# Eliminar la variable 33
# Usar backward elimination para tomar las variables que sean mas significativas a la hora de que una eps quiebre
# Crear un pca solo con las variables significativas
# De cada buen PCA crear una tabla de correlaciones

# 1-  considerando tasas por asegurados
table_ =  Table_index[ , c(2,5,6,8,9:15,17,18,19,20,21,22,23,25,26,27,36:42)]
colnames(Table_index)
 
be = backwardElimination(  tabla= table_ , 
                           Y = 'eps_status' , 
                           sl = 0.1 ) 
be[["columnas"]]

table_ = na.omit(Table_index[, c(  be[["columnas"]] )])
pca4=prcomp(table_  ,   scale. = T,  rank. =1)
summary(pca4)
summary(pca4$x)
hist(pca4$x)

# 2-  considerando tasas por atenciones
table_ =  Table_index[ , c(2,5,6,8,9:15,17,18,19,20,21,22,23,25,26,27,43:48)]


be = backwardElimination(  tabla= table_ , 
                           Y = 'eps_status' , 
                           sl = 0.1 ) 
be[["columnas"]]

table_ = na.omit(Table_index[, c(  be[["columnas"]] )])
pca5=prcomp(table_  ,   scale. = T,  rank. =1)
summary(pca5)
summary(pca5$x)
hist(pca5$x)

# 3-  considerando tasas por mas registros
colnames(Table_index)
table_ =  Table_index[ , c(2,8,14,18,22,25,26,43:48)]

be = backwardElimination(  tabla= table_ , 
                           Y = 'eps_status' , 
                           sl = 0.15 ) 


table_ = na.omit(Table_index[, colnames(table_)])
# table_ = na.omit(Table_index[, c(  be[["columnas"]] )])
colnames(table_)

pca6=prcomp(table_  ,   scale. = T,  rank. =1)
summary(pca6)
summary(pca6$x)
hist(pca6$x)


#################################################################




# Eliminar columna de Nodefinido.
# Anexar la tabla tipo long de asegurados
# Dividir desde la col 4 a la 28 todos los valroes por asegruados
# hacer el PCA
# Documentar el PCA. 
#1- prueba pooled- tasas por afiliado
cols_number = c(2,5:41)
#2- prueba pooled tasas por ipc
cols_number= c(5:34,42:47)
#3- backward elimination                          (PC4)
cols_number4= c(17,20,21,26,28,29,30)
cols_number= c(17,20,21,26,28,29,30)
#4- backward elimination                          (PC5)
cols_number5= c(10,13,18,20,21,26,29,30,42)
cols_number= c(10,13,18,20,21,26,29,30,42)
#5 - backward elimination                         (pooled-PC6)
cols_number6= c(17,13,18,20,21,26,29,30,46,38,36,40)
cols_number= c(17,13,18,20,21,26,29,30,46,38,36,40)
#,8:28, 31
# PCA

table_4 <- table_wo_na (base = Table_index, cols_number = cols_number4)
table_4 <- subset(table_4, table_4[[3]]!=Inf )
pca_eps4 <- prcomp(table_4,   scale. = T,  rank. =1)
Table_index  = cbind(Table_index , predict(pca_eps4, Table_index) )
colnames(Table_index)[ncol(Table_index)] = "PCA4"

table_5 <- table_wo_na (base = Table_index, cols_number = cols_number5)
table_5 <- subset(table_5, table_5[[3]]!=Inf )
pca_eps5 <- prcomp(table_5,   scale. = T,  rank. =1)
Table_index  = cbind(Table_index , predict(pca_eps5, Table_index) )
colnames(Table_index)[ncol(Table_index)] = "PCA5"

table_6 <- table_wo_na (base = Table_index, cols_number = cols_number6)
table_6 <- subset(table_6, table_6[[3]]!=Inf )
pca_eps6 <- prcomp(table_6,   scale. = T,  rank. =1)
Table_index   = cbind(Table_index , predict(pca_eps5, Table_index))
colnames(Table_index)[ncol(Table_index)] = "PCA6"

# Table_index= Table_index[, -33]
## Columns used for the pca,   literature: https://docs.google.com/spreadsheets/d/1d4cK0EHsyxfNxbTv0aeLw4pjaOdkpbivb0A8rtjLAVY/edit?usp=sharing
# technical accuracy of the medical diagnoses and procedures, or the conformance to professional specification
## maternal mortality 

# frac_nulls_eps(df = Table_index, column_name = 'EPS_CODE_')
 
# Eliminar columna de Nodefinido.
# Anexar la tabla tipo long de asegurados
# Dividir desde la col 4 a la 28 todos los valroes por asegruados
# hacer el PCA
# Documentar el PCA. 
# # 
# cols_number = c(   5:34 ) 
#  #,8:28, 31
#  # PCA
#  if(1==1){
#    # table_ <- subset(table_, Table_index$ANIO_==2013 )
#    # table_ <- table_wo_na (base = table_, cols_number = cols_number )
#    table_ <- table_wo_na (base = Table_index, cols_number = cols_number )
#    table_ <- subset(table_, table_[[3]]!=Inf )
#    print('---------------------------------------------')
#    print(  data.frame( colnames(table_ ) )      )
#    print('---------------------------------------------')
#    print( paste0('filas ' , nrow((table_ ) ))  )
#    print('---------------------------------------------')
#    pca_eps <- prcomp(table_ ,   scale. = T,  rank. =1)
#    
#    print('---------------------------------------------')
#    print(summary(pca_eps))
#    
#    Table_1   = cbind(Table_index , predict(pca_eps, Table_index) )
#    Table_1 <- subset(Table_1, Table_1[[29]]!=Inf )
#    # P(EPS_Quiebre = 1| PC1 ) ~ Pvallue < 0.05
#    A = summary(lm(data = Table_1,  eps_status ~ PC1  ))
#    rm(Table_1)
#    print(A)
#    
#  }
# 
# hist(pca_eps$x)
# 
# 
# ####################################### backwardElimination
# be = backwardElimination(  tabla=  ,  sl = 0.5 ) 
# be[["columnas"]]
# Y = 'eps_status' 
# 
# table_ = dplyr::select(table_, -Y)
# 
# if(1==1){
#   table_ <- Table_index # table_wo_na (base = Table_index, cols_number = c(2, cols_number) )
#   table_ = table_[, c('eps_status', be[["columnas"]] )]
#   
#   table_ <- table_wo_na (base = table_, cols_number = c(1:16,36:49) )
#   
#   be = backwardElimination(  tabla= table_[ , c(2, 4:ncol(table_))] , Y = 'eps_status' ,  sl = 0.5 ) 
#    
#   
#   table_ = table_[, c(  be[["columnas"]] )]
#   pca=prcomp(table_  ,   scale. = T,  rank. =1)
#   summary(pca)
#   summary(pca$x)
#   hist(pca$x)
#   # A
#   print(A)
#   # print(B)
#   # hist(pca$x,10)
# }
# 
# 
# summary((pca_eps$x))
# pca_eps$x[1:10,]
# var_explained <- pca_eps$sdev^2/sum(pca_eps$sdev^2)*100
# var_explained
# ############### Tiene sentido
# 
# eps_from_code_to_name_n_status(Tabla = Table_index,eps_code = 'EPS_CODE_' )
# 
# ####### Writing
# 
# Table_index   = cbind(Table_index , predict(pca_eps, Table_index) )
# 
# write.csv2(Table_index  , paste0(path_output, "Table_index_eps.csv" ) , row.names = F) 
# colnames(Table_index)[36] = 'quality_index'
# final = Table_index[, c(1:3,36) ]
# write.csv2(final  , paste0(path_output, "Index_eps.csv" ) , row.names = F) 
# 
# # It may be useful to normalize observations by the number of affiliates, 
# # in itself, each column would show an indicator per capita
