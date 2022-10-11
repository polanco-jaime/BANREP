# Calling libraries

packageList<-c("readxl", "readr", "tidyverse", "sqldf",
               "beepr", 'dplyr', 'readr', 'devtools', 'haven',
               'reshape2')
for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}
#################################################
# Reading tables used for PCA
#################################################
path_input = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/input/"
path_output = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/"

# EPS diccionario
diccionario_codigos_eps <- read_excel(paste0(path_input, "diccionario_codigos_eps.xlsx" ) )
 
 
# TABLE MORBILITIES
cu_morbilidad_asis <- read_delim( paste0(path_output, "index_cu_morbilidad_asis.csv" ), 
                                       delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip()), 
                                       locale = locale(encoding = "WINDOWS-1252"), 
                                       trim_ws = TRUE)
        
# TABLE HIGH COST DISEASES
alto_costo_subsidiado <- read_dta( paste0(path_output, "participacion_alto_costo_subsidiado.dta" ) )

# hooping cough
tos_ferina <- read_csv(paste0(path_output, "tos_ferina.csv" ) )

# Tutelas eps
                               
#################################################
# Setting tables for pca
#################################################
# TABLE AFILIADOS 

colnames(cu_morbilidad_asis)
cu_morbilidad_asis = sqldf::sqldf("
             SELECT
             DIVIPOLA,   B.EPS AS  NOM_EPS, NOM_REGIMEN, YEARS , quebrada ,
             
                (Signos_y_sintomas_mal_definidos)  as Signos_y_sintomas_mal_definidos, 
                (Lesiones)  as Lesiones, 
                (Enfermedades_no_transmisibles)  as Enfermedades_no_transmisibles, 
                (Condiciones_transmisibles_y_nutricionales)  as Condiciones_transmisibles_y_nutricionales,
               (value) as value
             
             FROM cu_morbilidad_asis A
            INNER JOIN diccionario_codigos_eps B
            ON  epS_=B.codigo 
            INNER JOIN afiliados C
            ON B.CODIGO = C.CODIGO AND VARIABLE = YEARS
             ")
gc()
cu_morbilidad_asis = sqldf::sqldf("
             SELECT
             DIVIPOLA,   NOM_EPS, NOM_REGIMEN, YEARS , quebrada ,
             
               SUM (Signos_y_sintomas_mal_definidos)  as Signos_y_sintomas_mal_definidos, 
                SUM(Lesiones)  as Lesiones, 
               SUM (Enfermedades_no_transmisibles)  as Enfermedades_no_transmisibles, 
               SUM (Condiciones_transmisibles_y_nutricionales)  as Condiciones_transmisibles_y_nutricionales,
             AVG  (value) as value
             
             FROM cu_morbilidad_asis
             GROUP BY DIVIPOLA,   NOM_EPS, NOM_REGIMEN, YEARS , quebrada
             ")
gc()
 

colnames(cu_morbilidad_asis)
columnas = c(  "Signos_y_sintomas_mal_definidos"  ,        
               "Lesiones"           ,                      
              "Enfermedades_no_transmisibles"    ,        
               "Condiciones_transmisibles_y_nutricionales")

correction_morbilidad(TABLA = 'cu_morbilidad_asis' , VARIABLE  =  "Signos_y_sintomas_mal_definidos")
 

cu_morbilidad_asis =  cu_morbilidad_asis[, c(1:9)]



# TABLE HIGH COST DISEASES
alto_costo_subsidiado$sub_porc <- alto_costo_subsidiado$sub_porc/100
alto_costo_subsidiado$ac_porc <- alto_costo_subsidiado$ac_porc/100
alto_costo_subsidiado$length <- nchar(alto_costo_subsidiado$cod_mpio)
alto_costo_subsidiado$cod_mpio <- as.character(alto_costo_subsidiado$cod_mpio)
alto_costo_subsidiado$cod_dpto <- as.character(alto_costo_subsidiado$cod_dpto)
alto_costo_subsidiado <- sqldf::sqldf(" SELECT   
             CASE 
             WHEN length = '1' THEN cod_dpto||'00'|| cod_mpio
             WHEN length = '2' THEN cod_dpto||'0'|| cod_mpio
             WHEN length = '3' THEN  cod_dpto||cod_mpio 
             ELSE NULL END AS DIVIPOLA, *
             FROM alto_costo_subsidiado
             ")
alto_costo_subsidiado <- alto_costo_subsidiado[ , -c(3,4,8) ] 
# hooping cough
######################################################################################
###########################################
######################################################################################
 
# Aca tengo que seguir dejando todo en terminos del nombre del codigo 
###########################################
options(scipen=999)
index_tos_ferina = sqldf::sqldf("
              SELECT NOM_EPS, ANO ,  SUM(TOS_FERINA)/( SUM(value) /1000) AS TOS_FERINA
              FROM ( 
                    SELECT COD_ASE, ANO, SUM(TOS_FERINA)  TOS_FERINA
                    FROM tos_ferina
                    GROUP BY 1,2 ) A
              INNER JOIN  afiliados B
              ON ANO = variable 
              AND CODIGO = COD_ASE
              GROUP BY 1,2 
              ")


# Tutelas eps
devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/functions.R") ## it deppends of devtools library

Btutelas_eps = homogenizacion_eps(tabla =Btutelas_eps ,
                                  Nombre_eps = 'eps' ,
                                  Regimen_salud = 'regimen')


 

index_tutelas_eps = sqldf::sqldf("
               SELECT EPS_CODE, ANIO , A.EPS,  TUTELAS/(value/1000) AS TUTELAS_INDEX,(value/1000)  afiliados_x_1000, TUTELAS 
               FROM
               (SELECT ANIO, K.EPS_CODE,C.EPS  
                      , SUM(TUTELAS)  TUTELAS
                    FROM Btutelas_eps K
                    INNER JOIN diccionario_codigos_eps C
                    ON  K.EPS_CODE = C.codigo 
                    GROUP BY ANIO, K.EPS_CODE,C.EPS ) A
               INNER JOIN  afiliados B
                ON ANIO = variable 
               AND B.CODIGO = A.EPS_CODE
              ")

 
#
OPOR_MEDICINA_GENERAL$OPOR_MEDICINA_GENERAL <- (OPOR_MEDICINA_GENERAL$OPOR_MEDICINA_GENERAL+0.01 )^-1
#
OPOR_ODONT_GENERAL$OPOR_ODONT_GENERAL <-  (OPOR_ODONT_GENERAL$OPOR_ODONT_GENERAL+0.01 )^-1


#################################################
# General in level 
#################################################
# Tables 
nrow(sqldf("SELECT years, NOM_EPS,
             AVG(Signos_y_sintomas_mal_definidos) AS CNT_Signos_y_sintomas_mal_definidos,
             AVG(Lesiones) AS CNT_Lesiones ,
             AVG(Condiciones_transmisibles_y_nutricionales ) AS CNT_Condiciones_transmisibles_y_nutricionales ,
             AVG(Enfermedades_no_transmisibles ) AS CNT_Enfermedades_no_transmisibles
           FROM cu_morbilidad_asis
           WHERE NOM_REGIMEN = '1 - CONTRIBUTIVO'
           GROUP BY 1,2"))

nrow(sqldf(" SELECT years, NOM_EPS,
             AVG(Signos_y_sintomas_mal_definidos ) AS SBS_Signos_y_sintomas_mal_definidos,
             AVG(Lesiones ) AS SBS_Lesiones ,
             AVG(Condiciones_transmisibles_y_nutricionales ) AS SBS_Condiciones_transmisibles_y_nutricionales ,
             AVG(Enfermedades_no_transmisibles) AS SBS_Enfermedades_no_transmisibles
           FROM cu_morbilidad_asis
           WHERE NOM_REGIMEN != '1 - CONTRIBUTIVO'
           GROUP BY 1,2"))

A = data.frame("NOMBRE" = unique(cu_morbilidad_asis$NOM_EPS) )
B = data.frame("ANIOS" = (2009:2022) )
C = data.frame("REGIMEN" = c('1 - CONTRIBUTIVO' , '2 - SUBSIDIADO') )
temp = sqldf("
        SELECT * FROM A INNER JOIN B ON 1=1 --INNER JOIN C ON 1=1
      ")
head(temp)

sbs <- sqldf("SELECT * FROM (
        SELECT 
        anios as years, NOMBRE  NOM_EPS , SBS_Signos_y_sintomas_mal_definidos,
        SBS_Lesiones , SBS_Condiciones_transmisibles_y_nutricionales,
        SBS_Enfermedades_no_transmisibles , SBS
      FROM temp
       LEFT JOIN (
          SELECT years, NOM_EPS,
             AVG(Signos_y_sintomas_mal_definidos ) AS SBS_Signos_y_sintomas_mal_definidos,
             AVG(Lesiones ) AS SBS_Lesiones ,
             AVG(Condiciones_transmisibles_y_nutricionales ) AS SBS_Condiciones_transmisibles_y_nutricionales ,
             AVG(Enfermedades_no_transmisibles) AS SBS_Enfermedades_no_transmisibles,
             1 SBS
           FROM cu_morbilidad_asis
           WHERE NOM_REGIMEN != '1 - CONTRIBUTIVO'
           GROUP BY 1,2
                )
       ON NOMBRE  = NOM_EPS
          )
      WHERE SBS = 1
      ")


unique(index_cu_morbilidad_asis$NOM_EPS)
if(1==1){
index_cu_morbilidad_asis <- sqldf::sqldf(" 
SELECT * FROM (
                                         SELECT A.*, 
                                         CNT_Signos_y_sintomas_mal_definidos,
                                         CNT_Lesiones,
                                         CNT_Condiciones_transmisibles_y_nutricionales,
                                         CNT_Enfermedades_no_transmisibles
                                         FROM (
              SELECT * FROM (
                      SELECT 
                      anios as years, NOMBRE  NOM_EPS , SBS_Signos_y_sintomas_mal_definidos,
                      SBS_Lesiones , SBS_Condiciones_transmisibles_y_nutricionales,
                      SBS_Enfermedades_no_transmisibles
                    FROM temp
                     LEFT JOIN (
                        SELECT years, NOM_EPS,
                           AVG(Signos_y_sintomas_mal_definidos ) AS SBS_Signos_y_sintomas_mal_definidos,
                           AVG(Lesiones ) AS SBS_Lesiones ,
                           AVG(Condiciones_transmisibles_y_nutricionales ) AS SBS_Condiciones_transmisibles_y_nutricionales ,
                           AVG(Enfermedades_no_transmisibles) AS SBS_Enfermedades_no_transmisibles
                         FROM cu_morbilidad_asis
                         WHERE NOM_REGIMEN != '1 - CONTRIBUTIVO'
                         GROUP BY 1,2
                              )
                     ON NOMBRE  = NOM_EPS
                        )
                                               )  A
                                          LEFT JOIN (
           SELECT years, NOM_EPS,
             AVG(Signos_y_sintomas_mal_definidos) AS CNT_Signos_y_sintomas_mal_definidos,
             AVG(Lesiones) AS CNT_Lesiones ,
             AVG(Condiciones_transmisibles_y_nutricionales ) AS CNT_Condiciones_transmisibles_y_nutricionales ,
             AVG(Enfermedades_no_transmisibles ) AS CNT_Enfermedades_no_transmisibles
           FROM cu_morbilidad_asis
           WHERE NOM_REGIMEN = '1 - CONTRIBUTIVO'
           GROUP BY 1,2
                                               )  B
                                          ON A.years = B.years AND 
                                          A.NOM_EPS= B.NOM_EPS
) A


                                         ")

index_cu_morbilidad_asis = sqldf::sqldf("
             select * FROM (
                        SELECT years, NOM_EPS,
                           AVG(Signos_y_sintomas_mal_definidos ) AS Signos_y_sintomas_mal_definidos,
                           AVG(Lesiones ) AS Lesiones ,
                           AVG(Condiciones_transmisibles_y_nutricionales ) AS Condiciones_transmisibles_y_nutricionales ,
                           AVG(Enfermedades_no_transmisibles) AS Enfermedades_no_transmisibles
                         FROM cu_morbilidad_asis
                       --  WHERE NOM_REGIMEN != '1 - CONTRIBUTIVO'
                         GROUP BY 1,2
                          
                              )
               
             ")


index_alto_costo_subsidiado <- sqldf("SELECT EPS AS NOMBRE_EPS, YEAR,
                                      AVG(QUEBRADA) AS QUEBRADA, 
                                      AVG(SUB_PORC) AS SUB_PORC, 
                                      AVG(AC_PORC) AS AC_PORC
                                      FROM (
                                      SELECT *
                                      FROM 
                                      diccionario_codigos_eps
                                      INNER JOIN
                                              (
                                     SELECT year, cod_eapb , 
                                     AVG(sub_porc) AS sub_porc, 
                                     AVG(ac_porc) AS ac_porc
                                     FROM alto_costo_subsidiado
                                     GROUP BY 1 , 2 )
                                     ON CODIGO = COD_EAPB
                                      )
                                      GROUP BY 1 , 2
                                     ")

}
if (1==1){
index_table_nal = sqldf::sqldf("
                            SELECT * FROM  index_cu_morbilidad_asis A
                            -----------------------------
                            LEFT JOIN index_alto_costo_subsidiado B
                            ON  year =  years
                            AND NOMBRE_EPS = NOM_EPS
                            -----------------------------
                            LEFT JOIN (
                            SELECT EPS AS NOM, ANIO AS ANIO_1 ,
                            SUM(AFILIADOS_X_1000) AS AFILIADOS_X_1000, 
                            SUM(TUTELAS) AS TUTELAS
                            FROM index_tutelas_eps
                            GROUP BY 1,2
                            ) A
                             ON NOM_EPS = NOM
                             AND ANIO_1 =  YEARS
                          -----------------------------
                          LEFT JOIN  (
                            SELECT NOM_EPS NOM_1, VARIABLE ANIO_2,
                            SUM(VALUE) AS AFILIADOS
                            FROM afiliados
                            GROUP BY 1,2
                          ) C
                          ON NOM_EPS = NOM_1
                             AND ANIO_2 =  YEARS
                         -----------------------------
                          LEFT JOIN  OPOR_MEDICINA_GENERAL D
                          ON D.ANO_CORTE =  YEARS
                          AND NOM_EPS = D.NOMBRE_EPS_
                          -----------------------------
                          LEFT JOIN  OPOR_ODONT_GENERAL E
                          ON E.ANO_CORTE =  YEARS
                          AND NOM_EPS = E.NOMBRE_EPS_
                           ")

index_table_nal = index_table_nal[,-c(7,8,12,13,16,17,19,20,22,23)]

#  
index_table_nal$AFILIADOS_X_1000 = index_table_nal$AFILIADOS/1000 
 
na_by_cols(index_table_nal)

# frac_nulls_eps(df = index_table_nal, column_name = 'NOM_EPS')
}
cols_number = c(3:6) #  ,9,10

# PCA
if(1==1){
  # table_ <- subset(index_table_nal, index_table_nal$years==2022 )
  # table_ <- table_wo_na (base = table_, cols_number = cols_number )
  table_ <- table_wo_na (base = index_table_nal, cols_number = cols_number )
  print('---------------------------------------------')
  print(  data.frame( colnames(table_ ) )      )
  print('---------------------------------------------')
  print( paste0('filas ' , nrow((table_ ) ))  )
  print('---------------------------------------------')
  pca_eps <- prcomp(table_ ,center = F,  scale. = T,  rank. =1)
  
  print('---------------------------------------------')
  summary(pca_eps)
}

index_table_nal = cbind(index_table_nal , predict(pca_eps, index_table_nal) )
#Logict
if(1==1){
table_ <- index_table_nal 
# table_ <- table_wo_na (base = table_, cols_number =  c(4,5,8,9 ,7,11,21 )  ) 
print('---------------------------------------------')
print(summary(glm(QUEBRADA ~ PC1 + I(PC1^2) , data = table_, family = 'binomial'))[["coefficients"]] )
print('---------------------------------------------')
print( summary(lm(QUEBRADA ~  (PC1) + I(PC1^2) , data = table_ )  )[["coefficients"]]  )
} 

 
index_cu_morbilidad_asis   = cbind(index_cu_morbilidad_asis , predict(pca_eps, index_cu_morbilidad_asis) )
# 
A = index_cu_morbilidad_asis %>% subset( NOM_EPS == 'EPS  FAMISANAR  LTDA')

# table_$res = model$residuals
# pca_eps <- prcomp(table_ , scale. = T,  rank. =1)
# table_ = cbind(table_ , predict(pca_eps, table_) )
# 
# cor(table_$res ,  table_$PC1)
# pca_eps$rotation
# summary(pca_eps)[["importance"]]
# a = summary(pca_eps)
# 
# plot(pca_eps, type = "l")
# biplot(pca_eps)
# biplot(pca_eps, scale = 0)
# 
# hist(index_table$PC1)
# 
# hist(pca_eps$x)
# 
# 
# hist((pca_eps$x - mean(pca_eps$x)) / sd(pca_eps$x) , 20)
# 
# 
# #################################################
# # Municipalities Level 
# #################################################
# 
# 
# if(1==1){
# index_mun_cu_morbilidad_asis <- sqldf::sqldf(" 
#                                          SELECT A.*, 
#                                          SBS_Signos_y_sintomas_mal_definidos,
#                                          SBS_Lesiones,
#                                          SBS_Condiciones_transmisibles_y_nutricionales,
#                                          SBS_Enfermedades_no_transmisibles
#                                          FROM (
#                                               SELECT DIVIPOLA,  years, EPS_, codigo, 
#                                                  AVG(Signos_y_sintomas_mal_definidos) AS CNT_Signos_y_sintomas_mal_definidos,
#                                                  AVG(Lesiones) AS CNT_Lesiones ,
#                                                  AVG(Condiciones_transmisibles_y_nutricionales) AS CNT_Condiciones_transmisibles_y_nutricionales ,
#                                                  AVG(Enfermedades_no_transmisibles) AS CNT_Enfermedades_no_transmisibles
#                                                FROM cu_morbilidad_asis
#                                                WHERE NOM_REGIMEN = '1 - CONTRIBUTIVO'
#                                                GROUP BY 1,2,3,4)  A
#                                           INNER JOIN (
#                                               SELECT DIVIPOLA, years, EPS_, codigo, 
#                                                  AVG(Signos_y_sintomas_mal_definidos) AS SBS_Signos_y_sintomas_mal_definidos,
#                                                  AVG(Lesiones) AS SBS_Lesiones ,
#                                                  AVG(Condiciones_transmisibles_y_nutricionales) AS SBS_Condiciones_transmisibles_y_nutricionales ,
#                                                  AVG(Enfermedades_no_transmisibles) AS SBS_Enfermedades_no_transmisibles
#                                                FROM cu_morbilidad_asis
#                                                WHERE NOM_REGIMEN = '1 - CONTRIBUTIVO'
#                                                GROUP BY 1,2,3,4)  B
#                                           ON A.years = B.years AND 
#                                           A.EPS_ = B.EPS_ AND 
#                                           A.codigo = B.codigo  AND
#                                           A.DIVIPOLA = B.DIVIPOLA
#                                          ")
# head(alto_costo_subsidiado) 
# alto_costo_subsidiado$length = length()
# index_mun_alto_costo_subsidiado =alto_costo_subsidiado
# 
# index_mun_table = sqldf::sqldf("
#                             SELECT * FROM 
#                             index_mun_cu_morbilidad_asis A
#                             LEFT JOIN index_mun_alto_costo_subsidiado B
#                             ON  year =  years
#                             AND cod_eapb = codigo AND
#                             A.DIVIPOLA = B.DIVIPOLA 
#                            ")
# index_mun_table = index_mun_table[,-c(13,14,15)]
# index_mun_table = sqldf::sqldf("
#                                SELECT * FROM index_mun_table A
#                                INNER JOIN diccionario_codigos_eps B
#                                ON A.codigo=B.codigo 
#                                ")
# 
# }
# 
# 
# diccionario_codigos_eps
# # PCA
#  
# index_mun_table$PCA1=  predict(pca_eps ,index_mun_table )
# 
# index_mun_table = na.omit(index_mun_table)
# 
# 
# 
# 
# 
# 
# 
# colnames(index_mun_table)
# table_ = index_mun_table[, -c(1:4,16:17,20)]
# pca_eps_mun <- prcomp(table_ , scale. = TRUE,  rank. =1)
# summary(pca_eps)
# plot(pca_eps, type = "l")
# biplot(pca_eps)
# biplot(pca_eps, scale = 0)
# index_table = cbind(index_table , pca_eps$x )
# 
# hist(index_table$PC1)
# 
# 
# hist((index_table$PC1 - mean(index_table$PC1)) / sd(index_table$PC1) , 10)
