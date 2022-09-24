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
# TABLE AFILIADOS 
afiliados <- read_excel(paste0(path_input, "asegurados.xlsx"))
 
# TABLE MORBILITIES
cu_morbilidad_asis <- read_delim( paste0(path_input, "index_cu_morbilidad_asis.csv" ), 
                                       delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip()), 
                                       locale = locale(encoding = "WINDOWS-1252"), 
                                       trim_ws = TRUE)

# TABLE HIGH COST DISEASES
alto_costo_subsidiado <- read_dta( paste0(path_input, "participacion_alto_costo_subsidiado.dta" ) )

# hooping cough
tos_ferina <- read_csv(paste0(path_input, "tos_ferina.csv" ) )

# Tutelas eps

Btutelas_eps <- read_excel(paste0(path_input, "Btutelas_eps.xlsx" ) , sheet = "Base")
#
OPOR_MEDICINA_GENERAL <- read_csv(paste0(path_input, "OPOR_MEDICINA_GENERAL.csv") )
#
OPOR_ODONT_GENERAL <- read_csv(paste0(path_input, "OPOR_ODONT_GENERAL.csv" ) ) 
 
#################################################
# Setting tables for pca
#################################################
# TABLE AFILIADOS 
afiliados <-afiliados[,c(1:15)]
afiliados <- melt(afiliados, id.vars=c("eps"))

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

 
options(scipen=999)
index_tos_ferina = sqldf::sqldf("
              SELECT COD_ASE, ANO ,  TOS_FERINA/(value/1000) AS TOS_FERINA
              FROM ( 
                    SELECT COD_ASE, ANO, SUM(TOS_FERINA)  TOS_FERINA
                    FROM tos_ferina
                    GROUP BY 1,2 ) A
              INNER JOIN  afiliados B
              ON ANO = variable 
              AND eps = COD_ASE
              ")


# Tutelas eps
devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/functions.R") ## it deppends of devtools library

Btutelas_eps = homogenizacion_eps(tabla =Btutelas_eps ,
                                  Nombre_eps = 'eps' ,
                                  Regimen_salud = 'regimen')

index_tutelas_eps = sqldf::sqldf("
              SELECT EPS_CODE, ANIO ,  TUTELAS/(value/1000) AS TUTELAS
              FROM ( 
                    SELECT ANIO, EPS_CODE, SUM(TUTELAS)  TUTELAS
                    FROM Btutelas_eps
                    GROUP BY 1,2 ) A
              INNER JOIN  afiliados B
              ON ANIO = variable 
              AND eps = EPS_CODE
              ")

### I NEED AFILIADOS AT MUNICIPALITIES AND EPS LEVEL
# index_mun_tos_ferina = sqldf::sqldf("
#               SELECT COD_ASE, ANO , COD_MUN_N, TOS_FERINA/(value/1000) AS TOS_FERINA
#               FROM ( 
#                     SELECT COD_ASE, ANO,COD_MUN_N, SUM(TOS_FERINA)  TOS_FERINA
#                     FROM tos_ferina
#                     GROUP BY 1,2,3 ) A
#               INNER JOIN  afiliados B
#               ON ANO = variable 
#               AND eps = COD_ASE
#               and DIVIPOLA =COD_MUN_N
#               ")
#################################################
# General in level 
#################################################
# Tables 

if(1==1){
index_cu_morbilidad_asis <- sqldf::sqldf(" 
                                         SELECT A.*, 
                                         SBS_Signos_y_sintomas_mal_definidos,
                                         SBS_Lesiones,
                                         SBS_Condiciones_transmisibles_y_nutricionales,
                                         SBS_Enfermedades_no_transmisibles
                                         FROM (
                                              SELECT years, EPS_, codigo, 
                                                 AVG(Signos_y_sintomas_mal_definidos) AS CNT_Signos_y_sintomas_mal_definidos,
                                                 AVG(Lesiones) AS CNT_Lesiones ,
                                                 AVG(Condiciones_transmisibles_y_nutricionales) AS CNT_Condiciones_transmisibles_y_nutricionales ,
                                                 AVG(Enfermedades_no_transmisibles) AS CNT_Enfermedades_no_transmisibles
                                               FROM cu_morbilidad_asis
                                               WHERE NOM_REGIMEN = '1 - CONTRIBUTIVO'
                                               GROUP BY 1,2,3)  A
                                          LEFT JOIN (
                                              SELECT years, EPS_, codigo, 
                                                 AVG(Signos_y_sintomas_mal_definidos) AS SBS_Signos_y_sintomas_mal_definidos,
                                                 AVG(Lesiones) AS SBS_Lesiones ,
                                                 AVG(Condiciones_transmisibles_y_nutricionales) AS SBS_Condiciones_transmisibles_y_nutricionales ,
                                                 AVG(Enfermedades_no_transmisibles) AS SBS_Enfermedades_no_transmisibles
                                               FROM cu_morbilidad_asis
                                               WHERE NOM_REGIMEN = '1 - CONTRIBUTIVO'
                                               GROUP BY 1,2,3)  B
                                          ON A.years = B.years AND 
                                          A.EPS_ = B.EPS_ AND 
                                          A.codigo = B.codigo 
                                         ")


index_alto_costo_subsidiado <- sqldf("
                                     SELECT year, cod_eapb , 
                                     AVG(sub_porc) AS sub_porc, 
                                     AVG(ac_porc) AS ac_porc
                                     FROM alto_costo_subsidiado
                                     GROUP BY 1 , 2
                                     ")


index_table = sqldf::sqldf("
                            SELECT * FROM  index_cu_morbilidad_asis A
                            LEFT JOIN index_alto_costo_subsidiado B
                            ON  year =  years
                            AND cod_eapb = codigo
                            -- LEFT JOIN index_tutelas_eps
                            -- ON  ANIO =  years
                            -- AND EPS_CODE = codigo
                           LEFT JOIN  afiliados C
                           ON years = variable 
                           AND codigo = c.EPS
                            LEFT JOIN  OPOR_MEDICINA_GENERAL D
                           ON years = D.ANO_CORTE 
                           AND codigo = D.COD_EPS
                          LEFT JOIN  OPOR_ODONT_GENERAL E
                          ON years = E.ANO_CORTE 
                           AND codigo = E.COD_EPS
                           ")
}
colnames(index_table)
index_table = index_table[-c(22,23)]
?prcomp
# index_table = na.omit(index_table)
na_by_cols(index_table)
cols_number = c(4,5,8,9) # c(4:11,18) ,21,22
if(1==1){
  table_ <- subset(index_table, index_table$years==2018 )
  table_ <- table_wo_na (base = table_, cols_number = cols_number )
  pca_eps <- prcomp(table_ , scale. = T,  rank. =1)
  summary(pca_eps)
  }

plot(pca_eps, type = "l")
biplot(pca_eps)
biplot(pca_eps, scale = 0)
index_table = cbind(index_table , predict(pca_eps, index_table) )
 

hist(pca_eps$x)


hist((pca_eps$x - mean(pca_eps$x)) / sd(pca_eps$x) , 10)


#################################################
# Municipalities Level 
#################################################


if(1==1){
index_mun_cu_morbilidad_asis <- sqldf::sqldf(" 
                                         SELECT A.*, 
                                         SBS_Signos_y_sintomas_mal_definidos,
                                         SBS_Lesiones,
                                         SBS_Condiciones_transmisibles_y_nutricionales,
                                         SBS_Enfermedades_no_transmisibles
                                         FROM (
                                              SELECT DIVIPOLA,  years, EPS_, codigo, 
                                                 AVG(Signos_y_sintomas_mal_definidos) AS CNT_Signos_y_sintomas_mal_definidos,
                                                 AVG(Lesiones) AS CNT_Lesiones ,
                                                 AVG(Condiciones_transmisibles_y_nutricionales) AS CNT_Condiciones_transmisibles_y_nutricionales ,
                                                 AVG(Enfermedades_no_transmisibles) AS CNT_Enfermedades_no_transmisibles
                                               FROM cu_morbilidad_asis
                                               WHERE NOM_REGIMEN = '1 - CONTRIBUTIVO'
                                               GROUP BY 1,2,3,4)  A
                                          INNER JOIN (
                                              SELECT DIVIPOLA, years, EPS_, codigo, 
                                                 AVG(Signos_y_sintomas_mal_definidos) AS SBS_Signos_y_sintomas_mal_definidos,
                                                 AVG(Lesiones) AS SBS_Lesiones ,
                                                 AVG(Condiciones_transmisibles_y_nutricionales) AS SBS_Condiciones_transmisibles_y_nutricionales ,
                                                 AVG(Enfermedades_no_transmisibles) AS SBS_Enfermedades_no_transmisibles
                                               FROM cu_morbilidad_asis
                                               WHERE NOM_REGIMEN = '1 - CONTRIBUTIVO'
                                               GROUP BY 1,2,3,4)  B
                                          ON A.years = B.years AND 
                                          A.EPS_ = B.EPS_ AND 
                                          A.codigo = B.codigo  AND
                                          A.DIVIPOLA = B.DIVIPOLA
                                         ")
 

index_mun_alto_costo_subsidiado = sqldf::sqldf("
             SELECT   
             CASE 
             WHEN length = '1' THEN cod_dpto||'00'|| cod_mpio
             WHEN length = '2' THEN cod_dpto||'0'|| cod_mpio
             WHEN length = '3' THEN  cod_dpto||cod_mpio 
             ELSE NULL END AS DIVIPOLA, *
             FROM alto_costo_subsidiado
             ")
index_mun_table = sqldf::sqldf("
                            SELECT * FROM 
                            index_mun_cu_morbilidad_asis A
                            LEFT JOIN index_mun_alto_costo_subsidiado B
                            ON  year =  years
                            AND cod_eapb = codigo AND
                            A.DIVIPOLA = B.DIVIPOLA 
                           ")
}

# PCA
 ?prcomp
index_mun_table$PCA1=  predict(pca_eps ,index_mun_table )
index_mun_table = na.omit(index_mun_table)
colnames(index_mun_table)
table_ = index_mun_table[, -c(1:4,16:17,20)]
pca_eps_mun <- prcomp(table_ , scale. = TRUE,  rank. =1)
summary(pca_eps)
plot(pca_eps, type = "l")
biplot(pca_eps)
biplot(pca_eps, scale = 0)
index_table = cbind(index_table , pca_eps$x )

hist(index_table$PC1)


hist((index_table$PC1 - mean(index_table$PC1)) / sd(index_table$PC1) , 10)
