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
# Reading tables used for index
#################################################
path_input = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/input/"
path_output = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/"

#  eps_from_code_to_name_n_status(Tabla = diccionario_codigos_eps, eps_code =  'codigo')
# TABLE MORBILITIES
cu_morbilidad_asis <- read_delim( paste0(path_output, "index_cu_morbilidad_asis.csv" ), 
                                  delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip()), 
                                  locale = locale(encoding = "WINDOWS-1252"), 
                                  trim_ws = TRUE)

# Table estadisticas vitales. 
 
EST_VITALES <- read_delim("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/EST_VITALES.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)


EST_VITALES <- eps_from_code_to_name_n_status(Tabla=EST_VITALES ,eps_code = "EPS_CODE_")

library(plm)
TABLA1 = sqldf("  SELECT DISTINCT ID AS ID2 FROM EST_VITALES WHERE ANIO_ BETWEEN 2011 AND 2020 ")
TABLA2 = sqldf("  SELECT DISTINCT ANIO_ AS ANIO_2 FROM EST_VITALES WHERE ANIO_ BETWEEN 2011 AND 2020 ")

TABLE_A = sqldf("  SELECT * FROM (SELECT * FROM TABLA1 LEFT JOIN TABLA2 ON 1=1)   ")

TABLE_A = sqldf::sqldf(" SELECT * FROM TABLE_A  
                            LEFT JOIN EST_VITALES  
                            ON   ID2= ID 
                            AND  ANIO_2 =  ANIO_ ")
TABLE_A$ID <- ifelse(is.na(TABLE_A$ID ) == T , TABLE_A$ID2,TABLE_A$ID   )
TABLE_A[[4]] <- substr(TABLE_A$ID, 0, 5)
TABLE_A[[5]] <- substr(TABLE_A$ID, 8, nchar(TABLE_A$ID) )
TABLE_A$ANIO_ <- ifelse(is.na(TABLE_A$ANIO_ ) == T , TABLE_A$ANIO_2,TABLE_A$ANIO_   )

TABLE_A <- TABLE_A[, -c(3,4,5,2)]
cu_morbilidad_asis = sqldf::sqldf("SELECT DIVIPOLA||' - '||EPS_ ID_ ,  * FROM cu_morbilidad_asis")

Table_index = sqldf::sqldf("
        SELECT * FROM TABLE_A 
              LEFT JOIN cu_morbilidad_asis
              ON  ID2 = ID_ 
              AND YEARS = ANIO_  
                           ")
# FROM 

Table_index <- Table_index[, -c(30:38)]
Table_index$DIVIPOLA <- substr(Table_index$ID2, 0, 5)
Table_index$EPS_CODE_ <- substr(Table_index$ID2, 8, nchar(Table_index$ID2)  ) 

Table_index <-   eps_from_code_to_name_n_status(Tabla= Table_index, eps_code = "EPS_CODE_")

Table_index <- subset(Table_index, is.na(Table_index$eps_status)==F )
# LEFT JOIN cu_morbilidad_asis B
Table_index[[4]]  = 100 * Table_index[[4]] /  ( Table_index[[3]] + Table_index[[4]] )
Table_index[[5]]  = 100 * Table_index[[4]] /  ( Table_index[[3]] + Table_index[[5]] )
colnames(Table_index)
write.csv2(Table_index , paste0(path_output, "Table_index.csv" ) ) 

#######################################################
####################  Data Quality ####################
tabla =  subset(Table_index, Table_index$ID2 == '08001 - CCF023')
QUALITY = sqldf::sqldf("
SELECT * FROM
(
 SELECT ID2 as ID, DIVIPOLA, EPS_CODE_,  eps_status,   
          COUNT(DISTINCT ANIO_) TOTAL , 
          ((MAX(ANIO_) - MIN(ANIO_)) +1 ) = COUNT(DISTINCT ANIO_)    Is_panel ,  
          MIN(ANIO_) || ' - ' || MAX(ANIO_) TEMPROAL_RANGE,  
          
          SUM(def_mat_42d_no) def_mat_42d_no ,  
          SUM(def_mat_42d_si) def_mat_42d_si ,
          SUM(def_mat_1y_si)  def_mat_1y_si ,
          SUM(No_Definido) AS No_Definido,
          SUM(N_Enfermedades_dentales) AS N_Enfermedades_dentales,
          SUM(M_Anomalias_congenitas) AS M_Anomalias_congenitas,
          SUM(L_Enfermedades_del_sistema_musculo_esqueletico) AS L_Enfermedades_del_sistema_musculo_esqueletico,
          SUM(K_Enfermedades_de_la_piel) AS K_Enfermedades_de_la_piel,
          SUM(J_Enfermedades_del_sistema_genito_urinario) AS J_Enfermedades_del_sistema_genito_urinario,
          SUM(I_Enfermedades_digestivas) AS I_Enfermedades_digestivas,
          SUM(H_Enfermedades_respiratorias) AS H_Enfermedades_respiratorias,
          SUM(G_Enfermedades_cardiovasculares) AS G_Enfermedades_cardiovasculares,
          SUM(F_Enfermedades_de_los_organos_de_los_sentidos) AS F_Enfermedades_de_los_organos_de_los_sentidos,
          SUM(Eventos_lesiones_de_intencion_no_determinada) AS Eventos_lesiones_de_intencion_no_determinada,
          SUM(E_Trastornos_mentales_y_enfermedades_del_sistema_nervioso) AS E_Trastornos_mentales_y_enfermedades_del_sistema_nervioso,
          SUM(E_Deficiencias_de_la_nutricion) AS E_Deficiencias_de_la_nutricion,
          SUM(D_Otras_enfermedades_endocrinas_metabolicas_hematologicas_e_immunologicas) AS D_Otras_enfermedades_endocrinas,
          SUM(D_Ciertas_afecciones_originadas_en_el_periodo_perinatal) AS D_Ciertas_afecciones_originadas_en_el_periodo_perinatal,
          SUM(C_Diabetes_mellitus) AS C_Diabetes_mellitus,
          SUM(C_Causas_maternas) AS C_Causas_maternas,
          SUM(B_Otros_tumores) AS B_Otros_tumores,
          SUM(B_Lesiones_intencionales) AS B_Lesiones_intencionales,
          SUM(B_Infecciones_respiratorias) AS B_Infecciones_respiratorias,
          SUM(A_Tumores_malignos) AS A_Tumores_malignos,
          SUM(A_Enfermedades_infecciosas_y_parasitarias) AS A_Enfermedades_infecciosas_y_parasitarias,
          SUM(A_Accidentes) AS A_Accidentes,
          SUM(eps_name) AS eps_name,
          SUM(eps_status) AS eps_status,
          SUM(Signos_y_sintomas_mal_definidos) AS Signos_y_sintomas_mal_definidos,
          SUM(Lesiones) AS Lesiones,
          SUM(Enfermedades_no_transmisibles) AS Enfermedades_no_transmisibles,
          SUM(Condiciones_transmisibles_y_nutricionales) AS Condiciones_transmisibles_y_nutricionales
 FROM Table_index 
WHERE  def_mat_42d_no IS NOT NULL
--   WHERE ANIO_ >= 2013 AND  ANIO_ <= 2017
 GROUP BY 1 ,2  , 3  ,4 
 )
 
             ")

                       
write.csv2(QUALITY  , paste0(path_output, "Quality.csv" ) ) 
Table_index_
#####################################################################
Table_index = read.csv2(paste0(path_output, "Table_index.csv" ) )
cols_number = c(4:30,32:35 ) 
Table_index_ = data.frame()
######
for (i in unique(Table_index$ID2)) {
  print(i)
  temp = subset(Table_index, Table_index$ID2 == i )
  for (j in cols_number) {
    # temp[[j]] = (temp[[j]] - mean(temp[[j]])   ) / sd(temp[[j]], na.rm = T)
    temp[[j]] = (temp[[j]] - min(temp[[j]] , na.rm = T )   ) /  (max(temp[[j]] , na.rm = T ) - min(temp[[j]] , na.rm = T ))
  }
  Table_index_ = rbind(Table_index_, temp)
}

######
for (i in cols_number ) {
  Table_index_[[i]] = as.numeric(Table_index_[[i]])
}
Table_index = subset(Table_index, Table_index$ANIO_ >=2013 & Table_index$ANIO_ <=2018   )
Quality = frac_nulls_eps(df = Table_index, column_name = 'ID2')
write.csv2(Quality, paste0(path_output, "nulls_eps_muni.csv" ) )
table(Table_index$ANIO_)
#################################################################
Table_temp = Table_index[, c(4:6)]
Table_temp$def_mat_42d_si = Table_temp$def_mat_42d_si/(Table_temp$def_mat_42d_si + Table_temp$def_mat_42d_no)
Table_temp$def_mat_42d_si = Table_temp$def_mat_1y_si/(Table_temp$def_mat_1y_si + Table_temp$def_mat_42d_no)
Table_temp = na.omit(Table_temp)
1 - nrow(Table_temp) /nrow(Table_index) 




Table_index = read.csv2(paste0(path_output, "Table_index_.csv" ) )
######
Table_index= Table_index[, c(2:40)]
cols_number = c(4:6 ) 
frac_nulls_eps(df = Table_index, column_name = 'ID2')
  
 #,8:28, 31
# PCA
if(1==1){
   # table_ <- subset(table_, Table_index$ANIO_==2013 )
   # table_ <- table_wo_na (base = table_, cols_number = cols_number )
  table_ <- table_wo_na (base = Table_index, cols_number = cols_number )
  print('---------------------------------------------')
  print(  data.frame( colnames(table_ ) )      )
  print('---------------------------------------------')
  print( paste0('filas ' , nrow((table_ ) ))  )
  print('---------------------------------------------')
  pca_eps <- prcomp(table_ ,   scale. = T,  rank. =1)
  
  print('---------------------------------------------')
  summary(pca_eps)
}

######
Table_index_   = cbind(Table_index_ , predict(pca_eps, Table_index_) )
colnames(Table_index_)[40] = 'quality_index'
write.csv2(Table_index_  , paste0(path_output, "Table_index_.csv" ) , row.names = F) 
hist(Table_index_$quality_index)
################################
final = Table_index_[, c(36:40, 3,2) ]
write.csv2(final  , paste0(path_output, "Index_.csv" ) , row.names = F) 



#########################################################################
# Indice por eps
Table_index = read.csv2(paste0(path_output, "Table_index.csv" ) )
Table_index =  Table_index[ c(3:30, 32:39)]

Table_index = sqldf::sqldf("
SELECT * FROM
(
 SELECT   EPS_CODE_,  eps_status,   ANIO_,  
          
          SUM(def_mat_42d_no) def_mat_42d_no ,  
          SUM(def_mat_42d_si) def_mat_42d_si ,
          SUM(def_mat_1y_si)  def_mat_1y_si ,
          SUM(No_Definido) AS No_Definido,
          SUM(N_Enfermedades_dentales) AS N_Enfermedades_dentales,
          SUM(M_Anomalias_congenitas) AS M_Anomalias_congenitas,
          SUM(L_Enfermedades_del_sistema_musculo_esqueletico) AS L_Enfermedades_del_sistema_musculo_esqueletico,
          SUM(K_Enfermedades_de_la_piel) AS K_Enfermedades_de_la_piel,
          SUM(J_Enfermedades_del_sistema_genito_urinario) AS J_Enfermedades_del_sistema_genito_urinario,
          SUM(I_Enfermedades_digestivas) AS I_Enfermedades_digestivas,
          SUM(H_Enfermedades_respiratorias) AS H_Enfermedades_respiratorias,
          SUM(G_Enfermedades_cardiovasculares) AS G_Enfermedades_cardiovasculares,
          SUM(F_Enfermedades_de_los_organos_de_los_sentidos) AS F_Enfermedades_de_los_organos_de_los_sentidos,
          SUM(Eventos_lesiones_de_intencion_no_determinada) AS Eventos_lesiones_de_intencion_no_determinada,
          SUM(E_Trastornos_mentales_y_enfermedades_del_sistema_nervioso) AS E_Trastornos_mentales_y_enfermedades_del_sistema_nervioso,
          SUM(E_Deficiencias_de_la_nutricion) AS E_Deficiencias_de_la_nutricion,
          SUM(D_Otras_enfermedades_endocrinas_metabolicas_hematologicas_e_immunologicas) AS D_Otras_enfermedades_endocrinas,
          SUM(D_Ciertas_afecciones_originadas_en_el_periodo_perinatal) AS D_Ciertas_afecciones_originadas_en_el_periodo_perinatal,
          SUM(C_Diabetes_mellitus) AS C_Diabetes_mellitus,
          SUM(C_Causas_maternas) AS C_Causas_maternas,
          SUM(B_Otros_tumores) AS B_Otros_tumores,
          SUM(B_Lesiones_intencionales) AS B_Lesiones_intencionales,
          SUM(B_Infecciones_respiratorias) AS B_Infecciones_respiratorias,
          SUM(A_Tumores_malignos) AS A_Tumores_malignos,
          SUM(A_Enfermedades_infecciosas_y_parasitarias) AS A_Enfermedades_infecciosas_y_parasitarias,
          SUM(A_Accidentes) AS A_Accidentes,
          SUM(eps_name) AS eps_name,
          SUM(eps_status) AS eps_status,
          SUM(Signos_y_sintomas_mal_definidos) AS Signos_y_sintomas_mal_definidos,
          SUM(Lesiones) AS Lesiones,
          SUM(Enfermedades_no_transmisibles) AS Enfermedades_no_transmisibles,
          SUM(Condiciones_transmisibles_y_nutricionales) AS Condiciones_transmisibles_y_nutricionales
 FROM Table_index 
-- WHERE  def_mat_42d_no IS NOT NULL
--   WHERE ANIO_ >= 2013 AND  ANIO_ <= 2017
 GROUP BY 1 ,2  , 3  
 ) ")

                           
cols_number = c(4:29 ) 
frac_nulls_eps(df = Table_index, column_name = 'EPS_CODE_')

#,8:28, 31
# PCA
if(1==1){
 # table_ <- subset(table_, Table_index$ANIO_==2013 )
 # table_ <- table_wo_na (base = table_, cols_number = cols_number )
 table_ <- table_wo_na (base = Table_index, cols_number = cols_number )
 print('---------------------------------------------')
 print(  data.frame( colnames(table_ ) )      )
 print('---------------------------------------------')
 print( paste0('filas ' , nrow((table_ ) ))  )
 print('---------------------------------------------')
 pca_eps <- prcomp(table_ ,   scale. = T,  rank. =1)
 
 print('---------------------------------------------')
 summary(pca_eps)
}

Table_index   = cbind(Table_index , predict(pca_eps, Table_index) )
colnames(Table_index)[36] = 'quality_index'
final = Table_index[, c(1:3,36) ]
write.csv2(final  , paste0(path_output, "Index_eps.csv" ) , row.names = F) 
