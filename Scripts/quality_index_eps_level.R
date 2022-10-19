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

devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/Scripts/functions.R") 
#################################################
# Reading tables used for index
#################################################
path_output = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/"

if(1==1){
Table_index = read.csv2(paste0(path_output, "Table_index.csv" ) )
Table_index =  Table_index[ c(3:30, 32:39)]

# Datawrangling.
Table_index = sqldf::sqldf("
SELECT * FROM
(
 SELECT   EPS_CODE_,  eps_status,   ANIO_,  
          
          SUM(def_mat_42d_no) def_mat_42d_no ,  
          SUM(def_mat_42d_si) def_mat_42d_si ,
          SUM(def_mat_1y_si)  def_mat_1y_si ,
         -- SUM(No_Definido) AS No_Definido,
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
          SUM(Signos_y_sintomas_mal_definidos) AS Signos_y_sintomas_mal_definidos,
          SUM(Lesiones) AS Lesiones,
          SUM(Enfermedades_no_transmisibles) AS Enfermedades_no_transmisibles,
          SUM(Condiciones_transmisibles_y_nutricionales) AS Condiciones_transmisibles_y_nutricionales
 FROM Table_index 
-- WHERE  def_mat_42d_no IS NOT NULL
--   WHERE ANIO_ >= 2013 AND  ANIO_ <= 2017
 GROUP BY 1 ,2  , 3  
 ) ")
                           
library(readxl)
asegurados <- read_excel(paste0(path_output, "asegurados.xlsx" ), 
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "skip", "skip", "skip", "skip"))
asegurados = asegurados %>% pivot_longer(!eps, names_to = "Years", values_to = "Asegurados")                          

Table_index = sqldf::sqldf("
             SELECT * FROM Table_index
             LEFT JOIN asegurados
             ON Years = ANIO_
             AND TRIM(eps)   = TRIM(EPS_CODE_)
             ")
NUMERICAS = c(7:32)
for (i  in NUMERICAS) {
   
  Table_index[[i]]  = Table_index[[i]] / (Table_index[[35]]/10000)
  
}

Table_index[[5]]  = 100*Table_index[[5]] / ( Table_index[[5]] + Table_index[[4]]  )
Table_index[[6]]  = 100*Table_index[[6]] / ( Table_index[[6]] + Table_index[[4]]  )

}                                           
## Columns used for the pca,   literature: https://docs.google.com/spreadsheets/d/1d4cK0EHsyxfNxbTv0aeLw4pjaOdkpbivb0A8rtjLAVY/edit?usp=sharing
# technical accuracy of the medical diagnoses and procedures, or the conformance to professional specification
## maternal mortality 

# frac_nulls_eps(df = Table_index, column_name = 'EPS_CODE_')
 
# Eliminar columna de Nodefinido.
# Anexar la tabla tipo long de asegurados
# Dividir desde la col 4 a la 28 todos los valroes por asegruados
# hacer el PCA
# Documentar el PCA. 
# 
cols_number = c(   16:32 ) 
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
   
   Table_   = cbind(Table_index , predict(pca_eps, Table_index) )
   # P(EPS_Quiebre = 1| PC1 ) ~ Pvallue < 0.05
   summary(lm(data = Table_,  eps_status ~ PC1  ))
 }
hist(pca_eps$x)

summary((pca_eps$x))
pca_eps$x[1:10,]
var_explained <- pca_eps$sdev^2/sum(pca_eps$sdev^2)*100

############### Tiene sentido



####### Writing

Table_index   = cbind(Table_index , predict(pca_eps, Table_index) )

write.csv2(Table_index  , paste0(path_output, "Table_index_eps.csv" ) , row.names = F) 
colnames(Table_index)[36] = 'quality_index'
final = Table_index[, c(1:3,36) ]
write.csv2(final  , paste0(path_output, "Index_eps.csv" ) , row.names = F) 

# It may be useful to normalize observations by the number of affiliates, 
# in itself, each column would show an indicator per capita
