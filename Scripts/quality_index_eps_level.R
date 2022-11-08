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
  
Table_index = read.csv2(paste0(path_output, "Table_index.csv" ) )
Table_index =  Table_index[ c(3:30, 32:37)]
Table_index = eps_homog (Tabla = Table_index , CODIGO__EPS = "EPS_CODE_" )
Table_index = eps_quiebra (Tabla = Table_index , CODIGO__EPS = "EPS_CODE_" )

Table_index = aggregate_function(aggregate = 'sum',
                   cols_to_agg = colnames(Table_index[,c(2:32)]) ,
                   group_by = colnames(Table_index[,c(35,36,1)]) ,
                   Tabla = Table_index)

 
##############                           
library(readxl)
asegurados <- read_excel(paste0(path_output, "asegurados.xlsx" ), 
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "skip", "skip", "skip", "skip"))
library(tidyr)
asegurados = asegurados %>% tidyr::pivot_longer(!eps, names_to = "Years", values_to = "Asegurados")                          



#####
colnames(asegurados)
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
