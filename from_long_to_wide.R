productos = data.frame()
setwd("C:/Users/USER/Desktop/CU_Morbilidad_ASIS/")
files <- list.files(pattern = ".csv")
library(readr)
library(sqldf)
library(tidyverse)
library(dplyr)
 
productos = data.frame()
for (i in files){
  path= paste0('C:/Users/USER/Desktop/CU_Morbilidad_ASIS/', i)
  print(path)
  tempo <- read_delim(path, ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                      trim_ws = TRUE)
  
  tempo <- sqldf::sqldf("SELECT * ,  substr(ltrim(CITY) , 0,6 ) DIVIPOLA FROM  tempo ") 
  productos<- rbind(tempo, productos )
}

EPS_CODE <- read_delim("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/EPS CODE.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)

dim_year = data.frame('years' = unique(productos$YEAR))
dim_year = data.frame('years' = unique(productos$YEAR))


VAR_INTERES = unique(productos$VAR_INTERES)
TYPEUSER = unique(productos$TYPE_USER)

SQL = sprintf("SELECT  * FROM  
              ( SELECT DIVIPOLA, CITY, YEAR, VALUE AS %s_INDEX ,TYPE_USER  FROM productos WHERE VAR_INTERES =  '%s')
              WHERE CITY NOT LIKE '%s' " , 
        toupper (gsub(VAR_INTERES[I], pattern = ' ', replacement = '_' ) ) ,
        VAR_INTERES[I], '%No Definido%')

EPS_Q = sqldf::sqldf(SQL)
 
toupper('jaime')
