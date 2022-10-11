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

# EPS diccionario
diccionario_codigos_eps <- read_excel(paste0(path_input, "diccionario_codigos_eps.xlsx" ) )

a = eps_from_code_to_name_n_status(Tabla = diccionario_codigos_eps, eps_code =  'codigo')
# TABLE MORBILITIES
cu_morbilidad_asis <- read_delim( paste0(path_output, "index_cu_morbilidad_asis.csv" ), 
                                  delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip()), 
                                  locale = locale(encoding = "WINDOWS-1252"), 
                                  trim_ws = TRUE)

# Table estadisticas vitales. 
 
EST_VITALES <- read_delim("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/EST_VITALES.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
