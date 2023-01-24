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



source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/Scripts/R/homog_eps_function.R")
#####################################################################
#
#####################################################################

devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/Scripts/functions.R") 

global_output = 'C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/'

library(readr)
def_fec_mun_eps_edad_1 <- read_delim("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/def1.csv", 
                                ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                trim_ws = TRUE)
 
def_fec_mun_eps_edad_2 <- read_delim("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/def2.csv", 
                                ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                trim_ws = TRUE)

def_fec_mun_eps_edad = rbind(def_fec_mun_eps_edad_1, def_fec_mun_eps_edad_2)
def_fec_mun_eps_edad = sqldf("SELECT DISTINCT *  FROM def_fec_mun_eps_edad")
def_fec_mun_eps_edad$Divipola = substr(def_fec_mun_eps_edad$Municipio, 0, 5)

library(stringr)
def_fec_mun_eps_edad$EPS_CODE =  str_split_fixed(def_fec_mun_eps_edad$EPS, " - ", 2)[,1]

Tabla = eps_homog(Tabla = def_fec_mun_eps_edad , CODIGO__EPS =  'EPS_CODE')

unique(def_fec_mun_eps_edad$GrupoEtario)
Tabla =  aggregate_function(Tabla = Tabla, aggregate = "SUM", 
                            cols_to_agg = c("Defunciones") , 
                            group_by = c("homo_code_eps" , "Divipola", "Anio", "Mes", "GrupoEtario") ) 

Tabla$Periodo =  ifelse( nchar(as.character(Tabla$Mes))==1,   paste0(as.character(Tabla$Anio),"0", as.character(Tabla$Mes) ) ,
                         paste0(as.character(Tabla$Anio),  as.character(Tabla$Mes) )   )

Result_tabla = reshape(  Tabla[,c(1,2,5,6,7)] , idvar = c('Periodo', 'homo_code_eps' ,'Divipola'),
                             timevar = "GrupoEtario" , direction = "wide")
library(tidyr )
Tabla[,c(1,2,5,6,7)] %>% pivot_wider(names_from = GrupoEtario, values_from = Defunciones)
 
colnames(Tabla)
write.csv2(Tabla,paste0(global_output , 'def_fec_mun_eps_edad.csv'),   row.names = F)
head(Tabla)
