
lista = c('readr','readxl','sqldf','plyr', 
          'did' , 'arrow',  'plyr', 'ggplot2',
          'dplyr','fixest' , 'gargle' , 'stringr'
          , 'bigrquery' ,'miceadds'  , 'haven' ,'estimatr' 
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
def_fec_mun_eps_1 <- read_delim("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/def_fec_mun_eps_1.csv", 
                                ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                trim_ws = TRUE)

def_fec_mun_eps_2 <- read_delim("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/def_fec_mun_eps_2.csv", 
                                ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                trim_ws = TRUE)

def_fec_mun_eps = rbind(def_fec_mun_eps_1, def_fec_mun_eps_2)
def_fec_mun_eps = sqldf("SELECT DISTINCT *  FROM def_fec_mun_eps")
def_fec_mun_eps$Divipola = substr(def_fec_mun_eps$Municipio, 0, 5)


library(stringr)
def_fec_mun_eps$EPS_CODE =  str_split_fixed(def_fec_mun_eps$EPS, " - ", 2)[,1]
str(def_fec_mun_eps$Anio)
#  sqldf("select * from def_fec_mun_eps where EPS_CODE like '% %' ") 
Tabla = eps_homog(Tabla = def_fec_mun_eps , CODIGO__EPS =  'EPS_CODE')
Tabla = subset(Tabla, Tabla$Anio>=2018)

Tabla$Periodo =  ifelse( nchar(as.character(Tabla$Mes))==1,   paste0(as.character(Tabla$Anio),"0", as.character(Tabla$Mes) ) ,
                         paste0(as.character(Tabla$Anio),  as.character(Tabla$Mes) )   )

Tabla =  aggregate_function(Tabla = Tabla, aggregate = "SUM", 
                            cols_to_agg = c("Defunciones") , 
                            group_by = c("homo_code_eps" , "Divipola", "Periodo") ) 


Tabla = Tabla  %>% subset(is.na(Divipola) == F)
colnames(Tabla)
if (1==1) {
  A = sqldf::sqldf(" SELECT distinct Divipola||'-'||homo_code_eps COD_,
                   Divipola Divipola_ , homo_code_eps code_eps_ FROM Tabla ")
  
  B = sqldf::sqldf(" SELECT distinct Periodo as Periodo_ FROM Tabla ")
  
  TEMPO = sqldf("SELECT * FROM A INNER JOIN B ON 1=1 
                WHERE PERIODO_ NOT LIKE '2016%' AND PERIODO_ NOT LIKE '2017%'
                ORDER BY 1,2")
  Tabla = sqldf::sqldf(" 
  SELECT COD_,Divipola_, code_eps_, 
  Periodo_, IFNULL(Defunciones, 0) AS Defunciones FROM TEMPO 
  LEFT JOIN Tabla
  ON Periodo = Periodo_ AND Divipola_  = Divipola AND  code_eps_ = homo_code_eps 
             ")
}



library(readr)
Master <- read_csv("C:/Users/USER/Desktop/BanRep/Output/Master.csv",  col_types = cols(date = col_character()))
Master = eps_homog(Tabla = Master , CODIGO__EPS =  'cod_eapb')
Master =  aggregate_function(Tabla = Master, aggregate = "SUM", 
                            cols_to_agg = c("total") ,   
                            group_by = c("homo_code_eps" , "date", "cod_mpio") ) 

colnames(Master)[1] = 'cod_eapb'
###############################3
# SE UNE LA BASE MAESTRA Y LA BASE DE DEFUNCIONES LIMPIAS PARA TODOS
# LOS PERIODOS DE ENERO 2018 A JULIO DE 2020
colnames(Tabla)
colnames(Master)
if(1==1){
Tabla_ = sqldf::sqldf(" SELECT * FROM Tabla left join
             Master
             on cod_mpio =  Divipola_ 
             and cod_eapb  = code_eps_ 
                     and date = Periodo_
             ORDER BY Periodo_
                      ")
Tabla_ = Tabla_[,c(1:5,9)]
colnames(Tabla_)[6] = 'Total_afilaidos'
options(scipen=999)

Tabla_$tasa_mortalidad  = Tabla_$Defunciones / (Tabla_$Total_afilaidos / 100000)

Tabla_$Total_afilaidos = ifelse( is.na(Tabla_$Total_afilaidos)==T, 0,Tabla_$Total_afilaidos  )

Tabla_ = Tabla_  %>% subset(is.na(Divipola_) == F)
Tabla_ = Tabla_ %>% subset(is.na(code_eps_) == F)
 

Control = Tabla_ %>% subset(Periodo_ == '201805')
sum(Control$Defunciones)
}

write.csv2(Tabla_,paste0(global_output , 'def_fec_mun_eps.csv'),   row.names = F)

Tabla_ = sqldf::sqldf("SELECT * FROM (
      SELECT SUBSTR(Periodo_,0, 5) ||'-'|| SUBSTR(Periodo_,5, 7) ||'-01'  AS FECHA , *
             FROM Tabla_ )
             order by  CAST( FECHA AS DATE) , COD_
             ")
############################################## 
######## Creamos el delta. ###################

Tabla_resultado = data.frame()

for (i in  unique(Tabla_$COD_) ) {
  temp = subset(Tabla_,Tabla_$COD_ ==i )
  temp$delta_mortality =  c(NA, diff (temp$Defunciones, lag = 1, differences = 1 )  )
  temp$delta_mortality_rate =  c(NA, diff (temp$tasa_mortalidad, lag = 1, differences = 1 )  )
  Tabla_resultado = rbind(Tabla_resultado,temp)  
  }
haven::write_dta(Tabla_resultado , paste0(global_output , 'def_fec_mun_eps.dta')  )

#####################################3
# library(haven)
# 
# event_data_bkcy_date <- read_dta("C:/Users/USER/Desktop/BanRep/Output/event_data_bkcy_date.dta")
# library(haven)
# Tabla_resultado = read_csv( paste0(global_output , 'def_fec_mun_eps.dta')  )
library(readr)
Tabla_resultado <- read_csv("C:/Users/USER/Desktop/BanRep/Output/def_fec_mun_eps.csv", 
                             col_types = cols(Periodo_ = col_character()))
library(readr)
event_data_bkcy_date <- read_csv("C:/Users/USER/Desktop/BanRep/Output/event_data_bkcy_date.csv", 
                                 col_types = cols(period = col_character(), 
                                                  date = col_character()))

# write.csv2("C:/Users/USER/Desktop/BanRep/Output/event_data_bkcy_date.csv")
str(Tabla_resultado)
str(event_data_bkcy_date)
Tabla = sqldf("SELECT * FROM event_data_bkcy_date
              LEFT JOIN Tabla_resultado 
              on Periodo_= period
              and cod_mpio = Divipola_
              and code_eps_ = cod_eapb ")

Tabla = sqldf("
      SELECT * FROM Tabla
      WHERE bankruptcy NOT LIKE 'PARCIAL%'
      AND bankruptcy NOT LIKE 'COMFAMILIAR NARIÑO'
      ")
haven::write_dta(Tabla , paste0(global_output , 'event_data_bkcy_date_mortality.dta')  )

library(miceadds)
library(estimatr)
Tabla$group = paste0(Tabla$code_eps_,'-',Tabla$cod_mpio,'-' ,Tabla$bankruptcy)
m3fe <- lm_robust(delta_alg ~  delta_real,
                  clusters = group,
                  fixed_effects = ~group+cod_eapb,
                  data = Tabla)
summary(m3fe)


  