global_path = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/Tutelas EPS/"

global_output = 'C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/'

setwd(global_path)
#dir.create(file.path(mainDir, subDir))

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

##################
temp = list.files('./',pattern="*.parquet")
tutelas = data.frame()
temp= temp[temp != "Tutelas_clean.parquet"]
temp= temp[temp != "Tutelas_mun_eps_date_full.parquet"]
temp= temp[temp != "Tutelas_mun_eps_date.parquet"]
for (i in temp) {
  tempo= arrow::read_parquet(i)
  tutelas = rbind(tempo, tutelas)
}
# tutelas = tutelas[, c(2:9)]
getwd()
colnames(tutelas)[1] = 'radicacion_expediente'
colnames(tutelas)[6] = 'fecha_radicacion'
tutelas$longitud = nchar(tutelas$radicacion_expediente)

tutelas =  sqldf::sqldf(" SELECT 
          REPLACE( substr(radicacion_expediente, 
                instr(radicacion_expediente,' '),
                longitud   ) ,  ' ' , '') rad_expediente,  *
                        FROM tutelas
                       ")
tutelas=sqldf("SELECT *, substr(corte, 0,5) year    FROM tutelas")

tutelas = sqldf("SELECT * FROM tutelas 
                WHERE Demandado NOT LIKE '%INPEC%'
                ")

source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/Scripts/R/homog_eps_function.R")
source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/Scripts/R/homoge_mun_nombre.R")

Tabla = eps_homog_nombre(Tabla = tutelas , eps_nombre = 'Demandado' )
colnames(Tabla)[12] = "code_eps"
Tabla = eps_homog(Tabla = Tabla, CODIGO__EPS = "code_eps" )

Tabla = eps_validator(Tabla = Tabla , eps_nombre = "Demandado")
Tabla = Tabla %>% dplyr::select(-'code_eps')

Tabla= sqldf("SELECT *, 
              case  
                    WHEN fecha_radicacion like 'Ene%' THEN year||'-01-01'  
                    when fecha_radicacion like 'Feb%' THEN year||'-02-01'  
                    when fecha_radicacion like 'Mar%' THEN year||'-03-01'  
                    when fecha_radicacion like 'Abr%' THEN year||'-04-01'  
                    when fecha_radicacion like 'May%' THEN year||'-05-01'  
                    when fecha_radicacion like 'Jun%' THEN year||'-06-01'  
                    when fecha_radicacion like 'Jul%' THEN year||'-07-01'  
                    when fecha_radicacion like 'Ago%' THEN year||'-08-01'  
                    when fecha_radicacion like 'Sep%' THEN year||'-09-01'  
                    when fecha_radicacion like 'Oct%' THEN year||'-10-01'  
                    when fecha_radicacion like 'Nov%' THEN year||'-11-01'  
                    when fecha_radicacion like 'Dic%' THEN year||'-12-01'        
      Else null end Fecha_corte
      FROM Tabla")
rm(tutelas)
# arrow::write_parquet(Tabla, 'Tutelas_clean.parquet')
# Tabla = arrow::read_parquet('Tutelas_clean.parquet')
colnames(Tabla)[6] = 'Primera_Instancia'
# Municipio = data.frame('Municipio'  = unique(Tabla$`Primera Instancia`) )
# rm(Tabla)
gc()
# Tabla = Municipio

Tabla = homoge_mun_nombre(Tabla = Tabla[ , 1:14],  nombre_mun = 'Primera_Instancia')
table( is.na(Tabla$DIVIPOLA_) )

colnames(Tabla)[7] = 'Segunda_Instancia'
Tabla = Tabla[ , c(2,4:7,12:15)]
colnames(Tabla) = c("rad_expediente" , "Demandante",
                    "Demandado" , "Primera_Instancia" ,
                    "Segunda_Instancia" ,  "year" ,
                    "new_code_eps" , "new_Fecha_corte" , "new_DIVIPOLA_"    )
arrow::write_parquet(Tabla, 'Tutelas_mun_eps_date_full.parquet')
haven::write_dta(Tabla, 'Tutelas_mun_eps_date_full.dta')
###################################################################
#
# Join the master base of health insurance affiliates with tutelas data bases, 
#
###################################################################
Tabla =  arrow::read_parquet('Tutelas_mun_eps_date.parquet')

colnames(Tabla)
temp
Tabla_ = sqldf("
              SELECT COUNT(DISTINCT rad_expediente) TOTAL_TUTELAS,
              homo_code_eps as code_eps_, 
              replace(substr( Fecha_corte, 0,8), '-', '') periodo_,
              DIVIPOLA_ as divipola_,
              DIVIPOLA_||'-'||homo_code_eps COD_
              FROM Tabla
              group by 2,3,4,5
              ")
arrow::write_parquet(Tabla_, 'Tutelas_mun_eps_date.parquet')
#####################################################3
source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/Scripts/R/homog_eps_function.R")
devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/Scripts/functions.R") 
# rm(Tabla)
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
Tabla =Tabla_
colnames(Tabla)
colnames(Master)
if(1==1){
  Tabla_ = sqldf::sqldf(" SELECT * FROM Master 
              left join Tabla
             on cod_mpio =  Divipola_ 
             and cod_eapb  = code_eps_ 
                     and date = Periodo_
             ORDER BY Periodo_
                      ")
  sum(Tabla_$TOTAL_TUTELAS, na.rm = T)
  sum(Tabla$TOTAL_TUTELAS, na.rm = T)
  
  Tabla_ = Tabla_[,c(1:5,9)]
  colnames(Tabla_)[4] = 'Total_afilaidos'
  options(scipen=999)
  
  Tabla_$tasa_tutelas  = Tabla_$TOTAL_TUTELAS / (Tabla_$Total_afilaidos/1000 )
  
  Tabla_$TOTAL_TUTELAS = ifelse( is.na(Tabla_$TOTAL_TUTELAS)==T, 0,Tabla_$TOTAL_TUTELAS  )
  
  # Tabla_ = Tabla_  %>% subset(is.na(Divipola_) == F)
  # Tabla_ = Tabla_ %>% subset(is.na(code_eps_) == F)
  
  Tabla_$COD_ = paste0(Tabla_$cod_mpio,'-' ,Tabla_$cod_eapb) 
  # Control = Tabla_ %>% subset(date == '201805')
  # sum(Control$Defunciones)
}

Tabla_ = sqldf::sqldf("SELECT * FROM ( SELECT  * FROM Tabla_ ) order by  DATE , COD_ " )
############################################## 
######## Creamos el delta. ###################

Tabla_resultado = data.frame()
iter =unique(Tabla_$COD_)

for (i in  1:length(iter) ) {
  # i = 1
  print( paste0(i/19825, '% de avance'))
  temp = subset(Tabla_,Tabla_$COD_ == iter[i] )
  temp$delta_tutelas =  c(NA, diff (temp$TOTAL_TUTELAS, lag = 1, differences = 1 )  )
  temp$delta_tutelas_rate =  c(NA, diff (temp$tasa_tutelas, lag = 1, differences = 1 )  )
  Tabla_resultado = rbind(Tabla_resultado,temp)  
}

haven::write_dta(Tabla_resultado , paste0(global_output , 'tutelas_mun_eps_full_dif.dta')  )

#####################################3
# library(haven)
# 
# event_data_bkcy_date <- read_dta("C:/Users/USER/Desktop/BanRep/Output/event_data_bkcy_date.dta")
# library(haven)
# Tabla_resultado = read_csv( paste0(global_output , 'def_fec_mun_eps.dta')  )
library(readr)
# Tabla_resultado <- read_csv("C:/Users/USER/Desktop/BanRep/Output/def_fec_mun_eps.csv",  col_types = cols(Periodo_ = col_character()))
library(readr)
event_data_bkcy_date <- read_csv("C:/Users/USER/Desktop/BanRep/Output/event_data_bkcy_date.csv", 
                                 col_types = cols(period = col_character(), 
                                                  date = col_character() ) )

# write.csv2("C:/Users/USER/Desktop/BanRep/Output/event_data_bkcy_date.csv")
colnames(Tabla_resultado)
colnames(event_data_bkcy_date)
# event_data_bkcy_date$

Tabla_1 = sqldf("SELECT A.*, TOTAL_TUTELAS, 
                          tasa_tutelas, delta_tutelas, delta_tutelas_rate
              FROM event_data_bkcy_date A
              LEFT JOIN Tabla_resultado  B
              on A.cod_eapb= B.cod_eapb
              and A.date = B.date
              and A.cod_mpio = B.cod_mpio ")


haven::write_dta(Tabla_1 , paste0(global_output , 'event_data_bkcy_date_tutelas.dta')  )

Tabla_resultado_ <- read_csv("C:/Users/USER/Desktop/BanRep/Output/def_fec_mun_eps.csv", 
                            col_types = cols(Periodo_ = col_character()))

Tabla = sqldf("SELECT * FROM Tabla_1
              LEFT JOIN Tabla_resultado_ 
              on Periodo_= period
              and cod_mpio = Divipola_
              and code_eps_ = cod_eapb ")



library(miceadds)
library(estimatr)
Tabla$group = paste0(Tabla$code_eps_,'-',Tabla$cod_mpio,'-' ,Tabla$bankruptcy)
m3fe <- lm_robust(delta_alg ~  delta_real,
                  clusters = group,
                  fixed_effects = ~group+cod_eapb,
                  data = Tabla)
summary(m3fe)






