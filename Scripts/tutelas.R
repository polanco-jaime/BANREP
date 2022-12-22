global_path = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/Tutelas EPS/"
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
for (i in temp) {
  tempo= arrow::read_parquet(i)
  tutelas = rbind(tempo, tutelas)
}
tutelas = tutelas[, c(2:9)]

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

EPS_ = data.frame('EPS' = unique(tutelas$Demandado))
source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/Scripts/homog_eps_function.R")

Tabla = eps_homog_nombre(Tabla = tutelas , eps_nombre = 'Demandado' )

Tabla = eps_validator(Tabla = Tabla , eps_nombre = "Demandado")

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
###################################################################
# write.csv2(Tabla, 'Tutelas_eps.csv' , row.names = F )

devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/Scripts/functions.R") 
aggregate_function(Tabla = Tabla, 
                   aggregate = 'COUNT',
                   cols_to_agg = '', 
                   group_by)