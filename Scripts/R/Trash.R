library(readxl)
defunciones <- read_excel("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/input/defunciones.xlsx")

library(tidyr)

defunciones <- gather(defunciones, mes, defunciones, Enero:Diciembre, factor_key=TRUE)
defunciones$Anio = as.character(defunciones$Anio)
library(sqldf)
Tabla= sqldf("SELECT *, 
              case  
                    WHEN mes like 'Ene%' THEN Anio||'-01-01'  
                    when mes like 'Feb%' THEN Anio||'-02-01'  
                    when mes like 'Mar%' THEN Anio||'-03-01'  
                    when mes like 'Abr%' THEN Anio||'-04-01'  
                    when mes like 'May%' THEN Anio||'-05-01'  
                    when mes like 'Jun%' THEN Anio||'-06-01'  
                    when mes like 'Jul%' THEN Anio||'-07-01'  
                    when mes like 'Ago%' THEN Anio||'-08-01'  
                    when mes like 'Sep%' THEN Anio||'-09-01'  
                    when mes like 'Oct%' THEN Anio||'-10-01'  
                    when mes like 'Nov%' THEN Anio||'-11-01'  
                    when mes like 'Dic%' THEN Anio||'-12-01'        
      Else null end Fecha_corte
      FROM defunciones")

library(tidyr)
Tabla = spread(Tabla, key = Edad, value = defunciones)
Tabla$`De 45 a 59 años`  = ifelse( is.na(Tabla$`De 45 a 59 años` )==T, 0, Tabla$`De 45 a 59 años`   )


Tabla$`De 60 y más`  = ifelse( is.na(Tabla$`De 60 y más` )==T, 0, Tabla$`De 60 y más`   )
colnames(Tabla)[c(1, 5:6)] = c( 'Cod', "De_45_a_59_años" , "De_60_y_más"  )


Tabla = sqldf("
      SELECT *,
      substr(Cod,  0, instr(Cod,' - ')  ) Codigo
      FROM Tabla 
      ")

Tabla =eps_homog(Tabla = Tabla, CODIGO__EPS ='Codigo')
Tabla$
Tabla =  aggregate_function(Tabla = Tabla, aggregate = "SUM", 
                             cols_to_agg = c("De_45_a_59_años", "De_60_y_más") ,   
                             group_by = c("homo_code_eps" , "Fecha_corte", "cod_mpio") ) 

