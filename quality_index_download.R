rm(list = ls())
packageList<-c("olapR", "foreign", "tidyverse", "haven","beepr", 'dplyr', 'readr')

for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}


##################
EPS_CODE <- read_delim("EPS CODE.csv", ";", 
                       escape_double = FALSE, trim_ws = TRUE)
K=150
TIME_YEAR = c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
EPS = EPS_CODE[K,1]
YEAR = TIME_YEAR[1]
##################

value = '[Measures].[ValorIndicador]'


qry <- olapR::Query(validate = TRUE)
olapR::cube(qry) <- paste0("[" ,source_cubes[2],"]")
olapR::cube(qry)
olapR::columns(qry) <- c("[hierarchy].[Causas de Morbilidad].[Gran Causa]") #.MEMBERS [hierarchy]
olapR::columns(qry)
olapR::rows(qry) <- c("[hierarchy].[Municipio Residencia - RIPS].[Municipio]") 
olapR::rows(qry)
olapR::slicers(qry) <- c( paste0("[Administradoras].[Codigo de Administradora].&[",EPS,"]"), paste0("[Tiempo].[Año].&[",YEAR,"]") )
olapR::slicers(qry)
result1 <- olapR::executeMD(olapCnn, qry)

# wd <- "C:/Users/mcard/Dropbox/c_2020_More than a Healing/Data/"
# setwd(wd)

# Initial Catalog=SGD_ReportesRIPS
cnnstr<-"Provider=MSOLAP;Password=u4_gu41n14;Persist Security Info=True;User ID=sispro.local\\UA_Guainia;Data Source=cubos.sispro.gov.co"
##########################################
olapCnn<-OlapConnection(cnnstr)
qry <- olapR::Query(validate = TRUE)
qry
explore(olapCnn)
source_cubes <- c('PER - Registro de Discapacidad', #'SGD_ReportesRIPS CU - Morbilidad_ASIS',
'PER - Registro Discapacidad - RIPS',
'PER - Registro Discapacidad - RUAF',
'PER - Registro Discapacidad PILA',
'SGD-Discapacidad')

df <- c()



query_cube_gran_causa <- function(source_cubes,value,columna  ) {
  paste0(" SELECT NON EMPTY { ",value , "} ON COLUMNS ,
                  NON EMPTY {
                        Hierarchize({DrilldownLevel( ",value , ")})
                        }
                        DIMENSION PROPERTIES MEMBER_CAPTION ON 1 
         ")
}
# mdx1 <- " SELECT NON EMPTY {[Measures].[ValorIndicador]}  
# ON COLUMNS ,
# NON EMPTY { 
# Hierarchize({DrilldownLevel({[Administradoras].[Codigo de Administradora].CHILDREN},,,INCLUDE_CALC_MEMBERS)}) 
# *Hierarchize({DrilldownLevel({[Causas de Morbilidad].[Gran Causa].CHILDREN},,,INCLUDE_CALC_MEMBERS)})
# *Hierarchize({DrilldownLevel({[Tiempo].[Mes].CHILDREN},,,INCLUDE_CALC_MEMBERS)}) 
# *Hierarchize({DrilldownLevel({[Municipio Residencia - RIPS].[Codigo Municipio].CHILDREN},,,INCLUDE_CALC_MEMBERS)})
# }
# 
# DIMENSION PROPERTIES MEMBER_CAPTION ON 1 
# FROM [PER - Registro Discapacidad - RIPS]
# WHERE ([Tiempo].[Año].&[CAMBIAR]) 
# CELL PROPERTIES VALUE, FORMAT_STRING "

#FROM [CU - Prestación Servicios de Salud_2009_2014]
#Variable containing input for text
#new <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

new <- c("2015")

for (i in 1:length(new)) {
  mdx <- sub(pattern = "CAMBIAR", replacement = new[i], x = mdx1)
  start.time <- Sys.time()
  results <- execute2D(olapCnn,mdx)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  results$year <- new[i]
  dim(results) 
  df <- bind_rows(df,results)
  dim(df)
}
rm(results)
dim(df)
names(df)
table(df$year)

df <- df %>%
  as.data.frame(.) %>%
  # CAMBIAR 
  rename(month = "[Fecha de Atención].[Mes].[Mes].[MEMBER_CAPTION]",
         administradora = "[Administradora].[Administradora].[Administradora].[MEMBER_CAPTION]",
         cups = "[Procedimientos médicos].[Procedimiento CUPS].[Procedimiento CUPS].[MEMBER_CAPTION]",
         num_admin = "[Measures].[Conteo de Administradoras]",
         num_aten = "[Measures].[Número de Atenciones]" ,
         municipioRips = "[Municipio Residencia RIPS].[Municipio].[Municipio].[MEMBER_CAPTION]",
         municipioPersona = "[Persona].[Municipio].[Municipio].[MEMBER_CAPTION]"
  )

beep()

write.csv(df,paste0(wd,"cups_muni_2021.csv")) 
beep() 
# 
# # Mostrar Campos
# [Administradoras].[Codigo de Administradora]
# [Administradoras].[Nombre de Administradora]
# [Administradoras].[Regimen Administradora]
# [Causas de Morbilidad].[Gran Causa]
# [Causas de Morbilidad].[Gran Causa - Subgrupo]
# [Causas de Morbilidad].[Subgrupo de Causa]
# [Grupos Etareos ASIS].[Ciclo Vital PDSP]
# [Grupos Etareos ASIS].[Edades Simples DANE]
# [Grupos Etareos ASIS].[Etapas Ciclo Vital]
# [Grupos Etareos ASIS].[Quinquenios DANE]
# [Municipio Residencia - RIPS].[Departamento]
# [Municipio Residencia - RIPS].[Municipio]
# [Municipio Residencia - RIPS].[País]
# [Municipio Residencia - RIPS].[País - Departamento - Municipio]
# [Persona].[Área de Residencia]
# [Persona].[Etnia]
# [Persona].[Sexo]
# [Persona].[Tipo Identificacion]
# [Tiempo].[Año]
# [Tiempo].[Año - Semestre - Trimestre - Mes]
# [Tiempo].[Mes]
# [Tiempo].[Semestre]
# [Tiempo].[Trimestre]
# [Tipo Atención].[Tipo Atención]
# [Tipo Usuario].[Tipo Usuario]
# [Administradoras].[Administradora ID]
# [Administradoras].[Clase Administradora]
# [Causas de Morbilidad].[Diagnostico ID]
# [Grupos Etareos ASIS].[Grupo Etareo ASISID]
# [Municipio Residencia - RIPS].[Codigo Municipio]
# [Municipio Residencia - RIPS].[Municipio ID]
# [Persona].[Edad]
# [Persona].[Persona Basica ID]
# [Persona].[Regimen Salud Desc]
# [Tiempo].[Fecha]
# [Tiempo].[Fecha ID]
# [Tipo Atención].[Tipo Evento RIPSID]
# [Tipo Usuario].[Tipousuario ID]
# [Measures].[Número de Atenciones]
# [Measures].[Número de Personas]
# [Measures].[ValorIndicador]
