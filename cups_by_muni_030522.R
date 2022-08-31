rm(list = ls())
packageList<-c("olapR", "foreign", "tidyverse","xlsx","haven","beepr")


for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}


wd <- "C:/Users/mcard/Dropbox/c_2020_More than a Healing/Data/"
setwd(wd)

cnnstr<-"Provider=MSOLAP.8;Password=u4_gu41n14;Persist Security Info=True;User ID=sispro.local\UA_Guainia;Data Source=cubos.sispro.gov.co;Update Isolation Level=2;Initial Catalog=SGD_ReportesRIPS"

olapCnn<-OlapConnection(cnnstr)
qry <- Query(validate = TRUE)
qry
explore(olapCnn)

df <- c()

mdx1 <- " SELECT NON EMPTY {[Measures].[Conteo de Administradoras], 
[Measures].[Número de Atenciones]}  
ON COLUMNS ,
NON EMPTY { 
Hierarchize({DrilldownLevel({[Administradora].[Administradora].CHILDREN},,,INCLUDE_CALC_MEMBERS)}) 
*Hierarchize({DrilldownLevel({[Procedimientos médicos].[Procedimiento CUPS].CHILDREN},,,INCLUDE_CALC_MEMBERS)})
*Hierarchize({DrilldownLevel({[Fecha de Atención].[Mes].CHILDREN},,,INCLUDE_CALC_MEMBERS)}) 
*Hierarchize({DrilldownLevel({[Municipio Residencia RIPS].[Municipio].CHILDREN},,,INCLUDE_CALC_MEMBERS)})
*Hierarchize({DrilldownLevel({[Persona].[Municipio].CHILDREN},,,INCLUDE_CALC_MEMBERS)})
}

DIMENSION PROPERTIES MEMBER_CAPTION ON 1 
FROM [CU - Prestación Servicios de Salud]
WHERE ([Fecha de Atención].[Anno].&[CAMBIAR]) 
CELL PROPERTIES VALUE, FORMAT_STRING "

#FROM [CU - Prestación Servicios de Salud_2009_2014]
#Variable containing input for text
#new <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

new <- c("2021")

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

# Mostrar Campos
# [Administradora].[Administradora]
# [Administradora].[Régimen de Administradora]
# [Administradora].[Tipo de Administradora]
# [Ambitos de procedimiento].[Ambito del Procedimiento]
# [Causas externas].[Causa Externa]
# [Condicion Final].[Condicion Final]
# [Diagnostico Principal].[Capitulo Desc]
# [Diagnostico Principal].[Diagnósticos]
# [Diagnostico Principal].[Nivel de diagnóstico]
# [Diagnostico Principal Egreso].[Capitulo Desc]
# [Diagnostico Principal Egreso].[Diagnósticos]
# [Diagnostico Principal Egreso].[Nivel de diagnóstico]
# [Edad].[Edad]
# [Edad].[Grupo Etáreo - Asistencia Social]
# [Edad].[Grupo Etáreo - Decenios DANE]
# [Edad].[Grupo Etáreo - Edades Simples DANE]
# [Edad].[Grupo Etáreo - Etapas Ciclo Vital]
# [Edad].[Grupo Etáreo - Grandes Grupos DANE]
# [Edad].[Grupo Etáreo - Momento de Curso de Vida]
# [Edad].[Grupo Etáreo - Pensiones]
# [Edad].[Grupo Etáreo - Quinquenios DANE]
# [Edad].[Grupo Etáreo - Riesgos Profesionales]
# [Edad].[Grupo Etáreo - Salud]
# [Edad].[Grupo Etáreo - UPC Nacional]
# [Fecha de Atención].[Anno]
# [Fecha de Atención].[Año - Semestre - Mes]
# [Fecha de Atención].[Fecha]
# [Fecha de Atención].[Mes]
# [Fecha de Atención].[Semestre]
# [Finalidad Consulta].[Finalidad Consulta]
# [Finalidad Procedimientos].[Finalidad Procedimientos]
# [Municipio Residencia RIPS].[Departamento]
# [Municipio Residencia RIPS].[Municipio]
# [Municipio Residencia RIPS].[País - Departamento - Municipio]
# [Persona].[Asistencia Social]
# [Persona].[Codigo]
# [Persona].[Decenios DANE]
# [Persona].[Departamento]
# [Persona].[Edad]
# [Persona].[Edades Simples DANE]
# [Persona].[Etapas Ciclo Vital]
# [Persona].[Etnia]
# [Persona].[Geografía residencia]
# [Persona].[Grandes Grupos DANE]
# [Persona].[Grupo Etáreo - Asistencia Social]
# [Persona].[Grupo Etáreo - Decenios DANE]
# [Persona].[Grupo Etáreo - Edades Simples DANE]
# [Persona].[Grupo Etáreo - Etapas del Ciclo Vital]
# [Persona].[Grupo Etáreo - Grandes Grupos DANE]
# [Persona].[Grupo Etáreo - Pensiones]
# [Persona].[Grupo Etáreo - Quinquenios DANE]
# [Persona].[Grupo Etáreo - Riesgos Profesionales]
# [Persona].[Grupo Etáreo - Salud]
# [Persona].[Grupo Etáreo - Trabajo y Empleo]
# [Persona].[Grupo Etáreo - UPC Nacional]
# [Persona].[Grupo Poblacional]
# [Persona].[Ind Extranjero]
# [Persona].[Municipio]
# [Persona].[Nivel SISBEN]
# [Persona].[Pais]
# [Persona].[Pensiones]
# [Persona].[Quinquenios DANE]
# [Persona].[Riesgos Profesionales]
# [Persona].[Salud]
# [Persona].[Sexo]
# [Persona].[Tipo Identificacion]
# [Persona].[Trabajo Y Empleo]
# [Persona].[UPC Nacional]
# [Persona].[Zona]
# [Personal que atiende].[Personal que atiende]
# [Prestadores Basicos].[Clase Prestador]
# [Prestadores Basicos].[Codigo]
# [Prestadores Basicos].[Geografia Prestador]
# [Prestadores Basicos].[Naturaleza Juridica]
# [Prestadores Basicos].[Nivel Atención]
# [Prestadores Basicos].[Prestador]
# [Procedimientos médicos].[Clasificación CUPS]
# [Procedimientos médicos].[Procedimiento CUPS]
# [Tipo de Atención].[Tipo de Atención]
# [Tipo Diagnóstico Principal].[Tipo Diagnóstico Principal]
# [Tipo Egreso].[Tipo Egreso]
# [Tipo Usuario].[Tipo de Usuario]
# [Administradora].[Clase de Administradora]
# [Administradora].[Naturaleza Jurídica]
# [Diagnostico Principal].[Grupo]
# [Diagnostico Principal].[Subgrupo]
# [Diagnostico Principal Egreso].[Grupo]
# [Diagnostico Principal Egreso].[Subgrupo]
# [Edad].[Asistencia Social]
# [Edad].[Decenios DANE]
# [Edad].[Edades Simples DANE]
# [Edad].[Etapas Ciclo Vital]
# [Edad].[Grandes Grupos DANE]
# [Edad].[Pensiones]
# [Edad].[Quinquenios DANE]
# [Edad].[Riesgos Profesionales]
# [Edad].[Salud]
# [Edad].[Trabajo Y Empleo]
# [Edad].[UPC Nacional]
# [Municipio Residencia RIPS].[ID Geografia]
# [Municipio Residencia RIPS].[País]
# [Persona].[Municipio ID]
# [Persona].[Persona]
# [Prestadores Basicos].[Departamento Desc]
# [Prestadores Basicos].[Municipio Desc]
# [Prestadores Basicos].[Municipio ID]
# [Prestadores Basicos].[Pais Desc]
# [Prestadores Basicos].[Prestador Basico ID]
# [Procedimientos médicos].[Capítulo]
# [Procedimientos médicos].[Categoría]
# [Procedimientos médicos].[Grupo]
# [Procedimientos médicos].[ID Procedimientos]
# [Procedimientos médicos].[Procedimiento RIPS]
# [Procedimientos médicos].[Sección]
# [Procedimientos médicos].[Subgrupo]
# [Measures].[Número Dias Estancia]
# [Measures].[Costo Procedimiento]
# [Measures].[Costo Consulta]
# [Measures].[Valor Cuota Moderadora]
# [Measures].[Neto A Pagar Consulta]
# [Measures].[Número de Atenciones]
# [Measures].[Número de Personas Atendidas]
# [Measures].[Conteo de Administradoras]
# [Measures].[Conteo de Prestadores]
# [Measures].[Concentración]
# [Measures].[Promedio de días de estancia]
# [Measures].[Porcentaje de Usuarios]
# [Measures].[Acumulado - Número de Atenciones]


