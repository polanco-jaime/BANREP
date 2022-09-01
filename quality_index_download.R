rm(list = ls())
packageList<-c("olapR", "foreign", "tidyverse", "haven","beepr", 'dplyr', 'readr')

for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}


################## ################## ################## ##################


#### Setting string formatting operator 


cnnstr<-"Provider=MSOLAP;Data Source=cubos.sispro.gov.co;Password=u4_gu41n14;
            Persist Security Info=True;
            User ID=sispro.local\\UA_Guainia;
            Initial Catalog=SGD_ReportesRIPS;
            Data Source=cubos.sispro.gov.co"

#### getting connection

olapCnn<-OlapConnection(cnnstr)
explore(olapCnn)


### SET THE GRADE OF INTEREST 
CAUSA = c('Condiciones transmisibles y nutricionales'	,
          'Enfermedades no transmisibles'	,
          'Lesiones'	,
          'Signos y sintomas mal definidos'	)

TIPO_USUARIO <- c('1 - CONTRIBUTIVO'	,
                  '2 - SUBSIDIADO'	,
                  '3 - VINCULADO'	,
                  '4 - PARTICULAR'	,
                  '5 - OTRO'	,
                  '6 - DESPLAZADO CON AFILIACIÓN A RÉGIMEN CONTRIBUTIVO'	,
                  '7 - DESPLAZADO CON AFILIACIÓN A RÉGIMEN SUBSIDIADO'	,
                  '8 - DESPLAZADO NO ASEGURADO O VINCULADO'	)

# taking the list of the entire eps

EPS_CODE <- read_delim("EPS CODE.csv", ";", 
                       escape_double = FALSE, trim_ws = TRUE)


TIME_YEAR = c(2013,2014,2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)

############################################
##############                ##############
##########                        ##########
#####                                  #####
#                                          #
############################################

RESULT = data.frame()

for (k  in 1:nrow(EPS_CODE) ) {
  print(EPS)
  EPS = EPS_CODE[K,1]  
  
  
  
}
# YEAR = TIME_YEAR[1]


#'[Tiempo].[Año - Semestre - Trimestre - Mes].[Año].&[2022]'
AXIS0 <- '[Measures].[ValorIndicador]'
AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[Año]'
AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'
GRAN_CAUSA = 'Condiciones maternas perinatales'
from_olap_catalog <- 'CU - Morbilidad_ASIS'
where_filter <- sprintf( '([Causas de Morbilidad].[Gran Causa].&[%s] ,
                         [Administradoras].[Codigo de Administradora].&[%s]) ' ,GRAN_CAUSA, EPS )


#, [Tiempo].[Año].&[2015]
mdx<- "SELECT {%s} ON AXIS(0),
               {%s.MEMBERS} ON AXIS(1), 
               {%s.MEMBERS} ON AXIS(2) 
        FROM [%s]
        WHERE %s "

mdx<- sprintf(mdx, AXIS0, AXIS1, AXIS2, from_olap_catalog, where_filter ) 

olapR::is.Query(mdx)
result01 <- execute2D(olapCnn, mdx)
result01[[2]] < - as.numeric(result01[[2]])
result01 = subset(result01, result01[[2]] >= 2009 &  result01[[2]]  <=2022)

colnames(result01) <- c('CITY', 'YEAR', 'VALUE')
result01$EPS <- as.character(EPS[1,1])
result01$IDEX_NAME <- as.character(GRAN_CAUSA)
















########################
mdx1 <- "SELECT {[Measures].[ValorIndicador]} ON AXIS(0),
               {[Administradoras].[Codigo de Administradora].[Codigo de Administradora].MEMBERS} ON AXIS(1), 
               {[Municipio Residencia - RIPS].[Municipio].MEMBERS} ON AXIS(2) 
        FROM [CU - Morbilidad_ASIS]
        WHERE [Causas de Morbilidad].[Gran Causa].&[Condiciones maternas perinatales] "
mdx == mdx1
mdx
mdx1

result01 <- execute2D(olapCnn, mdx1)
# qry <- olapR::Query(validate = TRUE)
olapR::cube(qry) <- paste0("[" ,source_cubes[2],"]")
olapR::cube(qry)
olapR::columns(qry) <- c("[Measures].[ValorIndicador]") #.MEMBERS [hierarchy]
olapR::columns(qry)
olapR::rows(qry) <-  c(" [Causas de Morbilidad].[Gran Causa],[Municipio Residencia - RIPS].[Municipio]") 
olapR::rows(qry)
olapR::slicers(qry) <- c( paste0("[Administradoras].[Codigo de Administradora].[",EPS,"]"), paste0("[Tiempo].[Año].[",YEAR,"]") )
olapR::slicers(qry)
olapR::compose(qry)
result1 <- olapR::executeMD(olapCnn, qry)
olapR::executeMD(olapCnn, qry)
# wd <- "C:/Users/mcard/Dropbox/c_2020_More than a Healing/Data/"
# setwd(wd)

# Initial Catalog=SGD_ReportesRIPS
# cnnstr<-"Provider=MSOLAP.7;Password=u4_gu41n14;Persist Security Info=True;User ID=sispro.local\\UA_Guainia;Data Source=cubos.sispro.gov.co"
##########################################
olapCnn<-OlapConnection(cnnstr)
qry <- olapR::Query(validate = TRUE)
qry
explore(olapCnn)


df <- c()



query_cube_gran_causa <- function(source_cubes,value,columna  ) {
  paste0(" SELECT NON EMPTY {[Measures].[ValorIndicador]} ON COLUMNS ,
                  NON EMPTY {
                        Hierarchize({DrilldownLevel( ",value , ")})
                        }
                        DIMENSION PROPERTIES MEMBER_CAPTION ON 1 
         ")
}





mdx1 <- " SELECT NON EMPTY {[Measures].[ValorIndicador]}  
ON COLUMNS ,
NON EMPTY { 
Hierarchize({DrilldownLevel({[Administradora].[Administradora].CHILDREN},,,INCLUDE_CALC_MEMBERS)}) 
*Hierarchize({DrilldownLevel({[Municipio Residencia RIPS].[Municipio].CHILDREN},,,INCLUDE_CALC_MEMBERS)})
}

DIMENSION PROPERTIES MEMBER_CAPTION ON 1 
FROM [CU - Prestación Servicios de Salud]
WHERE ([Fecha de Atención].[Anno].&[CAMBIAR]) 
CELL PROPERTIES VALUE, FORMAT_STRING "

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
