######### Libraries required #########
 
lista = c('readr','readxl', #'arrow', 
          'rio' , 'sqldf', 'haven', 'stringdist'
)

for (i in 1:length(lista) ) {
  if(lista[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(lista[i])
  }
  lapply(lista[i], library, character.only = TRUE)
}

######### URL tables #########
prefix = "https://docs.supersalud.gov.co/PortalWeb/SupervisionInstitucional/IndicadoresCalidadEAPB/"
### URL libraries 
calidad_eps_2015 = "Indicadores%20de%20Calidad%20EPS%20(I%20Semestre%202015).xlsx"
calidad_eps_2016 = "Indicadores%20de%20Calidad%20EPS%20(ISemestre%202016).xlsx"
calidad_eps_2014 = "Indicadores-Calidad-EPS-Consolidado-2014.xlsx"
calidad_eps_2013 = "Indicadores-Calidad-EPS-Consolidado-2013.xlsx"
calidad_eps_2012 = "Indicadores-Calidad-EPS-consolidado-2012.xlsx"
calidad_eps_2011 = "Indicadores-Calidad-EPS-consolidado-2011.xlsx"
calidad_eps_2010 = "Indicadores-Calidad-EPS-consolidado-2010.xlsx"
calidad_eps_2009 = "Indicadores-Calidad-EPS-Consolidado-2009.xlsx"

######### reading table: conditions #########
lista = data.frame(
  'tabla' = c("calidad_eps_2009", "calidad_eps_2010",
  "calidad_eps_2011", "calidad_eps_2012",
  "calidad_eps_2013", "calidad_eps_2014", 
  "calidad_eps_2015",  "calidad_eps_2016"
  ), 
  'sheet' = c("Hoja de Trabajo", "Hoja de Trabajo",
              "Hoja de Trabajo", "Hoja de Trabajo",
              "Hoja de Trabajo", "Hoja de Trabajo", 
              "Hoja de Trabajo",  "HOJA DE TRABAJO"
  )
  
)
 
######### reading table: tables Calidad EPS from 2009 to 2016 #########
for (i in 1:nrow(lista) ) {
  pre_tabla = lista[i, 1]
  tabla = get(pre_tabla)
  hoja = lista[i, 2]
  url = paste0(prefix, tabla)  
  assign(pre_tabla,rio::import(url,  sheet =hoja) , envir = .GlobalEnv)
  print(pre_tabla)
}





sqldf::sqldf("
             SELECT VALOR FROM calidad_eps_20
             ")








#############################################################################
##### Homogenization EPS

library(readxl)
EPS_CODE <- read_excel("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/diccionario_codigos_eps.xlsx", 
                       sheet = "codigo_entidad_regimen")


Btutelas_eps <- read_excel("C:/Users/USER/Downloads/Btutelas_eps.xlsx", 
                           sheet = "Base", col_types = c("numeric", 
                                                         "text", "skip", "skip", "skip", "text", 
                                                         "numeric"))


Btutelas_eps = homogenizacion_eps(tabla =Btutelas_eps ,
                   Nombre_eps = 'eps' ,
                   Regimen_salud = 'regimen_' )

source1 = 'EPS_CODE'
source2 = 'Btutelas_eps'
colname_source1 = 'entidad'
colname_source2='eps'

sql = sprintf("SELECT DISTINCT entidad, eps, codigo  FROM %s
             LEFT JOIN %s
             ON 1=1" ,source1,source2  )
tempo = sqldf::sqldf(sql)
 
tempo$lv_distance = stringdist( toupper(tempo[[colname_source1]]) , toupper(tempo[[colname_source2]])   )

tempo = subset(tempo, tempo$lv_distance <= 5)
