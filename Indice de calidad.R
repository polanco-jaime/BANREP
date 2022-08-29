### Libraries
lista = c('readr','readxl',
          'rio' , 'arrow'
)

for (i in 1:length(lista) ) {
  if(lista[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(lista[i])
  }
  lapply(lista[i], library, character.only = TRUE)
}

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


calidad_eps_2015 = rio::import(calidad_eps_2015, sheet = "Hoja de Trabajo")

