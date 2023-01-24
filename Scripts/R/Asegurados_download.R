packageList<-c("readxl","olapR", "foreign", "tidyverse", "haven","beepr", 'dplyr', 'readr', 'devtools', 'sqldf', 'tidyr')
# devtools::install_github("apache/arrow/r")

for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}
source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/Scripts/credentials.R")

#####################################################################
#
#####################################################################

devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/Scripts/functions.R") 

'[Entidades].[Cod Entidad].&[%s]'
'[Fecha Corte].[Mesanno].[All]'
A=  c( # '[Entidades].[Cod Entidad].&[%s]'
  'BDEX',  'CCF001',      'CCF002', 'CCF007','CCF009', 'CCF015','CCF018','CCF023','CCF024','CCF027', 
      'CCF028', 'CCF029','CCF031','CCF032','CCF033','CCF035','CCF037', 'CCF040', 'CCF045', 'CCF049', 
      'CCF050', 'CCF053', 'CCF054', 'CCF055', 'CCF101', 'CCF102', 'CCF103', 'CCFC02', 'CCFC09',
      'CCFC10', 'CCFC15', 'CCFC18', 'CCFC20', 'CCFC23', 'CCFC24', 'CCFC27', 'CCFC33', 'CCFC50', 'CCFC53', 
      'CCFC55', 'EAS016', 'EAS027', 'EPS001', 'EPS002', 'EPS003', 'EPS005', 'EPS006', 'EPS008', 'EPS009', 
      'EPS010', 'EPS012', 'EPS013', 'EPS014', 'EPS015', 'EPS016', 'EPS017', 'EPS018', 'EPS020', 'EPS022', 
      'EPS023', 'EPS025', 'EPS026', 'EPS028', 'EPS030', 'EPS031', 'EPS033', 'EPS034', 'EPS035', 'EPS037', 
      'EPS038', 'EPS039', 'EPS040', 'EPS041', 'EPS042', 'EPS044', 'EPS045', 'EPS046', 'EPS048', 'EPSC03', 
      'EPSC20', 'EPSC22', 'EPSC25', 'EPSC33', 'EPSC34', 'EPSI01', 'EPSI02', 'EPSI03', 'EPSI04', 'EPSI05', 
      'EPSI06',  'EPSIC1', 'EPSIC2', 'EPSIC3', 'EPSIC4', 'EPSIC5', 'EPSIC6', 'EPSM03', 'EPSM33', 'EPSS01', 
      'EPSS02', 'EPSS03', 'EPSS05', 'EPSS08', 'EPSS09', 'EPSS10', 'EPSS12', 'EPSS13', 'EPSS14', 'EPSS16', 
      'EPSS17', 'EPSS18', 'EPSS23', 'EPSS26', 'EPSS33', 'EPSS34', 'EPSS37', 'EPSS39', 'EPSS40', 'EPSS41', 
      'EPSS42', 'EPSS44', 'EPSS45', 'EPSS46', 'EPSS48', 'ESS002', 'ESS024', 'ESS062', 'ESS068', 'ESS076', 
      'ESS091', 'ESS115', 'ESS118', 'ESS133', 'ESS207', 'ESSC02', 'ESSC07', 'ESSC18', 'ESSC24', 'ESSC33', 
      'ESSC62', 'ESSC76', 'ESSC91', 'FMS001', 'INP001', 'POL001', 'RES002', 'RES004', 'RES005', 'RES006', 
      'RES007', 'RES008', 'RES009', 'RES010', 'RES011', 'RES012', 'RES013', 'RES014', 'UT-001', 'UT-002', 
      'UT-003', 'UT-004'
)

cnnstr = cnnstr_afiliados
olapCnn<-olapR::OlapConnection(cnnstr)
AXIS0 <- '[Measures].[Total]'
AXIS1 <- "[Fecha Corte].[Mesanno]" #  '[Administradora].[Administradora]'
AXIS2 <- '[Regimen].[Tipo Regimen]'
olapR::explore(olapCnn)
from_olap_catalog = 'CU_Estadisticas Afiliados a Salud General'
mdx  = query_olap(AXIS0=AXIS0,AXIS1=AXIS1, AXIS2= AXIS2,
                  from_olap_catalog = from_olap_catalog, where_filter =where_filter  )





B = c(   # [Regimen].[Tipo Regimen].&[%s]
'CONTRIBUTIVO',
'EXCEPCION',
'INPEC INTRAMURAL',
'SUBSIDIADO'
  )
