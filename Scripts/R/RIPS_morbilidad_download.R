packageList<-c("readxl","olapR", "foreign", "tidyverse", "haven","beepr", 'dplyr', 'readr', 'devtools', 'sqldf', 'tidyr')
# devtools::install_github("apache/arrow/r")

for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}
source("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/Scripts/credentials.R")


devtools::source_url("https://raw.githubusercontent.com/JAPJ182/BANREP/main/Scripts/functions.R") 

metadata <-  c( 'CU - Morbilidad_ASIS')
path_output = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/"

path_input = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/input/"




##################
if(1==1){
  A= data.frame("year" =c(2012:2021)  ) # 
  B = data.frame( 'EPS_CODE' = c('EPS001','EPSS01','EPSI04','EPSIC4','EPSIC1','EPSI03','EPSIC3','EPSI01',
                                 'ESS076','ESSC76','ESS118','ESSC18','ESS115','ESS062','ESSC62','EPS048',
                                 'EPSS48','ESS207','ESSC07','ESS068','EPSS03','EPS003','EPSM03','CCF007',
                                 'CCFC07','CCF018','CCFC18','CCFC50','CCF050','CCF001','CCF002','CCFC02',
                                 'CCF035','CCF009','CCFC09','CCF015','CCFC15','CCF053','CCFC53','CCF054',
                                 'CCF037','CCF028','CCF032','CCF023','CCFC23','CCF027','CCFC27','CCF033',
                                 'CCFC33','CCF103','CCF102','CCFC20', 'CCF024','CCFC24','CCF045','CCF055','CCFC55',
                                 'CCF031','UT-001','EPS028','EPSS34','EPSC34','EPS020','EPSC20','EPS025',
                                 'EPSC25','CCF010','CCFC10','CCF040','CCF029','UT-002','EPS012','EPSS12',
                                 'EPS008','EPSS08','UT-004','UT-003','EPS016','EPSS16','ESS133','ESSC33',
                                 'EPS042','EPSS42','ESS024','ESSC24','EPS023','EPSS23','RES002','ESS002',
                                 'ESSC02','EAS016','ESS091','ESSC91','EPS030','EPS022','EPSC22','EPS017',
                                 'EPSS17','EPS013','EPSS13','EPS005','EPSS05','EPS009','EPS018','EPSS18',
                                 'EPS010','EPSS10','EAS027','FMS001','EPSS46','EPS046','EPS039','EPSS39',
                                 'EPSS14','EPS014','RES006','EPS006','EPS037','EPS041','RES004','EPSI05',
                                 'EPSIC5','EPSI02','EPSIC2','EPS044','EPSS44','EPS045','EPSS45','EPS038',
                                 'EPSS41','EPSS37','EPSI06','EPSIC6','POL001','EPS035','EPS015','EPS002',
                                 'EPSS02','EPS034','EPSS33','EPSC33','EPS033','EPSS40','EPS040','EPS031',
                                 'EPSS26','EPS026','RES011','RES005','RES010','RES009','RES012','RES013',
                                 'RES014','RES007'
                                 )  )
  INTERATION = sqldf(" SELECT * FROM A INNER JOIN B ON 1=1  ")
  INTERATION = na.omit(INTERATION)
}

### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[ValorIndicador], [Measures].[Número de Personas] ,[Measures].[Número de Atenciones]'
AXIS1 <- '[Tiempo].[Mes]' #[Año]
AXIS2 <- "[Grupos Etareos ASIS].[Quinquenios DANE]"#"" #  '[Tiempo].[Año]'
INTERATION$EPS_CODE = as.character(INTERATION$EPS_CODE)
from_olap_catalog <- metadata[1]

tabla_etarea = data.frame() 
tabla_general = data.frame() 

for (i in 1:nrow(INTERATION) ) {
  var1 = as.character((INTERATION[[1]][i] )[1]) #year
  var2 =  as.character((INTERATION[[2]][i] )[1]) # EPS
  print("/------------------------------------------/")
  print(sprintf("/---------- Advance: %s  -------------/" , paste0(round(100*i/nrow(INTERATION) ,2), '%')   ) )
  
  Filter = paste0('[Tiempo].[Año].&[%s],[Administradoras].[Codigo de Administradora].&[%s]')
  where_filter <- sprintf(Filter,   var1,var2) #var2,
  # print(where_filter)
  mdx  = query_olap(AXIS0=AXIS0,AXIS1=AXIS1, AXIS2= AXIS2,
                    from_olap_catalog = from_olap_catalog, where_filter =where_filter  )
  tryCatch( {
    temp  = run_query_olap(cnnstr  = cnnstr_rips, 
                           mdx  = mdx,var1 = var1, var2 = var2 )
    colnames(temp) = c( 'grupo_etareo' , 'mes' , 'indicador',
                         'N_personas',  'N_Atenciones', 'year', 'EPS_code' )
    temp2 = subset(temp  , is.na(temp$grupo_etareo) == T & is.na(temp$mes) ==F )
    temp2 = temp2[,c(2:7)]
    temp = subset(temp  , is.na(temp$grupo_etareo) == F & is.na(temp$mes) ==F )
    # print("/------------------------------------------/")
    # print(sprintf("the number of rows of the full panel are: %s", nrow(temp) ) )
    print("/------------------------------------------/")
    tabla_etarea = rbind(tabla_etarea, temp)
    tabla_general =rbind(tabla_general, temp2)
    gc()
    # print(sprintf("the number of rows of the full panel are: %s", nrow(tabla_general) ) )
    # print(sprintf("the number of rows of the full panel(etareo) are: %s", nrow(tabla_etarea) ) )
  }, error=function(e){cat("ERROR CATCH: ",conditionMessage(e), "\n")}
  )
  
}
path_output = "C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Output/"

write.csv2(tabla_etarea, paste0(path_output, 'tabla_etarea_rips_morbilidad.csv'), row.names = F)
write.csv2(tabla_general, paste0(path_output, 'tabla_general_atx_rips_morbilidad.csv'), row.names = F)



