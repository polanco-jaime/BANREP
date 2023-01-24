#Calling for libraries

packageList<-c("readxl", "readr", "tidyverse", "sqldf","beepr", 'dplyr', 'readr', 'devtools')
for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}

#Lsiting all files dowloaded from cube

path_ ="C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep"

files <- list.files(pattern = ".csv", path = path_)
 
productos = data.frame()
for (i in files){
  path= paste0( i)
  print(path)
  tempo <- read_delim(path, ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                      trim_ws = TRUE)
  
  tempo <- sqldf::sqldf("SELECT * ,  substr(ltrim(CITY) , 0,6 ) DIVIPOLA FROM  tempo ") 
  productos<- rbind(tempo, productos )
}



# Reading eps usefull
# EPS_CODE <- read_delim("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/EPS CODE.csv", 
#                        ";", escape_double = FALSE, trim_ws = TRUE)
# EPS_CODE <- read_delim("codigo_entidad_regimen.csv", ",", escape_double = FALSE, trim_ws = TRUE)
 
EPS_CODE <- read_excel("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep/Code/BANREP/diccionario_codigos_eps.xlsx", 
                                      sheet = "codigo_entidad_regimen")

unique(productos$TYPE_USER) 
unique(EPS_CODE$regimen) 
EPS_CODE <- sqldf("
                  SELECT * FROM (
                  SELECT *, 
                  CASE WHEN UPPER(REGIMEN) LIKE '%SBS%' THEN '2 - SUBSIDIADO' 
                  WHEN UPPER(REGIMEN) LIKE '%CNT%' THEN '1 - CONTRIBUTIVO' ELSE NULL END AS NOM_REGIMEN
                  FROM EPS_CODE 
                  )
                  WHERE NOM_REGIMEN IS NOT NULL
                  ")

dim_divipola = data.frame('DIVIPOLA' = unique(productos$DIVIPOLA))  
dim_users = data.frame('TYPE_USER' = unique(productos$TYPE_USER))  
dim_eps = data.frame('EPS_' = unique(productos$EPS))   
dim_year = data.frame('years' = unique(productos$YEAR))
dim_ciudad = data.frame('CITY' = unique(productos$CITY))
dim_causa = data.frame('CAUSA' = unique(productos$VAR_INTERES))

dim  = sqldf("
      SELECT * FROM dim_divipola
      -- LEFT JOIN dim_users ON 1=1
      LEFT JOIN dim_eps ON 1=1
      LEFT JOIN dim_year ON 1=1
      -- LEFT JOIN dim_causa ON 1=1
      --  where dim_causa = '%s'
      ") 
dim_sbs = sqldf::sqldf("
                   SELECT * FROM dim 
                   inner join EPS_CODE 
                   on codigo = EPS_
                   where DIVIPOLA IS NOT NULL
                   and  NOM_REGIMEN = '2 - SUBSIDIADO' 
                   ")

dim_cnt = sqldf::sqldf("
                   SELECT * FROM dim 
                   inner join EPS_CODE 
                   on codigo = EPS_
                   where DIVIPOLA IS NOT NULL
                   and  NOM_REGIMEN = '1 - CONTRIBUTIVO' 
                   ")
colnames(dim_sbs)
head(productos)
##################################
dim_sbs_ = dim_sbs
indices = unique(productos$VAR_INTERES)
for (i in indices  ) {
  print(i)
  temp = subset(productos , productos$VAR_INTERES== i & productos$TYPE_USER == '2 - SUBSIDIADO'  )
  SQL = sprintf(  "
                      SELECT 
                      A.*, 
                      VALUE as %s
                      FROM dim_sbs_ A
                      left join temp B
                      on A.DIVIPOLA = B.DIVIPOLA and codigo = EPS and years = year AND TYPE_USER = NOM_REGIMEN
                      
                      " , gsub(i, pattern = ' ',replacement = '_') )
  
  dim_sbs_ = sqldf::sqldf(SQL)
  
}
##################################
dim_cnt_ = dim_cnt

for (i in  indices   ) {
  print(i)
  temp = subset(productos , productos$VAR_INTERES== i & productos$TYPE_USER == '1 - CONTRIBUTIVO'  )
  SQL = sprintf(  "
                      SELECT 
                      A.*, 
                      VALUE as %s
                      FROM dim_cnt_ A
                      left join temp B
                      on A.DIVIPOLA = B.DIVIPOLA and codigo = EPS and years = year AND TYPE_USER = NOM_REGIMEN
                      
                      " , gsub(i, pattern = ' ',replacement = '_') )
  
  dim_cnt_ = sqldf::sqldf(SQL)
  
}

index_cu_morbilidad_asis = rbind(dim_cnt_,dim_sbs_ )
setwd("C:/Users/USER/OneDrive - Pontificia Universidad Javeriana/02_UPJ 2020/Semestre 5/banrep")
write.csv2(index_cu_morbilidad_asis ,"index_cu_morbilidad_asis.csv")
