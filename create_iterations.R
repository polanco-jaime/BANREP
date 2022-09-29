# ### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
EPS_CODE = '[Administradoras].[Codigo de Administradora]'
SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]'

### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[ValorIndicador]'
AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[All]'
AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'

#####################################################
# RIPS
#####################################################


# #####################
if(1==1){
  
  ### SET THE GRADE OF INTEREST
  CAUSA = c('Condiciones transmisibles y nutricionales'
            , 'Enfermedades no transmisibles'	, 'Lesiones'	,'Signos y sintomas mal definidos'
  )
  
  TIPO_USUARIO <- c('1 - CONTRIBUTIVO'	,'2 - SUBSIDIADO'
                    # , '3 - VINCULADO'	,'4 - PARTICULAR'	,'5 - OTRO'	, '6 - DESPLAZADO CON AFILIACIÓN A RÉGIMEN CONTRIBUTIVO'	,
                    # '7 - DESPLAZADO CON AFILIACIÓN A RÉGIMEN SUBSIDIADO'	,'8 - DESPLAZADO NO ASEGURADO O VINCULADO'
  ) # [Tipo Usuario].[Tipo Usuario].&[1 - CONTRIBUTIVO]
  
  # taking the list of the entire eps
  
  EPS_CODE <-  read_excel(paste0(path_input, "diccionario_codigos_eps.xlsx" ) ) #<- read_delim("codigo_entidad_regimen.csv", ",", escape_double = FALSE, trim_ws = TRUE)
  
  connection_string = cnnstr_rips
  eapb_list <- EPS_CODE[['codigo']] #[2:10]
  from_olap_catalog <- 'CU - Morbilidad_ASIS'
  
  ################################# requried data
  
  
  ### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
  SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
  EPS_CODE = '[Administradoras].[Codigo de Administradora]'
  SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]'
  
  ### Mandatory variables to get observations a municipalities's level.
  AXIS0 <- '[Measures].[ValorIndicador]'
  AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[All], '
  AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'
  
  
  
  ITERATIONS = data.frame()
  for (k  in 1:length(eapb_list )  ) {
    # k=1
    EPS = as.character(eapb_list[[k]] )
    print(sprintf("The information for the EPS with code: %s will be downloaded", EPS))
    for (l in CAUSA) {
      VAR_INTERES <-  l
      
      for (m in TIPO_USUARIO) {
        print(sprintf("The EPS with code: %s, var. of interes: %s, and of type user: %s had beed downloaded", EPS, VAR_INTERES, m ))
        TEMPO = data.frame('EPS' = EPS, 'VAR_OF_INTEREST' = VAR_INTERES, 'TYPE_OF_USER' = m )
        
        ITERATIONS = rbind(TEMPO, ITERATIONS)
      }
      
      
    }
    
  }
  write.csv2(ITERATIONS, "ITERATIONS.csv", row.names = F)
}