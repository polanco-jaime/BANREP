lista = c('sqldf','plyr', 'dplyr')
for (i in 1:length(lista) ) {
  if(lista[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(lista[i])
  }
  lapply(lista[i], library, character.only = TRUE)
}
# rm(lista)
eps_validator = function(Tabla , eps_nombre ){
  
  if ('eps_nombre' %in% colnames(Tabla)) {
    colnames(Tabla['eps_nombre'] ) = 'eps_nombre_'
    Tabla$eps_nombre = Tabla[[eps_nombre]]
  }else{
    Tabla$eps_nombre = Tabla[[eps_nombre]]
  }
  
  
  library(sqldf)
  
  if ('eps_status' %in% colnames(Tabla)) {
    colnames(Tabla['eps_status'] ) = 'eps__status'
  }
  if ('homo_code_eps' %in% colnames(Tabla)) {
    colnames(Tabla['homo_code_eps'] ) = 'homo__code_eps'
  }
  
  query =  "
SELECT *  FROM (
SELECT  
  CASE 
  WHEN  UPPER(eps_nombre) like	'%EPS%'	THEN 1 	
  WHEN  UPPER(eps_nombre) like	'%E.P.S%'	THEN 1 	
  WHEN  UPPER(eps_nombre) like	'%E.P.S%'	THEN 1 		
  WHEN  UPPER(eps_nombre) like	'%E P S%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%E_P_S%'	THEN 1 		
  WHEN  UPPER(eps_nombre) like	'%A_R_S%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%A.R.S%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%PREPAGA%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%ASOCIACION INDIGENA DEL CAUCA%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%AMBUQ%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%ASMET%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%MEDICI%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%ASOCIACION MUTUAL%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%SALUD%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%ANASWAYU%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%EMSSANAR%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'% ARS%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%HUMANA VIV%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%CAJA%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%MEDICI%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%SANITAS%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%CAMACOL%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%E__P__S%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'% IPS%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%ECOOPSOS%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%E_S_P%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%CAFAM%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%ESP%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%COMPENSAR%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%COOMEVA%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%FAMISAN%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%MALLAMASY%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%MANEXKA%'	THEN 1
  WHEN  UPPER(REPLACE(eps_nombre, '.', '')) like	'%EPS%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%MEDIMAS%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%MUTUAL%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%SANITA%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%CRUZ BLANCA%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%CONVIDA%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%COMPARTA%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%COLSUBSIDIO%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'% ISS%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%CAJA%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%CAJA%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%CAJA%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%CAJA%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%CAJA%'	THEN 1
  WHEN  UPPER(eps_nombre) like	'%CAJA%'	THEN 1
      ELSE 0 END  AS IS_EPS , * 
             
FROM Tabla
) 
WHERE IS_EPS = 1
  "
Tabla = sqldf::sqldf(query)
Tabla = dplyr::select(Tabla, -'eps_nombre')
print(nrow(Tabla))
return( Tabla )
}

eps_homog_nombre = function(Tabla , eps_nombre ){
  
  if ('eps_nombre' %in% colnames(Tabla)) {
    colnames(Tabla['eps_nombre'] ) = 'eps_nombre_'
    Tabla$eps_nombre = Tabla[[eps_nombre]]
  }else{
    Tabla$eps_nombre = Tabla[[eps_nombre]]
  }
  
  
  library(sqldf)
  
  if ('eps_status' %in% colnames(Tabla)) {
    colnames(Tabla['eps_status'] ) = 'eps__status'
  }
  if ('homo_code_eps' %in% colnames(Tabla)) {
    colnames(Tabla['homo_code_eps'] ) = 'homo__code_eps'
  }
  
  query =  "
SELECT *  FROM (
SELECT * , 
  CASE 
  WHEN  UPPER(eps_nombre) like	'%CAMACOL%'	THEN 'CCF001'	 		
  WHEN  UPPER(eps_nombre) like	'%SURAMERICANA%'	THEN 'EPS010'	 
  WHEN  UPPER(eps_nombre) like	'%SOLSALUD%'	THEN  'EPS026'
  WHEN  UPPER(eps_nombre) like	'%SERVICIO OCCIDENTAL DE SALUD%'	THEN  'EPS018'
  WHEN  UPPER(eps_nombre) like	'%OCCIDENTAL DE SALUD%'	THEN  'EPS018'
  WHEN  UPPER(eps_nombre) like	'% SOS.%'	THEN  'EPS018'
  WHEN  UPPER(eps_nombre) like	'% SOS%'	THEN  'EPS018'
  WHEN  UPPER(eps_nombre) like	'%S.O.S%'	THEN  'EPS018'
  
  WHEN UPPER(eps_nombre) LIKE '%SERVISALUD%' THEN 'RES004'
  WHEN UPPER(eps_nombre) LIKE '%MEDICOS ASOCIADOS%' THEN 'RES004'
  
  
  WHEN  UPPER(eps_nombre) like	'%SELVASALUD%'	THEN  'EPS031'
  
  WHEN  UPPER(eps_nombre) like	'%SAVIA%'	THEN  'EPSS40'
  -- SANITAS
  WHEN  UPPER(eps_nombre) like	'%SANITAS%'	THEN  'EPS005'
  WHEN  UPPER(eps_nombre) like	'%SÃ_NITA%'	THEN  'EPS005'
  WHEN  UPPER(eps_nombre) like	'%SNITAS%'	THEN  'EPS005'
  WHEN  UPPER(eps_nombre) like	'%SASNITAS%'	THEN  'EPS005'
  -- 	SALUDVIDA
  WHEN UPPER(eps_nombre) like	'%COMFAMA%' THEN 'CCF002'
  WHEN UPPER(eps_nombre) like	'%COMFAMILIAR CARTAGENA%' THEN 'CCF007'
  WHEN UPPER(eps_nombre) like	'%COMFABOY%' THEN 'CCF009'
  WHEN UPPER(eps_nombre) like	'%COLSUBSIDIO%' THEN 'CCF010'
  WHEN UPPER(eps_nombre) like	'%FAMISANAR%' THEN 'CCF010'
   WHEN UPPER(eps_nombre) like	'%CAFAM%' THEN 'CCF010'
   
  WHEN UPPER(eps_nombre) like	'%COMFACOR%' THEN 'CCF015'
  WHEN UPPER(eps_nombre) like	'%COMFAGUAJIRA%' THEN 'CCF023 '
  WHEN UPPER(eps_nombre) like	'%COMFAMILIAR HUILA%' THEN 'CCF024 '
  WHEN UPPER(eps_nombre) like	'%COMFAMILIAR NARIÑO%' THEN 'CCF027'
  WHEN UPPER(eps_nombre) like	'%COMFAMILIAR NARIÑO%' THEN 'CCF028'
  WHEN UPPER(eps_nombre) like	'%COMFAMILIAR RISARALDA%' THEN 'CCF029'
  WHEN UPPER(eps_nombre) like	'%CAJASAN%' THEN 'CCF031'
  WHEN UPPER(eps_nombre) like	'%COMFENALCO SANTANDER%' THEN 'CCF032 '
  WHEN UPPER(eps_nombre) like	'%COMFASUCRE%' THEN 'CCF033 '
  WHEN UPPER(eps_nombre) like	'%CAFABA%' THEN 'CCF035'
  WHEN UPPER(eps_nombre) like	'%COMFENALCO TOLIMA%' THEN 'CCF037'
  WHEN UPPER(eps_nombre) like	'%COMFACARTAGO%' THEN 'CCF040'
  WHEN UPPER(eps_nombre) like	'%COMFANORTE%' THEN 'CCF045'
  WHEN UPPER(eps_nombre) like	'%COMFAORIENTE%' THEN 'CCF050 '
  WHEN UPPER(eps_nombre) like	'%COMFACUNDI%' THEN 'CCF053'
  WHEN UPPER(eps_nombre) like	'%COMFENALCO CUNDINAMARCA%' THEN 'CCF054'
  WHEN UPPER(eps_nombre) like	'%CAJACOPI ATLANTICO%' THEN 'CCF055 '
  WHEN UPPER(eps_nombre) like	'%COMFACHOCO%' THEN 'CCF102 '
  WHEN UPPER(eps_nombre) like	'%COMFACA%' THEN 'CCF103'
  WHEN UPPER(eps_nombre) like	'%EMPRESAS PUBLICAS DE MEDELLIN-DEPARTAMENTO MEDICO%' THEN 'EAS016 '
  WHEN UPPER(eps_nombre) like	'%FERROCARRILES%' THEN 'EAS027 '
  WHEN UPPER(eps_nombre) like	'%ALIANSALUD%' THEN 'EPS001 '
  WHEN UPPER(eps_nombre) like	'%SALUD TOTA_%' THEN 'EPS002 '
  WHEN UPPER(eps_nombre) like	'%SALUDTOTA_%' THEN 'EPS002 '
  
  WHEN UPPER(eps_nombre) like	'%CAFESALUD%' THEN 'EPS003'
  WHEN UPPER(eps_nombre) like	'%SANITAS%' THEN 'EPS005'
  WHEN UPPER(eps_nombre) like	'%SEGUROS SOCIALES%' THEN 'EPS006'
  WHEN UPPER(eps_nombre) like	'%ISS%' THEN 'EPS006'
  WHEN UPPER(eps_nombre) like	'%COMPENSAR%' THEN 'EPS008 '
  WHEN UPPER(eps_nombre) like	'%COMFENALCO ANTIOQUIA%' THEN 'EPS009'
  WHEN UPPER(eps_nombre) like	'%SURAMERICANA%' THEN 'EPS010 '
  WHEN UPPER(eps_nombre) like	'%COMFENALCO VALLE%' THEN 'EPS012 '
  WHEN UPPER(eps_nombre) like	'%SALUDCOOP%' THEN 'EPS013'
  WHEN UPPER(eps_nombre) like	'%HUMANA VIVIR%' THEN 'EPS014'
  WHEN UPPER(eps_nombre) like	'%HUMANAVIVIR%' THEN 'EPS014'
  WHEN UPPER(eps_nombre) like	'%COLPATRIA%' THEN 'EPS015'
  WHEN UPPER(eps_nombre) like	'%COOMEVA%' THEN 'EPS016'
  WHEN UPPER(eps_nombre) like	'%SERVICIO OCCIDENTAL DE SALUD%' THEN 'EPS018 '
  WHEN UPPER(eps_nombre) like	'%CAPRECOM%' THEN 'EPS020'
  WHEN UPPER(eps_nombre) like	'%CONVIDA%' THEN 'EPS022 '
  WHEN UPPER(eps_nombre) like	'%CRUZ BLANCA%' THEN 'EPS023'
  WHEN UPPER(eps_nombre) like	'%CAPRESOCA%' THEN 'EPS025 '
  WHEN UPPER(eps_nombre) like	'%SOLSALUD%' THEN 'EPS026'
  WHEN UPPER(eps_nombre) like	'%CALISALUD%' THEN 'EPS028'
  WHEN UPPER(eps_nombre) like	'%CONDOR%' THEN 'EPS030'
  WHEN UPPER(eps_nombre) like	'%SELVASALUD%' THEN 'EPS031'
  WHEN UPPER(eps_nombre) like	'%SALUDVIDA%' THEN 'EPS033'
  WHEN UPPER(eps_nombre) like	'%SALUDCOLOMBIA%' THEN 'EPS034'
  WHEN UPPER(eps_nombre) like	'%SALUD ATENCION HUMANA%' THEN 'EPS035'
  WHEN UPPER(eps_nombre) like	'%NUEVA EPS%' THEN 'EPS037 '
  WHEN UPPER(eps_nombre) like	'%MULTIMEDICAS SALUD%' THEN 'EPS038 '
  WHEN UPPER(eps_nombre) like	'%GOLDEN GROUP%' THEN 'EPS039'
  WHEN UPPER(eps_nombre) like	'%COOSALUD%' THEN 'EPS042 '
  WHEN UPPER(eps_nombre) like	'%MEDIMAS%' THEN 'EPS044'
  WHEN UPPER(eps_nombre) like	'%MEDIM_%' THEN 'EPS044'
  
  WHEN UPPER(eps_nombre) like	'%SALUD MIA%' THEN 'EPS046'
  WHEN UPPER(eps_nombre) like	'%MUTUAL SER%' THEN 'EPS048'
  WHEN UPPER(eps_nombre) like	'%DUSAKAWI%' THEN 'EPSI01 '
  WHEN UPPER(eps_nombre) like	'%MANEXKA%' THEN 'EPSI02'
  WHEN UPPER(eps_nombre) like	'%ASOCIACION INDIGENA DEL CAUCA%' THEN 'EPSI03 '
  WHEN UPPER(eps_nombre) like	'%ANASWAYUU%' THEN 'EPSI04 '
  WHEN UPPER(eps_nombre) like	'%MALLAMAS%' THEN 'EPSI05'
  WHEN UPPER(eps_nombre) like	'%MALLAM_%' THEN 'EPSI05'
  
  WHEN UPPER(eps_nombre) like	'%COLMEDICA%' THEN 'EMP017'
  WHEN UPPER(eps_nombre) like	'%COLSEGUROS%' THEN  'EPS011'
  WHEN UPPER(eps_nombre) like	'%PIJAOS SALUD%' THEN 'EPSI06'
  WHEN UPPER(eps_nombre) like	'%CAPITAL SALUD%' THEN 'EPSS34 '
  WHEN UPPER(eps_nombre) like	'%SAVIA SALUD%' THEN 'EPSS40'
  WHEN UPPER(eps_nombre) like	'%EMDISALUD%' THEN 'ESS002'
  WHEN UPPER(eps_nombre) like	'%COOSALUD%' THEN 'ESS024 '
  WHEN UPPER(eps_nombre) like	'%ASMET  SALUD%' THEN 'ESS062 '
  WHEN UPPER(eps_nombre) like	'%ASMET%' THEN 'ESS062 '
  
  WHEN UPPER(eps_nombre) like	'%ASTREA%' THEN 'ESS068'
  WHEN UPPER(eps_nombre) like	'%AMBUQ%' THEN 'ESS076'
  WHEN UPPER(eps_nombre) like	'%ECOOPSOS%' THEN 'ESS091 '
  WHEN UPPER(eps_nombre) like	'%MALLAMAS%' THEN 'ESS115'
  WHEN UPPER(eps_nombre) like	'%EMSSANAR%' THEN 'ESS118 '
  WHEN UPPER(eps_nombre) like	'%COMPARTA%' THEN 'ESS133'
  WHEN UPPER(eps_nombre) like	'%ASOCIACION MUTUAL SER EMPRESA SOLIDARIA DE SALUD%' THEN 'ESS207 '
  WHEN UPPER(eps_nombre) like	'%FUERZAS MILITARES%' THEN 'FMS001'
  WHEN UPPER(eps_nombre) like	'%POLICIA NACIONAL%' THEN 'POL001'
  WHEN UPPER(eps_nombre) like	'%A NACIONAL%' THEN 'POL001'
  WHEN UPPER(eps_nombre) like	'%ECOPETROL%' THEN 'RES002'
  WHEN UPPER(eps_nombre) like	'%MAGISTERIO%' THEN 'RES004'
  WHEN UPPER(eps_nombre) like	'%UATLANTICO%' THEN 'RES005'
  --WHEN UPPER(eps_nombre) like	'%INPEC%' THEN 'RES006'
  WHEN UPPER(eps_nombre) like	'%UVALLE%' THEN 'RES007'
  WHEN UPPER(eps_nombre) like	'%UCAUCA%' THEN 'RES009'
  WHEN UPPER(eps_nombre) like	'%UCARTAGENA%' THEN 'RES010'
  WHEN UPPER(eps_nombre) like	'%UANTIOQUIA%' THEN 'RES011'
  WHEN UPPER(eps_nombre) like	'%UCORDOBA%' THEN 'RES012'
  WHEN UPPER(eps_nombre) like	'%UNARIÑO%' THEN 'RES013'
  WHEN UPPER(eps_nombre) like	'%UPTC%' THEN 'RES014'
  WHEN UPPER(eps_nombre) like	'%CAJASALUD%' THEN 'UT-001'
  WHEN UPPER(eps_nombre) like	'%COMFAMILIARES EN SALUD UT%' THEN 'UT-002'
  WHEN UPPER(eps_nombre) like	'%CONVENIO COMFENALCO UT%' THEN 'UT-003'
  WHEN UPPER(eps_nombre) like	'%CONVENIO CAMACOL CONFAMA UT%' THEN 'UT-004'
  WHEN UPPER(eps_nombre) like	'%FAMISANAR%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%CAJA DE COMPENSACION FAMILIAR DE CORDOBA%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%CAJA DE COMPENSACION FAMILIAR DE LA GUAJIRA%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%CAJA DE COMPENSACION FAMILIAR DEL HUILA%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%CAJA DE COMPENSACION FAMILIAR DE NARIÑO%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%COMFENALCO QUINDIO%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%CAJA SANTANDEREANA DE SUBSIDIO FAMILIAR%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%CAJA DE COMPENSACION FAMILIAR DE BARRANCABERMEJA%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%CAJA DE COMPENSACION FAMILIAR DEL NORTE DE SANTANDER%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%CAJA DE COMPENSACION FAMILIAR CCF DEL ORIENTE COLOMBIANO%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%CAJA DE COMPENSACION FAMILIAR DE CUNDINAMARCA%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%ASMET %' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%ASOCIACION MUTUAL BARRIOS UNIDOS DE QUIBDO%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%ENTIDAD COOPERATIVA SOLDE SALUD DEL NORTE DE SOACHA%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%ASOCIACION MUTUAL%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%ASOCIACION MUTUAL EMPRESA SOLIDARIA DE SALUD DE NARIÑO%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%COOPERATIVA DE SALUD COMUNITARIA%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%CAFAM%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%COMFAMILIAR GUAJIRA%' THEN 'CCF001'
  WHEN UPPER(eps_nombre) like	'%CAJA DE COMPENSACION FAMILIAR DE FENALCO%' THEN 'CCF001'
      ELSE null END  AS homo_code_eps 
             
FROM Tabla
)
WHERE  homo_code_eps IS NOT NULL
  "
  Tabla = sqldf::sqldf(query)
  Tabla = dplyr::select(Tabla, -'eps_nombre')
  print(nrow(Tabla))
  return( Tabla )
}

eps_homog = function(Tabla , CODIGO__EPS ){
  
  if ('CODIGO__EPS' %in% colnames(Tabla)) {
    colnames(Tabla['CODIGO__EPS'] ) = 'CODIGO__EPS_'
    Tabla$CODIGO__EPS = Tabla[[CODIGO__EPS]]
  }else{
    Tabla$CODIGO__EPS = Tabla[[CODIGO__EPS]]
  }
  
  
  library(sqldf)
  
  if ('eps_status' %in% colnames(Tabla)) {
    colnames(Tabla['eps_status'] ) = 'eps__status'
  }
  if ('homo_code_eps' %in% colnames(Tabla)) {
    colnames(Tabla['homo_code_eps'] ) = 'homo__code_eps'
  }
  
  query =  "
  SELECT * , 
  CASE WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF001'	THEN 'CCF001'	 									
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF002'	THEN 'CCF002'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC02'	THEN 'CCF002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF002' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF007'	THEN 'CCF007'	WHEN   REPLACE(CODIGO__EPS, ' ', '') = 	'CCFC07'	THEN 'CCF007' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF007' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF007' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF009'	THEN 'CCF009'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC09'	THEN 'CCF009' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF009' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF009' 
       -- WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF010'	THEN 'CCF010'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC10'	THEN 'CCF010' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF010' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF010' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF015'	THEN 'CCF015'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC15'	THEN 'CCF015' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF015' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF015' 
       -- WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF018'	THEN 'CCF018'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC18'	THEN 'CCF018' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF018' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF018' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF023'	THEN 'CCF023'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC23'	THEN 'CCF023' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF023' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF023' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF024'	THEN 'CCF024'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC24'	THEN 'CCF024' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF024' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF024' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF027'	THEN 'CCF027'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC27'	THEN 'CCF027' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF027' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF027' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF028'	THEN 'CCF028'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF028' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF028' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF028' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF029'	THEN 'CCF029'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF029' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF029' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF029' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF031'	THEN 'CCF031'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF031' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF031' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF031' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF032'	THEN 'CCF032'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF032' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF032' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF032' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF033'	THEN 'CCF033'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC33'	THEN 'CCF033' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF033' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF033' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF035'	THEN 'CCF035'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF035' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF035' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF035' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF037'	THEN 'CCF037'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF037' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF037' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF037' 
        --WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF040'	THEN 'CCF040'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF040' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF040' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF040' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF045'	THEN 'CCF045'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF045' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF045' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF045' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF050'	THEN 'CCF050'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC50'	THEN 'CCF050' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF050' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF050' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF053'	THEN 'CCF053'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC53'	THEN 'CCF053' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF053' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF053' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF054'	THEN 'CCF054'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF054' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF054' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF054' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF055'	THEN 'CCF055'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC55'	THEN 'CCF055' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF055' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF055' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF102'	THEN 'CCF102'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC20'	THEN 'CCF102' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF102' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF102' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'CCF103'	THEN 'CCF103'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF103' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'CCF103' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'CCF103' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EAS016'	THEN 'EAS016'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EAS016' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EAS016' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EAS016' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EAS027'	THEN 'EAS027'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EAS027' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EAS027' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EAS027' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS001'	THEN 'EPS001'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS01'	THEN 'EPS001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS001' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS002'	THEN 'EPS002'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS02'	THEN 'EPS002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS002' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS003'	THEN 'EPS003'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS03'	THEN 'EPS003' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =		'EPSM03'	THEN 'EPS003' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSC03'	THEN 'EPS003' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS005'	THEN 'EPS005'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS05'	THEN 'EPS005' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS005' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS005' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS006'	THEN 'EPS006'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS006' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS006' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS006' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS008'	THEN 'EPS008'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS08'	THEN 'EPS008' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS008' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS008' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS009'	THEN 'EPS009'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS009' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS009' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS009' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS010'	THEN 'EPS010'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS10'	THEN 'EPS010' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS010' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS010' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS012'	THEN 'EPS012'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS12'	THEN 'EPS012' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS012' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS012' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS013'	THEN 'EPS013'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS13'	THEN 'EPS013' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS013' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS013' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS014'	THEN 'EPS014'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS14'	THEN 'EPS014' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS014' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS014' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS015'	THEN 'EPS015'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS015' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS015' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS015' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS016'	THEN 'EPS016'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS16'	THEN 'EPS016' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS016' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS016' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS017'	THEN 'EPS017'	
              WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS17'	THEN 'EPS017' 	
              WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCF010'	THEN 'EPS017' 
              WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC10'	THEN 'EPS017' 
              WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCF018'	THEN 'EPS017' 
              WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'CCFC18'	THEN 'EPS017' 
              
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS018'	THEN 'EPS018'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS18'	THEN 'EPS018' 	  WHEN   REPLACE(CODIGO__EPS, ' ', '') =		'CCF040'	THEN 'EPS018' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS018' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS020'	THEN 'EPS020'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSC20'	THEN 'EPS020' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS020' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS020' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS022'	THEN 'EPS022'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSC22'	THEN 'EPS022' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS022' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS022' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS023'	THEN 'EPS023'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS23'	THEN 'EPS023' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS023' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS023' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS025'	THEN 'EPS025'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSC25'	THEN 'EPS025' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS025' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS025' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS026'	THEN 'EPS026'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS26'	THEN 'EPS026' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS026' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS026' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS028'	THEN 'EPS028'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS028' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS028' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS028' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS030'	THEN 'EPS030'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS030' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS030' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS030' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS031'	THEN 'EPS031'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS031' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS031' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS031' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS033'	THEN 'EPS033'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSC33'	THEN 'EPS033' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =		'EPSS33'	THEN 'EPS033' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSM33'	THEN 'EPS033' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS034'	THEN 'EPS034'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS034' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS034' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS034' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS035'	THEN 'EPS035'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS035' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS035' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS035' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS037'	THEN 'EPS037'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS37'	THEN 'EPS037' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =		'EPS041'	THEN 'EPS037' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS41'	THEN 'EPS037' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS038'	THEN 'EPS038'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS038' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS038' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS038' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS039'	THEN 'EPS039'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS39'	THEN 'EPS039' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS039' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS039' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS042'	THEN 'EPS042'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS42'	THEN 'EPS042' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS042' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS042' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS044'	THEN 'EPS044'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS44'	THEN 'EPS044' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =		'EPS045'	THEN 'EPS044' 	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS45'	THEN 'EPS044' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS046'	THEN 'EPS046'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS46'	THEN 'EPS046' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS046' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS046' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPS048'	THEN 'EPS048'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSS48'	THEN 'EPS048' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPS048' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPS048' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSI01'	THEN 'EPSI01'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSIC1'	THEN 'EPSI01' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSI01' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSI01' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSI02'	THEN 'EPSI02'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSIC2'	THEN 'EPSI02' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSI02' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSI02' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSI03'	THEN 'EPSI03'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSIC3'	THEN 'EPSI03' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSI03' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSI03' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSI04'	THEN 'EPSI04'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSIC4'	THEN 'EPSI04' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSI04' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSI04' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSI05'	THEN 'EPSI05'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSIC5'	THEN 'EPSI05' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSI05' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSI05' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSI06'	THEN 'EPSI06'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSIC6'	THEN 'EPSI06' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSI06' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSI06' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSS34'	THEN 'EPSS34'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPSC34'	THEN 'EPSS34' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSS34' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSS34' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'EPSS40'	THEN 'EPSS40'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'EPS040'	THEN 'EPSS40' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'EPSS40' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'EPSS40' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS002'	THEN 'ESS002'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC02'	THEN 'ESS002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS002' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS024'	THEN 'ESS024'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC24'	THEN 'ESS024' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS024' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS024' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS062'	THEN 'ESS062'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC62'	THEN 'ESS062' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS062' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS062' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS068'	THEN 'ESS068'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS068' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS068' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS068' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS076'	THEN 'ESS076'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC76'	THEN 'ESS076' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS076' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS076' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS091'	THEN 'ESS091'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC91'	THEN 'ESS091' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS091' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS091' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS115'	THEN 'ESS115'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS115' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS115' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS115' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS118'	THEN 'ESS118'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC18'	THEN 'ESS118' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS118' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS118' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS133'	THEN 'ESS133'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC33'	THEN 'ESS133' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS133' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS133' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'ESS207'	THEN 'ESS207'	WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'ESSC07'	THEN 'ESS207' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'ESS207' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'ESS207' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'FMS001'	THEN 'FMS001'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'FMS001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'FMS001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'FMS001' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'POL001'	THEN 'POL001'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'POL001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'POL001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'POL001' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES002'	THEN 'RES002'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES002' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES004'	THEN 'RES004'	 WHEN   REPLACE(CODIGO__EPS, ' ', '') =	'REMG01'	THEN 'RES004' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES004' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES004' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES005'	THEN 'RES005'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES005' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES005' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES005' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES006'	THEN 'RES006'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES006' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES006' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES006' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES007'	THEN 'RES007'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES007' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES007' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES007' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES009'	THEN 'RES009'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES009' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES009' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES009' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES010'	THEN 'RES010'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES010' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES010' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES010' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES011'	THEN 'RES011'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES011' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES011' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES011' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES012'	THEN 'RES012'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES012' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES012' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES012' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES013'	THEN 'RES013'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES013' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES013' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES013' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'RES014'	THEN 'RES014'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES014' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'RES014' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'RES014' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'UT-001'	THEN 'UT-001'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'UT-001' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-001' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'UT-002'	THEN 'UT-002'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'UT-002' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-002' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'UT-003'	THEN 'UT-003'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-003' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'UT-003' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-003' 
        WHEN  REPLACE(CODIGO__EPS, ' ', '') =	'UT-004'	THEN 'UT-004'	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-004' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =		''	THEN 'UT-004' 	-- WHEN   REPLACE(CODIGO__EPS, ' ', '') =	''	THEN 'UT-004' 
        ELSE 'NO DEFINIDO' END  AS homo_code_eps 
             
FROM Tabla
  "
  Tabla = sqldf::sqldf(query)
  Tabla = dplyr::select(Tabla, -'CODIGO__EPS')
  return( Tabla )
}