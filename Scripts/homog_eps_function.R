eps_validator <- function(Tabla , eps_nombre ){
  
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
Tabla <- sqldf::sqldf(query)
Tabla <- dplyr::select(Tabla, -'eps_nombre')
print(nrow(Tabla))
return( Tabla )
}

eps_homog_nombre <- function(Tabla , eps_nombre ){
  
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
  Tabla <- sqldf::sqldf(query)
  Tabla <- dplyr::select(Tabla, -'eps_nombre')
  print(nrow(Tabla))
  return( Tabla )
}
