getwd()
source("https://raw.githubusercontent.com/JAPJ182/BANREP/main/functions.R")

source("functions.R", echo=TRUE)
#################################
connection_string = "Provider=MSOLAP;Data Source=cubos.sispro.gov.co;Password=u4_gu41n14;
            Persist Security Info=True;
            User ID=sispro.local\\UA_Guainia;
            Initial Catalog=SGD_ReportesRIPS;
            Data Source=cubos.sispro.gov.co"
#### variables for looping
VAR_INTERES = 'Condiciones transmisibles y nutricionales' ### Looped variable
EPS = "RES014"   ### Looped variable

### with the idea reduce the time require for a query I take a smaal segregation with less than 50000 observations.
SEGREGATION_EPS_CODE = '[Administradoras].[Codigo de Administradora]'
EPS_CODE = '[Administradoras].[Codigo de Administradora]'
SEGREGATION_VAR_INTERES = '[Causas de Morbilidad].[Gran Causa]' 
TYPE_USER = '1 - CONTRIBUTIVO'
### Mandatory variables to get observations a municipalities's level.
AXIS0 <- '[Measures].[ValorIndicador]'
AXIS1 <- '[Tiempo].[Año - Semestre - Trimestre - Mes].[Año]'
AXIS2 <- '[Municipio Residencia - RIPS].[Municipio]'

### the cube used
from_olap_catalog = 'CU - Morbilidad_ASIS'




mdx = query_cube_mdx(AXIS0 = AXIS0, 
                     AXIS1 = AXIS1,
                     AXIS2 = AXIS2,
                     TYPE_USER=TYPE_USER,
                     SEGREGATION_VAR_INTERES=SEGREGATION_VAR_INTERES ,
                     VAR_INTERES=VAR_INTERES, 
                     SEGREGATION_EPS_CODE=SEGREGATION_EPS_CODE  , 
                     EPS=EPS,  
                     cube = from_olap_catalog )



tempo = execue_query_mdx(mdx =mdx ,
                         connection_string = connection_string, 
                         EPS=EPS,
                         VAR_INTERES=VAR_INTERES, 
                         TYPE_USER= TYPE_USER  )
