           *********************************************************
          /*         Health care utilization estimation           */
         /*            Creation date: 12/01/2023                 */
        /*            Last modification: 12/01/2023             */
       /*                 Author: Naomi Calle                  */                                           
      **********************************************************

/* This do-file runs the estimations for health care utilization and quantity of
   services by municipality, EPS and date for the five total and nationwide 
   bankruptcies we're using: SALUDVIDA, AMBUQ, COMPARTA, COOMEVA and MEDIMAS.
   It also makes graphs of these outcomes over time and tables comparing 
   multiple regressions.
   
   -----------------------------------------------------------------------------
   INPUT FILES                        OUTPUT FILES
   utili_muni_2019_2022.dta           utili_muni_2019_2022_b.dta
   codigos_eps.dta                    utilization_time.dta
   event_data_bkcy_date.dta           
*/

********************************************************************************
**#                        Regressions tables
********************************************************************************
use "event_data_bkcy_date.dta", clear

egen group = group(cod_mpio bankruptcy)
egen muni_eps = group(cod_mpio cod_eapb)

pwcorr delta_real delta_alg , obs sig star(95)
local corr = r(rho)
local corr_obs = r(N)

replace utili = utili/1000000

label var delta_real "Affiliates variation"

preserve
	keep if bankruptcy!="PARCIAL COOMEVA"    & ///
			bankruptcy!="COMFAMILIAR NARIÑO" & ///
			bankruptcy!="PARCIAL MEDIMAS 2" & ///
			bankruptcy!="PARCIAL_MEDIMAS-COMFACUNDI-COMFAMILIAR_CARTAGENA" & ///
			bankruptcy!="CRUZ BLANCA y EMDISALUD"


	 local replace replace
	 foreach outcome in delta_real {
		
		******* No controls regression *******
		 reghdfe `outcome' delta_alg, absorb(group) cluster(group)
		 
		 sum `outcome' if e(sample)
		 local relative = r(mean)
		 local obs = r(N)
		 
		pwcorr delta_real delta_alg if e(sample), obs sig star(95)
		local corr = r(rho)
		 
		 reghdfe `outcome' delta_alg, absorb(group) cluster(group)
		 		
		 outreg2 using "Tabla_`outcome'.xls", replace dec(3) label keep(delta_alg) ctitle("`outcome'") nocons adds(Mean,`relative', Correlation,`corr') addtext(Insurer FE, NO) addnote("Standard errors clustered at the municipality*bankruptcy date level.")
		
			
		******* Insurer FE *******
		 reghdfe `outcome' delta_alg, a(cod_eapb group) cluster(group)
		 
		 sum `outcome' if e(sample)
		 local relative = r(mean)
		 local obs = r(N)
		
		pwcorr delta_real delta_alg if e(sample), obs sig star(95)
		local corr = r(rho)
		
		 reghdfe `outcome' delta_alg, a(cod_eapb group) cluster(group)
				
		 outreg2 using "Tabla_`outcome'.xls", append dec(3) label keep(delta_alg) ctitle("`outcome'") nocons adds(Mean,`relative', Correlation,`corr') addtext(Insurer FE, YES) addnote("Standard errors clustered at the municipality*bankruptcy date level.")	 
	 }

 
restore
local replace replace
foreach outcome in delta_real {

	sum `outcome' 
		local `outcome'_fixed = r(mean)

		sum delta_alg 
		local mean_delta_alg = r(mean)
		
		******* No controls regression *******
		 reghdfe `outcome' delta_alg, absorb(group) cluster(group)
		 
		 sum `outcome' if e(sample)
		 local relative = r(mean)
		 local obs = r(N)
		 
		 pwcorr delta_real delta_alg if e(sample), obs sig star(95)
		local corr = r(rho)
		 
		 reghdfe `outcome' delta_alg, absorb(group) cluster(group)
		 
		
		 outreg2 using "Tabla_`outcome'.xls", append dec(3) label keep(delta_alg) ctitle("`outcome'") nocons adds(Mean,`relative', Correlation,`corr') addtext(Insurer FE, NO) addnote("Standard errors clustered at the municipality*bankruptcy date level.")
		
			
		******* Insurer FE *******
		 reghdfe `outcome' delta_alg, a(cod_eapb group) cluster(group)
		 
		 sum `outcome' if e(sample)
		 local relative = r(mean)
		 local obs = r(N)
		 
		 pwcorr delta_real delta_alg if e(sample), obs sig star(95)
		local corr = r(rho)
		
		 reghdfe `outcome' delta_alg, a(cod_eapb group) cluster(group)
				
		 outreg2 using "Tabla_`outcome'.xls", append dec(3) label keep(delta_alg) ctitle("`outcome'") nocons  adds(Mean,`relative', Correlation,`corr') addtext(Insurer FE, YES) addnote("Standard errors clustered at the municipality*bankruptcy date level.")	 
}
 