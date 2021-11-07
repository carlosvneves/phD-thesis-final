set trace off

clear all

set more off

capture log close

// create output folder with current time and date included in the name
local D = c(current_date)
local T = c(current_time)
local T = subinstr("`T'",":","_",.)

// log file
log using "log-ardl1_pibacum.`D'`T'.smcl", replace

// muda diretório de trabalho
cd "D:\Google Drive\_DOUTORADO\__TESE\_TESE_3rd\artigo1\stata"

// importa dados do modelo com desembolsos totais
import delimited "D:\Google Drive\_DOUTORADO\__TESE\_TESE_3rd\artigo1\R code\dados\model3.pibacum.csv"

gen t = tm(2002m11) + _n - 1
tsset t, monthly

/*
parmby "xi:ardl fbcf cib curi embi er ibov ice ici iie ir ltot nfsp poup tt wp,", ///
"lags(3 0 0 0 1 0 0 0 0 0 2 0 0 0 1) trendvar(t) regstore(res)", ///
label norestore 
sencode parm, gene(parmid) 
eclplot estimate min95 max95 parmid
*/

// lag-selection: R (auto_ardl)
/*FBCF	CIB	CURI	EMBI	ER	IBOV	ICE	ICI	IIE	IR	LTOT	NFSP	POUP	TT	WP	
	3	0	0	0	1	0	0	0	0	0	2	0	2	0	3	*/


/**********************************************
Modelo ARDL com séries como percentual do PIB e 
desembolsos totais como percentual do PIB e acumulados.

***********************************************/
// ARDL regression
ardl fbcf cib curi embi er ibov ice ici iie ir ltot nfsp poup tt wp, ///
lags(3	0	0	0	1	0	0	0	0	0	2	0	2	0	3) ///
trendvar(t) regstore(ardl1_pibacum)

esttab ardl1_pibacum using "ardl1_pibacum.tex" , beta se r2 ar2 brackets label lines tex replace

// ECM LR and SR coefficients
ardl fbcf cib curi embi er ibov ice ici iie ir ltot nfsp poup tt wp, ///
lags(3	0	0	0	1	0	0	0	0	0	2	0	2	0	3) ec ///
trendvar(t) restricted regstore(ecm_ardl1_pibacum)

ardl fbcf cib curi embi er ibov ice ici iie ir ltot nfsp poup tt wp, ///
lags(3	0	0	0	1	0	0	0	0	0	2	0	2	0	3) ec1 ///
trendvar(t) restricted regstore(ecm1_ardl1_pibacum)

predict yhat if e(sample), xb
tsline yhat d.fbcf
graph save fcast_ardl1, replace

fcstats d.fbcf yhat

esttab ecm_ardl1_pibacum ecm1_ardl1_pibacum using "ecm_ardl1_pibacum.tex" , booktabs alignment(D{.}{.}{-1}) replace

* F-Bound test
estat ectest

/* assess the independence of the residuals by testing for normality 
using Skewness/Kurtosis tests by running*/
predict res1, residuals  
sktest res1
qnorm res1
graph save qnorm_ardl1, replace

pnorm res1
graph save pnorm_ardl1, replace

estimates restore ecm1_ardl1_pibacum 

/*examine the residuals of the estimated model for autocorrelation 
using BreuschGodfrey LM test by running*/

estat bgodfrey, lags(1/3) small

estat durbinalt, lags(1/3) small

/*we test for heteroskedasticity in the residuals using Cameron & 
Trivedi’s decomposition of IM-test by running*/
estat imtest, white

estat hettest

estat ovtest

estat sbcusum
graph save cusum_ardl1,replace

estat sbcusum, ols

/*set matsize 800

dynardl fbcf cib curi embi er ibov ice ici iie ir ltot nfsp pib poup tt wp, ///
lags(1, ., ., ., ., ., ., ., ., ., ., ., 1, 1, ., 1) ///
diffs(., ., ., ., ., ., ., ., ., ., ., ., 1, 1, ., 1) ///
levels(., 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ., ., 1, .)  ///
lagdiffs(1/2, ., ., ., ., ., ., ., ., ., ., ., 1/2, 1/2, ., .) ///
shockvar(ltot) time(2) range(36) ///
graph change sims(800) ec trend shockval(-1)*/


* estat archim

* estat vif

* close log file
log close
exit
>>>>> end here	