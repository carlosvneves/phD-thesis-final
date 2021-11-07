set trace off

clear all

set more off

capture log close

// log file
//log using "log`fname'.`D'`T'.smcl", replace

// muda diretório de trabalho
cd "D:\Google Drive\_DOUTORADO\__TESE\_TESE_3rd\artigo1\stata"

// importa dados do modelo com desembolsos totais
import delimited "D:\Google Drive\_DOUTORADO\__TESE\_TESE_3rd\artigo1\R code\dados\model3.ln.csv"

gen t = tm(2002m11) + _n - 1
tsset t, monthly

// ECM LR and SR coefficients (BIC)
ardl fbcf cib curi embi er ibov iie ir ltot nfsp pib poup tt wp, ///
lags(3 0 0 0 0 0 0 0 0 0 3 3 0 1) ec trendvar(t) restricted regstore(ecm_model3_ln) 

* F-Bound test
estat ectest

// ECM LR and SR coefficients (AIC)
/*
ardl fbcf cib curi embi er ibov iie ir ltot nfsp pib poup tt wp, ///
lags(3 1 0 1 2 0 4 1 3 4 3 3 2 2) ec trendvar(t) restricted regstore(ecm_model3_ln) */

* F-Bound test
* estat ectest

* eststo ecm_model3_ln

esttab ecm_model3_ln using "ecm_model3_ln.tex" , beta se r2 ar2 brackets label lines tex replace

estimates restore ecm_model3_ln

/* assess the independence of the residuals by testing for normality 
using Skewness/Kurtosis tests by running*/
predict res1, residuals  
sktest res1
qnorm res1
pnorm res1

/*examine the residuals of the estimated model for autocorrelation 
using BreuschGodfrey LM test by running*/
estat bgodfrey, lags(1/3) small

estat durbinalt, lags(1/3) small

/*we test for heteroskedasticity in the residuals using Cameron & 
Trivedi’s decomposition of IM-test by running*/
estat imtest, white
estat hettest

estat sbcusum

estat sbcusum, ols

dynardl fbcf cib curi embi er ibov iie ir ltot nfsp pib poup tt wp, ///
lags(1, ., ., ., ., ., ., ., ., ., 1, 1, ., 1) ///
diffs(., ., ., ., ., ., ., ., ., ., 1, 1, ., 1) ///
levels(., 1, 1, 1, 1, 1, 1, 1, 1, 1, ., ., 1, .)  ///
lagdiffs(1/2, ., ., ., ., ., ., ., ., ., 1/2, 1/2, ., .) ///
shockvar(ltot) time(2) range(38) ///
graph change sims(800) ec trend shockval(-1)


* estat archim

* estat ovtest

* estat vif
* close log file
/*log close
exit
>>>>> end here*/