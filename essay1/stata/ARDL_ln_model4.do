clear all

set more off

capture log close


// muda diretório de trabalho
cd "D:\Google Drive\_DOUTORADO\__TESE\_TESE_3rd\Tese3rda\artigo1\stata"

// importa dados do modelo com desembolsos totais
import delimited "D:\Google Drive\_DOUTORADO\__TESE\_TESE_3rd\Tese3rda\artigo1\R code\dados\database.ln.csv"

*local fname = "SFA3.`D'`T'"
local fname = "ardlmodel4ln"

// cria diretório para armazenar arquivos de saída (FAZER CÓDIGO PARA VERIFICAR EXISTÊNCIA DO DIRETÓRIO)
mata : st_numscalar("OK", direxists("`fname'"))

if scalar(OK) == 0{
	mkdir "`fname'"  
}


// entra no diretorio dos arquivos de simulação
cd "`fname'"


/// create output folder with current time and date included in the name
local D = c(current_date)
local T = c(current_time)
local T = subinstr("`T'",":","_",.)

// log file
log using "log-ardl4_ln.`D'`T'.smcl", replace


gen t = tm(2002m11) + _n - 1
tsset t, monthly

/**********************************************
Modelo ARDL com séries como percentual do wp e 
desembolsos setoriais como percentual do wp e acumulados.

***********************************************/

// ARDL regression
ardl fbcf cib curi embi er ibov ice ici iie ir lagr lcom lind linf /// 
nfsp pib poup tt wp, ///
lags(3 0 0 0 0 0 0 0 0 1 0 0 0 0 0 3 3 2 1) trendvar(t) regstore(ardl_model4_ln)

//eststo ardl_model4_wpacum
/*esttab ardl_model4_ln using "ardl_model4ln.tex" , r2 ar2 p /// 
booktabs title("ARDL desembolsos setoriais - ARDL(3 0 0 0 0 0 0 0 0 1 0 0 0 0 0 3 3 2 1)"\label{tab1}) ///
replace */

outreg2 using ardl2_ln, tex(frag) nocons replace

// ARDL LR e SR coefficients

* data until 2016/01
/*ardl fbcf cib curi embi er ibov ice ici iie ir lagr lcom lind linf /// 
nfsp wp poup tt wp if tin(2002m11, 2016m1), ///
lags(4 4 2 0 0 0 0 1 3 0 3 0 0 0 2 3 2 0 0) ec trendvar(t) restricted regstore(ecm_model4_ln1)*/

* F-Bound test
//estat ectest

* data until 2019/12
ardl fbcf cib curi embi er ibov ice ici iie ir lagr lcom lind linf /// 
nfsp pib poup tt wp, ///
lags(3 0 0 0 0 0 0 0 0 1 0 0 0 0 0 3 3 2 1) ec ///
trendvar(t) restricted regstore(ecm_model4_ln)

// gera a tabela com os coeficientes de curto-prazo na representação em t
outreg2 using ecm_ardl2_ln, tex(frag) nocons replace

ardl fbcf cib curi embi er ibov ice ici iie ir lagr lcom lind linf /// 
nfsp pib poup tt wp, ///
lags(3 0 0 0 0 0 0 0 0 1 0 0 0 0 0 3 3 2 1) ec1 ///
trendvar(t) restricted regstore(ecm_model4_ln1)

// gera a tabela com os coeficientes de curto-prazo na representação em t
outreg2 using sr-ecm1_ardl2_ln, eqdrop(LR) tex(frag) nocons replace

/******************************************/
// Configura o padrão das saídas gráficas
set scheme s2mono
grstyle init
grstyle gsize axis_title_gap tiny
grstyle yesno draw_major_hgrid yes
grstyle anglestyle horizontal_tick vertical
/******************************************/

predict yhat if e(sample), xb
tsline yhat d.fbcf
graph save fcast_ardl2, replace
graph export fcast_ardl2_ln.png, width(600) height(450) replace

fcstats d.fbcf yhat

/*esttab ecm_model4_ln ecm_model4_ln1 using "ecm_model4ln.tex" , r2 ar2 p /// 
booktabs ///
title("{Coeficientes de curto-prazo do modelo com desembolsos setoriais (ECM)."\label{tabsumardlmodel4ln-sr}) ///
mtitles("ECM" "ECM1") replace */

* F-Bound test
estat ectest
mat ftest = r(cvmat)
esttab matrix(ftest) using "ftest_ardl4ln.tex", ///
booktabs title("Pesaran, Shin, and Smith (2001) bounds test Case 4"\label{tab:model4ln-bounds}) ///
replace

/*(ardl fbcf cib curi embi er ibov ice ici iie ir lagr lcom lind linf /// 
nfsp wp poup tt wp, ec trendvar(t) restricted bic maxlags(4 1 1 1 1 1 1 1 1 2 1 1 1 1 1 4 4 3 2) ///
maxcombs(30000000) */

** POST-ESTIMATION


/* assess the independence of the residuals by testing for normality 
using Skewness/Kurtosis tests by running*/
predict res1, residuals  
sktest res1
qnorm res1
graph save qnorm_ardl2_ln, replace
graph export qnorm_ardl2_ln.png, width(600) height(450) replace

pnorm res1
graph save pnorm_ardl2_ln, replace
graph export pnorm_ardl2_ln.png, width(600) height(450) replace

estimates restore ecm_model4_ln1

* Shapiro-Wilk Test
swilk res1

/*performs three versions of the Breusch-Pagan (1979) and Cook-Weisberg (1983) test for heteroskedasticity.*/
estat hettest

/*we test for heteroskedasticity in the residuals using Cameron & 
Trivedi’s decomposition of IM-test by running*/
estat imtest, white

/*examine the residuals of the estimated model for autocorrelation 
using BreuschGodfrey LM test by running*/
estat bgodfrey, lags(1/3) small
/*examine the residuals of the estimated model for autocorrelation 
using Durbin alternative test by running*/
estat durbinalt, lags(1/3) small

// Stability tests
*Ramsey RESET test 
estat ovtest
*CUSUM - residuals fluctuation empirical test
estat sbcusum
graph save cusum_ardl2_ln,replace
graph export cusum_ardl2_ln.png, width(600) height(450) replace


estat sbcusum, ols
graph save cusumols_ardl2_ln,replace
graph export cusumols_ardl2_ln.png, width(600) height(450) replace

*CUMSUM and CUMSUM2
cusum9 D.fbcf L(1/3)D.fbcf
graph save cusumsq_ardl2_ln,replace
graph export cusumsq_ardl2_ln.png, width(600) height(450) replace

/*
** POST-ESTIMATION

estimates restore ecm_model4_ln2

/* assess the independence of the residuals by testing for normality 
using Skewness/Kurtosis tests by running*/
predict res2, residuals  
sktest res2
qnorm res2
pnorm res2

* Shapiro-Wilk Test
swilk res2

/*performs three versions of the Breusch-Pagan (1979) and Cook-Weisberg (1983) test for heteroskedasticity.*/
estat hettest

/*we test for heteroskedasticity in the residuals using Cameron & 
Trivedi’s decomposition of IM-test by running*/
estat imtest, white

/*examine the residuals of the estimated model for autocorrelation 
using BreuschGodfrey LM test by running*/
estat bgodfrey, lags(1/3) small
/*examine the residuals of the estimated model for autocorrelation 
using Durbin alternative test by running*/
estat durbinalt, lags(1/3) small

// Stability tests
*Ramsey RESET test 
estat ovtest
*CUSUM - residuals fluctuation empirical test
estat sbcusum

estat sbcusum, ols

*CUMSUM and CUMSUM2
cusum9 D.fbcf L(1/3)D.fbcf

// Simulação ECM
/*dynardl fbcf cib curi embi er ibov ice ici iie ir lagr lcom lind linf /// 
nfsp poup tt wp, ///
lags(3 1 0 0 1 0 1 2 0 0 0 0 0 3 0 0 0 0) ec1 trendvar(t) restricted*/
*/
* close log file
log close
exit
>>>>> end here	