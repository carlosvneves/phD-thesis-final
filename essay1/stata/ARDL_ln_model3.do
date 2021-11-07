set trace off

clear all

set more off

capture log close


// muda diretório de trabalho
cd "D:\Google Drive\_DOUTORADO\__TESE\_TESE_3rd\Tese3rda\artigo1\stata"

// importa dados do modelo com desembolsos totais
import delimited "D:\Google Drive\_DOUTORADO\__TESE\_TESE_3rd\Tese3rda\artigo1\R code\dados\database.ln.csv"

*local fname = "SFA3.`D'`T'"
local fname = "ardlmodel3ln"

// cria diretório para armazenar arquivos de saída (FAZER CÓDIGO PARA VERIFICAR EXISTÊNCIA DO DIRETÓRIO)
mata : st_numscalar("OK", direxists("`fname'"))

if scalar(OK) == 0{
	mkdir "`fname'"  
}


// entra no diretorio dos arquivos de simulação
cd "`fname'"

// create output folder with current time and date included in the name
local D = c(current_date)
local T = c(current_time)
local T = subinstr("`T'",":","_",.)

// log file
log using "log-ardl3_ln.`D'`T'.smcl", replace


gen t = tm(2002m11) + _n - 1
tsset t, monthly

/**********************************************
Modelo ARDL com séries como percentual do PIB e 
desembolsos totais como percentual do PIB e acumulados.

***********************************************/
// ARDL regression
ardl fbcf cib curi embi er ibov ice ici iie ir ltot nfsp pib poup tt wp, ///
lags(3 0 0 0 0 0 0 0 0 0 0 0 3 3 0 1) trendvar(t) regstore(ardl_model3_ln) 

/*esttab ardl_model3_ln using "ardl_model3_ln.tex" , r2 ar2 p /// 
booktabs title("ARDL Desembolsos Totais ARDL(3,0,0,0,0,0,0,0,0,0,0,0,3,3,0,1)"\label{tab1}) ///
replace */

outreg2 using ardl1_ln, tex(frag) nocons replace

//4 0 0 0 0 0 0 2 3 0 2 2 3 1 2 2
// ECM LR and SR coefficients
ardl fbcf cib curi embi er ibov ice ici iie ir ltot nfsp pib poup tt wp, ///
lags(3 0 0 0 0 0 0 0 0 0 0 0 3 3 0 1) ec trendvar(t) restricted regstore(ecm_model3_ln) 

// gera a tabela com os coeficientes de curto-prazo na representação em t
outreg2 using ecm_ardl1_ln, tex(frag) nocons replace

ardl fbcf cib curi embi er ibov ice ici iie ir ltot nfsp pib poup tt wp, ///
lags(3 0 0 0 0 0 0 0 0 0 0 0 3 3 0 1) ec1 trendvar(t) restricted regstore(ecm_model3_ln1) 

// gera a tabela com os coeficientes de curto-prazo na representação em t
outreg2 using sr-ecm1_ardl1_ln, eqdrop(LR) tex(frag) nocons replace

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
graph save fcast_ardl1, replace
graph export fcast_ardl1_ln.png, width(600) height(450) replace

fcstats d.fbcf yhat

/*esttab ecm_model3_ln ecm_model3_ln1 using "ecm_ardl1_ln.tex" , r2 ar2 p /// 
booktabs title("ECM desembolsos totais - coeficientes de curto-prazo"\label{tab1}) ///
mtitles("ECM(2a)" "ECM(2b)") replace */

* F-Bound test
estat ectest
mat ftest = r(cvmat)
esttab matrix(ftest) using "ftest_ardl3ln.tex", ///
booktabs title("Pesaran, Shin, and Smith (2001) bounds test Case 4"\label{tab1}) ///
replace

/* assess the independence of the residuals by testing for normality 
using Skewness/Kurtosis tests by running*/
predict res1, residuals  
sktest res1
qnorm res1
graph save qnorm_ardl1_ln, replace
graph export qnorm_ardl1_ln.png, width(600) height(450) replace

pnorm res1
graph save pnorm_ardl1_ln, replace
graph export pnorm_ardl1_ln.png, width(600) height(450) replace

estimates restore ecm_model3_ln1

* Shapiro-Wilk Test
swilk res1

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
graph save cusum_ardl1_ln,replace
graph export cusum_ardl1_ln.png, width(600) height(450) replace

estat sbcusum, ols
graph save cusumols_ardl1_ln,replace
graph export cusumols_ardl1_ln.png, width(600) height(450) replace

*CUMSUM and CUMSUM2
cusum9 D.fbcf L(1/3)D.fbcf
graph save cusumsq_ardl1_ln,replace
graph export cusumsq_ardl1_ln.png, width(600) height(450) replace

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