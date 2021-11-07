// limpa dados
clear all

// muda diretório de trabalho
cd "C:\Dropbox\ANTT\AR-2017-2018\Fator X\1.PROJETO\STATA\Analysis-DEA-SFA"

** /////////////////////////////////////////////////////////////////////////////
/************************************************************************
(2) ANÁLISE DEA, TOMANDO COMO VARIÁVEIS:
- INPUTS: REC_KM
- OUTPUTS: CUST_KM; AVG;


*************************************** Comentário anterior ************************************************************
* A Escolha das variáveis leva em conta a análise realizada na parte (1), em que se 
verifica a relação positiva entre Custos Totais por km e a avaliação geral do trecho, quando se avalia o efeito de cada variável por etapa no modelo RE. Por outro lado, o aumento da receita não implica em melhora na avaliação geral. Isto posto, o modelo parte da premissa de que, por meio da abordagem DEA orientada a input, é possível identificar aquelas unidades em há melhor equilíbrio relativo entre receitas, custos e avaliação geral. 
A ideia seria classificar como ineficiente, aquela unidade em que as receitas estão em um nível muito elevado em relação aos custos totais e a avaliação geral do trecho.

Vale a observação de que Profeta (PROFETA, 2014: REGULAÇÃO E EFICIÊNCIA DOS MODELOS DE CONCESSÕES DE RODOVIAS NO BRASIL, TESE DE DOUTORADO - UFV/MG) entendeu que a eficiência estaria relacionada simplesmente à minimização dos custos totais. Assim elegeu como INPUT custos totais, e como OUTPUS Receitas Totais e Avaliação Geral. Contudo, aparentemente, Profeta não se atentou ao fato de que, de acordo com os relatórios financeiros das concessionárias, os custos totais incluem investimentos, que naturalmente implicam em melhora da condição da rodovia (e por consequência na sua avaliação. Assim, pode ser que não faça sentido a eficiência estar baseada na minimização desses custos.
Os resultados da parte (1) parecem indicar também esse fato e, por isso, optou-se por uma configuração diferente de INPUTs e OUTPUTs.
*************************************** Comentário atualizado **********************************************************

OUTPUT ORIENTED (para compatibilizar com SFA)


*************************************************************************/


// carrega a base de dados original
use "dados.2012_2016.dta"

// apaga qualquer unidade que tenha valor de receita ou custo nulo 
drop if rec  <=0 
drop if cust <=0

/*
subsititui os id's originais por id's para dados em painel (um id para cada concessionária)
*/
// lista de concessionárias na base de dados 
levelsof conc, local(concs)
// contador local para gerar id
local count 1
// loop na lista de concessionárias
foreach v in `concs'{
	
	// substitui o id original, para os casos em que há match entre o nome da
	// concessionária na base e na lista de nomes 
	replace id = `count' if conc == "`v'"
	local ++count // atualiza o contador de id
}

// gera variáveis dummy para marcar as etapas
gen dEtapa2 = (etp==2)
gen dEtapa3 = (etp==3)

// Como a análise na parte (1) indicou não haver variação relevante nas variáveis ao longo do tempo, optou-se por calcular as médias do período das variáveis de input e output
quietly bysort id: egen custm = mean(cust_km)
quietly bysort id: egen recm = mean(rec_km)
quietly bysort id: egen avgm = mean(avg)
quietly bysort id: egen tarm = mean(tar)
// identifica duplicatas
quietly by id: gen dup = cond(_N==1, 0, _n)
// apaga duplicatas
drop if dup > 1

// gera índices para as DMUs
gen dmu = id

// apaga variáveis dispensáveis para a análise DEA
drop id ano anoconc cust cust_km rec rec_km avg dup

// salva dados para simulação DEA
save data_dea, replace

// define inputs e outputs da análise DEA
global inp custm
global out recm avgm


/***********************************************************************
ANÁLISE DEA CONSIDERANDO AS ORIENTAÇÕES INPUT E OUTPUT
************************************************************************/
foreach direction in i o{

	* DEA - variable returns to scale
	quietly: dea $inp = $out, rts(vrs) ort(`direction') stage(2) saving(dea_results)
	mat deascores = r(dearslt)
	mat deascores = deascores[1...,"theta"]
	sort dmu
	cap drop dea1
	svmat deascores, names(dea)
	rename dea1 deascore_`direction'
	gen efficient_`direction' = deascore_`direction' == 1

}

* Escores de eficiência após primeiro estágio
tabstat deascore_i deascore_o efficient_i efficient_o, columns(statistics) statistics(mean sd median min max) format(%7.0g)


/************************************************************************
SECOND-STAGE REGRESSION ANALYSIS USING EFFICIENCY SCORES

*************************************************************************/
* variáveis explicativas
global reglist npp ext tar dEtapa2 dEtapa3

tabstat $reglist, columns(statistics) statistics(mean sd median min max) format(%7.0g)
 
* (Naive) Censored Regression Analysis - eficiência orientada a input
stepwise, pr(.20): tobit deascore_i $reglist, ul(1) /* backward selection */

* (Naive) Censored Regression Analysis - eficiência orientada a output
stepwise, pr(.20): tobit deascore_o $reglist, ul(1) /* backward selection */

* Com base na regressão stepwise tobit, pode ser retirada a variável tar
global reglist_n npp ext dEtapa2 dEtapa3

/************************************************************************
SECOND-STAGE REGRESSION ANALYSIS - Procedimento Simar & Wilson (2007)

*************************************************************************/
* Simar & Wilson (2007) Procedure - orientado a input
simarwilson deascore_i $reglist, unit dots reps(1500) 

* Simar & Wilson (2007) Procedure - orientado a output
simarwilson deascore_o $reglist_n, unit dots reps(1500)

* Simar & Wilson (2007) Procedure - orientado a output - escore invertido
gen deascore_io = 1/deascore_o

simarwilson deascore_io $reglist_n, nounit dots reps(1500)


 
/*
bootstrap, reps(500): tobit CRS_TE , vce(jackknife) ll(0) ul(1) /*dados limitados ao intervalo entre 0 e 1 */

bootstrap, reps(500): tobit VRS_TE npp ext, vce(jackknife) ll(0) ul(1) /*dados limitados ao intervalo entre 0 e 1 */

/*

Although the jackknife—developed in the late 1940s and early 1950s—is of largely historical
interest today, it is still useful in searching for overly influential observations. This feature is often
forgotten. In any case, the jackknife is
-an alternative, first-order unbiased estimator for a statistic;
-a data-dependent way to calculate the standard error of the statistic and to obtain significance
levels and confidence intervals; and
-a way of producing measures called pseudovalues for each observation, reflecting the observation’s
influence on the overall statistic.

*/
#############################################################################
Conclusão da análise:

- Em nenhum dos modelos se pode concluir que a 5% de significância pelo menos 
um dos coeficientes não é igual a zero. 
- Para o nível de significância de 5%, em princípio, não é necessário realizar
correções por meio das variáveis de ambiente.

*/

/************************************************************************
(2) ANÁLISE SFA, TOMANDO COMO VARIÁVEIS:
- INPUTS: REC_KM; AVG;
- OUTPUTS: CUST_KM; 

OUTPUT ORIENTED
*************************************************************************/
* set the directory to the location of .ado files 
* .ado files from:
*
* A Practitioner's Guide to Stochastic Frontier Analysis Using Stata         
* Subal C. Kumbhakar, Hung-Jen Wang and Alan P. Horncastle                     
*                                                                               
adopath ++ "C:\ado\personal\sfbook\sfbook_ado"

/*
gera logaritmo das variáveis para a função de produção Cobb-Douglas

*/
gen l_rec = log(recm)
gen l_cust = log(custm)
gen l_avg = log(avgm)

// define inputs e outputs da análise SFA
global inp l_avg l_rec
global out l_cust

*********************************************************************************************
// (1) OLS - Fronteira determinística
reg $out $inp
eststo OLS

predict e_ols, residual /* salva o resíduo da estimativa OLS na variável e */
quietly summarize e_ols /* obtém as estatísticas sem imprimir nada na tela */
gen double eff_ols = exp(e_ols)
summarize eff_ols

*********************************************************************************************
// (2) COLS - Fronteira determinística (OLS Corrigido)
reg $out $inp
eststo COLS

predict e_cols, residual /* salva o resíduo da estimativa OLS na variável e */
quietly summarize e_cols /* obtém as estatísticas sem imprimir nada na tela */
gen double u_star = -(e_cols - r(max)) /* obtém ineficiência a partir dos resíduos corrigidos */ 
gen double eff_cols = exp(-u_star) /* calcula eficiência técnica */
summarize eff_cols
list conc u_star eff_cols

*********************************************************************************************
// (3) CMAD - Corrected Mean absolute Deviation 
qreg $out $inp /* regressão quantílica - utiliza a mediana */
eststo CMAD

predict e_cmad, residual /* salva o resíduo da estimativa OLS na variável e */
quietly summarize e_cmad /* obtém as estatísticas sem imprimir nada na tela */
gen double eta_star_q = -(e_cmad - r(max)) /* obtém ineficiência a partir dos resíduos corrigidos */ 
gen double eff_cmad = exp(-eta_star_q) /* calcula eficiência técnica */
summarize eff_ols eff_cols eff_cmad


list CRS_TE VRS_TE conc eff_ols eff_cols eff_cmad





frontier  $out $inp

local gama = (e(sigma_u)/e(sigma_v))^2/(1+(e(sigma_u)/e(sigma_v))^2)
display "`gama'"

/*
O resultado de 0.999999 indica que 99,9% da variação da produção é devida
à ineficiência técnica.
*/


predict double u_h, u
gen u_negativo = -(u_h) // ineficiencia
gen eficiencia = exp(u_negativo) // eficiencia
list dmu u_h eficiencia 
