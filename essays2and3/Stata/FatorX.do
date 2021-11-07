// limpa o workspace
clear all
// navega para o diretório de análises
// muda diretório de trabalho
cd "D:\Google Drive\_DOUTORADO\__TESE\__CODEandDATA\__TESE_code-data\efficiency\Stata"

// estatística descritiva dos dados do modelo DEA
use "DEA\data_dea.dta"
tabstat avgm custm recm ext npp tarm dEtapa2 dEtapa3, columns(statistics) statistics(mean sd median min max) format(%7.0g)

// usa resultados de eficiência para o dea
use "DEA\eff_dea.dta" 

// usa resuldados de eficiência para o sfa e os combina com o dea
merge 1:1 dmu using "SFA\eff_sfa.dta"

// calcula correlação entre as diversas medidas de eficiência
correlate  deascore_o deascore_bc deascore_sw  eff_ols eff_cmad eff_cols eff_exp eff_expx eff_hn eff_hnx 

mata:


//void calcX(string scalar varlist){
	
	//st_view(X=.,., tokens(varlist), touse)
	st_view(E=.,.,("deascore_o","deascore_bc", "deascore_sw","eff_ols","eff_cmad","eff_cols","eff_exp","eff_expx","eff_hn","eff_hnx"))
	
	c = correlation(E)

	E1=J(rows(E),0,.)
	
	// constrói nova matriz somente com os valores de eficiência 
	// cuja correlação é maior que 0.50
	for (i = 1; i<= cols(c); i++){
		
		if (abs(c[i,1]) > 0.5){
			E1 = E1 , E[.,i]
		}
	}
	// média aritmética (maior média entre as euclidianas)
	meanEff = mean(E1')
	
	st_matrix("eff_meanval", meanEff')
end


// gera valor mínimo de eficiência para cada concessão
egen eff_minval = rowmin(deascore_o-eff_expx) 
// gera valor máximo de eficiência para cada concessão
egen eff_maxval = rowmax(deascore_o-eff_expx)
// gera valor médio de eficiência para cada concessão
*gen eff_meanval = (eff_maxval + eff_minval)/2
// calcula o fator X a partir do menor valor de eficiência, e de acordo com:
// P_x(t)ϵ{0,34;0,25;0,18;0,13;0,10} ao longo de 5(cinco) anos
svmat eff_meanval


gen X1 = (1 - eff_meanval)*0.34 
gen X2 = (1 - eff_meanval)*0.25
gen X3 = (1 - eff_meanval)*0.18
gen X4 = (1 - eff_meanval)*0.13
gen X5 = (1 - eff_meanval)*0.10


foreach v of varlist X1-X5{
	replace `v' = .1 if `v' > .1
} 


// resultados
summarize eff_meanval X*

save x_factor, replace


export excel conc dmu deascore_o eff_cols eff_cmad eff_hn eff_exp eff_hnx eff_expx eff_minval eff_maxval eff_meanval X* using "FatorXSimul.xlsx", firstrow(variables) replace
