// prepara o ambiente de trabalho
clear
drop _all 

// importa dados
import delimited "D:\Google Drive\_DOUTORADO\__TESE\artigo 1\coding\stata\ln.data.csv"
drop date
drop v1


// prepara o dataset para série temporal
gen date = ym(2003,01) + _n - 1 
format %tm date
tsset date 



summarize


gen dum2008 = 0 
replace dum2008 = 1  if (date >= ym(2008,01) & date < ym(2009,01) )

gen dum2012 = 0 
replace dum2012 = 1  if (date >= ym(2012,01) & date < ym(2013,01) )

gen dum2015 = 0 
replace dum2015 = 1  if (date >= ym(2015,01) & date < ym(2016,01) )

gen dum2020 = 0 
replace dum2020 = 1  if (date >= ym(2020,01) & date < ym(2021,01) )

ardl fbcf pib dbgg er nfsp ibov ir poup ltot iie ici ice curi, exog(dum2008 dum2012 dum2015 dum2020) maxlags(12) aic maxcombs(2.80e+14)