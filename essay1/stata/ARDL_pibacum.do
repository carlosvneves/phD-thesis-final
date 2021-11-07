use "D:\Google Drive\_DOUTORADO\__TESE\__CODEandDATA\__TESE_code-data\macroeconometricsBNDES\R code\dados\model3.acumpib.csv"

tsset t, monthly

// ARDL regression
ardl fbcf cib curi embi er ibov ice ici iie ir ltot nfsp poup tt wp, ///
lags(3 0 0 0 1 0 0 0 0 0 2 0 0 0 1) trendvar(t) 

// ARDL LR e SR coefficients
ardl fbcf cib curi embi er ibov ice ici iie ir ltot nfsp poup tt wp, ///
lags(3 0 0 0 1 0 0 0 0 0 2 0 0 0 1) ec1 trendvar(t) restricted