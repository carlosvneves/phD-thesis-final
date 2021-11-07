rm(list = ls())
set.seed(42)
setwd("D:/Google Drive/_DOUTORADO/__TESE/_TESE_3rd/artigo1/R code")


## ---- Load Data --------------------------------------------------------------
source("00.libs.R")

load("dados.RData")
#load("dados-model.RData")
#database <- data[[1]]
rm(list=ls()[ls()!="data.m"])


## --- Creates path to save outputs --------------------------------------------
dir_prefix <- "out_bvarln"
dir.create(dir_prefix)
dir.create(file.path(dir_prefix, "figs"))

#setwd(file.path(mainDir, subDir))

sink(file = paste0(dir_prefix,"/bvarlnlog.txt"))

start_time <- Sys.time()

print("########################################################################")
print("INÍCIO")
print(start_time)
print("########################################################################")
## ---- Funções auxiliares -----------------------------------------------------

prior_mean <- function(data){
  ##############################################################################
  #' @description Sets prior mean using KPSS unit-root test.
  #' 1 - Random-walk (non stationary)
  #' 0 - White-noise (stationary)
  #'   
  b <- rep(0,ncol(data))
  sig <- 2
  names <- colnames(data)
  
  for(i in 1:ncol(data)){
    
    ###########################################################################
    kpss <- ur.kpss(data[,i], type="tau", lags = "short")
    
    cat(" *** Teste KPSS para a série: ", names[i],"\n")
    
    if (kpss@teststat > kpss@cval[1,sig]){
      cat(" A série: ", names[i]," possui raiz unitária.","-t-stat:", kpss@teststat,"\n")
      #type[["kpss"]] <- "NS"
      b[i] <- 1
    }
    else {
      
      cat(" A série: ", names[i]," não possui raiz unitária.","-t-stat:", kpss@teststat,"\n")
      #type[["kpss"]] <- "S"
      
    }
  }  
  
  return(b)
}



## ---- Criação das bases de dados para os dois problemas ----------------------
data.var1 <- data.m[,-c(11:14)]
data.var2 <- data.m[,-c(15)]


data.var1 <- data.var1[,c("FBCF",sort(colnames(data.var1)[2:ncol(data.var1)]))]
data.var2 <- data.var2[,c("FBCF",sort(colnames(data.var2)[2:ncol(data.var2)]))]

## ---- Transformação dos dados para log-diff ----------------------------------

#Basically, it would be possible to proceed without any further 
#transformation of the data. However, it is good practise to 
#multiply log-differenced data by 100 so that, for example, 
#the value 1% is indicated by 1 and not by 0.01.


## ---- Data transformation (log): MODEL1 (FBCF X LTOT)--------------------
data <- fred_transform(as.data.frame(data.var1), codes=c(5,5,5,5,5,5,
                                                         5,5,5,2,5,2,
                                                         5,5,5,5), scale=1)


write.csv(data, "dados/model3.ln.csv")


cat("\n#####################################################################\n")
cat("SIMULANDO MODELO BVAR1: FBCF X LTOT - log-transform")
cat("\n#####################################################################\n")
## ---- Setting-up the priors --------------------------------------------------
b1 <- prior_mean(data)

p1 <- (vars::VARselect(data, lag.max = 13, type="const"))$selection[2]

mn1 <- bv_minnesota(lambda = bv_lambda(mode = 2., sd = 0.15,
                                       min = 0.0001, max=2.), 
                    var= 1e7, b=b1, 
                    alpha = bv_alpha(mode = .5, sd = 0.2, min = 1e-4, max = 2),
                    psi = bv_psi(scale = 0.0005, shape = 0.0005, 
                                 mode = "auto"))


priors1 <- bv_priors(hyper = "auto", mn = mn1) 

mh1 <- bv_mh(scale_hess = 0.005,
             adjust_burn = 0.5,
             acc_lower = 0.25,
             acc_upper = 0.50,
             acc_change = 0.01, 
             adjust_acc=TRUE)




## ----- Run Model -------------------------------------------------------------
# non-parallel
bvar1 <- bvar(data, lags = p1, n_draw = 25000, n_burn = 5000, n_thin = 10,
              priors = priors1, mh = mh1, verbose = FALSE)

print(" ## Resumo do modelo BVAR1:")
print("")
print(bvar1)

## ----- Residual Diagnostics --------------------------------------------------
png(paste0(dir_prefix,"/figs/plot-bvar1-residuals.png"))
plot(residuals(bvar1, type = "mean"), vars = c("FBCF", "LTOT"))
dev.off()

cat("\n\n ### Residual mean of FBCF:", mean(residuals(bvar1)[,1]))

cat("\n\n ### Residual mean of LTOT:",mean(residuals(bvar1)[,11]))

## ----- MCMC Diagnostics ------------------------------------------------------
png(paste0(dir_prefix,"/figs/plot-bvar1-mcmc-trace-density.png"))
plot(bvar1)
dev.off()

png(paste0(dir_prefix,"/figs/plot-bvar1-fbcf-density.png"))
plot(bvar1, type = "dens", vars_response = "FBCF", vars_impulse = "FBCF-lag1")

bvar1_mcmc <- as.mcmc(bvar1, vars = "lambda")
print(" ## Diagnóstico de convergência GEWEKE: ")
print("")
print(geweke.diag(bvar1_mcmc))

#A z-score, or z-statistic, is a number representing how many standard deviations
#above or below the mean population the score derived from a z-test is. 
#Essentially, it is a numerical measurement that describes a value's relationship 
#to the mean of a group of values. If a Z-score is 0, it indicates that the data 
#point's score is identical to the mean score. A Z-score of 1.0 would indicate a 
#value that is one standard deviation from the mean. Z-scores may be positive 
#or negative, with a positive value indicating the score is above the mean and 
#a negative score indicating it is below the mean.

#The resulting scores are best interpreted graphically, 
#using the geweke_plot function. This displays the scores 
#in series, in relation to the 2 standard deviation boundaries 
#around zero. Hence, it is easy to see departures 
#from the standard normal assumption.
png(paste0(dir_prefix,"/figs/plot-bvar1-mcmc-geweke-lambda.png"))
geweke.plot(bvar1_mcmc)
dev.off()


# autocorrelation plot for lambda
png(paste0(dir_prefix,"/figs/plot-bvar1-mcmc-autocorr-lambda.png"))
autocorr.plot(bvar1_mcmc)
dev.off()

## ----- IRFs and  FEVDs -------------------------------------------------------


cat("### Calculando IRFs e FEVDs BVAR1 ...\n")

tic("Tempo para cálculo das IRFs e FEVDs BVAR1 ")
opt_irf <- bv_irf(horizon = 36,identification = TRUE, fevd=TRUE)
irf(bvar1) <- BVAR::irf(bvar1,opt_irf,conf_bands=c(0.05,0.10))
toc()


png(paste0(dir_prefix,"/figs/plot-irf-fbcfltot.png"))
plot(BVAR::irf(bvar1), area = TRUE,
     vars_impulse = c("LTOT"), vars_response = c("FBCF"))
dev.off()

BVARverse::bv_ggplot(BVAR::irf(bvar1), vars_impulse = c("LTOT"), 
                     vars_response = c("FBCF"))
ggplot2::ggsave(paste0(dir_prefix,"/figs/plot-irf-fbcfltot-1.png"))

max_shock <- max(bvar1$irf$quants[3,1,,11])

cat("\n### Valor máximo do choque (LTOT x FBCF):", max_shock)

fevd.FBCF <- as.data.frame(bvar1$irf$fevd$quants[3,1,,])
colnames(fevd.FBCF) <- colnames(data)
fevd.FBCF <- t(fevd.FBCF)

cat("\n### FEVD após 36 períodos é (%):") 
cat("\nLTOT:",fevd.FBCF["LTOT",36]*100)


png(paste0(dir_prefix,"/figs/plot-fevdvarltot.png"))
barplot(fevd.FBCF[c("FBCF","LTOT"),], xlab="", 
        col = gray.colors(2), names.arg=c(1:36),
        main="FEVD - LTOT x FBCF") 
legend("bottomleft", legend= c("FBCF","LTOT"), 
       col = gray.colors(2),pch=20)
dev.off()


irf_var1 <-bvar1$irf$quants[3,1,,]

# comparÃ¡veis aos multiplicadores de curto-prazo do modelo ARDL?
# IRF LTOT -> FBCF
irf.ltot.fbcf <- ts(irf_var1[,11]) # 50%, FBCF [1], T[1:30], LTOT[10]

#acumulando
# comparÃ¡veis aos multiplicadores de longo-prazo do modelo ARDL?

irfsc.ltot.fbcf = ts(cumsum(irf.ltot.fbcf))

max_shockcum <- max(irfsc.ltot.fbcf)

cat("\n### Valor máximo do choque acumulado (LTOT x FBCF):", 
    max_shockcum)



## ----IRF acumulada para FBCF devida ao choque de LTOT-------------------------
png(paste0(dir_prefix,"/figs/plot-cusumbvarltot.png"))
plot(irfsc.ltot.fbcf, main="CUSUM Shock LTOT on FBCF",  xlab = "", ylab="")
grid(4, 4, lwd = 2)
dev.off()

## -----------------------------------------------------------------------------
# extrai os coeficientes do modelo bvar e intervalos de confiança
bvar1_coefs <- augment(bvar1, conf_bands = 0.05)
# extrai os coeficientes da equaÃ§Ã£o para FBCF relacionados a LTOT
fbcf_coefs <- bvar1_coefs %>% dplyr::filter(variable == "FBCF" & grepl('LTOT', term))

# extrai os coeficientes da equaÃ§Ã£o para LTOT relacionados a FBCF
ltot_coefs <- bvar1_coefs %>% dplyr::filter(variable == "LTOT" & grepl('FBCF', term))


# constrói tabela com os coeficientes da equação para FBCF
print("")
print(xtable::xtable(fbcf_coefs, digits = -2))

## --------- Model Forecast Evaluation------------------------------------------
# terei que separar entre treino e teste para avaliar a capacidade preditiva do 
# modelo
data.var1_train <- window(data.var1, end=2018)

data_train <- fred_transform(as.data.frame(data.var1_train), codes=c(5,5,5,5,5,5,
                                                                    5,5,5,2,5,2,
                                                                    5,5,5,5), 
                                                                    scale=1)
bvar1_train <- bvar(data_train, lags = p1, n_draw = 25000, n_burn = 5000, n_thin = 10,
              priors = priors1, mh = mh1, verbose = FALSE)

print("")
print(" ## Resumo do modelo BVAR1_train:")
print("")
print(bvar1_train)

y <- predict(bvar1_train, horizon=22)

png(paste0(dir_prefix,"/figs/plot-bvar1-fcast1.png"))
plot(y, area=TRUE, t_back=48, vars=c("FBCF"))
dev.off()

fbcf_test <- tail(data[,1],22)

fbcf_fcast <-y$fcast[1,,1]

png(paste0(dir_prefix,"/figs/plot-bvar1-fcast2.png"))
ts.plot(cbind(fbcf_fcast, fbcf_test),
        gpars=list(xlab="time", ylab="log(FBCF)", lty=c(1:2),
                   col=c("red","blue")))
legend("topleft", bty="n", lty=c(1,1), col=c("red","blue"),
       legend=c(" fcast ", "data "))
dev.off()

rmse1 <- MLmetrics::RMSE(fbcf_fcast, fbcf_test)

cat("\n### RMSE do modelo1:",rmse1)

arima_fbcf <- auto.arima(data.var1_train[,1])
arima_fcast <-forecast::forecast(arima_fbcf,h=23)
arima_fcast <- diff(log(arima_fcast$mean))

png(paste0(dir_prefix,"/figs/plot-bvar1-fcast-ARIMA.png"))
ts.plot(arima_fcast, fbcf_test,
        gpars=list(xlab="time", ylab="FBCF", lty=c(1:2),
                   col=c("red","blue")))
legend("topleft", bty="n", lty=c(1,1), col=c("red","blue"),
       legend=c(" fcast ", "data "))
dev.off()

rmse_AR1 <- MLmetrics::RMSE(arima_fcast, fbcf_test)

cat("\n ### MODELO ARIMA AJUSTADO: ")
print(summary(arima_fbcf))
cat("\n### RMSE do modelo - ARIMA:",rmse_AR1)



## -----------------------------------------------------------------------------

################################################################################
# Model 2: data acummulated as % PIB (and log transformation) - 
# FBCF x LAGR,LCOM,LIND,LINF
################################################################################

## ---- Data transformation (log-diff): MODEL2 (FBCF X LAGR,LCOM,LIND,LINF) ----
data <- fred_transform(as.data.frame(data.var2), codes=c(5,5,5,5,5,5,
                                                         5,5,5,2,5,5,
                                                         5,5,2,5,5,5,5), 
                       scale=1)

write.csv(data, "dados/model4.ln.csv")


cat("\n#####################################################################\n")
cat("SIMULANDO MODELO BVAR2: FBCF X LAGR,LCOM,LIND,LINF - DADOS COMO % PIB")
cat("\n#####################################################################\n")

## ---- Setting-up the priors --------------------------------------------------
p2 <- (vars::VARselect(data, lag.max = 13, type="const"))$selection[2]

b2 <- prior_mean(data)

mn2 <- bv_minnesota(lambda = bv_lambda(mode = .2, sd = 0.15,
                                       min = 0.0001, max=2.), 
                    var= 1e7, b=b2,
                    alpha = bv_alpha(mode = 1.6, sd = .5, 
                                     min = 0.001, max = 3.),
                    psi = bv_psi(scale = 0.005, shape = 0.005, 
                                 mode = "auto"))


priors2 <- bv_priors(hyper = "auto", mn = mn2) 

mh2 <- bv_mh(scale_hess = 0.01,
             adjust_burn = 0.5,
             acc_lower = 0.25,
             acc_upper = 0.50,
             acc_change = 0.005, 
             adjust_acc=TRUE)

## ----- Run Model2 ------------------------------------------------------------
bvar2 <- bvar(data, lags = p2, n_draw = 25000, n_burn = 5000, n_thin = 10,
              priors = priors2, mh = mh2, verbose = FALSE)
print("")
print(" ## Resumo do modelo BVAR2:")
print("")
print(bvar2)


## ----- Residual Diagnostics --------------------------------------------------
png(paste0(dir_prefix,"/figs/plot-bvar2-residuals.png"))
plot(residuals(bvar2, type = "mean"), vars = c("FBCF", "LAGR", 
                                               "LCOM", "LIND", "LINF"))
dev.off()

cat("\n### Residual mean of FBCF:", mean(residuals(bvar2)[,1]))
cat("\n### Residual mean of LAGR:",mean(residuals(bvar2)[,11]))
cat("\n### Residual mean of LCOM:",mean(residuals(bvar2)[,12]))
cat("\n### Residual mean of LIND:",mean(residuals(bvar2)[,13]))
cat("\n### Residual mean of LINF:",mean(residuals(bvar2)[,14]))

## ----- MCMC Diagnostics ------------------------------------------------------
png(paste0(dir_prefix,"/figs/plot-bvar2-mcmc-trace-density.png"))
plot(bvar2)
dev.off()

png(paste0(dir_prefix,"/figs/plot-bvar2-fbcf-density.png"))
plot(bvar2, type = "dens", vars_response = "FBCF", vars_impulse = "FBCF-lag1")
dev.off()

bvar2_mcmc <- as.mcmc(bvar2, vars = "lambda")
print("")
print(" ## Diagnóstico de convergência GEWEKE: ")
print("")
print(geweke.diag(bvar2_mcmc))

#The resulting scores are best interpreted graphically, 
#using the geweke_plot function. This displays the scores 
#in series, in relation to the 2 standard deviation boundaries 
#around zero. Hence, it is easy to see departures 
#from the standard normal assumption.

png(paste0(dir_prefix,"/figs/plot-bvar2-mcmc-geweke-lambda.png"))
geweke.plot(bvar2_mcmc)
dev.off()

png(paste0(dir_prefix,"/figs/plot-bvar2-mcmc-autocorr-lambda.png"))
# autocorrelation plot for lambda
autocorr.plot(bvar2_mcmc)
dev.off()

## --------------------IRFs and FEVDs MODEL2------------------------------------
cat("\n### Calculando IRFs e FEVDs ...\n")

tic("Tempo para cálculo das IRFs e FEVDs BVAR2 ")
opt_irf2 <- bv_irf(horizon = 36,identification = TRUE, fevd=TRUE)
irf(bvar2) <- BVAR::irf(bvar2,opt_irf,conf_bands=c(0.05,0.10))
toc()


irf_var2 <-bvar2$irf$quants[3,1,,]
max_shocks <- list()
max_shocks[["max"]] <- apply(irf_var2,2,max)
max_shocks[["pos"]] <- apply(irf_var2,2,which.max)

j=1
for(i in colnames(data)){
  
  cat("\n### Valor máximo do choque -", i ," : ", 
      max_shocks$max[j], "- t = ", max_shocks$pos[j])
  j = j+1
}
  

## -----------------------------------------------------------------------------
png(paste0(dir_prefix,"/figs/plot-irf-fbcflagr-1.png"))
plot(BVAR::irf(bvar2), area = TRUE,
     vars_impulse = c("LAGR"), vars_response = c("FBCF"))
dev.off()

png(paste0(dir_prefix,"/figs/plot-irf-fbcflcom-1.png"))
plot(BVAR::irf(bvar2), area = TRUE,
     vars_impulse = c("LCOM"), vars_response = c("FBCF"))
dev.off()

png(paste0(dir_prefix,"/figs/plot-irf-fbcflind-1.png"))
plot(BVAR::irf(bvar2), area = TRUE,
     vars_impulse = c("LIND"), vars_response = c("FBCF"))
dev.off()

png(paste0(dir_prefix,"/figs/plot-irf-fbcflinf-1.png"))
plot(BVAR::irf(bvar2), area = TRUE,
     vars_impulse = c("LINF"), vars_response = c("FBCF"))
dev.off()

bv_ggplot(BVAR::irf(bvar2), vars_impulse = c("LAGR"), vars_response = c("FBCF"))
ggsave(paste0(dir_prefix,"/figs/plot-irf-fbcflagr.png"))

bv_ggplot(BVAR::irf(bvar2), vars_impulse = c("LCOM"), vars_response = c("FBCF"))
ggsave(paste0(dir_prefix,"/figs/plot-irf-fbcflcom.png"))

bv_ggplot(BVAR::irf(bvar2), vars_impulse = c("LIND"), vars_response = c("FBCF"))
ggsave(paste0(dir_prefix,"/figs/plot-irf-fbcflind.png"))

bv_ggplot(BVAR::irf(bvar2), vars_impulse = c("LINF"), vars_response = c("FBCF"))
ggsave(paste0(dir_prefix,"/figs/plot-irf-fbcflinf.png"))


# comparÃ¡veis aos multiplicadores de curto-prazo do modelo ARDL?
# IRF LTOT -> FBCF
irf.lagr.fbcf <- ts(irf_var2[,11]) # 50%, FBCF [1], T[1:30], LTOT[10]
irf.lcom.fbcf <- ts(irf_var2[,12])
irf.lind.fbcf <- ts(irf_var2[,13])
irf.linf.fbcf <- ts(irf_var2[,14])

#acumulando
# comparÃ¡veis aos multiplicadores de longo-prazo do modelo ARDL?

irfsc.linf.fbcf = ts(cumsum(irf.linf.fbcf))
irfsc.lagr.fbcf = ts(cumsum(irf.lagr.fbcf))
irfsc.lind.fbcf = ts(cumsum(irf.lind.fbcf))
irfsc.lcom.fbcf = ts(cumsum(irf.lcom.fbcf))

cat("\n### Valor máximo do choque acumulado em 36 períodos é:") 
cat("\nLAGR:",irfsc.lagr.fbcf[36])
cat("\nLCOM:",irfsc.lcom.fbcf[36])
cat("\nLIND:",irfsc.lind.fbcf[36])
cat("\nLINF:",irfsc.linf.fbcf[36])

png(paste0(dir_prefix,"/figs/plot-bvar2irfcumsum-lagr.png"))
plot(irfsc.lagr.fbcf, main="CUSUM Shock LAGR on FBCF",  xlab = "", ylab="")
dev.off()

png(paste0(dir_prefix,"/figs/plot-bvar2irfcumsum-lcom.png"))
plot(irfsc.lcom.fbcf, main="CUSUM Shock LCOM on FBCF",  xlab = "", ylab="")
dev.off()

png(paste0(dir_prefix,"/figs/plot-bvar2irfcumsum-lind.png"))
plot(irfsc.lind.fbcf, main="CUSUM Shock LIND on FBCF",  xlab = "", ylab="")
dev.off()

png(paste0(dir_prefix,"/figs/plot-bvar2irfcumsum-linf.png"))
plot(irfsc.linf.fbcf, main="CUSUM Shock LINF on FBCF",  xlab = "", ylab="")
dev.off()


fevd.FBCF2 <- as.data.frame(bvar2$irf$fevd$quants[3,1,,])
colnames(fevd.FBCF2) <- colnames(data.var2)
fevd.FBCF2 <- t(fevd.FBCF2)

cat("\n### FEVD após 36 períodos é (%):") 
cat("\nLAGR:",fevd.FBCF2["LAGR",36]*100)
cat("\nLCOM:",fevd.FBCF2["LCOM",36]*100)
cat("\nLIND:",fevd.FBCF2["LIND",36]*100)
cat("\nLINF:",fevd.FBCF2["LINF",36]*100)


png(paste0(dir_prefix,"/figs/plot-bvar2fevd.png"))
barplot(fevd.FBCF2[c("FBCF","LAGR","LCOM","LIND","LINF"),], xlab="", 
        col = gray.colors(5), names.arg=c(1:36),
        main="FEVD - Desembolsos setoriais do BNDES x FBCF") 
legend("topright", legend= c("FBCF","LAGR","LCOM","LIND","LINF"), 
       col = gray.colors(5),pch=20)
dev.off()

## -----------------------------------------------------------------------------
# extrai os coeficientes do modelo bvar e intervalos de confianÃ§a
bvar2_coefs <- augment(bvar2, conf_bands = 0.05)
# extrai os coeficientes da equaÃ§Ã£o para FBCF 
fbcf_coefs.linf <- bvar2_coefs %>% 
  dplyr::filter(variable == "FBCF" & (grepl('LINF-lag1', term) | 
                                        grepl('LINF-lag5', term))) 
fbcf_coefs.lind <- bvar2_coefs %>% 
  dplyr::filter(variable == "FBCF" & (grepl('LIND-lag1', term) | 
                                        grepl('LIND-lag5', term))) 
fbcf_coefs.lagr <- bvar2_coefs %>% 
  dplyr::filter(variable == "FBCF" & (grepl('LAGR-lag1', term)| 
                                        grepl('LAGR-lag5', term))) 
fbcf_coefs.lcom <- bvar2_coefs %>% 
  dplyr::filter(variable == "FBCF" & (grepl('LCOM-lag1', term) | 
                                        grepl('LCOM-lag5', term))) 

#
print("")
print(xtable::xtable(rbind(fbcf_coefs.lagr,fbcf_coefs.lcom,
                     fbcf_coefs.lind,fbcf_coefs.linf), digits = -2))

## --------- Model Forecast Evaluation------------------------------------------
# terei que separar entre treino e teste para avaliar a capacidade preditiva do 
# modelo
data.var2_train <- window(data.var2, end=2018)

data_train <- fred_transform(as.data.frame(data.var2), codes=c(5,5,5,5,5,5,
                                                                       5,5,5,2,5,5,
                                                                       5,5,2,5,5,5,5), 
                                     scale=1)

bvar2_train <- bvar(data_train, lags = p2, n_draw = 25000, n_burn = 5000, n_thin = 10,
              priors = priors2, mh = mh2, verbose = FALSE)

print("")
print(" ## Resumo do modelo BVAR2_train:")
print("")
print(bvar2_train)

y <- predict(bvar2_train, horizon=22)

png(paste0(dir_prefix,"/figs/plot-bvar2-fcast1.png"))
plot(y, area=TRUE, t_back=48, vars=c("FBCF"))
dev.off()

fbcf_test <- tail(data[,1],22)

fbcf_fcast <-y$fcast[1,,1]

png(paste0(dir_prefix,"/figs/plot-bvar2-fcast2.png"))
ts.plot(cbind(fbcf_fcast, fbcf_test),
        gpars=list(xlab="time", ylab="log(FBCF)", lty=c(1:2),
                   col=c("red","blue")))
legend("topleft", bty="n", lty=c(1,1), col=c("red","blue"),
       legend=c(" fcast ", "data "))
dev.off()

rmse2 <- MLmetrics::RMSE(fbcf_fcast, fbcf_test)

cat("\n### RMSE do modelo2:",rmse2)

arima_fbcf <- auto.arima(data.var2_train[,1])
arima_fcast <-forecast::forecast(arima_fbcf,h=23)
arima_fcast <- diff(log(arima_fcast$mean))

png(paste0(dir_prefix,"/figs/plot-bvar2-fcast-ARIMA.png"))
ts.plot(arima_fcast, fbcf_test,
        gpars=list(xlab="time", ylab="log(FBCF)", lty=c(1:2),
                   col=c("red","blue")))
legend("topleft", bty="n", lty=c(1,1), col=c("red","blue"),
       legend=c(" fcast ", "data "))

rmse_AR2 <- MLmetrics::RMSE(arima_fcast, fbcf_test)

cat("\n ### MODELO ARIMA AJUSTADO: ")
print(summary(arima_fbcf))
cat("\n### RMSE do modelo - ARIMA:",rmse_AR2)

graphics.off()

## -----------------------------------------------------------------------------
## ----savedatamodel,include=FALSE----------------------------------------------
end_time <- Sys.time()
cat("\n ##### FINAL DA EXECUÇÃO: ")
print(end_time)
save.image(file = "dados-simul-VAR.RData")
sink(file = NULL)

