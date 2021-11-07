set.seed(42)
setwd("D:/Google Drive/_DOUTORADO/__TESE/_TESE_3rd/artigo1/R code")

sink(file = "bvar_output.txt")

## ---- Load Data --------------------------------------------------------------
load("dados.RData")
#load("dados-model.RData")
#database <- data[[1]]
rm(list=ls()[ls()!="data.m"])

load("dados-uroot.RData")
urres.ln.mth <- data[[1]]


## ---- Funções auxiliares -----------------------------------------------------

prior_mean <- function(data){
  #######################################################################################
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


## ---- Data transformation (log-diff): MODEL1 (FBCF X LTOT)--------------------
data <- fred_transform(as.data.frame(data.var1), codes=c(5,5,5,5,5,5,
                                                         5,5,5,2,5,2,
                                                         5,5,5,5), 
                       scale=1)

## ---- Setting-up the priors --------------------------------------------------
b1 <- prior_mean(data)


mn1 <- bv_minnesota(lambda = bv_lambda(mode = .2, sd = 0.15,
                                       min = 0.0001, max=2.), 
                    var= 1e7, b=b1,
                    alpha = bv_alpha(mode = 1., sd = .5, 
                                     min = 0.001, max = 5.),
                    psi = bv_psi(scale = 0.0005, 
                                 shape = 0.0005, 
                                 mode = "auto"))


priors1 <- bv_priors(hyper = "auto", mn = mn1) 

mh1 <- bv_mh(scale_hess = 0.01,
             adjust_burn = 0.5,
             acc_lower = 0.25,
             acc_upper = 0.50,
             acc_change = 0.01, 
             adjust_acc=TRUE)

p1 <- (vars::VARselect(data, lag.max = 13, type="const"))$selection[2]


## ----- Run Model -------------------------------------------------------------
# non-parallel
bvar1 <- bvar(data, lags = p1, n_draw = 25000, n_burn = 5000, n_thin = 5,
              priors = priors1, mh = mh1, verbose = TRUE)

# parallel
# n_cores <- 8
# cl <- makeCluster(n_cores)
# 
# tic()
# cat("### Calculando BVAR ...\n")
# 
# 
# bvar1p <- par_bvar(cl=cl, data=data, lags = p1, n_draw = 25000, n_burn = 5000, 
#                    n_thin = 5,
#                 priors = priors1, mh = mh1)
# stopCluster(cl)
# toc()

## ----- Residual Diagnostics --------------------------------------------------
png("figs/plot-bvar1-residuals.png")
plot(residuals(bvar1, type = "mean"), vars = c("FBCF", "LTOT"))
dev.off()

cat("\n### Residual mean of FBCF:", mean(residuals(bvar1)[,1]))

cat("\n### Residual mean of LTOT:",mean(residuals(bvar1)[,11]))

## ----- MCMC Diagnostics ------------------------------------------------------
png("figs/plot-bvar1-mcmc-trace-density.png")
plot(bvar1)
dev.off()

png("figs/plot-bvar1-fbcf-density.png")
plot(bvar1, type = "dens", vars_response = "FBCF", vars_impulse = "FBCF-lag1")

bvar1_mcmc <- as.mcmc(bvar1, vars = "lambda")

geweke.diag(bvar1_mcmc)

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
png("figs/plot-bvar1-mcmc-geweke-lambda.png")
geweke.plot(bvar1_mcmc)
dev.off()


# autocorrelation plot for lambda
png("figs/plot-bvar1-mcmc-autocorr-lambda.png")
autocorr.plot(bvar1_mcmc)
dev.off()

## ----- IRFs and  FEVDs -------------------------------------------------------
tic()

cat("### Calculando IRFs e FEVDs ...\n")

opt_irf <- bv_irf(horizon = 36, identification = TRUE, fevd=TRUE)
irf(bvar1) <- BVAR::irf(bvar1,opt_irf,conf_bands=c(0.05,0.10))

toc()

png("figs/plot-irf-fbcfltot.png")
plot(irf(bvar1), area = TRUE,
     vars_impulse = c("LTOT"), vars_response = c("FBCF"))
dev.off()

BVARverse::bv_ggplot(BVAR::irf(bvar1), vars_impulse = c("LTOT"), 
                     vars_response = c("FBCF"))
ggplot2::ggsave("figs/plot-irf-fbcfltot-1.png")

max_shock <- max(bvar1$irf$quants[3,1,,11])
cat("\n### Valor máximo do choque (LTOT x FBCF):", max_shock*100)

fevd.FBCF <- as.data.frame(bvar1$irf$fevd$quants[3,1,,])
colnames(fevd.FBCF) <- colnames(data)
fevd.FBCF <- t(fevd.FBCF)

png("figs/plot-fevdvarltot.png")
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
    max_shockcum*100,"(%)")



## ----IRF acumulada para FBCF devida ao choque de LTOT-------------------------
png("figs/plot-cusumbvarltot.png")
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
xtable::xtable(fbcf_coefs, digits = -2)

## --------- Model Forecast Evaluation------------------------------------------
# terei que separar entre treino e teste para avaliar a capacidade preditiva do 
# modelo
data.var1_test <- window(data.var1, end=2018)

data <- fred_transform(as.data.frame(data.var1_test), codes=c(5,5,5,5,5,5,
                                                         5,5,5,2,5,2,
                                                         5,5,5,5), 
                                                         scale=1) 
bvar1_train <- bvar(data, lags = p1, n_draw = 25000, n_burn = 5000, n_thin = 5,
              priors = priors1, mh = mh1, verbose = TRUE)

 
y <- predict(bvar1_test, horizon=23)
graphics.off()
plot(y, area=TRUE, t_back=12, vars=c("FBCF"))

fbcf_test <-  diff(log(window(data.var1[,1], start=2018)))

fbcf_fcast <-y$fcast[1,,1]

ts.plot(fbcf_fcast, fbcf_test)

## -----------------------------------------------------------------------------


## ---- Data transformation (log-diff): MODEL2 (FBCF X LAGR,LCOM,LIND,LINF) ----
data <- fred_transform(as.data.frame(data.var2), codes=c(5,5,5,5,5,5,
                                                         5,5,5,2,5,5,
                                                         5,5,2,5,5,5,5), 
                       scale=1)

## ---- Setting-up the priors --------------------------------------------------
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

p2 <- (vars::VARselect(data, lag.max = 13, type="const"))$selection[2]

## ----- Run Model2 ------------------------------------------------------------
bvar2 <- bvar(data, lags = p2, n_draw = 25000, n_burn = 5000, n_thin = 5,
              priors = priors2, mh = mh2, verbose = TRUE)

## ----- Residual Diagnostics --------------------------------------------------
png("figs/plot-bvar2-residuals.png")
plot(residuals(bvar2, type = "mean"), vars = c("FBCF", "LAGR", 
                                               "LCOM", "LIND", "LINF"))
dev.off()

cat("\n### Residual mean of FBCF:", mean(residuals(bvar2)[,1]))
cat("\n### Residual mean of LAGR:",mean(residuals(bvar2)[,11]))
cat("\n### Residual mean of LCOM:",mean(residuals(bvar2)[,12]))
cat("\n### Residual mean of LIND:",mean(residuals(bvar2)[,13]))
cat("\n### Residual mean of LINF:",mean(residuals(bvar2)[,14]))

## ----- MCMC Diagnostics ------------------------------------------------------
png("figs/plot-bvar2-mcmc-trace-density.png")
plot(bvar2)
dev.off()

png("figs/plot-bvar2-fbcf-density.png")
plot(bvar2, type = "dens", vars_response = "FBCF", vars_impulse = "FBCF-lag1")
dev.off()

bvar2_mcmc <- as.mcmc(bvar2, vars = "lambda")
geweke.diag(bvar2_mcmc)

#The resulting scores are best interpreted graphically, 
#using the geweke_plot function. This displays the scores 
#in series, in relation to the 2 standard deviation boundaries 
#around zero. Hence, it is easy to see departures 
#from the standard normal assumption.

png("figs/plot-bvar2-mcmc-geweke-lambda.png")
geweke.plot(bvar2_mcmc)
dev.off()

png("figs/plot-bvar2-mcmc-autocorr-lambda.png")
# autocorrelation plot for lambda
autocorr.plot(bvar2_mcmc)
dev.off()

## --------------------IRFs and FEVDs MODEL2------------------------------------
tic()

cat("### Calculando IRFs e FEVDs ...\n")
opt_irf2 <- bv_irf(horizon = 36, identification = TRUE, fevd=TRUE)
irf(bvar2) <- BVAR::irf(bvar2,opt_irf,conf_bands=c(0.05,0.10))

toc()

irf_var2 <-bvar2$irf$quants[3,1,,]
max_shocks <- list()
max_shocks[["max"]] <- apply(irf_var2,2,max)
max_shocks[["pos"]] <- apply(irf_var2,2,which.max)

## -----------------------------------------------------------------------------
png("figs/plot-irf-fbcflagr-1.png")
plot(BVAR::irf(bvar2), area = TRUE,
     vars_impulse = c("LAGR"), vars_response = c("FBCF"))
dev.off()

png("figs/plot-irf-fbcflcom-1.png")
plot(BVAR::irf(bvar2), area = TRUE,
     vars_impulse = c("LCOM"), vars_response = c("FBCF"))
dev.off()

png("figs/plot-irf-fbcflind-1.png")
plot(BVAR::irf(bvar2), area = TRUE,
     vars_impulse = c("LIND"), vars_response = c("FBCF"))
dev.off()

png("figs/plot-irf-fbcflinf-1.png")
plot(BVAR::irf(bvar2), area = TRUE,
     vars_impulse = c("LINF"), vars_response = c("FBCF"))
dev.off()

bv_ggplot(BVAR::irf(bvar2), vars_impulse = c("LAGR"), vars_response = c("FBCF"))
ggsave("figs/plot-irf-fbcflagr.png")

bv_ggplot(BVAR::irf(bvar2), vars_impulse = c("LCOM"), vars_response = c("FBCF"))
ggsave("figs/plot-irf-fbcflcom.png")

bv_ggplot(BVAR::irf(bvar2), vars_impulse = c("LIND"), vars_response = c("FBCF"))
ggsave("figs/plot-irf-fbcflind.png")

bv_ggplot(BVAR::irf(bvar2), vars_impulse = c("LINF"), vars_response = c("FBCF"))
ggsave("figs/plot-irf-fbcflinf.png")


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


png("figs/plot-bvar2irfcumsum-lagr.png")
plot(irfsc.lagr.fbcf, main="CUSUM Shock LAGR on FBCF",  xlab = "", ylab="")
dev.off()

png("figs/plot-bvar2irfcumsum-lcom.png")
plot(irfsc.lcom.fbcf, main="CUSUM Shock LCOM on FBCF",  xlab = "", ylab="")
dev.off()

png("figs/plot-bvar2irfcumsum-lind.png")
plot(irfsc.lind.fbcf, main="CUSUM Shock LIND on FBCF",  xlab = "", ylab="")
dev.off()

png("figs/plot-bvar2irfcumsum-linf.png")
plot(irfsc.linf.fbcf, main="CUSUM Shock LINF on FBCF",  xlab = "", ylab="")
dev.off()


fevd.FBCF2 <- as.data.frame(bvar2$irf$fevd$quants[3,1,,])
colnames(fevd.FBCF2) <- colnames(data.var2)
fevd.FBCF2 <- t(fevd.FBCF2)

png("figs/plot-bvar2fevd.png")
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
xtable::xtable(rbind(fbcf_coefs.lagr,fbcf_coefs.lcom,
                     fbcf_coefs.lind,fbcf_coefs.linf), digits = -2)




## ----savedatamodel,include=FALSE----------------------------------------------
save.image(file = "dados-simul-VAR.RData")
sink(file = NULL)

