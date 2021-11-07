rm(list = ls())

#######################################################################################
## ----Fun��es Auxiliares ----------------
#######################################################################################


#' ############################################################################
#' @description Realiza as simula��es
#' 
simulate <- function(){
  
  # checa se os dados não existem 
  if(file.exists( here("dados-simul-ARDL.RData"))==TRUE){
    return(as.logical(FALSE))
  }else{
    load("dados-model.RData")
    
  }
  
 
}

#######################################################################################
make_formula <- function(data, depvar){
  ind <- which(colnames(data) == depvar )
  x <- colnames(data)[-ind]
  Formula <- formula(paste0(depvar,paste0(" ~ ", paste(x, collapse=" + "))))
  
  return(Formula)
}

####################################################################################### 
eval_shocks <- function(formula, data, lvls=NULL,lgs=NULL, lgs.d=NULL, d=NULL, shval=NULL, constant=NULL, trend=NULL){

  set.seed(020990)
  simul.shocks <- list()
  
  x <- as.list(colnames(data)[2:ncol(data)])
  
  data = as_tibble(data)
  
  for (i in 1:length(x)){
    
    simul <- dynardl(formula,data = data, levels = lvls, lags = lgs,lagdiffs = lgs.d,
                     diffs = d,ec = TRUE, simulate = TRUE,fullsims = TRUE, time=0,
                     shockvar =  paste0(x[[i]],colapse=""), range = 36,
                     constant = TRUE, trend = TRUE, qoi = "median",)
    
    simul.shocks[[x[[i]]]] <- simul
    
  }
  
  
  return(simul.shocks)
  
}

#######################################################################################
plot_shocks <- function(model.shocks,start=0, depvar, all.plots = FALSE, sav.dir = NULL){
  x <- as.list(colnames(data)[1:ncol(data)])
  
  plts <- list()
  
  for(i in 1:length(model.shocks)){
    
    if (all.plots == TRUE){
      
      dynardl.all.plots(model.shocks[[i]], main = paste(names(model.shocks)[i],"==>",depvar))
      
      dev.print(pdf,        # copies the plot to a the PDF file
          paste0(sav.dir,"/",names(model.shocks)[i],"-",depvar,".pdf") )
      
    }
    
    else{
      
      
      par(mfrow = c(2, 1))
    
    
      
      dynardl.simulation.plot(model.shocks[[i]], bw=TRUE, 
                              response = "diffs", 
                              start.period = start, 
                              ylab = paste(names(model.shocks)[i],"->",depvar),
                                            main=paste("Varia��o de ", depvar))
      textplot("(a)", halign = "center")

      dynardl.simulation.plot(model.shocks[[i]], bw=TRUE, 
                              type = "area",
                              response = "cumulative.diffs", 
                              ylab = paste(names(model.shocks)[i],"->",depvar),
                              main=paste("Varia��o acumulada de ", depvar))
      
      
      
    }
    
  }
  
  
}
#######################################################################################
plot_each_shock <- function(model.shocks,start=0, indepvar, depvar, all.plots=FALSE){
  if (all.plots == TRUE){
    dynardl.all.plots(model.shocks[[indepvar]], main = paste(indepvar,"==>",depvar),start.period = start)
  }
  else{
    
   
    
    par(mfrow = c(2, 1))
    par(mar = c(4, 4, 1.2, 1.2))

    
    
    dynardl.simulation.plot(model.shocks[[indepvar]], bw=TRUE, fullsims= TRUE,
                            response = "diffs", 
                            start.period = start, 
                            ylab = paste(indepvar,"->",depvar),
                            main=paste("Varia��o de ", depvar))
    title(sub = "sub")    

    dynardl.simulation.plot(model.shocks[[indepvar]], bw=TRUE, fullsims= TRUE,
                            type = "area",
                            response = "cumulative.diffs", ylab = paste(indepvar,"->",depvar),
                            main=paste("Varia��o acumulada de ", depvar))
    
  


  }
  
  
}
#######################################################################################
print_output <- function(output, cex = 0.7) {
  tmp <- capture.output(output)
  plot.new()
  text(0, 1, paste(tmp, collapse='\n'), adj = c(0,1), family = 'mono', cex = cex)
  box()
}

#######################################################################################

omit_coefs<- function(coefs,type = c("sr","lr")){
  
  coefs <- names(coefs)
  #sr.coefs <- c(".1",".2",".3",".4",".5",".6",".7",".8",".9")
  omit.coefs <- c("d",".1",".2",".3",".4",".5",".6",".7",".8",".9")
  
  
  omit <- c()
  
  for(name in coefs ){
    
    
    # se o modelo for long-run (ARDL)
    if (type=="lr"){
      pattern <- substr(name,1,stop = 1)
      detected <- stri_detect_fixed(omit.coefs,pattern)
        
      if(any(detected)){
            omit <- c(omit,name)
          }
    } 
    
    # se o modelo for short-run (ECM)
    if(type=="sr"){
      
      # detecta lags de ordem maior que 1
      pattern <- substr(name,nchar(name)-1,stop = nchar(name))
      detected <- stri_detect_fixed(omit.coefs,pattern)
      
      if(any(detected) && name != "ec.1"){
        
        prefix <- substr(name,1,stop = 1)
        detected <- stri_detect_fixed(omit.coefs,prefix)
        if(any(detected)){
            omit <- c(omit,name)
          }      
        
      }  
      
    }
    
  }  
    
    
  return(omit)
}



## Carrega a base de dados

load("dados-model.RData")
database <- data[[1]]
write.csv(database, "dados/database.ln.csv")


#########################################################################
## ---- MODELO 3 : USANDO  DESEMBOLSOS TOTAIS BASE MENSAL --------------
#########################################################################


##### Dados em log
# exclui os desembolsos por setor econ�mico
# model3.ln.data <- window(database[,c("FBCF","CIB","CURI","EMBI","ER","IBOV",
#                               "ICE","ICI","IIE","IR",
#                               "LTOT","NFSP","PIB","POUP","TT","WP")], end = 2016)
model3.ln.data <- database[,c("FBCF","CIB","CURI","EMBI","ER","IBOV",
                              "ICE","ICI","IIE","IR",
                              "LTOT","NFSP","PIB","POUP","TT","WP")]
f3.ln <- make_formula(model3.ln.data,"FBCF")

data <- model3.ln.data

# escreve dados em arquivo
write.csv(data, "dados/model3.ln.csv")


## ----selectmodel3ln------------------------------------------------------------------------------------------------
model3.ln <- auto_ardl(f3.ln, data = model3.ln.data, selection = "BIC", max_order = 6)
f3.ln <- formula(paste0(f3.ln,"+trend(FBCF)"))
order <- model3.ln$best_order

kable(model3.ln$top_orders[1:5,], caption = paste0("ARDL - Coeficientes p e q dos 5 melhores modelos de acordo com a sele��o autom�tica."),align = "l",longtable = F,booktabs = T) %>% kable_styling(latex_options = c("hold_position"))%>%kable_classic(full_width = T, html_font = "Cambria")

best_order1 <- order


## ----tab:lagselectionmodel3ln--------------------------------------------------------------------------------------
ardl_order <- paste0("(",glue_collapse(best_order1, ", "),")")
print(ardl_order)


## ---- summodel3ln ----------------------------------
#escolhe o modelo em segundo lugar
order <- best_order1#model3.ln$top_orders[2,1:ncol(best_order)-1]

model3.ln <- ardl(f3.ln, data = model3.ln.data, order = order)

# --- Modelo ECM Irrestrito --------
uecm_model3.ln <- uecm(model3.ln)
summary(uecm_model3.ln)

## ------------------------------------------------------------------------------------------------------------------
# The model specification of the ardl_3132 model can be created as easy as order=c(3,1,3,2)
# or else, it could be done using the dynlm package as:

p <- list("CIB"=0,"CURI"=0,"EMBI"=0,"ER"=0,"IBOV"=0,
          "ICE"=0,"ICI"=0,"IIE"=0,"IR"=0,
          "LTOT"=0,"NFSP"=0,"PIB"=3,"POUP"=3,"TT"=0,"WP"=1)
q <- 3

#p <- list("PIB"=3,"POUP"=3,"WP"=1)

p <- data.frame(q, p)+1


ardlBound.model3ln_diag<- ardlBound(data = as.data.frame(data), 
                                case = 4, formula = f3.ln, p=p, 
                                ECM=TRUE, stability = FALSE,
                                remove = list("CIB"=1,"CURI"=1,"EMBI"=1,"ER"=1,"IBOV"=1,
                                "ICE"=1,"ICI"=1,"IIE"=1,"IR"=1,
                                "LTOT"=1,"NFSP"=1,"TT"=1)
                                  
                                  )


# resultado igual ao modelo do Eviews
model3_ardl <- dynlm(model3.ln$full_formula, data = model3.ln.data)
summary(model3_ardl)

## ---- ARDL --------------
lags.ardl <- list("FBCF"=c(1:3),"PIB"=c(1:3),"POUP"=c(1:3),"WP"=c(1))
levels.ardl <-  colnames(model3.ln.data)[-1]

### ECM
#lagdiffs.ecm <- list("FBCF"=c(1:2),"LTOT"=c(1))
#diffs.ecm <- c("CIB","LTOT","PIB","POUP")

### ECM - Case IV - FBCF ~ IIE + LTOT + PIB + POUP 
model3.ln.ardl <- dynardl(formula=f3.ln, data = as.data.frame(data),
                          levels = levels.ardl,
                          lags = lags.ardl,
                          lagdiffs = NULL,
                          diffs = NULL,
                          ec = FALSE, simulate = FALSE,constant = TRUE, trend = TRUE)


# Resultados semelhantes ao do obtido com o Eviews
summary(model3.ln.ardl)

# 
# A taxa de juros (IR) possui impacto negativo, com a FBCF diminuindo 0,0897% a cada 1% de aumento em IR. Já CURI também impacta negativamente a FBCF, pois a cada 1% de aumento naquela, temos uma redução de aproximadamente 4,3% na última. 


## ----model3lnecm-dynardl-2-----------------------------------------------------------------------------------------
##############################################
# Estimativa do modelo em log usando o pacote ARDL dynamac
# Seguindo o fluxograma de Phillips (2018) - Figura 1

### ECM
lagdiffs.ecm <- list("FBCF"=c(1:2),"PIB"=c(1:2),"POUP"=c(1:2))
diffs.ecm <- c("PIB","POUP","WP")
levels.ecm <-  c("CIB","CURI","EMBI","ER","IBOV","ICE","ICI","IIE",
                 "IR","LTOT","NFSP","TT")
lags.ecm <- list("PIB"=1,"POUP"=1,"WP"=1)

### ECM - Case IV - FBCF ~ IIE + LTOT + PIB + POUP 
model3.ln.ecm <- dynardl(formula=f3.ln, data = as.data.frame(data),
                          levels = levels.ecm,
                          lags = lags.ecm,
                          lagdiffs = lagdiffs.ecm,
                          diffs = diffs.ecm,
                          ec = TRUE, simulate = FALSE,constant = TRUE, trend = TRUE)


# Resultados semelhantes ao do obtido com o Eviews
summary(model3.ln.ecm)

# 
# A taxa de juros (IR) possui impacto negativo, com a FBCF diminuindo 0,0897% a cada 1% de aumento em IR. Já CURI também impacta negativamente a FBCF, pois a cada 1% de aumento naquela, temos uma redução de aproximadamente 4,3% na última. 


## ------------------------------------------------------------------------------------------------------------------
dynardl.auto.correlated(model3.ln.ecm)


## ------------------------------------------------------------------------------------------------------------------
dynamac::pssbounds(model3.ln.ecm, restriction=TRUE)


## ------------------------------------------------------------------------------------------------------------------
bbf <- bounds_f_test(model3.ln, case=4, alpha = 0.05)

bbf$tab


## ------------------------------------------------------------------------------------------------------------------
# The t-bounds test applies only when 'case' is either 1, 3 or 5
#bbt <- bounds_t_test(model3.ln, case=3, alpha = 0.05)

#bbt$tab


## ----model3lnecm-irf, include=FALSE--------------------------------------------------------------------------------
##############################################
# Estimativa do modelo em log usando o pacote ARDL dynamac
# Seguindo o fluxograma de Phillips (2018) - Figura 1

### ECM
# lagdiffs.ecm <- list("FBCF"=c(1:2),"LTOT"=c(1))
# diffs.ecm <- c("ER","WP","LTOT")
# levels.ecm <-  c("CIB","CURI","EMBI","IBOV","ICE","ICI","IIE","IR","NFSP","POUP","TT")
# lags.ecm <- list("ER"=1,"LTOT"=1,"WP"=1)

model3.ln.ecm.shocks <- list()
model3.ln.ecm.shocks <- eval_shocks(formula=f3.ln,data=data,lvls = levels.ecm,
                                    lgs = lags.ecm, 
                                     lgs.d = lagdiffs.ecm, d = diffs.ecm)



## ----shmodel3lnm-LTOT, warning= FALSE, echo=FALSE, fig.cap="IRF LTOT x FBCF"---------------------------------------
plot_each_shock(model3.ln.ecm.shocks, indep = "LTOT", depvar = "FBCF")



## ------------------------------------------------------------------------------------------------------------------


## ----fig:ardlBoundmodel1pib_fit, fig.caption="Teste de flutuação empírica"-----------------------------------------
library(dLagM)

# orders2 <- ardlBoundOrders(data = as.data.frame(model3.ln.data), formula = f3.ln, ic = "BIC", max.p = 2, max.q = 3)

p1 <- data.frame(0,0,0,1,0,0,0,0,0,2,0,0,0,1)
colnames(p1) <- colnames(model3.ln.data)[-1]

p <- data.frame(FBCF=3,p1)+1

ardlBound.model3pib_fit<-ardlBound(data = as.data.frame(model3.ln.data), case = 4, 
                                   formula = model3.ln$full_formula, p = p, ECM = TRUE, stability = FALSE)


## ----model3ln.stability--------------------------------------------------------------------------------------------
# A geração dos gráficos de estabilidade para o caso 4 não funciona, então é necessário gerar a 
# análise de resíduos manualmente
ecm.test <- model3.ln$full_formula
data.test <- model3.ln.data
diff.data <- diff(model3.ln.data)

data.test <- ts.intersect(model3.ln.data, diff.data, trend(seas(model3.ln.data[,"FBCF"])))

colnames(data.test) <- c("FBCF","CURI","EMBI","ER","IBOV","ICE","ICI","IIE","IR",
                              "LTOT","NFSP","PIB","POUP","TT","WP",
                         "dFBCF","dCURI","dEMBI","dER","dIBOV","dICE","dICI","dIIE","dIR",
                              "dLTOT","dNFSP","dPIB","dPOUP","dTT","dWP","trend")


reccus <- efp(ecm.test, type="Rec-CUSUM", data=data.test)
recmos <- efp(ecm.test, type="Rec-MOSUM", data=data.test)

#boundary(ocus, alpha=0.05)

#plot(ocus)

#resid <- recresid(ecm.test,data=data.test)

#plot(cumsum(resid), type = "l")

plot(reccus)
plot(recmos)




## ------------------------------------------------------------------------------------------------------------------
mult <-multipliers(uecm_model3.ln, type="lr")
pander(mult)
#xtable::xtable(mult)



## ----setmodel4aln, include=FALSE-----------------------------------------------------------------------------------
############################################################################################
###### MODELO 4 : USANDO  DESEMBOLSOS por setor BASE MENSAL
############################################################################################



###################################################################
# MODELO MODIFICADO APÓS A RETIRADA DE UMA DAS VARIÁVEIS I(0) - ICI

# exclui os desembolsos totais
model4a.ln.data <- database[,-c(15)]

f4a.ln <- make_formula(model4a.ln.data,"FBCF")
data <- model4a.ln.data

write.csv(data, "dados/model4ln.csv")


## ----selectmodel4aln, include=FALSE--------------------------------------------------------------------------------
# modelo em que as variáveis são todas I(1)
model4a.ln <- auto_ardl(f4a.ln, data = data, selection = "BIC", max_order = 6 )

# caso 4 de PSS - inclui a tendência da variável dependente
f4a.ln <- formula(paste0(f4a.ln,"+trend(FBCF)"))

order <- model4a.ln$best_order 

best_order2 <- order


## ----tab:model4alndatalagselection, echo=FALSE, warning=FALSE------------------------------------------------------
ardl_order <- paste0("(",glue_collapse(best_order2, ", "),")")
set.caption("(\\#tab:model4alndatalagselection)ARDL - Coeficientes p e q dos 5 melhores modelos de acordo com a seleção automática")
panderOptions('table.style','grid')
pander(model4a.ln$top_orders[1:3,])



## ----ardlmodel4aln, echo=FALSE, warning=FALSE----------------------------------------------------------------------
order <- best_order2
# order[1] <- order[1]+1 
# order[which(order==0)] <- 1

model4a.ln <- ardl(f4a.ln, data = data, order = order)


summary(model4a.ln)

### Modelo ECM Irrestrito
uecm_model4a.ln <- uecm(model4a.ln)

summary(uecm_model4a.ln)
### Modelo ECM Restrito (somente se ARDL(p,qi) qi > 0)
#recm_model4a.ln <- recm(uecm_model4a.ln, case = 4)



## ------------------------------------------------------------------------------------------------------------------
# The model specification of the ardl_3132 model can be created as easy as order=c(3,1,3,2)
# or else, it could be done using the dynlm package as:
library(dynlm)
# resultado igual ao modelo do Eviews
model4_ardl <- dynlm(model4a.ln$full_formula, data = model4a.ln.data)
summary(model4_ardl)
#resid <- m$residuals


## ----model4lnecm-dynardl, include=FALSE----------------------------------------------------------------------------
##############################################
# Estimativa do modelo em log usando o pacote ARDL dynamac
# Seguindo o fluxograma de Phillips (2018) - Figura 1

# ARDL(3, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 3, 3, 2, 1)
# FBCF | CURI | EMBI | ER | IBOV | ICE | ICI | IIE | IR | LAGR | LCOM |
#+======+======+======+====+======+=====+=====+=====+====+======+======+
#|  3   |  0   |  1   | 1  |  0   |  0  |  0  |  0  | 1  |  0   |  0   |
#LIND | LINF | NFSP | PIB | POUP | TT | WP |  BIC  |
#+======+======+======+=====+======+====+====+=======+
#|  0   |  0   |  0   |  3  |  3   | 2  | 1  | -1907 |

### ARDL
lags.ardl <- list("FBCF"=c(1:3),"CIB"=c(1),"ER"=c(1),"ICE"=c(1),"ICI"=c(1:2),"LINF"=c(1:3))
levels.ardl <-  colnames(model4a.ln.data)[-1]

### ECM - Case IV - FBCF ~ IIE + LTOT + PIB + POUP 
model4.ln.ardl <- dynardl(formula=f4a.ln, data = as.data.frame(data),
                          levels = levels.ardl,
                          lags = lags.ardl,
                          lagdiffs = NULL,
                          diffs = NULL,
                          ec = FALSE, simulate = FALSE,constant = TRUE, trend = TRUE)


# Resultados semelhantes ao do obtido com o Eviews
summary(model4.ln.ardl)



## ----model3lnecm-dynardl, include=FALSE----------------------------------------------------------------------------
##############################################
# Estimativa do modelo em log usando o pacote ARDL dynamac
# Seguindo o fluxograma de Phillips (2018) - Figura 1

# ARDL(3, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 3, 3, 2, 1)
# FBCF | CURI | EMBI | ER | IBOV | ICE | ICI | IIE | IR | LAGR | LCOM |
#+======+======+======+====+======+=====+=====+=====+====+======+======+
#|  3   |  0   |  1   | 1  |  0   |  0  |  0  |  0  | 1  |  0   |  0   |
#LIND | LINF | NFSP | PIB | POUP | TT | WP |  BIC  |
#+======+======+======+=====+======+====+====+=======+
#|  0   |  0   |  0   |  3  |  3   | 2  | 1  | -1907 |



### ECM
lagdiffs.ecm <- list("FBCF"=c(1:2),"ICI"=c(1),"LINF"=c(1:2))
diffs.ecm <- c("CIB","ER","ICE","ICI","LINF")
levels.ecm <-  c("CURI","EMBI","IBOV","IIE","IR","LAGR","LCOM",
                 "LIND","NFSP","POUP","TT","WP")
lags.ecm <- list("CIB"=1,"ER"=1,"ICE"=1,"ICI"=1,"LINF"=1)

### ECM - Case IV - FBCF ~ IIE + LTOT + PIB + POUP 
model4.ln.ecm <- dynardl(formula=f4a.ln, data = as.data.frame(data),
                          levels = levels.ecm,
                          lags = lags.ecm,
                          lagdiffs = lagdiffs.ecm,
                          diffs = diffs.ecm,
                          ec = TRUE, simulate = FALSE,constant = TRUE, trend = TRUE)


# Resultados semelhantes ao do obtido com o Eviews
summary(model4.ln.ecm)

# 
# A taxa de juros (IR) possui impacto negativo, com a FBCF diminuindo 0,0897% a cada 1% de aumento em IR. Já CURI também impacta negativamente a FBCF, pois a cada 1% de aumento naquela, temos uma redução de aproximadamente 4,3% na última. 


## ------------------------------------------------------------------------------------------------------------------
dynardl.auto.correlated(model4.ln.ecm)


## ------------------------------------------------------------------------------------------------------------------
dynamac::pssbounds(model4.ln.ecm, restriction=TRUE)


## ------------------------------------------------------------------------------------------------------------------
bbf <- bounds_f_test(model4a.ln, case=4, alpha = 0.05)

bbf$tab


## ------------------------------------------------------------------------------------------------------------------
# The t-bounds test applies only when 'case' is either 1, 3 or 5
#bbt <- bounds_t_test(model3.ln, case=3, alpha = 0.05)

#bbt$tab


## ----model4lnecm-irf, include=FALSE--------------------------------------------------------------------------------
##############################################
# Estimativa do modelo em log usando o pacote ARDL dynamac
# Seguindo o fluxograma de Phillips (2018) - Figura 1

model4.ln.ecm.shocks <- list()
model4.ln.ecm.shocks <- eval_shocks(formula=f4a.ln,data=data,lvls = levels.ecm,
                                    lgs = lags.ecm, 
                                     lgs.d = lagdiffs.ecm, d = diffs.ecm)



## ----shmodel4lnm-LAGR, warning= FALSE, echo=FALSE, fig.cap="IRF LAGR x FBCF"---------------------------------------
png("figs/plot-shmodel4lnm-AGR.png")
plot_each_shock(model4.ln.ecm.shocks, indep = "LAGR", depvar = "FBCF")
dev.off()


## ----shmodel4lnm-LCOM, warning= FALSE, echo=FALSE, fig.cap="IRF LCOM x FBCF"---------------------------------------
png("figs/plot-shmodel4lnm-LCOM.png")
plot_each_shock(model4.ln.ecm.shocks, indep = "LCOM", depvar = "FBCF")
dev.off()



## ----shmodel4lnm-LIND, warning= FALSE, echo=FALSE, fig.cap="IRF LIND x FBCF"---------------------------------------
png("figs/plot-shmodel4lnm-LIND.png")
plot_each_shock(model4.ln.ecm.shocks, indep = "LIND", depvar = "FBCF")
dev.off()


## ----shmodel4lnm-LINF, warning= FALSE, echo=FALSE, fig.cap="IRF LINF x FBCF"---------------------------------------
png("figs/plot-shmodel4lnm-LINF.png")
plot_each_shock(model4.ln.ecm.shocks, indep = "LINF", depvar = "FBCF")
dev.off()


## ------------------------------------------------------------------------------------------------------------------
mult <-multipliers(uecm_model4a.ln, type="lr")

xtable::xtable(mult)


## ----prevmodel4aln,message=FALSE, warning=FALSE,render = 'knit_print', fig.cap=paste0("(#fig:prevmodel4aln)Previsão x dados reais-", caption)----
### Gráfico das previsões do modelo
#  graphically check the estimated long-run relationship (cointegrating equation) against the dependent variable FBCF
ce.model4a.ln <- coint_eq(uecm_model4a.ln, case = 4)



model4a.prev <- cbind.zoo(FBCF = model4a.ln.data[,1], ce.model4a.ln)
model4a.prev <- xts(model4a.prev)

# make the plot
#plot(model4a.prev, legend.loc = "right", main="Previsão x Dados")
# make the plot
plot(model4a.prev, main="FBCF", 
     col=c("black","gray"), lty=c(1,2))
addLegend("topleft", col=c("black","gray"), 
          lty=c(1,2), lwd=c(2, 2), legend.names = c("Dados", "Previsão"))


## ----savedata,include=FALSE----------------------------------------------------------------------------------------

save.image("dados-simul-ARDL.RData")


