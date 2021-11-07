## ----warning=FALSE, include=FALSE----------------------------------------------------------------------------------
# library(thesisdown)
# 
# #bibliotecas para obtenção dos dados
# library(ipeadatar)
# library(rbcb)
# library(sidrar)
# library(quantmod)
# 
# 
# # biblioteca para leitura de dados
# library(here)
# library(readxl)
# library(stringr)
# 
# # bibliotecas para análise séries temporais
# library(urca)
# library(mFilter)
# library(tseries)
# library(forecast)
# library(seasonal)
# library(tempdisagg)
# library(aod)
# library(xts)
# library(zoo) # for cbind.zoo
# library(lubridate)
# library(fUnitRoots)
# 
# # bibliotecas de formatação das saídas
# library(knitr);
# library(corrplot)
# library(kableExtra)
# library(ggplot2)
# library(glue)
# library(gtools)
# library(stringi)
# library(pander)
# library(gridExtra)
# library(grid)
# library(R.utils)
# library(xtable)
# 
# # bibliotecas para manipulação dos dados
# library(tidyr);
# library(dplyr);
# library(tsibble)
# library(tidyverse)
# 
# # biblioteca para modelagem VAR Bayesiano e ARDL
# library(BVAR)
# library(vars)
# library(ARDL)
# library(dynamac)
# library(dLagM)
# 
# # bibliotecas para análise de performance dos modelos
# library(MLmetrics)
# 
# options("getSymbols.warning4.0"=FALSE)


## ----uroot-func, include=FALSE-------------------------------------------------------------------------------------

#############################################
# TESTE DE RAIZ UNITÁRIA PARA VERIFICAR SE SÉRIES SÃO I(0) OU I(1)
#############################################

# função para realizar os testes de raiz unitária ADF, PP, KPSS, ADF-GLS - 5% de significância
uroot <- function(data, sig = 5, cval=FALSE){
  # data - objeto do tipo série temporal
  # sig = 5 (5% de significância); sig = 10 (10% de significância)
  
  # elimina dados do tipo NA para que se possa realizar os testes
  if(anyNA(data)){
    
    data = na.remove(data)
  }
  
  
  #máximo número de lags para o teste ADF (short)
  p_max <-as.integer(4*(length(data)/100)^(1/4)) 
  
  
  names <- colnames(data)
  n <- ncol(data)
  
  res <- list()
  
  type <- list()
  
  # seleciona o nível de significância
  if (sig == 5){
    sig <- 2
    
  }
  else if(sig==10){
    sig <- 3
  } 
  else{
    
    cat("** Utilizando 5% de significância. **")
    sig <- 2
  }
  
  
  #cat("\n******************************************\n")
  for(i in 1:n){
    
    ###########################################################################
    adf <- ur.df(data[,i],type=c("trend"), lags=p_max, selectlags = c("AIC"))
    
    #cat(" *** Teste ADF para a série: ", names[i],  "\n")
    
    if (abs(adf@cval[1,sig]) > abs(adf@teststat[1])){
      #cat(" A série: ", names[i]," possui raiz unitária.", "\n")
      type[["adf"]] <- "NS"
      
    }
    else {
      
      #cat(" A série: ", names[i]," não possui raiz unitária.", "\n")
      type[["adf"]] <- "S"
    }
    
    ###########################################################################
    kpss <- ur.kpss(data[,i], type="tau", lags = "short")
    
    #cat(" *** Teste KPSS para a série: ", names[i],"\n")
    
    if (kpss@teststat > kpss@cval[1,sig]){
      #cat(" A série: ", names[i]," possui raiz unitária.", "\n")
      type[["kpss"]] <- "NS"
      
    }
    else {
      
      #cat(" A série: ", names[i]," não possui raiz unitária.", "\n")
      type[["kpss"]] <- "S"
    }
    
    ###########################################################################
    pp <- ur.pp(data[,i], type="Z-tau",model = c("trend"), lags = "short")
    
    #cat(" *** Teste Phillips-Perron para a série: ", names[i],"\n")
    
    if (abs(pp@cval[1,sig]) > abs(pp@teststat)){
      #cat(" A série: ", names[i]," possui raiz unitária.", "\n")
      type[["pp"]] <- "NS"
    }
    else {
      
      #cat(" A série: ", names[i]," não possui raiz unitária.", "\n")
      type[["pp"]] <- "S"
    }
    
    ###########################################################################
    dfgls <- ur.ers(data[,i], type="DF-GLS", model="trend", lag.max = p_max)
    
    #cat(" *** Teste ADF-GLS para a série: ", names[i],"\n")
    
    if (abs(dfgls@cval[1,sig]) > abs(dfgls@teststat)){
      #cat(" A série: ", names[i]," possui raiz unitária.", "\n")
      type[["dfgls"]] <- "NS"
      
    }
    else {
      
      #cat(" A série: ", names[i]," não possui raiz unitária.", "\n")
      type[["dfgls"]] <- "S"
    }
    
    ###########################################################################
    za <- ur.za(data[,i], model="trend", lag = p_max)
    
    #cat(" *** Teste Zivot-Andrews para a série: ", names[i],"\n")
    
    if (abs(za@cval[sig]) > abs(za@teststat)){
      #cat(" A série: ", names[i]," possui raiz unitária.", "\n")
      type[["za"]] <- "NS"
    }
    else {
      
      #cat(" A série: ", names[i]," não possui raiz unitária.", "\n")
      type[["za"]] <- "S"
    }
    
    ###########################################################################

    if (cval == TRUE){
      
      res[[i]] <- cbind(round(adf@teststat[1],2),adf@cval[1,sig],round(kpss@teststat,2),
                        kpss@cval[1,sig],round(pp@teststat,2),pp@cval[1,sig],
                        round(dfgls@teststat,2),dfgls@cval[1,sig],
                        round(za@teststat,2),za@cval[sig])
      cols <- c("ADF-stat","ADF-cval","KPSS-stat","KPSS-cval",
                "PP-stat","PP-cval","ADF/GLS-stat","ADF/GLS-cval","Zivot-Andrews-stat","Zivot-Andrews-cval")
      
    } else{
      
      res[[i]] <- cbind(round(adf@teststat[1],2),type[["adf"]],round(kpss@teststat,2),
                        type[["kpss"]],round(pp@teststat,2),type[["pp"]],
                        round(dfgls@teststat,2),type[["dfgls"]],
                        round(za@teststat,2),type[["za"]])
      
      cols <- c("ADF-stat","ADF-S/NS","KPSS-stat","KPSS-S/NS",
                "PP-stat","PP-S/NS","ADF/GLS-stat","ADF/GLS-S/NS","Zivot-Andrews-stat","Zivot-Andrews-S/NS")
      
    }

  }  
  
  rows <- colnames(data)

  ur.tbl <- data.frame(matrix(unlist(res), nrow = length(res), byrow = T))

  colnames(ur.tbl) <- cols
  rownames(ur.tbl) <- rows
  
  
  return(ur.tbl)
}



## ----uroot-calc, warning=FALSE, include=FALSE----------------------------------------------------------------------
#library(readr)
#database <- read_csv("D:/Google Drive/_DOUTORADO/__TESE/artigo 1/coding/R/database.csv")
#database <- as.data.frame(database)
#database <- database[,-1]
load("dados-model.RData")
database <- data[[1]]

# Cáculo de raiz unitária
# nível de significância para os testes
sig = 5

urres.ln.mth <- uroot(database)
urres.dln.mth <- uroot(diff(database))


## ----tab:table-ur-ln-m, results='asis'-----------------------------------------------------------------------------

# knitr::kable(urres.ln.mth, caption = paste0("Testes de Raiz Unitária nas Séries Mensais em log."),align = "l",longtable = F,booktabs = T) %>% kable_styling(position="center", latex_options = c("hold_position","scale_down"), font_size = 9)%>%
#   kable_classic(full_width = T, html_font = "Cambria")

# %>% add_footnote(c("S/NS - estacionária/não-estacionária",paste("Nível de significância:",sig,"%"),"Testes com drift."), notation = "symbol") 
#set.caption("(#tab:table-ur-ln-m)Testes de Raiz Unitária nas Séries Mensais em log.")
#panderOptions('table.style','grid')
#pander(urres.ln.mth)
#pandoc.footnote(c("+S/NS - estacionária/não-estacionária",paste("++Nível de significância:",sig,"%"),"Testes com drift."))

#xtable::xtable(jo_res)

ur_tbl <- xtable(urres.ln.mth, auto = TRUE,
                    caption = "Testes de Raiz Unitária.",
                    label = "tab:table-ur-ln-m",digits = 2)

print(ur_tbl, type="latex", file="tables/ur_tbl.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), booktabs=TRUE)

knitr::kable(urres.ln.mth)


## ----tab:table-ur-dln-m, results='asis'----------------------------------------------------------------------------

# knitr::kable(urres.dln.mth, caption = paste0("Testes de Raiz Unitária nas Séries Mensais em log - primeira diferença"),align = "l",longtable = F,booktabs = T) %>% kable_styling(position="center",latex_options = c("hold_position","scale_down"), font_size = 9)%>%
# kable_classic(full_width = T, html_font = "Cambria")%>% add_footnote(c("S/NS - estacionária/não-estacionária",paste("Nível de significância:",sig,"%"),"Testes com drift."), notation = "symbol")
# set.caption("(#tab:table-ur-dln-m)Testes de Raiz Unitária nas Séries Mensais em log - primeira diferença")
# panderOptions('table.style','grid')
# pander(urres.dln.mth)
# pandoc.footnote(c("+S/NS - estacionária/não-estacionária",paste("++Nível de significância:",sig,"%"),"Testes com drift."))


ur_diff_tbl <- xtable(urres.dln.mth, auto = TRUE,
                    caption = "Testes de Raiz Unit\\'{a}ria - primeira diferen\\c{c}a.",label = "tab:table-ur-dln-m",
				digits = 2)

print(ur_diff_tbl, type="latex", file="tables/ur_diff_tbl_diff.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), booktabs=TRUE)

knitr::kable(urres.dln.mth)


## ------------------------------------------------------------------------------------------------------------------
data <- list(urres.ln.mth)
save(data, file = "dados-uroot.RData")



## ----tab:lagselectionm-1-------------------------------------------------------------------------------------------
coint.datam.ln_1 <- database[,c("FBCF","CIB","CURI","EMBI","ER","IBOV","ICE","ICI","IIE","IR",
                              "LTOT","NFSP","PIB","POUP","TT","WP")]

def <- VARselect(coint.datam.ln_1,lag.max=15,type="both")
# 
kable(t(def$selection), caption="Critérios de Informação para seleção do número de defasagens - dados mensais em log.")%>%  kable_classic(full_width = F, html_font = "Cambria")


var_selec_ltot_tbl <- xtable(t(def$selection), auto = TRUE,
                    caption = "Crit\\'{e}rios de Informa\\c{c}\\~{a}o para sele\\c{c}\\~{a}o do n\\'{u}mero de defasagens - modelo com desembolsos totais",label = "tab:lagselectionm-1",
				digits = 2)

print(var_selec_ltot_tbl, type="latex", file="tables/var_selec_ltot_tbl_1.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), booktabs=TRUE)




## ----tab:lagselectionm-2-------------------------------------------------------------------------------------------
coint.datam.ln_2 <- database[,c("FBCF","CIB","CURI","EMBI","ER","IBOV","ICE","ICI","IIE","IR",
                              "LAGR","LCOM","LIND","LINF","NFSP","PIB","POUP","TT","WP")]

def <- VARselect(coint.datam.ln_2,lag.max=15,type="both")
# 
kable(t(def$selection), caption="Critérios de Informação para seleção do número de defasagens - dados mensais em log.")%>%  kable_classic(full_width = F, html_font = "Cambria")


var_selec_ltot_tbl_2 <- xtable(t(def$selection), auto = TRUE,
                    caption = "Crit\\'{e}rios de Informa\\c{c}\\~{a}o para sele\\c{c}\\~{a}o do n\\'{u}mero de defasagens - modelo com desembolsos setoriais",label = "tab:lagselectionm-2",
				digits = 2)

print(var_selec_ltot_tbl_2, type="latex", file="tables/var_selec_ltot_tbl_2.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), booktabs=TRUE)




## ----tab:cointegrationm-1------------------------------------------------------------------------------------------
jo.eigen <- ca.jo(coint.datam.ln_1[,c(1:11)], type='trace', K=12, ecdet='const',spec='transitory')

jo_res <- data.frame(teste = jo.eigen@teststat, cval=jo.eigen@cval)

kable(jo_res, caption = "Valores das estatísticas de teste e valores críticos - Johansen.")%>%  kable_classic(full_width = F, html_font = "Cambria")
#set.caption("(#tab:cointegrationm)Valores das estatísticas de teste e valores críticos - Johansen.")
#panderOptions('table.style','grid')
#pander(jo_res )

#xtable::xtable(jo_res)

jo_tbl_1 <- xtable(jo_res, auto = TRUE,
                    caption = "Valores das estat\\'{i}sticas de teste e valores cr\\'{i}ticos - Johansen - Desembolsos Totais - FBCF a NFSP.",label = "tab:cointegrationm-1",
				digits = 2)

print(jo_tbl_1, type="latex", file="tables/jo_tbl_1.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), booktabs=TRUE)



## ----tab:cointegrationm-2------------------------------------------------------------------------------------------
jo.eigen <- ca.jo(coint.datam.ln_1[,c(1,10:16)], type='trace', K=13, ecdet='const',spec='transitory')

jo_res <- data.frame(teste = jo.eigen@teststat, cval=jo.eigen@cval)

kable(jo_res, caption = "Valores das estatísticas de teste e valores críticos - Johansen.")%>%  kable_classic(full_width = F, html_font = "Cambria")

jo_tbl_2 <- xtable(jo_res, auto = TRUE,
                    caption = "Valores das estat\\'{i}sticas de teste e valores cr\\'{i}ticos - Johansen - Desembolsos Totais - FBCF, LTOT a WP",label = "tab:cointegrationm-2",
				digits = 2)

print(jo_tbl_2, type="latex", file="tables/jo_tbl_2.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), booktabs=TRUE)


## ----tab:cointegrationm-3------------------------------------------------------------------------------------------
#coint.datam.ln <- database[,c(1,10:18)]

jo.eigen <- ca.jo(coint.datam.ln_2[,c(1:11)], type='eigen', K=11, ecdet='const',spec='transitory')

jo_res <- data.frame(teste = jo.eigen@teststat, cval=jo.eigen@cval)

kable(jo_res, caption = "Valores das estatísticas de teste e valores críticos - Johansen.")%>%  kable_classic(full_width = F, html_font = "Cambria")


jo_tbl_3 <- xtable(jo_res, auto = TRUE,
                    caption = "Valores das estat\\'{i}sticas de teste e valores cr\\'{i}ticos - Johansen - Desembolsos Setoriais - FBCF a LCOM",label = "tab:cointegrationm-3",
				digits = 2)

print(jo_tbl_3, type="latex", file="tables/jo_tbl_3.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), booktabs=TRUE)



## ----tab:cointegrationm-4------------------------------------------------------------------------------------------
#coint.datam.ln <- database[,c(1,10:18)]

jo.eigen <- ca.jo(coint.datam.ln_2[,c(1,10:19)], type='eigen', K=11, ecdet='const',spec='transitory')

jo_res <- data.frame(teste = jo.eigen@teststat, cval=jo.eigen@cval)

kable(jo_res, caption = "Valores das estatísticas de teste e valores críticos - Johansen.")%>%  kable_classic(full_width = F, html_font = "Cambria")

jo_tbl_4 <- xtable(jo_res, auto = TRUE,
                    caption = "Valores das estat\\'{i}sticas de teste e valores cr\\'{i}ticos - Johansen - Desembolsos Setoriais - FBCF a LCOM",label = "tab:cointegrationm-4",
				digits = 2)

print(jo_tbl_4, type="latex", file="tables/jo_tbl_4.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), booktabs=TRUE)



## ----tab:cointegration-comb----------------------------------------------------------------------------------------

#<- coint.datam.ln_1  

johansen_by2 <- function(coint.data){
    
    coint_res <- list()
    
    for (i in 2:ncol(coint.data)) {
      
      bic <- VARselect(coint.data[,c(1,i)],lag.max=15,type="both")$selection[[2]]
  
      jo.eigen <- ca.jo(coint.data[,c(1,i)], type='eigen', K = bic, ecdet='const',spec='transitory')
    
      jo_res <- data.frame(teststat = jo.eigen@teststat, cval = jo.eigen@cval)
      
      colname <- colnames(coint.data)[i]
      
      coint_res[[colname]] <- jo_res
      
    }
  
    return(coint_res)
  
}


coint_res_1 <- johansen_by2(coint.datam.ln_1) 
coint_df_1 <- do.call(rbind, lapply(coint_res_1, data.frame, stringsAsFactors=FALSE))

coint_res_2 <- johansen_by2(coint.datam.ln_2) 
coint_df_2 <- do.call(rbind, lapply(coint_res_2, data.frame, stringsAsFactors=FALSE))



## ----tab:cointegration-by2-1---------------------------------------------------------------------------------------

coint_tbl_1 <- xtable(coint_df_1, auto = TRUE,
                    caption = "Valores das estat\\'{i}sticas de teste e valores cr\\'{i}ticos - Johansen - por pares com desembolsos totais.",label = "tab:cointegration-by2-1",
				digits = 2)

print(coint_tbl_1, type="latex", file="tables/coint_tbl_1.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), booktabs=TRUE)


kable(coint_tbl_1) %>% kable_classic(full_width = F, html_font = "Cambria")



## ----tab:cointegration-by2-2---------------------------------------------------------------------------------------

coint_tbl_2 <- xtable(coint_df_2, auto = TRUE,
                    caption = "Valores das estat\\'{i}sticas de teste e valores cr\\'{i}ticos - Johansen - por pares com desembolsos setoriais.",label = "tab:cointegration-by2-2",
				digits = 2)

print(coint_tbl_2, type="latex", file="tables/coint_tbl_2.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), booktabs=TRUE)

kable(coint_tbl_2) %>% kable_classic(full_width = F, html_font = "Cambria")


## ------------------------------------------------------------------------------------------------------------------

database_df <- as.data.frame(database)
database_df$date <- seq(from=as.Date("2002/11/01"), to=as.Date("2019/12/01"), by='month')

for (i in 1:(ncol(database_df)-1)){
  
  
  assign("col", database_df[,i])
  assign("colname", colnames(database_df)[i])
  
  ggplot(database_df) +
  aes(x = date, y = col) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme_minimal() + ylab(colname) 
  
  ggsave(paste0(paste0("figs/plot-",colname),".png"))

  
}
  
  
  

