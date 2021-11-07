#' ############################################################################
#' @description Realiza do download/leitura dos dados para as simulações, 
#' e organiza a base de dados de acordo com o formato desejado.

#######################################################################################
#' @description Aplica a função `f` para cada célula de um dataframe `mat` 
#' @description se a célula correspondente em `p` é TRUE
#'
#' @param mat A matrix or data frame
#' @param p A matrix with the same dimension as `mat`
#' @param f A function to apply
#' @return `mat` with `f` applied to each cell where `p` is TRUE.
#' @examples
#' x <- rbind(c(1,2,3), c(4,5,6), c(7,8,9))
#' apply_if(x, upper.tri(x), function(x) x + 5)
apply_if <- function(mat, p, f) {
  # Fill NA with FALSE
  p[is.na(p)] <- FALSE
  mat[p] <- f(mat[p])
  mat
}

#######################################################################################
#' @description Constrói a matriz de correlação
#' 
#' @param mat an rcorr object or a double matrix
#' @param corrtype is either pearson or spearman. Will be passed into Hmsic::rcorr if mat is not already an rcorr object
#' @return `mat` with stars appended for each level of significants (p < 0.05, p < 0.01, p < 0.001)
apaCorr <- function(mat, corrtype = "pearson") {
  matCorr <- mat
  if (class(matCorr) != "rcorr") {
    matCorr <- rcorr(mat, type = corrtype)
  }

  # Add one star for each p < 0.05, 0.01, 0.001
  stars <- apply_if(round(matCorr$r, 1), matCorr$P < 0.05, function(x) paste0(x, "*"))
  stars <- apply_if(stars, matCorr$P < 0.01, function(x) paste0(x, "*"))
  stars <- apply_if(stars, matCorr$P < 0.001, function(x) paste0(x, "*"))
  # Put - on diagonal and blank on upper diagonal
  stars[upper.tri(stars, diag = T)] <- "X"
  stars[upper.tri(stars, diag = F)] <- ""
  n <- length(stars[1,])
  colnames(stars) <- 1:n
  # Remove _ and convert to title case
  row.names(stars) <- tools::toTitleCase(sapply(row.names(stars), gsub, pattern="_", replacement = " "))
  # Add index number to row names
  row.names(stars) <- paste(paste0(1:n,"."), row.names(stars))
  stars
}

## -------------------------------------------------------------------------------------
# kable(t(as.array(summary(database))), caption = "Estatística descritiva da base de dados dessazonalizados",longtable = F, booktabs = T, digits=2)%>% kable_styling(latex_options = c("hold_position","scale_down"))%>%
#    kable_classic(full_width = T, html_font = "Cambria")

#quick_latex(t(as.array(summary(database))), file= "tables/sum_data.tex")
#panderOptions('table.style','grid')
#pander(summary(database), caption="(\\#tab:sum-mth)Estatísticas descritivas da base de dados.")

log4r_info(" Produzindo a tabela com o sum�rio estat�stico dos dados ...") 


database_stat <- xtable(t(as.array(summary(database))), 
       caption =  "Estatat\\'{i}sticas descritivas da base de dados.",
       label = "sumario_dados", auto = TRUE)

print(database_stat, type="latex", file="tables/database_stat.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = FALSE)
      

kable(t(as.array(summary(database))))


## ----fig1,fig.cap="PIB, FBCF e desembolsos do BNDES em log (mensal e acumulado em 12 meses)", include=TRUE, eval=FALSE----
## # autoplot(ts.intersect(ln.data.m[,"PIB"],ln.data.m[,"FBCF"],ln.data.m[,"LTOT"],
## #                       ln.data.m[,"LINF"],ln.data.m[,"LAGR"],ln.data.m[,"LIND"],
## #                       ln.data.m[,"LCOM"]))+ ylab("")+ xlab("")
## data_df = data.frame(date=as.yearmon(time(database)),database)
## 
## colnames(data_df) <- c("date",colnames(database))
## 
## scale_value = max(data_df$FBCF, na.rm = TRUE)/ max(data_df$LTOT, na.rm = TRUE)
## 
## ggplot(data_df) +
##   geom_line(aes(x=date, y=FBCF, color = 'FBCF')) +
##   #geom_line(aes(x=date, y=PIB, color = 'PIB')) +
##   geom_line(aes( x=date, y = (LTOT*scale_value),color = 'LTOT')) +
##   geom_line(aes( x=date, y = (LINF*scale_value),color = 'LINF')) +
##   geom_line(aes( x=date, y = (LAGR*scale_value),color = 'LAGR')) +
##   geom_line(aes( x=date, y = (LIND*scale_value),color = 'LIND')) +
##   geom_line(aes( x=date, y = (LCOM*scale_value),color = 'LCOM')) +
##   scale_y_continuous(sec.axis = sec_axis(~./scale_value, name = 'Desembolsos do BNDES')) +
##   #scale_colour_manual("", values = c("FBCF"="green", "PIB"="blue","LTOT"="red")) +
##   xlab(" ") +ylab("FBCF-PIB")
## 
## 


## ----fig2, fig.cap="FBCF e variáveis macroeconômicas selecionadas em log (mensal e acumulado em 12 meses)", include=TRUE, eval=FALSE----
## # autoplot(ts.intersect(ln.data.m[,"PIB"],ln.data.m[,"FBCF"],ln.data.m[,"DBGG"],ln.data.m[,"ER"],
## #                       ln.data.m[,"NFSP"],ln.data.m[,"IBOV"], ln.data.m[,"IR"],ln.data.m[,"POUP"]))+ ylab("")+ xlab("")
## 
## scale_value = max(data_df$FBCF, na.rm = TRUE)/ max(data_df$ER, na.rm = TRUE)
## 
## ggplot(data_df) +
##   geom_line(aes(x=date, y=FBCF, color = 'FBCF')) +
##   geom_line(aes(x=date, y=CIB*scale_value, color = 'CIB')) +
##   geom_line(aes(x=date, y=NFSP, color = 'NFSP')) +
##   geom_line(aes(x=date, y=IBOV*scale_value, color = 'IBOV')) +
##   geom_line(aes(x=date, y=IR, color = 'IR')) +
##   #geom_line(aes(x=date, y=POUP, color = 'POUP')) +
##   geom_line(aes( x=date, y = (ER*scale_value),color = 'ER')) +
##   geom_line(aes( x=date, y = (POUP),color = 'POUP')) +
## 
##   scale_y_continuous(sec.axis = sec_axis(~./scale_value, name = 'ER/POUP')) +
##   #scale_colour_manual("", values = c("FBCF"="green", "PIB"="blue","LTOT"="red")) +
##   xlab(" ")+ylab("FBCF/DBGG/NFSP/IR")
## 
## 


## ----fig2.1, fig.cap="FBCF e variáveis macroeconômicas selecionadas em log (base mensal e acumulada em 12 meses)", include=TRUE, eval=FALSE----
## # autoplot(ts.intersect(ln.data.m[,"PIB"],ln.data.m[,"FBCF"],ln.data.m[,"DBGG"],ln.data.m[,"ER"],
## #                       ln.data.m[,"NFSP"],ln.data.m[,"IBOV"], ln.data.m[,"IR"],ln.data.m[,"POUP"]))+ ylab("")+ xlab("")
## 
## scale_value = max(data_df$FBCF, na.rm = TRUE)/ max(data_df$CURI, na.rm = TRUE)
## 
## ggplot(data_df) +
##   geom_line(aes(x=date, y=FBCF, color = 'FBCF')) +
##   geom_line(aes( x=date, y = (CURI*scale_value),color = 'CURI')) +
## 
##   scale_y_continuous(sec.axis = sec_axis(~./scale_value, name = 'CURI')) +
##   #scale_colour_manual("", values = c("FBCF"="green", "PIB"="blue","LTOT"="red")) +
##   xlab(" ")+ylab("FBCF")
## 
## 


## ----fig3, fig.cap="PIB, FBCF e indicadores da FGV", include=TRUE, eval=FALSE--------------------------------------
## # autoplot(ts.intersect(ln.data.m[,"PIB"],ln.data.m[,"FBCF"],ln.data.m[,"IIE"],ln.data.m[,"ICI"],
## # ln.data.m[,"ICE"],ln.data.m[,"CURI"]))+ ylab("")+ xlab("")
## 
## 
## scale_value = max(data_df$FBCF, na.rm = TRUE)/ max(data_df$ICI, na.rm = TRUE)
## 
## ggplot(data_df) +
##   geom_line(aes(x=date, y=FBCF, color = 'FBCF')) +
##   #geom_line(aes( x=date, y = (ER*scale_value),color = 'ER')) +
##   geom_line(aes( x=date, y = (ICI*scale_value),color = 'ICI')) +
##   geom_line(aes( x=date, y = (ICE*scale_value),color = 'ICE')) +
##   geom_line(aes( x=date, y = (CURI*scale_value),color = 'CURI')) +
##   scale_y_continuous(sec.axis = sec_axis(~./scale_value, name = 'Indicadores da FGV')) +
##   #scale_colour_manual("", values = c("FBCF"="green", "PIB"="blue","LTOT"="red")) +
##   xlab(" ")+ylab("FBCF")
## 
## 


## ----fig3.1, fig.cap="PIB, FBCF e indicadores da FGV", include=TRUE, eval=FALSE------------------------------------
## # autoplot(ts.intersect(ln.data.m[,"PIB"],ln.data.m[,"FBCF"],ln.data.m[,"IIE"],ln.data.m[,"ICI"],
## # ln.data.m[,"ICE"],ln.data.m[,"CURI"]))+ ylab("")+ xlab("")
## 
## 
## scale_value = max(data_df$FBCF, na.rm = TRUE)/ max(data_df$ER, na.rm = TRUE)
## 
## ggplot(data_df) +
##   geom_line(aes(x=date, y=FBCF, color = 'FBCF')) +
##   geom_line(aes( x=date, y = (ER*scale_value),color = 'ER')) +
##   #geom_line(aes( x=date, y = (ICI*scale_value),color = 'ICI')) +
##   #geom_line(aes( x=date, y = (ICE*scale_value),color = 'ICE')) +
##   #geom_line(aes( x=date, y = (CURI*scale_value),color = 'CURI')) +
##   scale_y_continuous(sec.axis = sec_axis(~./scale_value, name = 'Indicadores da FGV')) +
##   #scale_colour_manual("", values = c("FBCF"="green", "PIB"="blue","LTOT"="red")) +
##   xlab(" ")+ylab("FBCF")
## 
## 


## ----fig3.2, fig.cap="PIB, FBCF e indicadores da FGV", include=TRUE, eval=FALSE------------------------------------
## # autoplot(ts.intersect(ln.data.m[,"PIB"],ln.data.m[,"FBCF"],ln.data.m[,"IIE"],ln.data.m[,"ICI"],
## # ln.data.m[,"ICE"],ln.data.m[,"CURI"]))+ ylab("")+ xlab("")
## 
## 
## scale_value = max(data_df$FBCF, na.rm = TRUE)/ max(data_df$TT, na.rm = TRUE)
## 
## ggplot(data_df) +
##   geom_line(aes(x=date, y=FBCF, color = 'FBCF')) +
##   geom_line(aes( x=date, y = (TT*scale_value),color = 'TT')) +
##   geom_line(aes( x=date, y = (WP*scale_value),color = 'WP')) +
##   geom_line(aes( x=date, y = (CIB*scale_value),color = 'CIB')) +
##   geom_line(aes( x=date, y = (ER*scale_value),color = 'ER')) +
##   scale_y_continuous(sec.axis = sec_axis(~./scale_value, name = 'TT// WP // CIB // ER')) +
##   #scale_colour_manual("", values = c("FBCF"="green", "PIB"="blue","LTOT"="red")) +
##   xlab(" ")+ylab("FBCF")
## 
## 


## ----fig:corrtablemln, fig.cap="(\\#fig:corrtablem) Correlação entre os dados mensais em log.", eval=FALSE---------
## #############################################
## # CORRELAÇÃO
## #############################################
## 
## #df.mth.ln <- as.data.frame(ts.intersect(data.ln.m,data.ln.m.ipea[,c(1:2)]))
## #colnames(df.mth.ln) <- c(colnames(data.m),colnames(data.m.ipea)[1:2])
## df.mth.ln <- as.data.frame(database)
## colnames(df.mth.ln) <- colnames(database)
## # pval <- psych::corr.test(database, adjust="none")$p
## # cex.before <- par("cex")
## # par(cex = 0.6)
## # corrplot(cor(df.mth.ln),tl.cex = 1/par("cex"), p.mat = pval,
## #          cl.cex = 1/par("cex"), addCoefasPercent = TRUE,
## #          method = "number",
## #          order = "original",type ='lower',tl.col = "black",
## #          sig.level = 0.05, insig = "blank",col = gray.colors(100), mar = c(0,0,3,0),
## #          title = "Correlação entre os dados mensais em log.")
## # par(cex = cex.before)
## 
## 
## databaseStars <- apaCorr(as.matrix(df.mth.ln), corrtype = "pearson")
## 
## # kable(databaseStars, caption = "Correlação entre os dados.",longtable = T, booktabs = T, digits=1)%>%
## #   kable_styling(font_size = 8)%>%
## #    kable_classic(full_width = T, html_font = "Cambria") %>%
## #    row_spec(1:18, extra_css = "border-bottom: 1px solid;") %>%
## #    column_spec(1, bold = T, border_right = T) #%>%
## #    #column_spec(2:18, bold = F, border_right = T)
## set.caption("Correlação entre os dados.")
## panderOptions('table.style','grid')
## pander(databaseStars)


## ----corr_table, include=TRUE--------------------------------------------------------------------------------------

log4r_info(" Produzindo a tabela de correlações ...") 

#database2 <- data.ln.m

#database2[,"ER"] <- data.m[,"ER"]
 #make hux
df.mth.ln <- as.data.frame(database)
colnames(df.mth.ln) <- colnames(database)

cmat <- rstatix::cor_mat(df.mth.ln, conf.level = 0.95)
# rstatix::cor_mark_significant(cmat,
#           cutpoints = c(0, 0.05, 0.10, 0.20, 1),
#           symbols = c("****", "***", "**", "*")
#         )


cmat_ht <-rstatix::cor_mark_significant(cmat,
          cutpoints = c(0, 0.05, 0.10, 0.20, 1),
          symbols = c("****", "***", "**", "*")
          )%>%as_huxtable()%>%set_all_padding(row =everywhere,col =everywhere,value =6)%>%set_bold(1, everywhere)%>%set_background_color(evens, everywhere,"grey92")%>%map_text_color(-1,-1,by_colorspace("red4","darkgreen"))%>%set_col_width(everywhere,value =c(.16,.15,.11,.2,.2,.2))%>%set_width(1.02)%>%theme_article() #note how sum of col widths == total table widtht
 

#cmat_ht %>% as_flextable%>%flextable::as_raster(.) 
cmat_ht %>% as_flextable%>%flextable::save_as_image(path = "tables/corr_table.png")



#quick_latex(cmat_ht, file = "tables/corr_table.tex")


corr_tbl <- xtable(cmat_ht, 
       caption =  "Correla\\c{c}\\~{a}o entre os dados.",
       label = "tab:corr_table", auto = TRUE)

print(corr_tbl, type="latex", file="tables/corr_tbl.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = FALSE,
      caption.placement=getOption("xtable.caption.placement","top"),
      booktabs=TRUE)




## ----rolling-cor---------------------------------------------------------------------------------------------------

# rcorr_CURI <- rolCorPlot(y = database[,"FBCF"], x = database[,"CURI"], width = c(2), level = 0.95,
#                         main = "Rolling correlations between FBCF and CURI",
#                         SDtest = TRUE,N=1000)


rcorr_ICI <- rolCorPlot(y = database[,"FBCF"], x = database[,"ICI"], width = c(2), level = 0.95,
           main = "Rolling correlations between FBCF and ICI",
           SDtest = TRUE,N=1000)

# rcorr_ICE <- rolCorPlot(y = database[,"FBCF"], x = database[,"ICE"], width = c(2), level = 0.95,
#                         main = "Rolling correlations between FBCF and ICE",
#                         SDtest = TRUE,N=1000)




## ------------------------------------------------------------------------------------------------------------------
#rcorr_CURI$test	



## ------------------------------------------------------------------------------------------------------------------
#rcorr_ICE$test	


## ------------------------------------------------------------------------------------------------------------------
rcorr_ICI$test	



## ------------------------------------------------------------------------------------------------------------------

log4r_info(" Produzindo gráfico de decomposição da série temporal FBCF ...") 

fbcf_dcp <- decompose(database[,"FBCF"])

plot(fbcf_dcp)


## ----pibfbcf, fig.cap="FBCF e desembolsos totais do BNDES (LTOT).", fig.show='hold', include=TRUE------------------
library(TTR)

n.hor = 12
# séries em média móvel de 12 meses
fbcf.pib = data.acum.pib[,"FBCF"]
ltot.pib = data.acum.pib[,"LTOT"]
fbcf_trend = SMA(fbcf.pib, n=n.hor)#(decompose(tx_invest))$trend
lbndes_trend = SMA(ltot.pib, n=n.hor)#(decompose(despib_tot))$trend


log4r_info(" Produzindo o gráfico de investimento x desembolsos totais do BNDES ...") 

data_pib <- cbind(fbcf_trend, lbndes_trend)
colnames(data_pib) <- c("INVEST", "LTOT")

data_pib <- data_pib
data_pib_df <- data.frame(date=as.yearmon(time(data.acum.pib)),data_pib)
data_pib_df <- drop_na(data_pib_df)
colnames(data_pib_df) <- c("date","INVEST", "LBNDES")

scale_value = max(data_pib_df$INVEST, na.rm = TRUE)/ max(data_pib_df$LBNDES, na.rm = TRUE)

ggplot(data_pib_df) + 
  geom_line(aes(x=date, y=INVEST, color = 'INVEST')) +
  geom_line(aes( x=date, y = (LBNDES*scale_value),color = 'LBNDES')) +
  scale_y_continuous(sec.axis = sec_axis(~./scale_value, name = 'LBNDES')) +
  scale_colour_manual("", values = c("INVEST"="green", "LBNDES"="red")) +
  xlab(" ") 
  #+labs(title="FBCF e desembolsos totais do BNDES (LTOT).")

#png("figs/plot-fbcf-ltot-pib.png")
#print(fbcf_ltot_pib_plot)
#dev.off()
ggsave("figs/plot-fbcf-ltot-pib.png")

#fbcf_ltot_pib_plot


## ----02.01-func-ccf_table------------------------------------------------------------------------------------------

log4r_info(" Produzindo a tabela de correlação cruzada ...") 

# função para realizar a análise de autocorrelação cruzada e montar tabela com os resultados.
ccf_table <- function(data, name="",table=FALSE, n_per = 12){
  
  #' TODO: fazer com que a depvar seja a coluna escolhida. A função parte do pressuposto de que a depvar é a coluna 1.
  
  
  depvar = colnames(data)[1]
  table_ccf <- tibble(data.frame(lag=numeric(),acf=numeric()))

  #data  = data.ln[,colnames(data.ln)]#data.ma.pib_sa
    
  for (i in 2:ncol(data)){
    
    name = colnames(data)[i]
    ccf_obj = ccf(data[,i], data[,1],lag.max = n_per,ylab=c('CCF - ', name ,'->', depvar), na.action = na.pass,plot = FALSE)
    
    #res <- calc_ccf(ccf_obj,name, n_per = 4)
    
    res <- data.frame(ccf_obj$lag,ccf_obj$acf)
    colnames(res) <- c("Lag","ACF")
    
    #procura o acf máximo em módulo
    
    if(all(res$ACF <= 0)){
      
      coef <- min(res$ACF)
      ind <- which.min(res$ACF)
      
    } 
    else if (all(res$ACF >= 0)){
      
      coef <- max(res$ACF)
      ind <- which.max(res$ACF)
      
      
    }
    else{
      
      
      min <- min(res$ACF)
      max <- max(res$ACF)
      
      if (abs(min) >= abs(max))
      {
        coef = min
        ind <- which.min(res$ACF)  
        
      } else{
        
        coef = max
        ind <- which.max(res$ACF)  
        
      }
      
      
    } 
    
    
    if (table == TRUE){
      
      kable(res, digits=2,format="simple",
            caption=c("Resultados da CCF ", name))
      
      
    }
    else{
      #cat("\n## CCF: ", name)
      #cat("\n## O máximo valor de ACF ocorre no lag (", res$Lag[ind]*n_per,")=",coef)
      
    }
    ccf.resp <- tibble(lag=res$Lag[ind]*n_per, acf=coef)
    
    table_ccf = table_ccf %>% add_row(ccf.resp[1],ccf.resp[2])
    
    
  }

  table_ccf = table_ccf %>% mutate(series=colnames(data)[2:ncol(data)])
  table_ccf = table_ccf[,c(3,1,2)]
  
  

  
  return <- table_ccf
}


## ----tab:ccftablem, include=TRUE-----------------------------------------------------------------------------------

ccf_tbl_m <- ccf_table(data = database,n_per =12)

# kable(ccf_tbl_m, caption = "Correlação Cruzada das séries mensais (em log) - períodos em que ocorre o máximo da ccf em relação à FBCF (em valor absoluto).",longtable = T, booktabs = T, digits=2) %>% kable_classic(full_width = F, html_font = "Cambria") %>% kable_styling(position="center")
set.caption("(\\#tab:ccftablem)Correlação Cruzada das séries mensais (em log) - períodos em que ocorre o máximo da ccf em relação à FBCF (em valor absoluto).")
panderOptions('table.style','grid')
pander(ccf_tbl_m )


ccf_tbl <- xtable(ccf_tbl_m, auto = TRUE,
                    caption = "Correla\\c{c}\\~{a}o Cruzada das s\\'{e}ries mensais (em log)
				- per\\'{i}odos em que ocorre o m\\'{a}ximo da fun\\c{c}\\~{a}o de correla\\c{c}\\~{a}o cruzada em rela\\c{c}\\~{a}o \\`{a} FBCF (em valor	absoluto).",label = "tab:ccftablem",
				digits = 3)

print(ccf_tbl, type="latex", file="tables/ccf_table.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), booktabs=TRUE)




## ----02.01.corr-5--------------------------------------------------------------------------------------------------


#summary(data_1)


## ------------------------------------------------------------------------------------------------------------------
# função para a contrução da tabela de break-points

log4r_info(" Produzindo a tabela de quebras estruturais ...") 

break_table <- function(time_series){
  
  #time_series <- data.ln
  
  brk_table = list()
  
  names <- colnames(time_series)
  
  res <- list()
  
  for (i in 1:ncol(time_series)){
    data = time_series[,i]
    # if(anyNA(data)){
    #   
    #   data = na.omit(data)
    # }
    #breakpoints(data_compl[,i] ~ 1, h=0.15, data=data_compl)
    brk <- breakpoints(data ~ 1, h=0.15, data=time_series)
    brk_date = list()
    for (j in 1:length(brk$breakpoints)){
      
      brk_date[[j]] = round(time(data)[brk$breakpoints[j]],2)
    }
    
    
    brk_table[[names[i]]] <- brk_date
    
  }
  
  #break_table <- breaktable_qtr
  max_cols <- 1

  for (i in 1:length(brk_table)){

    ncols.i <- length(brk_table[[i]])
    ifelse(ncols.i > max_cols, max_cols <- ncols.i, max_cols)
  }

  m <- matrix("", nrow = length(brk_table), ncol = max_cols)

  for (i in 1:nrow(m)){

    m[i,c(1:length(brk_table[[i]]))] <- unlist(brk_table[[i]])

  }

  rownames(m) <- names(brk_table)
  m[is.na(m)] <- "-"
  
  brktable_df <- as.data.frame(unlist(brk_table))
  colnames(brktable_df) <- c("breaks")
  brktable_df = data.frame(rownames(brktable_df),brktable_df$breaks)
  colnames(brktable_df) <- c("S�rie/nro. da quebra", "Per�odo")

  res[["brk_df"]] <-brktable_df
  res[["brk_matrix"]] <- m
  
  return(res)
}




## ----tab:breaktablem, include=TRUE---------------------------------------------------------------------------------
breaktable_m = break_table(database)

# kable(breaktable_m$brk_matrix, digits = 2, caption = "Quebras estruturais identificadas nas séries mensais (em log) por ano de ocorrência.", longtable = T, booktabs = T) %>% kable_styling(position = "center")%>%
#   kable_classic(full_width = T, html_font = "Cambria") 
set.caption("(\\#tab:breaktablem)Quebras estruturais identificadas nas séries mensais (em log) por ano de ocorr�ncia.")
panderOptions('table.style','grid')
panderOptions('digits',2)
pander(breaktable_m$brk_matrix)


brk_tbl <- xtable(breaktable_m$brk_matrix, auto = TRUE,
                    caption = "Quebras estruturais
				identificadas.",label = "tab:breaktablem",
				digits = 2)

print(brk_tbl, type="latex", file="tables/brk_table.tex",
      table.placement = getOption("xtable.table.placement", "!htb"), 
      include.colnames = TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), booktabs=TRUE)



## ----histbreaksm, fig.cap="Histograma de frequência das quebras estruturais identificadas nas séries mensais (em log).", fig.show='hold', include=TRUE----

log4r_info(" Produzindo o histograma de distribuição de quebras estruturais ...") 

png("figs/plot-hist_break.png")
hist(breaktable_m$brk_df[,2],breaks=8, main="Histograma de Quebras Estruturais", xlab="Ano") 
axis(side=1, at=seq(2004,2020, 3), labels=seq(2004,2020, 3))
dev.off()

p1 = sum(round(breaktable_m$brk_df[,2],1) > 2005 & round(breaktable_m$brk_df[,2],1) < 2012)
p1 = round(p1/nrow(breaktable_m$brk_df)*100,1)

p2 = sum(round(breaktable_m$brk_df[,2],1) > 2012 & round(breaktable_m$brk_df[,2],1) < 2016)
p2 = round(p2/nrow(breaktable_m$brk_df)*100,1)

p3 = sum(round(breaktable_m$brk_df[,2],1) > 2016 & round(breaktable_m$brk_df[,2],1) < 2020)
p3 = round(p3/nrow(breaktable_m$brk_df)*100,1)

p4 = sum(round(breaktable_m$brk_df[,2],1) > 2008 & round(breaktable_m$brk_df[,2],1) < 2010)
p4 = round(p4/nrow(breaktable_m$brk_df)*100,1)

p5 = sum(round(breaktable_m$brk_df[,2],1) > 2011 & round(breaktable_m$brk_df[,2],1) < 2016)
p5 = round(p4/nrow(breaktable_m$brk_df)*100,1)

print(p1)
print(p2)
print(p3)
print(p4)
print(p5)


## ----savedatacorr--------------------------------------------------------------------------------------------------
#ls(pattern = "data", all.names = TRUE)
log4r_info(" Salvando a base de dados com os resultados da análise exploratória ...") 

data <- list(database)
save(data, file = "dados-model.RData")
#save.image("dados-model.RData")

