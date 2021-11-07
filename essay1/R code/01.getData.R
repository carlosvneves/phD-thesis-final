#' ############################################################################
#' @description Realiza do download/leitura dos dados para as simula??es, 
#' e organiza a base de dados de acordo com o formato desejado.

#' ############################################################################
#' @section Fun??es auxiliares:
#'

## fun??o que transforma S?ries mensais em acumuladas anuais
to_year_acum <- function(data){
  
  anual <- data
  anual <- (anual+stats::lag(anual,-1)+stats::lag(anual,-2)+stats::lag(anual,-3)+
              stats::lag(anual,-4)+stats::lag(anual,-5)+stats::lag(anual,-6)+
              stats::lag(anual,-7)+stats::lag(anual,-8)+stats::lag(anual,-9)+
              stats::lag(anual,-10)+stats::lag(anual,-11))
  colnames(anual) <- colnames(data)
  
  
  
  return(anual)
  
}

## fun??o que transforma S?ries anuais acumuladas em trimestrais
to_qtr <- function(data){
  
  ti <- tk_index(data)[1]
  qtr<- ts(aggregate(data, nfrequency = 4, FUN=mean),
           start=as.numeric(ti), freq=4)  
  
  return(as.xts(qtr))
}

## fun??o que transforma as S?ries em mensais acumuladas
to_mth <- function(data){
  
  ti <- tk_index(data)[1]
  m <- ts(aggregate(data, nfrequency = 12, FUN=mean),
          start=as.numeric(ti), freq=12) 
  
  return(as.xts(m))
}

# fun??o para converter dados de base trimestral para mensal
to.month <- function(series)
{
  mod = td(series ~ 1, "mean", to = "month",method = "chow-lin-maxlog")
  series_m = mod$values
  
  return(series_m)
}

get_data <- function(){
  
  # checa se os dados n?o existem 
  if(file.exists("dados.RData")==TRUE){
    
    return(as.logical(FALSE))
    #return(as.logical(TRUE))
  }else{
    
    return(as.logical(TRUE))
  }
    
  
  
}

#' ############################################################################
#' @description Obten??o e tratamento dos dados a partir do SIDRA-IBGE
#'

##############################################################
### Obten??o e tratamento dos dados a partir do SIDRA-IBGE
### transforma??o das S?ries de base trimestral para mensal e
### dessazonaliza??o 
##############################################################

# data-base : Fevereiro 2021

log4r_info("Carregando IPCA (IBGE)...") 

# IPCA n?mero ?ndice para deflacionar S?ries 
ipca = get_sidra(api='/t/1737/p/all/v/2266/N1/all')%>%
  mutate(date = as.yearqtr(`Mês (Código)`, format='%Y%m')) %>%
  dplyr::select(date, Valor) %>%
  as_tibble()

ipca = ts(ipca$Valor, start=c(1979,12,01), frequency = 12)

ipca = window(ipca, start=2002)

log4r_info("Carregando Poupan?a Bruta (IBGE)...") 
# Poupan?aBruta em milh?es de reais correntes
data = get_sidra(api='/t/2072/n1/all/v/933,940,941/p/all')

dados <- data %>% dplyr::select(`Trimestre (Código)`, Variável, Valor) %>%
tidyr::pivot_wider(names_from=`Variável`, values_from=`Valor`) %>%
dplyr::mutate(date=as.yearqtr(parse_date_time(`Trimestre (Código)`, '%y%q'))) %>%
dplyr::select(-`Trimestre (Código)`)

poup.q <- ts(dados$`(=) Poupança bruta`, start=c(2000,01,01), frequency = 4)
poup.q <- window(poup.q, 2002)

log4r_info("Carregando PIB e FBCF (IBGE)...") 
# PIB e FBCF em milh?es de reais correntes
table <- get_sidra(api='/t/1846/n1/all/v/all/p/all/c11255/90707,93406') 

pib.q <- ts(table$Valor[table$`Setores e subsetores (Código)` ==90707], start=c(1996, 01), frequency = 4)
pib.q <- window(pib.q, 2002)

fbcf.q <- ts(table$Valor[table$`Setores e subsetores (Código)` ==93406], start=c(1996, 01), frequency = 4)
fbcf.q <- window(fbcf.q, 2002)

rm(data)
rm(table)


## ----convert-trimdata----------------------------------------------------------------------------------------------
log4r_info("Convertendo dados de periodicidade trimestral para mensal...") 

#' ############################################################################
#' @description Converte dados trimestrais para mensais (tempdisagg)
#'

# FBCF base mensal
fbcf <- to.month(fbcf.q)
# PIB base mensal 
pib <- to.month(pib.q)
# POUP base mensal 
poup <- to.month(poup.q)


#' ############################################################################
#' @description Obten??o e tratamento dos dados do Banco Central do Brasil
#'

############################################################################
## Dados do Banco Central do Brasil
############################################################################


# Capacidade de investimento da economia brasileira est? relacionada ?
# necessidade de financiamento do setor p?blico. A literatura aponta o 
# Estado como o principal financiador hist?rico do investimento p?blico.

# NFSP sem desvaloriza??o cambial - Fluxo mensal corrente - Resultado prim?rio - Total - Setor p?blico consolidado
#nfsp <- get_series(4649, start_date = '2002/01/01') 
#nfsp <- ts(nfsp[[2]], start = c(2002,01), frequency = 12)

###### No artigo de barboza e vasconcelos (2019), eles utilizam a 
## Real Effective Exchange Rate (?ndice da taxa de c?mbio real efetiva (IPCA) - Jun/1994=100), S?rie 11752 do BCB

log4r_info("Carregando dados do ?ndice da taxa de c?mbio real efetiva (SGS-BCB) ...") 

er <- ts(get_series(11752, start_date='2002-01-01')$`11752`,start=c(2002,01), freq=12)

#  BACEN 4189 - Taxa de Juros- Selic acumulada no M?s anualizada - % a.a. 

log4r_info("Carregando dados da Taxa Selic (SGS-BCB) ...") 

selic <- ts(get_series(4189, start_date='2002-01-01')$`4189`,start=c(2002,01), freq=12)

# 13522	?ndice nacional de pre?os ao consumidor - amplo (IPCA) - em 12 meses - Infla??o
log4r_info("Carregando dados do ?ndice de Infla??o IPCA (SGS-BCB) ...") 

inflacao <- ts(get_series(13522, start_date='2002-01-01')$`13522`,start=c(2002,01), freq=12)

# taxa de juros da economia brasileira
log4r_info("Calculando a Taxa de Juros Efetiva da Economia Brasileira com base na Selic e no IPCA ...") 
ir <- (((1+(selic/100))/(1+(inflacao/100)))-1)*100

# D?vida Bruta do Governo Geral - BCB - S?rie 4502
log4r_info("Carregando dados da D?vida Bruta do Governo Geral (SGS-BCB) ...") 
dbgg <- ts(get_series(4502, start_date='2002-01-01')$`4502`,start=c(2002,01), freq=12)

# ?ndice de commodities Brasil - BCB - S?rie 27574
log4r_info("Carregando dados da D?vida Bruta do ?ndice de Commodities do Brasil (SGS-BCB) ...") 
cib <- ts(get_series(27574, start_date='2002-01-01')$`27574`,start=c(2002,01), freq=12)

# PIB acumulado dos ?ltimos 12 meses - Valores correntes (R$ milh?es) - S?rie 4382 - at? mar-2021
log4r_info("Carregando dados do PIB mensal acumulado em 12 meses (SGS-BCB) ...") 
pib.acum <- ts(get_series(4382, start_date='2002-01-01')$`4382`,start=c(2002,01), freq=12)
pib.acum <- window(pib.acum, end=2020.95)

#' ############################################################################
#' @description Obten??o e tratamento dos dados do IBOVESPA
#'
#'
############################################################################
## Dados do IBOVESPA
############################################################################
log4r_info("Carregando dados do IBOVESPA (Yahoo Finance) ...") 

env <- new.env()
g<-getSymbols("^BVSP", src = "yahoo", env = env, from = as.Date('1996-01-01'))

ibovespa = env$BVSP[,4]
ibovespa = ibovespa[complete.cases(ibovespa)] %>% na.omit

# agrega mensalmente com a m?dia
ibovespa = apply.monthly(ibovespa, FUN=mean)
ibovespa = ts(ibovespa, start=c(1996,01,01), freq=12)

ibovespa <- window(ibovespa, start=c(2002))



#' ############################################################################
#' @description Obten??o e tratamento dos dados do IPEADATA
#'
#'
############################################################################
## Dados do IPEADATA
############################################################################

# checa se os dados n?o existem 
# Termos de Troca - ?ndice (m?dia - 2006 = 100) - S?rie considerada como em barboza e vasconcelos(2019)
log4r_info("Carregando dados do ?ndice de Termos de Troca (IPEA-DATA) ...") 

ttr <- ipeadatar::ipeadata('FUNCEX12_TTR12')
#ttr <- read.csv("dados/ttr-05-2021-ipeadata.csv")
ttr <- ts(ttr$value, start=c(1978,01,01), frequency = 12)

ttr <- window(ttr, 2002)

# EMBI + Risco-Brasil
log4r_info("Carregando dados do EMBI+ Risco Brasil (IPEA-DATA) ...") 

embi <- ipeadata(code="JPM366_EMBI366", language = "br")	
embi <- data.frame(date=embi$date,embi = embi$value)
embi.m <- apply.monthly(as.xts(embi$embi,order.by=embi$date), FUN=mean)
embi.m <- ts(embi.m[,1], start = c(1994,04,29), frequency = 12)

embi.m <- window(embi.m, 2002)

# NFSP - setor p?blico -resultado prim?rio - s/ desvalor. cambial - fluxo acum. 12 meses - SP consol. (% PIB)
log4r_info("Carregando dados da Necessidade de Financiamento do Setor P?blico (IPEA-DATA) ...") 

nfsp <- ipeadata(code="BM12_NFSPPYS12", language = "br")
nfsp <- ts(nfsp$value, start=c(2002,11,01), frequency = 12)


#' ############################################################################
#' @description Obten??o e tratamento dos dados do World Trade Monitor
#'
#'
# Produ??o mundial de acordo com o CPB - World Trade Monitor 
log4r_info("Carregando dados de Produ??o Mundial (World Trade Monitor ) ...") 

wprod <- read_excel("dados/CPB_WPROD.xlsx", trim_ws = TRUE, skip=0,
                      col_names = TRUE, col_types = "guess")

wprod <- ts(wprod$WPROD, start=c(2000,01,01), frequency = 12)

wprod <- window(wprod, 2002)




#' ############################################################################
#' @description Dessazonaliza??o dos dados utilizando o m?todo X13-ARIMA
#'
#'
# dessazonaliza??o X13-ARIMA
log4r_info("Dessazonalizando S?ries temporais com X13-ARIMA ...") 

er_sa = seasonal::final(seas(er, na.action = na.omit))
ibovespa_sa = seasonal::final(seas(ibovespa, na.action = na.omit))
embi_sa = seasonal::final(seas(embi.m, na.action = na.omit))
nfsp_sa = seasonal::final(seas(nfsp, na.action = na.omit))

fbcf_sa = seasonal::final(seas(fbcf, na.action = na.omit))
pib_sa = seasonal::final(seas(pib, na.action = na.omit))
poup_sa = seasonal::final(seas(poup, na.action = na.omit))

ttr_sa = seasonal::final(seas(ttr, na.action = na.omit))

cib_sa = seasonal::final(seas(cib, na.action = na.omit))


#' ############################################################################
#' @description Obten??o e tratamento dos dados da FGV
#'
#'

############################################################
############################################################
#-- Carrega dados a partir da FGV -----#
#
# FGV 
# Obs: as S?ries est?o dessazonalizadas, exceto IIE-BR, ICE e CURC
############################################################

# checa se os dados n?o existem 

log4r_info("Carregando dados da FGV (IIE,ICI,ICE,ICS,ICOM,ICST,CURI,CURC)...") 
# dados a partir de 01/2000
data_fgv = read_excel("dados/dadosFGV.xls", 
                      sheet = 'sheet', trim_ws = TRUE, skip=0,
                      range="A22:I270", col_names = FALSE, col_types = "guess")

colnames(data_fgv) <- c("date","IIE","ICI","ICE","ICS","ICOM","ICST","CURI","CURC")

data_fgv = as_tibble(data_fgv)

data_fgv = sapply(data_fgv[,c(2:9)], as.numeric) %>% as_tibble()

data_fgv = data_fgv %>% 
  dplyr::mutate(date = seq(as.Date("2000/01/01"), as.Date("2020/09/01"), by = "month")) 


# dados na forma de S?ries temporais
data_fgv_ts <- ts(data_fgv, start = c(2000,01,01), frequency = 12)
data_fgv_ts <- data_fgv_ts[,-9]

data_fgv_ts <- window(data_fgv_ts, 2002)

# seleciona as S?ries IIE, ICI, ICE e CURI
data_fgv_ts <- na.remove(data_fgv_ts[,c(1,2,3,7)])


#' ############################################################################
#' @description Obten??o e tratamento dos dados do BNDES
#'
#'

############################################################
############################################################
#-- Carrega dados a partir do BNDES -----#
#
# BNDES a pre?os correntes (R$ milh?es )
#
############################################################

# checa se os dados n?o existem 

log4r_info("Carregando dados de Desembolsos do BNDES por setor econ?mico (pre?os de dezembro de 2020)...") 

## Fun??o para ler dados do BNDES do arquivo e retornar os dados como S?ries temporais e data frames 
# (as.pib=TRUE -> calcula os valores como percentual do pib)
get_bndes_data <- function(data_bndes, as.pib=FALSE){
  
  
  data <- list()
  
  data_bndes = t(data_bndes)
  
  
  old_names = data_bndes[1,]
  
  
  # new_names = old_names %>% str_replace_all(c("\\s+"=".","?" = "a",
  #                                             "?" = "e","?" = "i","?" = "u",
  #                                             "?"="a","?"="c","?"="a",
  #                                             "?"="e","?"="o","á"="a","é"="e",
  #                                             "ú"="u", "í"="i", "â"="a",
  #                                             "ç"="c","õ"="o","ã"="a"))
  # 
  # new_names = old_names %>% str_replace_all(c("\\s+"=".", "?"="u",
  #                                             "?"="a", "?"="e", "?"="i",
  #                                             "?"="c","?"="a","?"="e", "?"="a"))
  new_names = old_names %>% str_replace_all(c("\\s+"=".","á"="a","é"="e",
                                              "ú"="u", "í"="i", "â"="a",
                                              "ç"="c","õ"="o","ã"="a", "ê"="e"))
  
  colnames(data_bndes) = new_names
  
  data_bndes = data_bndes[-1,]
  
  data_bndes = as_tibble(data_bndes)
  
  data_bndes = sapply(data_bndes[,c(1:23)], as.numeric) %>% as_tibble()
  
  data_bndes = data_bndes %>% 
    dplyr::mutate(date = seq(as.Date("1995/01/01"), as.Date("2020/06/01"), by = "month")) %>%
    dplyr::select(date, everything()) %>% dplyr::filter(date > as.Date("1995-12-01"))
  
  
  # selecionar dados de desembolso do BNDES por setor
  desembolsos_df = data_bndes %>% 
    dplyr::select(Infraestrutura,Industria,Agropecuaria, Comercio.e.servicos, Total)
  
  # dados na forma de S?ries temporais
  desembolsos_ts <- ts(desembolsos_df, start = c(1996,01,01), frequency = 12)
  
  desembolsos_ts <- window(desembolsos_ts, 2002)
  
  # dados dessazonalizados
  desembolsos_ts_sa <- ts.intersect(seasonal::final(seas(desembolsos_ts[,1])),
                                   seasonal::final(seas(desembolsos_ts[,2])),
                                   seasonal::final(seas(desembolsos_ts[,3])),
                                   seasonal::final(seas(desembolsos_ts[,4])),
                                   seasonal::final(seas(desembolsos_ts[,5])))
  
  colnames(desembolsos_ts_sa) <- c('linfra','lagro','lind','lcomer','ltotal')
  
  desembolsos_ts_sa <- window(desembolsos_ts_sa, 2002)
  
  if (as.pib){
    
    # S?ries de desembolso (%PIB) - ts.obj
    despib_ts = desembolsos_ts/pib * 100
    
    # S?ries de desembolso (%PIB) dessazonalizadas - data.frame
    despib_df_sa = data.frame(desembolsos_ts) %>%
      dplyr::mutate(Infraestrutura_sa = seasonal::final(seas(desembolsos_ts[,'Infraestrutura']))/pib_sa *100) %>%
      dplyr::mutate(Agropecuaria_sa = seasonal::final(seas(desembolsos_ts[,'Agropecuaria']))/pib_sa*100)%>%
      dplyr::mutate(Industria_sa = seasonal::final(seas(desembolsos_ts[,'Industria']))/pib_sa*100 )%>%
      dplyr::mutate(Comercio_sa = seasonal::final(seas(desembolsos_ts[,'Comercio.e.servicos']))/pib_sa*100)%>%
      dplyr::mutate(Total_sa = seasonal::final(seas(desembolsos_ts[,'Total']))/pib_sa*100)%>%
      dplyr::mutate(date = data_bndes$date) %>%
      as_tibble()
    
    infra = ts(despib_df_sa$Infraestrutura_sa, start = c(1996,01,01), frequency = 12)
    agro = ts(despib_df_sa$Agropecuaria_sa, start = c(1996,01,01), frequency = 12)
    ind = ts(despib_df_sa$Industria_sa, start = c(1996,01,01), frequency = 12)
    comer = ts(despib_df_sa$Comercio_sa, start = c(1996,01,01), frequency = 12)
    tot = ts(despib_df_sa$Total_sa, start = c(1996,01,01), frequency = 12)
    
    despib_ts_sa = ts.intersect(infra,agro,ind,comer, tot)
    
    data[["pib_ts"]] <- despib_ts
    data[["pib_ts_sa"]] <- despib_ts_sa
  }
  
  data[["dataframe"]] <- desembolsos_df
  data[["ts"]] <- desembolsos_ts
  data[["ts_sa"]] <- desembolsos_ts_sa
  
  return(data)
  
}


# dados de desembolsos mensais a pre?os correntes (dezembro de 2020)
data_bndes = read_excel("dados/202012_SERIES_SETORIAIS.xlsx", 
                        sheet = 'Série III.A', trim_ws = TRUE, skip=0,
                        range="A11:KU33", col_names = FALSE, col_types = "guess")


# dados de desembolsos mensais acumulados a pre?os constantes de dezembro de 2020
data_bndes.acum = read_excel('dados/202012_SERIES_SETORIAIS.xlsx', 
                              sheet = 'Série III.D', trim_ws = TRUE, skip=0,
                              range="A11:KU33", col_names = FALSE, col_types = "guess")

# desembolsos mensais do BNDES (preços correntes de dezembro/2020)
data_bndes.m <- get_bndes_data(data_bndes)
data_bndes.m <- data_bndes.m$ts_sa

# desembolsos do BNDES acumulados em 12 meses (pre?os constantes de dezembro/2020)
data_bndes.acum <- get_bndes_data(data_bndes.acum)
data_bndes.acum <- data_bndes.acum$ts_sa



#' ############################################################################
#' @description Deflaciona os dados para dezembro de 2020 (FBCF, PIB, POUP)
#'
#'
log4r_info("Deflacionando dados para dezembro de 2020 (FBCF, PIB, POUP)...") 

# deflacionar S?ries com base no IPCA
ipca <- window(ipca, end=2020.95)

# fator de ajuste a ser aplicado a cada M?s
adj <- sapply(as.vector(ipca), function(x) 1/x * tail(ipca,1))

# FBCF mensal
fbcf_defl <- fbcf_sa * adj

# PIB mensal
pib_defl <- pib_sa * adj

# PIB acumulado em 12 meses
pib.acum_defl <- pib.acum * adj

# POUP mensal
poup_defl <- poup_sa * adj



#' ############################################################################
#' @description Constru??o da base de dados mensais e em log.
#'
#'
#######################################################################################
# BASE DE DADOS MENSAIS SEM DADOS DO IPEA
#######################################################################################
log4r_info("Construindo a base de dados com periodicidade mensal e em log...") 

data.m <- ts.intersect(fbcf_defl, pib_defl, poup_defl, nfsp_sa, ttr_sa, wprod, 
                       er_sa, ir, ibovespa_sa, embi_sa, data_bndes.m, data_fgv_ts, cib_sa)

### r?tulos para as S?ries
ser.names <- c("FBCF", "PIB","POUP","NFSP","TT","WP","ER","IR","IBOV","EMBI",
               "LINF","LAGR","LIND","LCOM","LTOT",colnames(data_fgv_ts),"CIB")

colnames(data.m) <- ser.names

# dados at? dezembro de 2019
data.m <- timeSeries::window(data.m, end = 2019.95)

write.csv(cbind(as.yearmon(time(data.m)), as.data.frame(data.m)), "dados/data.csv")

#############################################
# TRANSFORMA??O LOGAR?TMICA
#############################################
data.ln.m <- data.m
#data.ln.m.12 <- data.m.12
#vari?veis a serem logaritmizadas
# log.var <- c("FBCF", "PIB","POUP","NFSP","DBGG","ER","IR","IBOV","EMBI",
#                "LINF","LAGR","LIND","LCOM","LTOT","IIE", "ICI","ICE","CURI")

log.var <- ser.names

data.ln.m[,log.var] = suppressWarnings(na.fill(log(data.ln.m[,log.var]),0))

# para n?o perder dados da S?rie NFSP, deve ser utilizada a S?rie sem transforma??o
data.ln.m[,"NFSP"] <- data.m[,"NFSP"]

# para ser compar?vel ao modelo de barbosa e vasconcelos (2019)
data.ln.m[,"IR"] <- data.m[,"IR"]


#' ############################################################################
#' @description Constru??o da base de dados como percentual do PIB e com desembolsos acumulados.
#'
#'
log4r_info("Construindo a base de dados com periodicidade mensal, como percentual do PIB e 
           com desembolsos acumulados em 12 meses...") 

ser.names <- colnames(data.m[,-2])

# Forma??o Bruta de Capital Fixo como percentual do PIB (dados mensais)
fbcf.pib <- fbcf_defl/pib_defl * 100

# Poupan?acomo percentual do PIB (dados mensais)
poup.pib <- poup_defl/pib_defl * 100

# Desembolsos do BNDES acumulados como percentual do PIB (dados mensais)
data_bndes.pib <- data_bndes.acum/pib.acum_defl * 100

# Dados como percentual do PIB (desembolsos acumulados em 12 meses)
data.acum.pib <- ts.intersect(fbcf.pib, poup.pib, nfsp_sa, ttr_sa, wprod, 
                       er_sa, ir, ibovespa_sa, embi_sa, data_bndes.pib, data_fgv_ts, cib_sa)
colnames(data.acum.pib) <- ser.names


# para adequar a escala do IBOVESPA aos dados, temos IBOV em mil pontos
data.acum.pib[,"IBOV"] <- data.acum.pib[,"IBOV"]/1000

# para adequar a escala do EMBI aos dados, temos EMBI em mil pontos
data.acum.pib[,"EMBI"] <- data.acum.pib[,"EMBI"]/1000

# limita os dados entre outubro de 2002 e dezembro de 2019
data.acum.pib <- window(data.acum.pib,end=2019.95)


## ----data-PIB------------------------------------------------------------------------------------------------------
# Se formos utilizar os dados como percentual do PIB e sem ser em log.
# ser.names <- colnames(data.m)
# 
# data <- sapply(data.m[,c("FBCF","POUP","LINF","LAGR","LIND","LCOM","LTOT")], 
#                      function(x) x/data.m[,"PIB"] * 100)
# 
# data.bndes <- data.m[,c("LINF","LAGR","LIND","LCOM","LTOT")]
# 
# data.bndes <- 
# 
# data.bndes <- to_year_acum(data.bndes)
# 
# data <- sapply(data.bndes[,c("LINF","LAGR","LIND","LCOM","LTOT")], 
#                      function(x) x/window(data.m[,"PIB"], start=2003.75) * 100)
# 
# 
# # Se formos utilizar os dados de desembolsos acumulados em 12 meses.
# data <- to_year_acum(data.m[,c("PIB","LINF","LAGR","LIND","LCOM","LTOT")])
# 
# data <- sapply(data[,c("LINF","LAGR","LIND","LCOM","LTOT")], 
#                      function(x) x/data[,"PIB"] * 100)
# # exclui PIB
# data.m12pib <- data[,-2]
# 
# data.m12pib[,c("FBCF","POUP","LINF","LAGR","LIND","LCOM","LTOT")] <- data[,c("FBCF","POUP","LINF","LAGR","LIND","LCOM","LTOT")]
# 
# # para adequar a escala do IBOVESPA aos dados, temos IBOV em mil pontos
# data.m12pib[,"IBOV"] <- data.m12pib[,"IBOV"]/1000
# 
# # para adequar a escala do EMBI aos dados, temos EMBI em mil pontos
# data.m12pib[,"EMBI"] <- data.m12pib[,"EMBI"]/1000


#data.pib.ln <- log(data.pib)

#data.pib.ln[,"NFSP"] <- data.pib[,"NFSP"]
# data.yr <- to_year_acum(data.m)
 
# data.ln.yr <- log(data.yr)
# 
# # para n?o perder dados da S?rie NFSP, deve ser utilizada a S?rie sem transforma??o
# data.ln.yr[,"NFSP"] <- data.yr[,"NFSP"]
# 
# data.ln.yr[,"IR"] <- data.yr[,"IR"]


#autoplot(data.pib.ln[,c("FBCF","POUP","NFSP","LINF","LAGR","LIND","LCOM","LTOT")])



## ----data-PIB-1----------------------------------------------------------------------------------------------------
# Se formos utilizar os dados como percentual do PIB e sem ser em log.
# ser.names <- colnames(data.m)
# 
# 
# data <- sapply(data.m[,c("FBCF","POUP","LINF","LAGR","LIND","LCOM","LTOT")], 
#                      function(x) x/data.m[,"PIB"] * 100)
# # exclui PIB
# data.pib <- data.m[,-2]
# 
# data.pib[,c("FBCF","POUP","LINF","LAGR","LIND","LCOM","LTOT")] <- data[,c("FBCF","POUP","LINF","LAGR","LIND","LCOM","LTOT")]
# 
# # para adequar a escala do IBOVESPA aos dados, temos IBOV em mil pontos
# data.pib[,"IBOV"] <- data.pib[,"IBOV"]/1000
# 
# # para adequar a escala do EMBI aos dados, temos EMBI em mil pontos
# data.pib[,"EMBI"] <- data.pib[,"EMBI"]/1000


#data.pib.ln <- log(data.pib)

#data.pib.ln[,"NFSP"] <- data.pib[,"NFSP"]
# data.yr <- to_year_acum(data.m)
 
# data.ln.yr <- log(data.yr)
# 
# # para n?o perder dados da S?rie NFSP, deve ser utilizada a S?rie sem transforma??o
# data.ln.yr[,"NFSP"] <- data.yr[,"NFSP"]
# 
# data.ln.yr[,"IR"] <- data.yr[,"IR"]


#autoplot(data.pib.ln[,c("FBCF","POUP","NFSP","LINF","LAGR","LIND","LCOM","LTOT")])



#' ############################################################################
#' @description Salva as bases de dados em disco.
#'
#'
log4r_info("Salvando bases de dados em disco - arquivo 'dados.RData' ...") 
# reorganiza colunas em ordem alfab?tica, com FBCF ocupando a primeira coluna
data.ln.m <- data.ln.m[,c("FBCF",sort(colnames(data.ln.m)[2:ncol(data.ln.m)]))]

#data.ln.m12 <- data.ln.m12[,c("FBCF",sort(colnames(data.ln.m12)[2:ncol(data.ln.m12)]))]

#data.pib <- data.pib[,c("FBCF",sort(colnames(data.pib)[2:ncol(data.pib)]))]

data.acum.pib <- data.acum.pib[,c("FBCF",sort(colnames(data.acum.pib)[2:ncol(data.acum.pib)]))]

# Elimina os objetos desnecess?rios para a continuidade da an?lise
#rm(list = ls()[!(ls() %in% c('data.acum.pib','data.ln.m', 'data.m'))])

# salva dados em disco
save.image("dados.RData")


