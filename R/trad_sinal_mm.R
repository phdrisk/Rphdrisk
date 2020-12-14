#' TRADE SINAL
#' 
#' @param ativo VETORES
#' @param media1 VETORES
#' @param media2 VETORES
#' @param plotar plotar/dados
#'
#' @import quantmod
#' @import xts
#' @importFrom stats cov
#' @importFrom TTR SMA
#'  
#' @examples   
#' inicial <- Sys.Date() - 180
#' ultima <- Sys.Date()
#' ativo <- c("BOBR4.SA")
#' at<-quantmod::getSymbols(ativo, from=inicial, to=ultima, auto.assign=F)
#' head(at)
#'  
#'  
#' @export
tradeSinalMedia<- function(ativo, media1, media2,plotar=TRUE) {
  # Criando Indicadores Lead and Lag
  # a. Simple Moving Average (SMA)
  # 1. ativo
 
  
  sma20_ativo <- SMA(Ad(ativo), n = media1)
  sma50_ativo <- SMA(Ad(ativo), n = media2)
  
  # Creating Trading signal with Indicators
  # SMA 
  # a. ativo
  # SMA 20 Crossover Signal 
  sma20_ativo_ts <- Lag(
    ifelse(Lag(Ad(ativo)) < Lag(sma20_ativo) & Ad(ativo) > sma20_ativo,1,
           ifelse(Lag(Ad(ativo)) > Lag(sma20_ativo) & Ad(ativo) < sma20_ativo,-1,0)))
  sma20_ativo_ts[is.na(sma20_ativo_ts)] <- 0
  
  # SMA 50 Crossover Signal
  sma50_ativo_ts <- Lag(
    ifelse(Lag(Ad(ativo)) < Lag(sma50_ativo) & Ad(ativo) > sma50_ativo,1,
           ifelse(Lag(Ad(ativo)) > Lag(sma50_ativo) & Ad(ativo) < sma50_ativo,-1,0)))
  sma50_ativo_ts[is.na(sma50_ativo_ts)] <- 0
  
  
  # SMA 20 and SMA 50 Crossover Signal
  sma_ativo_ts <- Lag(
    ifelse(Lag(sma20_ativo) < Lag(sma50_ativo) & sma20_ativo > sma50_ativo,10,
           ifelse(Lag(sma20_ativo) > Lag(sma50_ativo) & sma20_ativo < sma50_ativo,-10,0)))
  sma_ativo_ts[is.na(sma_ativo_ts)] <- 0
  
  
  ##RESULTADO 1: GRAFICO SMA DE ALERTA DE COMPRA VENDA
  sinal<-merge.xts(Ad(ativo), sma_ativo_ts)
  if(sum(is.na(sinal))>0)
    nan[is.na(sinal)] <- 0
  
  #colnames(sinal) <- c("Valor Ajustado","Largura")
  
  if(plotar){
    
    plot(sinal, main="Grafico SMA de Alertas de Compra e Venda")
    addLegend("topleft", on=1, 
              legend.names = names(sinal), 
              lty=c(1, 1), lwd=c(2, 1))
    
  }else{
    return (sinal)
  }

}

