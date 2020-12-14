#' FUNCAOO PHDRISK DE SINALIZACAO DE NEGOCIOS
#' 
#' TRADE SINAL ROC
#' 
#' @param ativo base de dados
#' @param plotar TRUE/FALSE
#' @param n INDEFINIDO
#' 
#' @import quantmod
#' @import xts
#' @importFrom stats cov
#' @importFrom TTR ROC
#'
#' @export
tradeSinalRoc<- function(ativo,n,plotar=TRUE) {

  # d. Rate of Change (ROC)
  # 1. ativo
  roc_ativo <- ROC(Ad(ativo), n = n)
  # 4. Rate of Change (ROC)
  # a. ativo1
  roc_ativo_ts <- Lag(
    ifelse(Lag(roc_ativo) < (-0.05) & roc_ativo > (-0.05),-10,
           ifelse(Lag(roc_ativo) < (0.05) & roc_ativo > (0.05),10,0)))
  roc_ativo_ts[is.na(roc_ativo_ts)] <- 0
  
  ##RESULTADO 4: GRAFICO SMI DE ALERTA DE COMPRA VENDA
  sinal4<-merge.xts(Ad(ativo), roc_ativo_ts)
  sinal4[is.na(sinal4)] <- 0
  if(plotar){
    plot(sinal4, main="Grafico ROC de Alertas de Compra e Venda")
    addLegend("topleft", on=1, 
              legend.names = names(sinal4), 
              lty=c(1, 1), lwd=c(2, 1))
  }else{
    return (sinal4)
  }
  
}