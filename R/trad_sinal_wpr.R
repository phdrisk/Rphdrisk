#' FUNCAOO PHDRISK DE SINALIZACAO DE NEGOCIOS
#' 
#' TRADE SINAL WPR
#' 
#' @param ativo base de dados
#' @param plotar TRUE/FALSE
#' @param n INDEFINIDO
#' 
#' @import quantmod
#' @import xts
#' @importFrom stats cov
#' @importFrom TTR WPR
#'
#' @export 

tradeSinalWpr<- function(ativo,n,plotar=TRUE) {

  # f. Williams %R
  # 1. ativo
  wpr_ativo <- WPR(HLC(ativo), n = n)
  colnames(wpr_ativo) <- 'wpr'
  # 6. williams %R
  # a. ativo
  wpr_ativo_ts <- Lag(
    ifelse(Lag(wpr_ativo) > 0.8 & wpr_ativo < 0.8,-10,
           ifelse(Lag(wpr_ativo) > 0.2 & wpr_ativo < 0.2,10,0)))
  wpr_ativo_ts[is.na(wpr_ativo_ts)] <- 0
  
  ##RESULTADO 6: GRAFICO W%R DE ALERTA DE COMPRA VENDA
  sinal6<-merge.xts(Ad(ativo), wpr_ativo_ts)
  sinal6[is.na(sinal6)] <- 0
  if(plotar){
    plot(sinal6, main="GrAfico W%R-Especial de Alertas de Compra e Venda")
    addLegend("topleft", on=1, 
              legend.names = names(sinal6), 
              lty=c(1, 1), lwd=c(2, 1))
  }else{
    return (sinal6)
    
  }
}
