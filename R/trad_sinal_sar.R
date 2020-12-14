#' FUNCAOO PHDRISK DE SINALIZACAO DE NEGOCIOS
#' 
#' TRADE SINAL SAR
#' 
#' @param ativo base de dados
#' @param plotar TRUE/FALSE
#' 
#' @import quantmod
#' @import xts
#' @importFrom stats cov
#' @importFrom TTR SAR
#' 
#' @export
tradeSinalSar<- function(ativo,plotar=TRUE) {

  # Parabolic Stop And Reverse (SAR)
  # ativo
  sar_ativo <- SAR(cbind(Hi(ativo),Lo(ativo)), accel = c(0.02, 0.2))
  
  # Parabolic Stop And Reverse (SAR) 
  # a. ativo
  sar_ativo_ts <- Lag(
    ifelse(Lag(Ad(ativo)) < Lag(sar_ativo) & Ad(ativo) > sar_ativo,10,
           ifelse(Lag(Ad(ativo)) > Lag(sar_ativo) & Ad(ativo) < sar_ativo,-10,0)))
  sar_ativo_ts[is.na(sar_ativo_ts)] <- 0
  
  ##RESULTADO 2: GRAFICO SAR DE ALERTA DE COMPRA VENDA
  sinal2<-merge.xts(Ad(ativo), sar_ativo_ts)
  
  sinal2[is.na(sinal2)] <- 0
  #colnames(sinal2) <- c("ValorAjustado","Largura")
  
  if(plotar){
    plot(sinal2, main="Grafico SAR de Alertas de Compra e Venda")
    addLegend("topleft", on=1, 
              legend.names = names(sinal2), 
              lty=c(1, 1), lwd=c(2, 1))
  }else{
    return (sinal2)
    
  }
  
}
