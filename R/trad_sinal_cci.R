#' FUNCAOO PHDRISK DE SINALIZACAO DE NEGOCIOS
#' 
#' TRADE SINAL SAR
#' 
#' @param ativo base de dados
#' @param plotar TRUE/FALSE
#' @param n INDEFINIDO
#' 
#' @import quantmod
#' @import xts
#' @importFrom stats cov
#' @importFrom TTR CCI
#'
#' @export 

tradeSinalCci<- function(ativo,n,plotar=TRUE) {
  # c. Commodity Channel Index (CCI)
  # 1. ativo
  cci_ativo <- CCI(HLC(ativo), n = n, c = 0.015)
  # 3. Commodity Channel Index  (CCI)
  # a. ativo
  cci_ativo_ts <- Lag(
    ifelse(Lag(cci_ativo) < (-100) & cci_ativo > (-100),-10,
           ifelse(Lag(cci_ativo) < (100) & cci_ativo > (100),10,0)))
  cci_ativo_ts[is.na(cci_ativo_ts)] <- 0
  
  ##RESULTADO 3: GRAFICO CCI DE ALERTA DE COMPRA VENDA
  sinal3<-merge.xts(Ad(ativo), cci_ativo_ts)
  sinal3[is.na(sinal3)] <- 0
  
  if(plotar){
    plot(sinal3, main="Grafico CCI-Especial de Alertas de Compra e Venda")
    addLegend("topleft", on=1, 
              legend.names = names(sinal3), 
              lty=c(1, 1), lwd=c(2, 1))
  }else{
    return(sinal3)
    
  }
}
