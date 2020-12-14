#' FUNCAOO PHDRISK DE SINALIZACAO DE NEGOCIOS
#' 
#' TRADE SINAL SMI
#' 
#' @param ativo base de dados
#' @param plotar TRUE/FALSE
#' @param n INDEFINIDO
#' 
#' @import quantmod
#' @import xts
#' @importFrom stats cov
#' @importFrom TTR SMI
#'
#' @export

tradeSinalSmi<- function(ativo,n,plotar=TRUE) {

  # e. Stochastic Momentum Index (SMI)
  # 1. ativo
  smi_ativo <- SMI(HLC(ativo),
                   n = n, nFast = 2, nSlow = 25, nSig = 9)
    # 5. Stochastic Momentum Index (SMI)
  # a. ativo
  smi_ativo_ts <- Lag(
    ifelse(Lag(smi_ativo[,1]) < Lag(smi_ativo[,2]) & smi_ativo[,1] > smi_ativo[,2],10, 
           ifelse(Lag(smi_ativo[,1]) > Lag(smi_ativo[,2]) & smi_ativo[,1] < smi_ativo[,2],-10,0)))
  smi_ativo_ts[is.na(smi_ativo_ts)] <- 0
  
  ##RESULTADO 5: GRAFICO SMI DE ALERTA DE COMPRA VENDA
  sinal5<-merge.xts(Ad(ativo), smi_ativo_ts)
  sinal5[is.na(sinal5)] <- 0
  if(plotar){
    plot(sinal5, main="GrAfico SMI de Alertas de Compra e Venda")
    addLegend("topleft", on=1, 
              legend.names = names(sinal5), 
              lty=c(1, 1), lwd=c(2, 1))
  }else{
    return (sinal5);
  }
  }
