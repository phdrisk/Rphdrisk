#' FUNCAO PHDRISK DE SINALIZACAO DE NEGOCIOS
#' 
#' TRADE SINAL ESPECIAL
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

tradeEspecial<- function(ativo,n,plotar=TRUE) {

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
  # a. ativo
  smi_ativo_strat <- ifelse(smi_ativo_ts > 1,10,0)
  for (i in 1 : length(Ad(ativo))) {
    smi_ativo_strat[i] <- ifelse(smi_ativo_ts[i] == 1,1,ifelse(smi_ativo_ts[i] == -1,0,smi_ativo_strat[i-1]))
  }
  smi_ativo_strat[is.na(smi_ativo_strat)] <- 1
  smi_ativo_stratcomp <- cbind(smi_ativo[,1],smi_ativo[,2],smi_ativo_ts,smi_ativo_strat)
  colnames(smi_ativo_stratcomp)[2] <- c('SMI POSITION')
  
  if(plotar){
    plot(smi_ativo_stratcomp, main = "Estrategia Especial PHDRISK")
    addLegend("topleft", on=1, 
              legend.names = names(smi_ativo_stratcomp), 
              lty=c(1, 1), lwd=c(2, 1))
  }else{
    return(smi_ativo_stratcomp)
    
  }
}
