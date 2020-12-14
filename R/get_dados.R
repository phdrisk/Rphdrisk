#' FUNCAOO PHDRISK DE SINALIZACAO DE NEGOCIOS
#' 
#' GET SYMBOL
#' 
#' @param carteira base de dados
#' @param from data inicial
#' @param to data final
#' @param auto auto
#' 
#' @import quantmod
#'
#' @export
getDados <- function(carteira,from,to,auto = FALSE){
  
  return (quantmod::getSymbols(carteira, from=from, to=to, auto.assign=auto))
}