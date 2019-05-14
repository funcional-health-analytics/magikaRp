one_hot. <- function(dados, id_var, coded_var){
  tmp <- eval(parse(text = paste0("dados[, .(", id_var, ",", coded_var, ")]")))

  if(length(unique(eval(parse(text = paste0("dados$",id_var))))) < nrow(dados)){
    cat(paste0("\t \t O numero de id_var unico é menor que nrow(dados) será
               \t \t feito  tmp <- tmp[!duplicated(tmp)], em que,
               \t \t  tmp <- ",paste0("dados[, .(", id_var, ",", coded_var, ")] \n")))
    tmp <- tmp[!duplicated(tmp)]
  }
  melted  <- data.table::melt(tmp, id = id_var, value.factor = F, na.rm=TRUE)
  one_hot_ <- data.table::dcast(melted, eval(parse(text = paste0(id_var, " ~ value")) ),
                                drop = T, fun.aggregate = length)
  return(one_hot_)
}


#' @title Onr hot com data.table
#'
#' @description O objetivo desta função é, por meio de um objeto data.table com uma coluna identificadora do indivíduo
#' e uma variável categórica de interesse, criar um one_hot encoding.
#'
#' @param dados Um objeto \code{data.table}
#' @param var_id Uma \code{string} contendo o nome da variável identificadora
#' @param var_categorica_alvo Uma \code{string} com o nome da variável alvo. A variável que vc quer ver o one_hot
#'
#' @return Um objeto \code{data.table} com o Id e colunas para cada nível da categoria alvo
#'
#' @import data.table
#'
#' @export

misc_one_hot <- function(dados,
                         var_id,
                         var_categorica_alvo){

  tmp <- eval(parse(text = paste0("dados[, list(", var_id, ",",
                                                   var_categorica_alvo,
                                        ")]"
                                  )
                    )
              )
  if(eval(parse(text = paste0("is.character(tmp$",var_categorica_alvo,")")))){
    eval(
      parse(
        text = paste0(
          "tmp[is.na(",  var_categorica_alvo, "), ", var_categorica_alvo, " := '", var_categorica_alvo, "_NA']"
          )
        )
      )
  }else{
    eval(
      parse(
        text = paste0(
          "tmp <- tmp[!is.na(", var_categorica_alvo, ")]"
        )
      )
    )
  }

  tmp <- tmp[!duplicated(tmp)]


  result <- magikaRp:::one_hot.(dados = tmp,
                                id_var = var_id,
                                coded_var = var_categorica_alvo
                                )
  return(result)

}
