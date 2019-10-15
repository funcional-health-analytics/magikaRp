j_count <- function(coluna_alvo,
                    vetor_niveis_unicos,
                    nome_colunas){
  if(length(coluna_alvo) != 1 | !is.character(coluna_alvo)){
    stop("Coluna_alvo deve ser o nome, em character, da coluna que pretende calcular algo.")
  }
  if(base::is.character(vetor_niveis_unicos)){
    calculo <- paste0("sum( ", coluna_alvo, " == '", vetor_niveis_unicos, "')")
  }else{
    calculo <- paste0("sum( ", coluna_alvo, " ==  ", vetor_niveis_unicos, ")")
  }

  string <- paste( "list(",paste(nome_colunas,
                              calculo,
                              sep = " = ",
                              collapse = ", "), ")")
  return(parse(text = string))
}

#' @title Contagem de eventos por um ID
#'
#' @description Dado uma variável de evento, um ID e, opcionalmente, uma variável tempora. Calcula-se quantas vezes um evento ocorrou para determinado ID.
#' Caso tenha uma variável tempora será contado quantas unidades de tempo tiveram o evento do tipo (se houver dois eventos numa mesma unidade de tempo será contado como apenas um)
#'
#' @param dados Um objeto \code{data.table} que tenha pelo menos uma coluna identificando o ID do indivíduo e uma coluna com o evento
#' @param var_id Tipo \code{string} com o nome da coluna que identifica o indivíduo, um ID
#' @param var_temporal Tipo \code{string} com o nome da coluna que representa o ponto no tempo em que o evento ocorreu. DEFAULT: NULL
#' @param var_categorica_alvo Tipo \code{string} como o nome da coluna alvo, que possui os eventos
#' @param subset_niveis_cat_alvo Tipo vetor de com o tipo de sua variável. Caso queira um subset de niveis da categoria alvo os inform aqui
#'
#' @return Um objeto \code{data.table} Com a contagem de cada niveo para cada ID num formato WIDE.
#'
#' @import data.table
#'
#' @export

misc_contagem_niveis <- function(dados,
                                 var_id,
                                 var_temporal = NULL,
                                 var_categorica_alvo,
                                 subset_niveis_cat_alvo = NULL
                                 ){
  requireNamespace("data.table")

  if(is.null(var_temporal)){
    tmp <- eval(parse(text = paste0("dados[, list(", var_id,",",
                                    var_categorica_alvo, ")]")
                      )
                )
  }else{
    tmp <- eval(parse(text = paste0("dados[, list(", var_id,",",
                                    var_temporal, ",",
                                    var_categorica_alvo, ")]")
                      )
                )
    tmp <- tmp[!duplicated(tmp)]
  }

  eval(parse(text = paste0("data.table::setkey(tmp, ", var_id, ")")))
  niveis_unicos <- eval(parse(text = paste0("levels(as.factor(dados$", var_categorica_alvo, "))")))
  if(is.null(subset_niveis_cat_alvo)){
    nome_colunas. <- paste0(var_categorica_alvo, "_", niveis_unicos)
    result <- tmp[,
                  eval(magikaRp:::j_count(coluna_alvo         = var_categorica_alvo,
                                          vetor_niveis_unicos = niveis_unicos,
                                          nome_colunas        = nome_colunas.)
                       ),
                  by = c(var_id)
                  ]
  }else{
    nome_colunas. <- paste0(sort(subset_niveis_cat_alvo), "_", niveis_unicos)
    result <- tmp[,
                  eval(magikaRp:::j_count(coluna_alvo         = var_categorica_alvo,
                                          vetor_niveis_unicos = niveis_unicos,
                                          nome_colunas        = nome_colunas.)
                       ),
                  by = c(var_id)
                  ]
  }

  return(result)
}
