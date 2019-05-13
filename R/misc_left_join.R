pull_right <- function(columns,
                       exceptions = NULL){
  if(is.null(exceptions)){
    stop("Exceptions deve ser as colunas usadas como Key na tabela da direita")
  }
  string <- paste( "`:=`(",paste(columns, columns, sep = "=", collapse = ","), ")")
  return(parse(text = string))
}

#' @title Left Join INPLACE com data.table
#'
#' @description  Realiza um Left Join INPLACE entre dois data.table por uma ou mais chaves
#'
#' @param dados_left Um objeto \code{data.table} que será o lado esquerdo da operação e será alterado INPLACE
#' @param dados_right Um objeto \code{data.table} que será o lado direito da operação, ele se manterá da mesma forma (com um index a mais)
#' @param columns_to_pull Um vetor de \code{string} contendo o nome das colunas que vc gostaria de puxar de dados_right.
#' Caso não seja especificado, será puxado todas.
#' @param columns_not_to_pull Um veto de \code{string} contendo o nome de colunas que vc não gostaria de puxar
#' @param by Um vetor nomeado indicando quais as colunas ID que será feito o Left Join. \code{by = c('lhs' = 'rhs')}
#'
#' @return Nada. A operação resultará na alteração INPLACE de \code{dados_left}
#'
#' @import data.table
#'
#' @export

misc_left_join <- function(dados_left,
                           dados_right,
                           columns_to_pull     = NULL,
                           columns_not_to_pull = NULL,
                           by){

  if(is.null(columns_to_pull)){
    columns_to_pull <- colnames(dados_right)[!colnames(dados_right) %in% by]
  }
  if(is.null(columns_not_to_pull)){
    columns_not_to_pull <- by
  }else{
    columns_not_to_pull <- c(columns_not_to_pull, by)
  }

  lhs_index <- paste0(names(by))

  eval(
    parse(
      text = paste0(
        "data.table::setindex(dados_left,'", paste0(names(by), collapse = "','"),"' )"
      )
    )
  )

  eval(
    parse(
      text = paste0(
        "data.table::setindex(dados_right,'", paste0(by, collapse = "','"),"' )"
      )
    )
  )


  #data.table::setindex(dados_left, names(by))
  #data.table::setindex(dados_left, by)

  dados_left[dados_right,
             eval(magikaRp:::pull_right(columns    = columns_to_pull,
                                        exceptions = columns_not_to_pull)
                  ),
             on = by
             ]
}
