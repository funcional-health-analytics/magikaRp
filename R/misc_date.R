
#' @title Identifica tipo de data
#'
#' @description Identifica e transforma uma string de data para um formato data para o lubriate
#'
#' @param dados Um objeto \code{data.table} que tenha alguma coluna de strings de datas
#' @param colun_data Uma string com o nome da coluna em que está armazenada a data
#' @param conferir Um \code{logical} para conferir se há dois tipos diferentes de datas na mesma coluna. Se sim a função para. DEFAULT: T
#'
#' @import data.table
#'
#' @return Nada. Altera INPLACE a coluna do \code{data.table} especificada
#'
#' @export

misc_date <- function(dados,
                      coluna_data,
                      conferir = T){

  requireNamespace('data.table')


  eval(
    parse(
      text = paste0(
        "dados[, ", coluna_data,
        " := lubridate::as_date(lubridate::parse_date_time(",
        coluna_data,
        ",orders = c('Ymd', 'Ydm', 'mYd', 'dYm', 'dmy','mdy')))]"
        )
      )
    )

}









