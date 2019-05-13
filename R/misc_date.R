month_costum <- function(x, divisor = "/"){
  x[2] <- paste0("0",x[2])
  x <- paste0(x, collapse = divisor)
  x
}

insert_zero_month <- function(x, divisor = "/"){
  x <- str_split(x, pattern = divisor)
  return(unlist(lapply(x, magikaRp:::month_costum, divisor = divisor)))
}



#' @title Identifica tipo de data
#'
#' @description Identifica e transforma uma string para lubridate data (que use '/' ou '-') INPLACE nos formatos
#' YYYY-mm-dd, YYYY-dd-mm, YYYY-m-d, YYYY-d-m, mm-dd-YYYY, m-d-YYYY, dd-mm-YYYY, d-m-YYYY
#' YYYY/mm/dd, YYYY/dd/mm, YYYY/m/d, YYYY/d/m, mm/dd/YYYY, m/d/YYYY, dd/mm/YYYY, d/m/YYYY. E combinações entre mm, m, dd, d.
#'
#' @param dados Um objeto \code{data.table} que tenha alguma coluna de strings de datas
#' @param colun_data Uma string com o nome da coluna em que está armazenada a data
#' @param conferir Um \code{logical} para conferir se há dois tipos diferentes de datas na mesma coluna. Se sim a função para. DEFAULT: T
#'
#' @import stringr data.table
#'
#' @return Nada. Altera INPLACE a coluna do \code{data.table} especificada
#' @export

misc_date <- function(dados,
                      coluna_data,
                      conferir = T){

  requireNamespace('stringr')
  requireNamespace('data.table')


  eval(parse(text = paste0("dados[, ", coluna_data, " := stringr::str_replace_all(", coluna_data, ", pattern = '![0-9|/|-]', replacement = '')]")))

  #  verificador_barra <- eval(parse(text = paste0("grepl(dados$", coluna_data,", pattern = '/')" )))
  #  verificador_traco <- eval(parse(text = paste0("grepl(dados$", coluna_data,", pattern = '-')" )))

  eval(parse(text = paste0("dados[, ", coluna_data," := stringr::str_replace_all(",coluna_data,", pattern = '/', replacement = '-')]")))

  eval(parse(text = paste0("dados[grepl(pattern = '^[0-9]-', ",
                           "x = ",coluna_data, "),",
                           coluna_data, " := paste0( '0', ", coluna_data, ")]"
                           )
             )
       )

  eval(parse(text = paste0("dados[grepl(pattern = '-[0-9]-', ",
                           "x = ",coluna_data, "),",
                           coluna_data, " := magikaRp:::insert_zero_month( ", coluna_data, ", divisor = '-')]"
                           )
             )
       )


  primeiro <- as.integer(stringr::str_extract(string = eval(parse(text = paste0("dados$",coluna_data))),pattern = "^[0-9]+"))
  max_primeiro <- max(primeiro, na.rm = T)

  segundo <- stringr::str_extract(string = eval(parse(text = paste0("dados$",coluna_data))),pattern = "-[0-9]+-")
  segundo <- as.integer(stringr::str_extract(string = segundo, pattern = "[0-9]+"))
  max_segundo  <- max(segundo, na.rm = T)

  terceiro <- stringr::str_extract(string = eval(parse(text = paste0("dados$",coluna_data))),pattern = "-[0-9]+$")
  terceiro <- as.integer(stringr::str_remove_all(string = terceiro, pattern = "-"))
  max_terceiro  <- max(terceiro, na.rm = T)



  if(conferir){
    if(max_primeiro > 1000){
      if(max_segundo >= 20 & max_terceiro >= 20){
        stop("data dupla")
      }
    }else{
      if(max_segundo >= 20 & max_primeiro >= 20){
        stop("data dupla")
      }
    }
  }

  if(max_primeiro > 1000){
    n_primeiro_d <- sum(segundo > 12, na.rm = T)
    n_segundo_d  <- sum(terceiro > 12, na.rm = T)

    if(!(n_primeiro_d == 0 | n_segundo_d == 0)){
      if(n_primeiro_d <= n_segundo_d){
        b_p_12 <- primeiro > 12
        b_p_12[is.na(b_p_12)] <- F


        dia <- substr(eval(parse(text = paste0("dados$", coluna_data,"[b_p_12]"))), 6, 7)
        mes <- substr(eval(parse(text = paste0("dados$", coluna_data,"[b_p_12]"))), 9, 10)
        ano <- substr(eval(parse(text = paste0("dados$", coluna_data,"[b_p_12]"))), 1, 4)

        eval(parse(text = paste0("dados[b_p_12,", coluna_data," := paste0(dia,'-', mes,'-', ano)]")))
      }else{
        b_s_12 <- segundo > 12
        b_s_12[is.na(b_s_12)] <- F


        dia <- substr(eval(parse(text = paste0("dados$", coluna_data,"[b_s_12]"))), 9, 10)
        mes <- substr(eval(parse(text = paste0("dados$", coluna_data,"[b_s_12]"))), 6, 7)
        ano <- substr(eval(parse(text = paste0("dados$", coluna_data,"[b_s_12]"))), 1, 4)

        eval(parse(text = paste0("dados[b_p_12,", coluna_data," := paste0(mes,'-', dia,'-', ano)]")))
      }

    }
  }else{
    n_primeiro_d <- sum(primeiro > 12, na.rm = T)
    n_segundo_d  <- sum(segundo > 12, na.rm = T)

    if(!(n_primeiro_d == 0 | n_segundo_d == 0)){
      if(n_primeiro_d <= n_segundo_d){
        b_p_12 <- primeiro > 12
        b_p_12[is.na(b_p_12)] <- F



        dia <- substr(eval(parse(text = paste0("dados$", coluna_data,"[b_p_12]"))), 1, 2)
        mes <- substr(eval(parse(text = paste0("dados$", coluna_data,"[b_p_12]"))), 4, 5)
        ano <- substr(eval(parse(text = paste0("dados$", coluna_data,"[b_p_12]"))), 7, 10)

        eval(parse(text = paste0("dados[b_p_12,", coluna_data," := paste0(dia,'-', mes,'-', ano)]")))
      }else{
        b_s_12 <- segundo > 12
        b_s_12[is.na(b_s_12)] <- F


        dia <- substr(eval(parse(text = paste0("dados$", coluna_data,"[b_s_12]"))), 4, 5)
        mes <- substr(eval(parse(text = paste0("dados$", coluna_data,"[b_s_12]"))), 1, 2)
        ano <- substr(eval(parse(text = paste0("dados$", coluna_data,"[b_s_12]"))), 7, 10)

        eval(parse(text = paste0("dados[b_p_12,", coluna_data," := paste0(mes,'-', dia,'-', ano)]")))
      }

    }
  }



  if(max_primeiro > 1000){
    if(max_segundo >12 & max_terceiro <= 12){
      data_pattern <- "%Y-%d-%m"
    }else{
      data_pattern <- "%Y-%m-%d"
    }
  }else{
    if(max_segundo > 12  & max_primeiro <= 12){
      data_pattern <- "%m-%d-%Y"
    }else{
      data_pattern <- "%d-%m-%Y"
    }
  }




  eval(parse(text = paste0("dados[, ", coluna_data, " := lubridate::as_date(strptime(", coluna_data,",format = '",data_pattern,"'))]")))

}









