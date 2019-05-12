



reldtMtd <- function(dados) {
  requireNamespace('data.table')
  requireNamespace('xgboost')

  melt(dados, id.vars="id")[,
                            .N, by=.(id, value)
                            ][,
                              value[which.max(N)], by=.(id)
                              ]
}

#' @title xgb_predict_ensemble
#'
#' @description Esta função tem como objetivo carregar os \code{n} modelos criados pela função \code{xgb_treino_ensemble}, aplicá-los aos dados criando a distribuição de previsão.
#'Após isso, caso seja regressão ou o retorno de uma probabilidade de classe, a previsão será a mediana, caso contrário a moda.
#'
#' @param folder Pasta onde estão armazenados os \code{n} modelos retornados função \code{xgb_treino_ensemble}
#' @param newdata Um objeto \code{data.table} com os novos dados para aplicação
#' @param classe Um indicador do que é retornado pelo modelo. Se uma classe ou um número contínuo: DEFALUT: F
#'
#' @import magrittr data.table xgboost
#'
#' @return Retonar um objeto \code{data.table} com uma coluna nomeada \code{predicao}
#'
#' @export


xgb_predict_ensemble <- function(folder, newdata, classe = F){


  requireNamespace('data.table')
  requireNamespace('xgboost')
  requireNamespace('magrittr')



  lista_modelos <- list.files(folder,
                              pattern    = "*.xgb$",
                              all.files  = F,
                              full.names = F)
  modelos_xgboost <- list()
  length(modelos_xgboost) <- length(lista_modelos)
  for(i in 1:length(lista_modelos)){
    modelos_xgboost[[i]] <- xgb.load(paste0(folder,"/",lista_modelos))
  }


  multiple_models_dados <- matrix(NA, ncol = length(modelos_xgboost), nrow = nrow(newdata))
  for(i in 1:length(modelos_xgboost)){
    cat(paste0("Modelo: ", i," de ", length(modelos_xgboost),". \r"))
    code <- paste0("V", i,"<- predict(modelos_xgboost[[i]],
                   newdata = newdata[,
                   colnames(newdata)[!colnames(newdata) %in% c('target', 'fold')],
                   with = F
                   ] %>%
                   as.matrix())"
                   )
    eval(parse(text = code))
    if(i %%10 == 0){
      gc(reset = T)
    }
    #code1 <- paste0("multiple_models_dados <- cbind(multiple_models_dados, V", i,") ; rm(V",i,") ")
    code1 <- paste0("multiple_models_dados[,i] <- V", i, "; rm(V", i, ")")
    eval(parse(text = code1))
    # multiple_models_dados <- cbind(multiple_models_dados, tmp)
  }
  if(classe){
    #multiple_models <- as.matrix(multiple_models_dados) %>%
    multiple_models <- multiple_models_dados %>%
      apply(1, FUN = stats::median) %>%
      as.data.table()
    colnames(multiple_models) <-  c("predicao")
  }else{
#    multiple_models <- as.data.table(as.matrix(multiple_models_dados))
    multiple_models <- as.data.table(multiple_models_dados)
    multiple_models[, id := 1:nrow(multiple_models)]
    multiple_models <- magikaRp:::reldtMtd(dados = multiple_models)
    multiple_models[, id := NULL]
    colnames(multiple_models) <- c("predicao")
  }
  #return(cbind(multiple_models_dados, newdata))

  return(multiple_models)

}
