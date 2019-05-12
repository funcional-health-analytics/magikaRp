
rnorm_t <- function(n, mu, sd, upper = Inf, lower = -Inf){
  result <- stats::rnorm(n    = n,
                  mean = mu,
                  sd   = sd
  )
  result[result < lower] <- lower
  result[result > upper] <- upper
  return(result)
}


#' @title Treino ensemble de modelos XGBoost
#'
#' @description O objetivo desta função é treinar vários modelos por meio de sessões. Cada sessão possui uma parte dos dados, um modelo ali então é treinado e salvo numa pasta especificada.
#'
#' @param dados Objeto \code{data.table} com os dados já pre processados e com uma coluna \code{target}.
#' @param n_samples O tamanho do sorteio par acada classe. Se regressão o tamanbho total da amostra
#' @param nthreads A quantidade de CPU threads disponíveis par ao XGBoost. DEFAULT: 3
#' @param parametros_treino A lista de parâmetros retornado pela função \code{xgb_select_params}
#' @param n_models A quantidade de modelos gerados. Também é o número de sessões realizado. DEFAULT: 100
#' @param save_importance Indicador para o salvamento das importâncias das variáveis para cada modelo. DEFAULT: F
#' @param folder_to_save Pasta em que será salvo os \code{n_models} gerados
#'
#' @return A função não retorna nada, apenas salva os modelos na pasta
#'
#' @import data.table xgboost magrittr
#'
#' @export


xgb_treino_ensemble <- function(dados,
                                n_samples,
                                #  metrica         = "logloss",
                                #  objetivo        = "binary:logistic",
                                nthreads           = 3,
                                parametros_treino = NULL,
                                n_models          = 100,
                                save_importance   = F,
                                folder_to_save   = NULL){


  requireNamespace('data.table')
  requireNamespace('xgboost')
  requireNamespace('magrittr')

  if(is.null(folder_to_save)){
    cat(paste0("É necessário que haja uma pasta para salvar os modelos, \n
               será criado uma pasta chamada 'xgboost_models' no seu projeto R"))
    dir.create(file.path("xgboost_models"), showWarnings = FALSE)
    folder_to_save <- "xgboost_models"
  }
  if(is.null(parametros)){
    stop("É necessário que haja uma lista de parâmetros")
  }



  if(parametros$parametros$eval_metric %in% c("auc",
                                              "aucpr",
                                              "ndcg",
                                              "map")
  ){
    maximize. <- T
  }else{
    maximize. <- F
  }


  N_MODELS  <- n_models
  importance <- list()

  for(j in 1:N_MODELS){

    cat(paste0("\n \n \n \n \n \n \t Sessao: ",j," ===============================================================\n"))

    # separando teste e treino ------------------------------------------------
    cat(paste0("separando teste e treino \t \t \t \t  ---- \n"))

    treino <- dados[,.SD[sample(.N, n_samples,replace = T)],by = target]

    # preparando matrizes para rede -------------------------------------------
    cat(paste0("preparando matrizes para rede \t \t \t \t ---- \n"))

    treino <- treino[, lapply(.SD, as.numeric)]

    dtrain <- xgb.DMatrix(
      treino[,
             colnames(treino)[colnames(treino) != "target"],
             with = F
             ] %>%
        as.matrix(),
      label = treino$target
    )
    # seleção de parametros  --------------------------------------------------
    if(parametros_treino$parametros$objective %in% c("multi:softmax", "multi:softprob")){
      tmp_param <- list(
        objective        = parametros_treino$parametros$objective,
        eval_metric      = parametros_treino$parametros$eval_metric,
        base_score       = sum(treino$target)/nrow(treino),
        max_leaves       = ceiling(rnorm_t(n = 1, mu = parametros_treino$parametros$max_leaves, sd = 2, lower = 1)),
        #max.depth        = ceiling(runif(1,7,20)),
        eta              = magikaRp:::rnorm_t(n = 1, mu = parametros_treino$parametros$eta, sd = 0.02, lower = 0.001),
        gamma            = magikaRp:::rnorm_t(n = 1, mu = parametros_treino$parametros$gamma, sd = 0.5, lower = 0),
        subsample        = magikaRp:::rnorm_t(n = 1, mu = parametros_treino$parametros$subsample, sd = 0.1, lower = 0.01, upper = 1),
        colsample_bytree = magikaRp:::rnorm_t(n = 1, mu = parametros_treino$parametros$colsample_bytree, sd = 0.1, lower = 0.01, upper = 1),
        min_child_weight = magikaRp:::rnorm_t(n = 1, mu = parametros_treino$parametros$min_child_weight, sd = 2, lower = 0),
        grow_policy      = parametros_treino$parametros$grow_policy,
        tree_method      = parametros_treino$parametros$tree_method,
        num_class        = parametros_treino$parametros$num_class
        #scale_pos_weight = sum(target == 0)/ sum(target == 1)
      )
    }else{
      tmp_param <- list(
        objective        = parametros_treino$parametros$objective,
        eval_metric      = parametros_treino$parametros$eval_metric,
        base_score       = sum(treino$target)/nrow(treino),
        max_leaves       = ceiling(rnorm_t(n = 1, mu = parametros_treino$parametros$max_leaves, sd = 2, lower = 1)),
        #max.depth        = ceiling(runif(1,7,20)),
        eta              = magikaRp:::rnorm_t(n = 1, mu = parametros_treino$parametros$eta, sd = 0.02, lower = 0.001),
        gamma            = magikaRp:::rnorm_t(n = 1, mu = parametros_treino$parametros$gamma, sd = 0.5, lower = 0),
        subsample        = magikaRp:::rnorm_t(n = 1, mu = parametros_treino$parametros$subsample, sd = 0.1, lower = 0.01, upper = 1),
        colsample_bytree = magikaRp:::rnorm_t(n = 1, mu = parametros_treino$parametros$colsample_bytree, sd = 0.1, lower = 0.01, upper = 1),
        min_child_weight = magikaRp:::rnorm_t(n = 1, mu = parametros_treino$parametros$min_child_weight, sd = 2, lower = 0),
        grow_policy      = parametros_treino$parametros$grow_policy,
        tree_method      = parametros_treino$parametros$tree_method
        #scale_pos_weight = sum(target == 0)/ sum(target == 1)
      )
    }


    cat(paste0("Treinando modelo \t \t \t \t ---- \n"))

    md <- xgboost(data = dtrain,
                  params = tmp_param,
                  nrounds = parametros$nrounds,
                  nthread = nthreads,
                  verbose = F,
                  maximize = maximize.
                  )

    #importance[[j]] <- xgb.importance(model = md)

    try({
      importance[[j]] <- xgb.importance(model = md)

    })


    xgb.save(model = md,
             fname = paste0(folder_to_save,"/xgb_SESSION_", j, ".xgb")
    )

    rm(md)
    gc(reset = T)
  }
  try({
    saveRDS(importance, file = paste0(folder_to_save, "/importancia_modelos",".RDS"))
  })

}
