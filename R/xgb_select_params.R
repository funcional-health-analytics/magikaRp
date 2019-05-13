#' @title Selecionador de hyperparametros para Xgboost
#'
#' @description  Seu objetivo é selecionar hyperparametros que maximizem ou minimizem seu objetivo utilizando CV e Sessões.
#' Para isto ele se utiliza do pacote mlrMBO, realizando optimizações de estatística bayesiana
#'
#' @param dados Um objeto data.table com os dados pre processados e uma coluna target
#' @param n_samples O tamanho do sorteio par acada classe. Se regressão o tamanbho total da amostra
#' @param var_id A variável que identifica cada observação. Ela será removida antes da seleção de parametros. DEFAULT: \code{NULL} (não irá remover nada)
#' @param metrica A métrica selecionada para o xgboost lidar. DEFAULT: logloss
#' @param objetivo A finalidade do modelo. DEFAULT: binary:logistic
#' @param niter_data Quantas sessões serão feitas. Quantos sorteios serão feitos nos dados. DEFAULT: 10
#' @param niter_bayes Quantas itereções serão utilizadas no otimizador de bayes. DEFAULT: 50
#' @param cv.nfolds  Quantos folds para o CV. DEFAULT: 5
#' @param cv.nrounds Quantidade máxima de nrounds a ser testado. DEFAULT: 3000,
#' @param nthreads Quantidade de CPU threads disponíveis para o XGBoost. DEFAULT: 3
#' @param tree_method Método de treino das árvores. DEFAULT: hist. Em hist será treinado na CPU, em gpu_hist na GPU
#'
#' @return Uma lista com três posições: Os parâmetros procurados, o número de nrounds e a melhor métrica encontrada
#'
#' @import data.table xgboost magrittr mlrMBO smoof DiceKriging rgenoud lhs ParamHelpers
#'
#' @export



# require(data.table)
# require(xgboost)
# require(magrittr)
# require(mlrMBO)
# require(smoof)
# require(DiceKriging)
# require(rgenoud)

## Selecionador de parâmetros xgboost


# DADOS deve ser um tata.table pronto para treino

xgb_select_params <- function(dados,
                              n_samples,
                              var_id          = NULL,
                              metrica         = "logloss",
                              objetivo        = "binary:logistic",
                              niter_data      = 10,
                              niter_bayes     = 50,
                              cv.nfolds       = 5,
                              cv.nrounds      = 3000,
                              nthreads        = 3,
                              tree_method     = "hist"){

  base::requireNamespace('data.table')
  base::requireNamespace('xgboost')
  base::requireNamespace('magrittr')
  base::requireNamespace('mlrMBO')
  base::requireNamespace('smoof')
  base::requireNamespace('DiceKriging')
  base::requireNamespace('rgenoud')

  if(!objetivo %in% c("reg:squarederror",
                      "reg:logistic",
                      "binary:logistic",
                      "binary:logitraw",
                      "binary:hinge",
                      "count:poisson",
                      "max_delta_step",
                      "survival:cox",
                      "multi:softmax",
                      "multi:softprob",
                      "rank:pairwise",
                      "rank:ndcg",
                      "rank:map",
                      "reg:gamma",
                      "reg:tweedie")
  ){
    stop(paste0("Seu objetivo não se encontra na lista abaixo: \n

                \t \t reg:squarederror: regressão com erro quadrático \n
                \t \t reg:logistic: regressão logística \n
                \t \t binary:logistic: regressão logística para classificação binária, a saída é probabilidade \n
                \t \t binary:logitraw: regressão logística para classificação binária, a saída é o logito antes da transformação \n
                \t \t binary:hinge: hinge loss para classificação binária. Isso faz predição das classes 1 e 0 e não retorna probabilidade \n
                \t \t count:poisson regressão poisson para contagens, a saída é a média de uma distribuição de poisson \n
                \t \t max_delta_step is set to 0.7 by default in poisson regression (used to safeguard optimization) \n
                \t \t survival:cox: Regressão Cox para análise de sobrevivência censurado à direita (Valores negativos são considerados censurados).\n A saída é hazard ratio scale (i.e., as HR = exp(marginal_prediction) in the proportional hazard function h(t) = h0(t) * HR) \n
                \t \t multi:softmax: Classificação multi-classe usando softmax,
                \t \t multi:softprob: same as softmax, but output a vector of ndata * nclass, which can be further reshaped to ndata * nclass matrix. The result contains predicted probability of each data point belonging to each class \n
                \t \t rank:pairwise: Use LambdaMART to perform pairwise ranking where the pairwise loss is minimized \n
                \t \t rank:ndcg: Use LambdaMART to perform list-wise ranking where Normalized Discounted Cumulative Gain (NDCG) is maximized \n
                \t \t rank:map: Use LambdaMART to perform list-wise ranking where Mean Average Precision (MAP) is maximized \n
                \t \t reg:gamma: gamma regression with log-link. Output is a mean of gamma distribution. It might be useful, e.g., for modeling insurance claims severity, or for any outcome that might be gamma-distributed \n
                \t \t reg:tweedie: Tweedie regression with log-link. It might be useful, e.g., for modeling total loss in insurance, or for any outcome that might be Tweedie-distributed \n
                "))
  }

  if(!metrica %in% c("rmse",
                     "mae",
                     "logloss",
                     "error",
                     "error@t",
                     "merror",
                     "mlogloss",
                     "auc",
                     "aucpr",
                     "ndcg",
                     "map",
                     "poisson-nloglik",
                     "gamma-nloglik",
                     "cox-nloglik",
                     "cox-nloglik",
                     "gamma-deviance",
                     "tweedie-nloglik"
  )
  ){
    stop(paste0("A métrica escolhida não está entre: \n
                \t \t rmse: root mean square error\n
                \t \t mae: mean absolute error\n
                \t \t logloss: negative log-likelihood\n
                \t \t error: Binary classification error rate. It is calculated as #(wrong cases)/#(all cases). For the predictions, the evaluation will regard the instances with prediction value larger than 0.5 as positive instances, and the others as negative instances \n
                \t \t merror: Multiclass classification error rate. It is calculated as #(wrong cases)/#(all cases) \n
                \t \t mlogloss: Multiclass logloss \n
                \t \t auc: Area under the curve \n
                \t \t aucpr: Area under the PR curve \n
                \t \t ndcg: Normalized Discounted Cumulative Gain \n
                \t \t map: Mean Average Precision \n
                \t \t poisson-nloglik: negative log-likelihood for Poisson regression \n
                \t \t gamma-nloglik: negative log-likelihood for gamma regression \n
                \t \t cox-nloglik: negative partial log-likelihood for Cox proportional hazards regression \n
                \t \t gamma-deviance: residual deviance for gamma regression \n
                \t \t tweedie-nloglik: negative log-likelihood for Tweedie regression (at a specified value of the tweedie_variance_power parameter) \n
                ")
    )
  }
  cat(paste0("Criando uma amostra balanceada para as iterações \t \t \t \t ---- \n"))

  if(metrica %in% c("auc",
                    "aucpr",
                    "ndcg",
                    "map")
  ){
    maximize. <- T
  }else{
    maximize. <- F
  }



  #cat(paste0("Iniciando o loop  \t \t \t \t ---- \n"))

  best_param         <- list()
  best_metric        <- ifelse(maximize., -Inf, Inf)
  best_metric_index  <- 0
  for (iter in 1:niter_data) {

    if(grepl(objetivo, pattern = 'binary|multi')){
      treino <- dados[,.SD[sample(.N, n_samples,replace = T)],by = target]
    }else{
      treino <- dados[sample(.N, n_samples,replace = F)]
    }

    if(!is.null(var_id)){
      eval(parse(text = paste0("treino[,", var_id," := NULL]")))
    }

    if(objetivo %in% c("multi:softmax", "multi:softprob")){
      num_class <- length(unique(treino$target))
    }


    #cat(paste0("Transformando as colunas para numeric \t \t \t \t ---- \n"))
    treino <- treino[, lapply(.SD, as.numeric)]


    #cat(paste0("Criando a xgb matrix \t \t \t \t ---- \n"))
    dtrain <- xgb.DMatrix(
      treino[,
             colnames(treino)[colnames(treino) != "target"],
             with = F
             ] %>%
        as.matrix(),
      label = treino$target
    )



    if(objetivo %in% c("multi:softmax", "multi:softprob")){
      obj.func <- smoof::makeSingleObjectiveFunction(
        name = "xgboost",
        fn = function(x){
          mdcv <- xgb.cv(
            data                  = dtrain,
            nthread               = nthreads,
            nfold                 = cv.nfolds,
            nrounds               = x["nrounds"],
            verbose               = F,
            #            early_stopping_rounds = early_stop,
            maximize              = maximize.,
            params                = list(
              objective        = objetivo,
              eval_metric      = metrica,
              base_score       = sum(treino$target)/nrow(treino),
              max_leaves       = x["max_leaves"],
              #max.depth       = ceiling(runif(1,7,20)),
              eta              = x["eta"],
              gamma            = x["gamma"],
              subsample        = x["subsample"],
              colsample_bytree = x["colsample_bytree"],
              min_child_weight = x["min_child_weight"],
              nrounds          = x["nrounds"],
              grow_policy      = "lossguide",
              tree_method      = tree_method,
              num_class        = num_class
            )
          )
          if(maximize.){
            return(max(mdcv$evaluation_log[, 4][[1]]))
          }else{
            return(min(mdcv$evaluation_log[, 4][[1]]))
          }

        },
        par.set = ParamHelpers::makeParamSet(
          ParamHelpers::makeIntegerParam(id = "max_leaves", lower = 2, upper = 60),
          ParamHelpers::makeNumericParam(id = "eta", lower = 0.0001, upper = 0.3),
          ParamHelpers::makeNumericParam(id = "gamma", lower = 0, upper = 30),
          ParamHelpers::makeNumericParam(id = "subsample", lower = 0.1, upper = 1),
          ParamHelpers::makeNumericParam(id = "colsample_bytree", lower = 0.1,  upper = 1),
          ParamHelpers::makeNumericParam(id = "min_child_weight", lower = 0, upper = 100),
          ParamHelpers::makeIntegerParam(id = "nrounds", lower = 10, upper = cv.nrounds)
        ),
        minimize = !maximize.

      )
    }else{

      obj.func <- smoof::makeSingleObjectiveFunction(
        name = "xgboost",
        fn = function(x){
          mdcv <- xgb.cv(
            data                  = dtrain,
            nthread               = nthreads,
            nfold                 = cv.nfolds,
            nrounds               = x["nrounds"],
            verbose               = F,
            #        early_stopping_rounds = early_stop,
            maximize              = maximize.,
            params                = list(
              objective        = objetivo,
              eval_metric      = metrica,
              base_score       = sum(treino$target)/nrow(treino),
              max_leaves       = x["max_leaves"],
              #max.depth       = ceiling(runif(1,7,20)),
              eta              = x["eta"],
              gamma            = x["gamma"],
              subsample        = x["subsample"],
              colsample_bytree = x["colsample_bytree"],
              min_child_weight = x["min_child_weight"],
              nrounds          = x["nrounds"],
              grow_policy      = "lossguide",
              tree_method      = tree_method
            )
          )
          if(maximize.){
            return(max(mdcv$evaluation_log[, 4][[1]]))
          }else{
            return(min(mdcv$evaluation_log[, 4][[1]]))
          }

        },
        par.set = ParamHelpers::makeParamSet(
          ParamHelpers::makeIntegerParam(id = "max_leaves", lower = 2, upper = 60),
          ParamHelpers::makeNumericParam(id = "eta", lower = 0.0001, upper = 0.3),
          ParamHelpers::makeNumericParam(id = "gamma", lower = 0, upper = 30),
          ParamHelpers::makeNumericParam(id = "subsample", lower = 0.1, upper = 1),
          ParamHelpers::makeNumericParam(id = "colsample_bytree", lower = 0.1,  upper = 1),
          ParamHelpers::makeNumericParam(id = "min_child_weight", lower = 0, upper = 100),
          ParamHelpers::makeIntegerParam(id = "nrounds", lower = 10, upper = cv.nrounds)
        ),
        minimize = !maximize.
      )
    }


    design <- ParamHelpers::generateDesign(
      n       = 30,
      par.set = getParamSet(obj.func),
      fun     = lhs::randomLHS
    )

    control <- mlrMBO::makeMBOControl() %>%
      setMBOControlTermination(iters = niter_bayes)

    run <- mlrMBO::mbo(
      fun       = obj.func,
      design    = design,
      control   = control,
      show.info = TRUE
    )



    if(maximize.){
      actual_metric       <- run$y
      #      actual_metric_index <- which.max((mdcv$evaluation_log[, 4][[1]]))
      if (actual_metric > best_metric) {
        best_metric         <- actual_metric
        #        best_metric_index   <- actual_metric_index
        best_param          <- run$x
      }
    }else{
      actual_metric       <- run$y
      #      actual_metric_index <- which.min((mdcv$evaluation_log[, 4][[1]]))
      if (actual_metric < best_metric) {
        best_metric         <- actual_metric
        #        best_metric_index   <- actual_metric_index
        best_param          <- run$x
      }
    }

    cat(paste0("\r Sorteio de parametros de n: ",iter," :::: best metric: ",best_metric, " Nrounds: ",best_metric_index, "!!! \r"))

    gc(reset = T)
  }
  if(objetivo %in% c("multi:softmax", "multi:softprob")){
    best_param['objective']     <- objetivo
    best_param['eval_metric']  <- metrica
    best_param['base_score']   <- sum(treino$target)/nrow(treino)
    best_param['grow_policy']  <- 'lossguide'
    best_param['tree_method'] <- tree_method
    best_param['num_class']   <- num_class
  }else{
    best_param['objective']     <- objetivo
    best_param['eval_metric']  <- metrica
    best_param['base_score']   <- sum(treino$target)/nrow(treino)
    best_param['grow_policy']  <- 'lossguide'
    best_param['tree_method']  <- tree_method
  }
  best_metric_index       <- best_param[['nrounds']]
  best_param[['nrounds']] <- NULL


  return(
    list(
      parametros  = best_param,
      nrounds     = best_metric_index,
      best_metric = best_metric
    )
  )
  }


#parametros_select <- xgb_select_params(dados = dados, positivos = 5000, niter = 10)
