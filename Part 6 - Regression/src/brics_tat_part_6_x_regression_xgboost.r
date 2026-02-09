################################################################################
## Clear environment
################################################################################

rm(list = ls())
gc(full = TRUE)

################################################################################
## libraries
################################################################################

library(writexl       )  ## Write excel files
library(readxl        )  ## Read excel files
library(AER           )  ## Tobit regression
library(xgboost       )  ## XGBoost
library(pbapply       )  ## Progress bar apply functions
library(ggplot2       )  ## Plots
library(tidyverse     )  ## Data manipulation

################################################################################
## Paths and seeds
################################################################################

## Library to get path
library(this.path)

## Get base path
path <- this.dir()
path <- strsplit(path, "/")[[1]]
path <- rev(path)[-1]
path <- paste(rev(path), collapse = "/")

## Create Paths
path_data   <- paste0(path, "/data/"  )
path_output <- paste0(path, "/output/")

## Seed
set.seed(100)

################################################################################
## Read data
################################################################################

## Read data
db_reg <- read_excel(file.path(path_output, "table_regression_dataset.xlsx"))

################################################################################
## Data manipulation
################################################################################

## Remove unused variables
db_reg <- db_reg %>% select(-origin, -destination, -g20_to_g20)

## Logs
db_reg <- db_reg %>% mutate(
  gdp_pp_ppp_origin      = log(gdp_pp_ppp_origin      ),
  population_origin      = log(population_origin      ),
  gdp_pp_ppp_destination = log(gdp_pp_ppp_destination ),
  inflation_destination  = log(inflation_destination  ),
  exchange_rate_ratio    = log(exchange_rate_ratio + 1)
)

################################################################################
## Regression
################################################################################

## Regression formula
frl <- tourism_attractiveness_index ~.

## Tobit regression
res_tobit <- tobit(frl, data = db_reg, left = 0, right = 1)

## Summary
summary(res_tobit)

################################################################################
## XGBoost regression cross validation
################################################################################

## Parameter grid (with objective)
grid_xgb <- expand.grid(
  nrounds          = c(100, 200, 500)                     ,
  max_depth        = c(3, 6)                              ,
  eta              = c(0.01, 0.1)                         ,
  gamma            = 0                                    ,
  min_child_weight = c(1, 3)                              ,
  colsample_bytree = c(0.8, 1)                            ,
  subsample        = c(0.8, 1)                            ,
  objective        = c("reg:logistic", "reg:squarederror")
)

## Dataset for train xgboost
dtrain <- xgb.DMatrix(
  data  = model.matrix(res_tobit)[, -1]      ,
  label = db_reg$tourism_attractiveness_index
)

## Initial time
t_ini <- Sys.time()

## Cross validation
res_cv <- pblapply(1:nrow(grid_xgb), \(i) xgb.cv(
  data = dtrain,
  params = list(
    max_depth        = grid_xgb$max_depth[i],
    eta              = grid_xgb$eta[i],
    gamma            = grid_xgb$gamma[i],
    min_child_weight = grid_xgb$min_child_weight[i],
    colsample_bytree = grid_xgb$colsample_bytree[i],
    subsample        = grid_xgb$subsample[i],
    objective        = as.character(grid_xgb$objective[i])
  ),
  nrounds = grid_xgb$nrounds[i],
  nfold = 10,
  early_stopping_rounds = 50,
  verbose = 0
))

## Final time
t_end <- Sys.time()

## Print total evaluation time
print(t_end - t_ini)

## Find best parameter
grid_xgb$rmse <- do.call(c, lapply(res_cv, \(cv) cv$evaluation_log$test_rmse_mean[cv$best_iteration]))

## Best parameter
param_xgb <- grid_xgb %>% slice_min(rmse)

## Train best model
res_xgb <- xgboost(
  data             = dtrain                           ,
  nrounds          = param_xgb$nrounds                ,
  max_depth        = param_xgb$max_depth              ,
  eta              = param_xgb$eta                    ,
  gamma            = param_xgb$gamma                  ,
  min_child_weight = param_xgb$min_child_weight       ,
  colsample_bytree = param_xgb$colsample_bytree       ,
  subsample        = param_xgb$subsample              ,
  objective        = as.character(param_xgb$objective),
  verbose          = 0                                 
)

## Importance
mat_imp <- xgb.importance(model = res_xgb)

## Plot
xgb.ggplot.importance(mat_imp, top_n = 20)













plot(density(db_reg$tourism_attractiveness_index))





################################################################################
## Feature selection with boruta 
################################################################################

boruta_xgb <- function(X, y, params, maxRuns = 100, pValue = 0.01, 
                       mcAdj = TRUE, verbose = TRUE) {
  
  n_vars <- ncol(X)
  var_names <- colnames(X)
  
  # Initialize
  decision <- factor(rep("Tentative", n_vars), 
                     levels = c("Tentative", "Confirmed", "Rejected"))
  names(decision) <- var_names
  hits <- rep(0, n_vars)
  names(hits) <- var_names
  imp_history <- list()
  
  runs <- 0
  
  while(any(decision == "Tentative") && runs < maxRuns) {
    runs <- runs + 1
    
    # Shadow attributes
    X_tent <- X[, decision != "Rejected", drop = FALSE]
    X_shadow <- X_tent
    while(ncol(X_shadow) < 5) X_shadow <- cbind(X_shadow, X_shadow)
    X_shadow <- as.data.frame(lapply(X_shadow, sample))
    names(X_shadow) <- paste0("shadow_", 1:ncol(X_shadow))
    
    X_combined <- cbind(X[, decision != "Rejected", drop = FALSE], X_shadow)
    
    # Train XGBoost
    dtrain <- xgb.DMatrix(data = as.matrix(X_combined), label = y)
    model <- xgboost(
      data = dtrain,
      nrounds = params$nrounds,
      max_depth = params$max_depth,
      eta = params$eta,
      gamma = params$gamma,
      min_child_weight = params$min_child_weight,
      colsample_bytree = params$colsample_bytree,
      subsample = params$subsample,
      objective = params$objective,
      verbose = 0
    )
    
    # Importance
    imp_raw <- xgb.importance(model = model)
    imp_vec <- setNames(imp_raw$Gain, imp_raw$Feature)
    
    # Reconstruct full importance
    imp <- rep(-Inf, n_vars)
    names(imp) <- var_names
    imp[decision != "Rejected"] <- imp_vec[var_names[decision != "Rejected"]]
    
    shadow_imp <- imp_vec[grepl("^shadow_", names(imp_vec))]
    
    # Assign hits
    hits_now <- imp > max(shadow_imp)
    hits[hits_now] <- hits[hits_now] + 1
    
    # Store history
    imp_history[[runs]] <- c(imp, 
                             shadowMax = max(shadow_imp),
                             shadowMean = mean(shadow_imp),
                             shadowMin = min(shadow_imp))
    
    # Test
    pAdjMethod <- ifelse(mcAdj, "bonferroni", "none")
    
    p_confirm <- p.adjust(pbinom(hits - 1, runs, 0.5, lower.tail = FALSE), 
                          method = pAdjMethod)
    p_reject <- p.adjust(pbinom(hits, runs, 0.5, lower.tail = TRUE), 
                         method = pAdjMethod)
    
    to_confirm <- (decision == "Tentative" & p_confirm < pValue)
    to_reject <- (decision == "Tentative" & p_reject < pValue)
    
    if(verbose && (sum(to_confirm) + sum(to_reject) > 0)) {
      if(sum(to_confirm) > 0) cat("Run", runs, "- Confirmed:", 
                                  paste(var_names[to_confirm], collapse = ", "), "\n")
      if(sum(to_reject) > 0) cat("Run", runs, "- Rejected:", 
                                 paste(var_names[to_reject], collapse = ", "), "\n")
    }
    
    decision[to_confirm] <- "Confirmed"
    decision[to_reject] <- "Rejected"
  }
  
  imp_history <- do.call(rbind, imp_history)
  
  return(list(
    finalDecision = decision,
    ImpHistory = imp_history,
    hits = hits,
    runs = runs
  ))
}

# Execute
boruta_result <- boruta_xgb(
  X = model.matrix(res_tobit)[, -1],
  y = db_reg$tourism_attractiveness_index,
  params = as.list(param_xgb[1, -which(names(param_xgb) %in% c("rmse", "objective"))])
)

################################################################################
## 
################################################################################

## Dataset for train xgboost
dtrain <- xgb.DMatrix(
  data  = model.matrix(res_tobit)[, -1]      ,
  label = db_reg$tourism_attractiveness_index
)

## XGBoost
model_xgb <- xgboost(
  data      = dtrain            , 
  nrounds   = 100               , 
  objective = "reg:squarederror",
  eta       = 0.1               ,
  max_depth = 6                 ,
  verbose   = 0                  
)

## Importance
mat_imp <- xgb.importance(model = model_xgb)





## https://www.jstatsoft.org/article/view/v036i11





## Importância padrão
importance_matrix <- xgb.importance(model = model_xgb)

# Plot
xgb.ggplot.importance(importance_matrix, top_n = 20)

## Boruta-style test
X <- model.matrix(res_tobit)[, -1]
y <- db_reg$tourism_attractiveness_index

# Adicionar shadow features
set.seed(100)
X_shadow <- apply(X, 2, sample)
colnames(X_shadow) <- paste0("shadow_", colnames(X_shadow))
X_combined <- cbind(X, X_shadow)

# Treinar
dtrain_shadow <- xgb.DMatrix(data = X_combined, label = y)
model_shadow <- xgboost(
  data = dtrain_shadow, nrounds = 100, 
  objective = "reg:squarederror", eta = 0.1, 
  max_depth = 6, verbose = 0
)

# Importância
imp_shadow <- xgb.importance(model = model_shadow)

# Threshold: max das shadows
shadow_imp <- imp_shadow$Gain[grepl("^shadow_", imp_shadow$Feature)]
threshold <- max(shadow_imp)

real_imp <- imp_shadow[!grepl("^shadow_", imp_shadow$Feature), ]
real_imp$Important <- real_imp$Gain > threshold

print(real_imp[, c("Feature", "Gain", "Important")])

# Plot com threshold
ggplot(real_imp, aes(x = reorder(Feature, Gain), y = Gain, fill = Important)) +
  geom_col() +
  geom_hline(yintercept = threshold, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(x = NULL, y = "Gain", title = "Variable Importance (Boruta-style)") +
  theme_minimal()


