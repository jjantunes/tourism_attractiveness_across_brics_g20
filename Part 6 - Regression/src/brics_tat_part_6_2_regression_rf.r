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
library(car           )  ## VIF
library(AER           )  ## Tobit regression
library(randomForest  )  ## Random forest
library(caret         )  ## Machine learning functions
library(doParallel    )  ## Parallel execution
library(Boruta        )  ## Variable Selection
library(pbapply       )  ## Progress bar apply
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
## Descriptive Statistics
################################################################################

## Descriptive statistics function
fn_desc <- function(x) data.frame(
  Min    = min(x)       ,
  Max    = max(x)       ,
  Median = median(x)    ,
  Mean   = mean(x)      ,
  SD     = sd(x)        ,
  CV     = sd(x)/mean(x)
)

## Descriptive Stats table
db_desc <- db_reg %>%
  select(-tourism_attractiveness_index) %>% 
  map(fn_desc) %>%
  do.call(rbind, .) %>%
  rownames_to_column("Variables")

## Filter rows
db_desc <- db_desc %>% slice(1:14)

## Variables
vec_vars = c("GDP PPP Per Capita"      ,
             "Population"              ,
             "Unemployment"            ,
             "GDP PPP Per Capita"      ,
             "FDI inflow"              ,
             "Inflation"               ,
             "Logistics Performance"   ,
             "Press Freedom"           ,
             "Control Corruption"      ,
             "Government Effectiveness",
             "Rule of Law"             ,
             "Voice and Accountability",
             "Common Border"           ,
             "Exchange Rate ratio"     )

## Country
vec_coun = c("Origin"     ,
             "Origin"     ,
             "Origin"     ,
             "Destination",
             "Destination",
             "Destination",
             "Destination",
             "Destination",
             "Destination",
             "Destination",
             "Destination",
             "Destination",
             "Bilateral"  ,
             "Bilateral"  )

## Units
vec_unit = c("$"            ,
             "#"            ,
             "% labor force",
             "$"            ,
             "% of GDP"     ,
             "% annual"     ,
             "-"            ,
             "-"            ,
             "-"            ,
             "-"            ,
             "-"            ,
             "-"            ,
             "-"            ,
             "-"            )

## Data source
vec_src  = c("Worldbank Data 360", 
             "Worldbank Data 360", 
             "Worldbank Data 360",
             "Worldbank Data 360",
             "Worldbank Data 360",
             "Worldbank Data 360", 
             "Worldbank Data 360", 
             "Worldbank Data 360",
             "Worldbank Data 360",
             "Worldbank Data 360",
             "Worldbank Data 360", 
             "Worldbank Data 360", 
             "Worldbank Data 360",
             "Worldbank Data 360")

## Add data
db_desc <- db_desc %>% mutate(
  Variables = vec_vars,
  Country   = vec_coun,
  Units     = vec_unit,
  Source    = vec_src   
)

## Relocate
db_desc <- db_desc %>% relocate(Variables, Country, Units, Source)

## Save descriptive stats
write_xlsx(db_desc, file.path(path_output, "table_descriptive_stats_regression.xlsx"))

################################################################################
## Multicolinearity
################################################################################

## Regression formula
frl <- tourism_attractiveness_index ~ .

## Check VIF before PCA
vif(lm(frl, data = db_reg))

## Select governance variables
governance_vars <- c(
  "press_freedom_destination"           ,
  "control_corruption_destination"      , 
  "government_effectiveness_destination",
  "rule_of_law_destination"             ,
  "voice_accountability_destination"     
)

## Extract governance data
governance_data <- db_reg %>% select(all_of(governance_vars))

## Perform PCA (scale and center)
pca_governance <- prcomp(governance_data, scale. = TRUE, center = TRUE)

## Use both PC1 and PC2 (97% variance)
db_reg <- db_reg %>% select(-all_of(governance_vars)) %>% mutate(
  governance_quality = -pca_governance$x[, 1],
  democratic_openness = pca_governance$x[, 2]
)

## Loadings table
db_pca_loadings <- list(
  loadings   = pca_governance$rotation            %>% data.frame() %>% rownames_to_column("Variable"),
  importance = summary(pca_governance)$importance %>% data.frame() %>% rownames_to_column("Variable")
)

## Save
write_xlsx(db_pca_loadings, file.path(path_output, "table_pca_governance_loadings.xlsx"))

## Check VIF after PCA
db_vif <- lm(frl, data = db_reg) %>%
  vif %>% 
  data.frame(vif = .) %>% 
  rownames_to_column("variable")

## Save vif table
write_xlsx(db_vif, file.path(path_output, "table_vif.xlsx"))

################################################################################
## Tobit regression
################################################################################

## Tobit regression
res_tobit <- tobit(frl, data = db_reg, left = 0, right = 1)

## Summary
db_coef_tobit <- summary(res_tobit)$coefficients[,]
db_coef_tobit <- db_coef_tobit %>% data.frame(variables = rownames(.), .)
db_coef_tobit <- db_coef_tobit %>% data.frame(row.names = NULL)
db_coef_tobit <- db_coef_tobit %>% mutate(sig = cut(
  x              = Pr...z..                       ,
  breaks         = c(0, 0.001, 0.01, 0.05, 0.1, 1),
  labels         = c("***", "**", "*", ".", "")   ,
  include.lowest = TRUE                            
))

## Rename
colnames(db_coef_tobit) <- c("variables", "estimate", "std_error", "z_value", "p_value", "significance")

## Save tobit results
saveRDS(res_tobit, file.path(path_output, "rds_tobit.rds"))

## Save tobit coefficients
write_xlsx(db_coef_tobit, file.path(path_output, "table_coefs_tobit.xlsx"))

################################################################################
## Random Forest regression cross validation
################################################################################

## Parameter grid
grid_rf <- expand.grid(mtry = 2:19)
n_trees <- seq(from = 1000, to = 100, by = -100)

## Error metrics
reg_summary <- function(data, lev = NULL, model = NULL) {
  
  ## Metrics
  rmse <- sqrt(mean((data$obs - data$pred)^2))
  mae  <- mean(abs(data$obs - data$pred))
  r2   <- summary(lm(pred ~ obs, data))$r.squared
  mape <- mean(abs((data$obs - data$pred) / data$obs)) * 100
  
  ## Return metrics
  c(MAPE = mape, MAE = mae, RMSE = rmse, Rsquared = r2)
}

## Train control
train_control <- trainControl(
  method          = "LOOCV"    , 
  allowParallel   = TRUE       ,    
  verboseIter     = FALSE      ,
  summaryFunction = reg_summary 
)

## Initial time
t_ini <- Sys.time()

## Parallel backend
cl <- makePSOCKcluster(parallel::detectCores())

## Register parallel backend
registerDoParallel(cl)

## Cross validation
res_cv <- pblapply(n_trees, \(n_tree) train(
  form        = frl          , 
  data        = db_reg       ,
  method      = "rf"         ,
  trControl   = train_control,
  tuneGrid    = grid_rf      ,
  ntree       = n_tree       ,    
  importance  = TRUE         ,
  metric      = "MAPE"       ,
  maximize    = FALSE         
))

## Stop parallel backend
stopCluster(cl)

## Final time
t_end <- Sys.time()

## Print total evaluation time
print(t_end - t_ini)

## Change names
names(res_cv) <- paste0("RF_", n_trees)

## Data frame of comparison
db_cv_comp <- bind_rows(lapply(res_cv, \(x) mutate(
  x$results, n_tree = x$finalModel$ntree
)))

## best model
name_best_rf <- db_cv_comp %>% slice_min(MAPE) %>% pull(n_tree)
name_best_rf <- paste0("RF_", name_best_rf)

## print best model
print(name_best_rf)

## Train best model
res_rf <- res_cv[[name_best_rf]]$finalModel

## Importance
mat_imp <- varImpPlot(res_rf)

## Save cross validation results
saveRDS(res_cv, file.path(path_output, "rds_cv_rf.rds"))

## Save best model
saveRDS(res_rf, file.path(path_output, "rds_rf.rds"))

## Save cross validation comparison
write_xlsx(db_cv_comp, file.path(path_output, "table_model_comparison.xlsx"))

################################################################################
## Feature selection with boruta 
################################################################################

## Boruta feature selection
res_boruta <- Boruta(
  formula = frl         ,
  data    = db_reg      ,
  doTrace = 1           ,
  maxRuns = 1000        ,
  ntree   = res_rf$ntree,
  mtry    = res_rf$mtry  
)

## Resolve tentative variables
res_boruta <- TentativeRoughFix(res_boruta)

## Save boruta results
saveRDS(res_boruta, file.path(path_output, "rds_boruta.rds"))

################################################################################
## Importance analysis
################################################################################

## Data frame
db_coefs <- data.frame(variables = rownames(mat_imp), mat_imp, row.names = NULL)

## Relevant variables
db_coefs <- db_coefs %>%
  select(variables, X.IncMSE) %>%
  rename(importance_rf = X.IncMSE)

## Merge with boruta
db_coefs <- data.frame(variables = names(res_boruta$finalDecision), significance_rf = res_boruta$finalDecision) %>%
  data.frame(row.names = NULL) %>%
  full_join(db_coefs)

## Merge with tobit
db_coefs <- summary(res_tobit)$coefficients[,] %>%
  data.frame(variables = rownames(.), .) %>% 
  data.frame(row.names = NULL) %>%
  select(variables, Estimate, `Pr...z..`) %>%
  rename(coef_tobit = Estimate, significance_tobit = `Pr...z..`) %>% 
  mutate(significance_tobit = ifelse(significance_tobit < 0.05, "Confirmed", "Rejected")) %>% 
  right_join(db_coefs) %>% 
  relocate(variables, coef_tobit, importance_rf)
  
## Save coefficients table
write_xlsx(db_coefs, file.path(path_output, "table_coefs_comparison.xlsx"))
