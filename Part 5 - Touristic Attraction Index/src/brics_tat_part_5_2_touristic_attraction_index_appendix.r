################################################################################
## Clear environment
################################################################################

rm(list = ls())
gc(full = TRUE)

################################################################################
## libraries
################################################################################

library(readxl     )  ## Read excel files
library(VIM        )  ## KNN Imputation
library(entropy    )  ## Information entropy
library(ggplot2    )  ## Plots
library(tidyverse  )  ## Data manipulation

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
db_visa <- read_excel(file.path(path_data, "table_countries_visa.xlsx"     ))
db_dist <- read_excel(file.path(path_data, "table_country_distance.xlsx"   ))
db_sent <- read_excel(file.path(path_data, "table_sentiment_analysis.xlsx" ))
db_data <- read_excel(file.path(path_data, "table_data_360_indicators.xlsx"))

################################################################################
## Data manipulation
################################################################################

## Select relevant data
db_visa <- db_visa %>% select(country_origin, country_dest, visa)
db_dist <- db_dist %>% select(country_origin, country_dest, cultural_dist, geographic_dist, religious_dist, linguistic_dist)
db_data <- db_data %>% select(COUNTRY_NAME, INDICATOR_NAME, VALUE)

## Filter relevant data
db_data <- db_data %>% filter(INDICATOR_NAME %in% c(
  "Number of World Heritage cultural sites"                             ,
  "Number of World Heritage natural sites"                              ,
  "Human Development Index (HDI) [highest = 1]"                         ,
  "Terrestrial and marine protected areas (% of total territorial area)",
  "Political Stability and Absence of Violence/Terrorism: Estimate"     ,
  "Intentional homicides (per 100,000 people)"                          
))

## Pivot data
db_data <- db_data %>% pivot_wider(names_from = INDICATOR_NAME, values_from = VALUE)

## Change names
db_visa <- db_visa %>% rename(origin = country_origin, destination = country_dest)
db_dist <- db_dist %>% rename(origin = country_origin, destination = country_dest)
db_sent <- db_sent %>% rename(destination = Country, sentiment = Tourism_Attraction_Index)
db_data <- db_data %>% rename(
  destination                        = "COUNTRY_NAME"                                                        ,                            
  hdi                                = "Human Development Index (HDI) [highest = 1]"                         ,
  protected_areas_terrestrial_marine = "Terrestrial and marine protected areas (% of total territorial area)",
  political_stability                = "Political Stability and Absence of Violence/Terrorism: Estimate"     ,
  homicides                          = "Intentional homicides (per 100,000 people)"                          ,
  number_cultural_sites              = "Number of World Heritage cultural sites"                             ,           
  number_natural_sites               = "Number of World Heritage natural sites"                               
)

## Change case
db_visa <- db_visa %>% mutate(destination = tolower(destination), origin = tolower(origin))
db_dist <- db_dist %>% mutate(destination = tolower(destination), origin = tolower(origin))
db_sent <- db_sent %>% mutate(destination = tolower(destination))
db_data <- db_data %>% mutate(destination = tolower(destination))

## Change country names
db_visa <- db_visa %>% mutate(destination = case_match(
  destination                                     ,
  "egypt"                   ~ "egypt, arab rep."  ,
  "iran"                    ~ "iran, islamic rep.",
  c("korea", "south korea") ~ "korea, rep."       ,
  "russia"                  ~ "russian federation",
  c("turkiye", "turkey")    ~ "türkiye"           , 
  .default                  = destination
))

db_visa <- db_visa %>% mutate(origin = case_match(
  origin                                          ,
  "egypt"                   ~ "egypt, arab rep."  ,
  "iran"                    ~ "iran, islamic rep.",
  c("korea", "south korea") ~ "korea, rep."       ,
  "russia"                  ~ "russian federation",
  c("turkiye", "turkey")    ~ "türkiye"           , 
  .default                  = origin
))

db_dist <- db_dist %>% mutate(destination = case_match(
  destination                                     ,
  "egypt"                   ~ "egypt, arab rep."  ,
  "iran"                    ~ "iran, islamic rep.",
  c("korea", "south korea") ~ "korea, rep."       ,
  "russia"                  ~ "russian federation",
  c("turkiye", "turkey")    ~ "türkiye"           , 
  .default                  = destination
))

db_dist <- db_dist %>% mutate(origin = case_match(
  origin                                          ,
  "egypt"                   ~ "egypt, arab rep."  ,
  "iran"                    ~ "iran, islamic rep.",
  c("korea", "south korea") ~ "korea, rep."       ,
  "russia"                  ~ "russian federation",
  c("turkiye", "turkey")    ~ "türkiye"           , 
  .default                  = origin
))

db_data <- db_data %>% mutate(destination = case_match(
  destination                                     ,
  "egypt"                   ~ "egypt, arab rep."  ,
  "iran"                    ~ "iran, islamic rep.",
  c("korea", "south korea") ~ "korea, rep."       ,
  "russia"                  ~ "russian federation",
  c("turkiye", "turkey")    ~ "türkiye"           , 
  .default                  = destination
))

db_sent <- db_sent %>% mutate(destination = case_match(
  destination                                     ,
  "egypt"                   ~ "egypt, arab rep."  ,
  "iran"                    ~ "iran, islamic rep.",
  c("korea", "south korea") ~ "korea, rep."       ,
  "russia"                  ~ "russian federation",
  c("turkiye", "turkey")    ~ "türkiye"           , 
  .default                  = destination
))

################################################################################
## Create pairs
################################################################################

## Countries pairs
db_pairs <- data.frame(expand.grid(
  origin      = unique(db_data$destination),
  destination = unique(db_data$destination)
))

## Remove equal pairs
db_pairs <- db_pairs %>% filter(!(origin == destination))

## Join data
db_pairs <- db_pairs %>% 
  left_join(db_visa) %>%
  left_join(db_dist) %>%
  left_join(db_data) %>%
  left_join(db_sent)

## Estimate missing values in United Arab Emirates using other islamic countries
db_pairs <- kNN(
  data     = db_pairs                              , 
  variable = c("cultural_dist")                    ,
  dist_var = c("religious_dist", "linguistic_dist"),
  k        = 5                                     , 
  imp_var  = FALSE                                  
)

## Create visa friction score
db_pairs <- db_pairs %>% mutate(visa = case_match(
  visa                                   ,
  "visa_free_access"                  ~ 0,
  "electronic_travel_authorisation"   ~ 1,
  c("visa_on_arrival", "visa_online") ~ 2,
  "visa_required"                     ~ 4,
  .default                            = NA 
))

## Min-Max normalization function
fn_norm_01 <- function(x) (x - min(x)) / (max(x) - min(x))

## Min max scale in political stability
db_pairs <- db_pairs %>% mutate(political_stability = fn_norm_01(political_stability))

################################################################################
## COPRAS
################################################################################

## COPRAS function
COPRAS <- function(x_mat, w, crit){
  
  ## Step 2 - Normaliza a matriz
  r_mat <- apply(x_mat, 2, \(x) x / sum(x))
  
  ## Step 3 - Matriz Normalizada com os pesos
  r_mat <- r_mat * matrix(w, nrow = nrow(r_mat), ncol = length(w), byrow = TRUE)
  
  ## Step 4 - Soma dos criterios beneficos e nao beneficos
  benefico     <- apply(r_mat[,crit == "+", drop = FALSE], 1, sum) + 1e-6
  nao_benefico <- apply(r_mat[,crit == "-", drop = FALSE], 1, sum) + 1e-6
  
  ## Step 5 - Calculo do Q
  Q <- benefico + sum(nao_benefico)/(nao_benefico * sum(1/nao_benefico))

  ## Step 6 - Calculo da utilidade
  U <- 100*Q/max(Q)

  ## Return
  return(U)
  
}

## Decision matrix
mat_dec <- db_pairs %>% select(-origin, -destination)

## Criteria directions
vec_dir <- c(rep("-", 5),
             rep("+", 3),
             rep("-", 1),
             rep("+", 3))

## Criteria weights (information entropy)
vec_w_ie <- do.call(c, map(mat_dec, \(x) entropy(discretize(x, 10))))
vec_w_ie <- (1 - vec_w_ie) / sum(1 - vec_w_ie)

## Criteria weights (equal)
vec_w_equal <- rep(1/length(vec_dir), length(vec_dir))

## MCDM result
res_w_ie    <- COPRAS(x_mat = as.matrix(mat_dec), w = vec_w_ie   , crit = vec_dir)/100
res_w_equal <- COPRAS(x_mat = as.matrix(mat_dec), w = vec_w_equal, crit = vec_dir)/100

## Correlation result
cor.test(x = res_w_ie, y = res_w_equal, alternative = "two.sided", exact = TRUE, method = "pearson" )
cor.test(x = res_w_ie, y = res_w_equal, alternative = "two.sided", exact = TRUE, method = "spearman")

## Standard deviation
sd(res_w_ie   )
sd(res_w_equal)

## IQR
IQR(res_w_ie   )
IQR(res_w_equal)

## Density
p1 <- ggplot() + 
  geom_density(aes(x = res_w_ie   , fill = "IE weights"   ), alpha = 0.3) + 
  geom_density(aes(x = res_w_equal, fill = "Equal weights"), alpha = 0.3) + 
  xlim(0.2, 1.05) +
  labs(x = "Tourism attraction index", y = "Density") +
  scale_fill_manual(name   = "", values = c("IE weights" = "steelblue", "Equal weights" = "darkgreen")) +
  theme_minimal()

## Save plot
ggsave(plot     = p1                                     ,
       filename = "plot_appendix_copras_distribution.pdf",
       path     = path_output                            ,
       device   = "pdf"                                  ,
       width    = 6                                      ,
       height   = 5                                      ,
       units    = "in"                                   )



