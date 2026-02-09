################################################################################
## Clear environment
################################################################################

rm(list = ls())
gc(full = TRUE)

################################################################################
## libraries
################################################################################

library(writexl  )  ## Write excel files
library(readxl   )  ## Read excel files
library(ggplot2  )  ## Plots
library(ggridges )  ## Ridge density plot
library(tidyverse)  ## Data manipulation

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
path_data   <- paste0(path, "/data"  )
path_output <- paste0(path, "/output")

## Seed
set.seed(100)

################################################################################
## Read data
################################################################################

## Read data
db_coefs      <- read_excel(file.path(path_data, "table_coefs_comparison.xlsx"           ))
db_ie_weights <- read_excel(file.path(path_data, "table_information_entropy_weights.xlsx"))
db_reg        <- read_excel(file.path(path_data, "table_regression_dataset.xlsx"         ))
db_cv_comp    <- read_excel(file.path(path_data, "table_model_comparison.xlsx"           ))

################################################################################
## Importance plot
################################################################################

## Change names
db_coefs <- db_coefs %>% mutate(
  variables = str_replace_all(variables, "gdp_pp_ppp"         , "gdp_pc_ppp"                     ),
  variables = str_replace_all(variables, "democratic_openness", "democratic_openness_destination"),
  variables = str_replace_all(variables, "governance_quality" , "governance_quality_destination" )
)

## Random Forest importance plot
p1 <- ggplot(db_coefs, aes(x = importance_rf, y = fct_reorder(variables, importance_rf), color = significance_rf)) +
  geom_segment(aes(x = 0, xend = importance_rf, y = variables, yend = variables)) + 
  geom_label(aes(label = round(importance_rf, 2)), show.legend = FALSE) + 
  scale_color_manual("Boruta Importance", values = c("Confirmed" = "steelblue", "Rejected" = "gray70")) +
  xlab("Importance (% Inc. MSE)") +
  ylab("") +
  theme_minimal() +
  theme(legend.position = "top")

## Save coefficients plot
ggsave(plot     = p1                 ,
       filename = "plot_coefs_rf.pdf",
       path     = path_output        , 
       device   = "pdf"              ,
       width    = 8                  ,
       height   = 6                  ,
       units    = "in"               )


################################################################################
## Information Entropy Weights
################################################################################

## Change variables names
db_ie_weights <- db_ie_weights %>% mutate(variables = case_match(
  variables                                                                                   ,
  "visa"                               ~ "Visa restriction"                                   ,
  "cultural_dist"                      ~ "Cultural distance"                                  ,
  "geographic_dist"                    ~ "Geographic distance"                                ,
  "religious_dist"                     ~ "Religious distance"                                 ,
  "linguistic_dist"                    ~ "Linguistic distance"                                ,
  "hdi"                                ~ "HDI destination"                                    ,
  "protected_areas_terrestrial_marine" ~ "Terrestrial and marine protected areas destination" ,
  "political_stability"                ~ "Political stability destination"                    ,
  "homicides"                          ~ "Intentional homicides destination"                  ,
  "number_cultural_sites"              ~ "Number of World Heritage cultural sites destination",
  "number_natural_sites"               ~ "Number of World Heritage natural sites destination" ,
  "sentiment"                          ~ "Country sentiment destination"                      ,
  .default                             = variables                                 
))

## Plot
p2 <- ggplot(data = db_ie_weights, aes(x = weights, y = fct_reorder(variables, weights))) + 
  geom_segment(aes(y = variables, x = 0, xend = weights), color = "steelblue") + 
  geom_label(aes(label = round(weights, 3)), color = "steelblue") + 
  xlab("Weights") + 
  ylab("") +
  theme_minimal()

## Save plot
ggsave(plot     = p2                                    ,
       filename = "plot_information_entropy_weights.pdf",
       path     = path_output                           , 
       device   = "pdf"                                 ,
       width    = 9                                     ,
       height   = 5                                     ,
       units    = "in"                                  )

################################################################################
## Touristic Attraction Index density
################################################################################

## Aggregate density
p3 <- ggplot(db_reg) + 
  geom_density(aes(x = tourism_attractiveness_index)) + 
  xlab("Tourism attraction index") + 
  ylab("Density") +
  theme_minimal()

## Save plot
ggsave(plot     = p3                                         ,
       filename = "plot_tourism_attraction_index_density.pdf",
       path     = path_output                                , 
       device   = "pdf"                                      ,
       width    = 5                                          ,
       height   = 4                                          ,
       units    = "in"                                       )


################################################################################
## Touristic Attraction Index ridge density
################################################################################

## Select data
db_ridge <- db_reg %>% select(tourism_attractiveness_index, g20_to_g20:g20brics_to_g20brics)

## Transform to factor
db_ridge <- db_ridge %>% 
  pivot_longer(-tourism_attractiveness_index, names_to = "cat", values_to = "valor") %>%
  filter(valor == 1) %>%
  select(-valor) %>% 
  mutate(cat = str_replace_all(cat, pattern = "_"    , replacement = " "    ),
         cat = str_replace_all(cat, pattern = "g20"  , replacement = "G20"  ),
         cat = str_replace_all(cat, pattern = "brics", replacement = "BRICS"))

## Aggregate density
p4 <- ggplot(db_ridge, aes(x = tourism_attractiveness_index, y = cat)) + 
  geom_density_ridges(fill = "steelblue", alpha = 0.4) + 
  xlab("Tourism attraction index") + 
  ylab("") +
  theme_minimal()

## Save plot
ggsave(plot     = p4                                               ,
       filename = "plot_tourism_attraction_index_ridge_density.pdf",
       path     = path_output                                      , 
       device   = "pdf"                                            ,
       width    = 6                                                ,
       height   = 7                                                ,
       units    = "in"                                             )

################################################################################
## LOOCV results
################################################################################

db_cv_comp <- db_cv_comp %>% 
  filter(mtry > 2) %>%
  mutate(mtry = paste0("m = ", mtry)) %>% 
  mutate(mtry = factor(mtry, levels = unique(mtry)))

db_bg <- db_cv_comp %>% mutate(group_var = mtry) %>% select(-mtry) 

p5 <- ggplot(db_cv_comp, aes(x = n_tree, y = MAPE)) +
  facet_wrap(~mtry) +
  geom_line(data = db_bg, aes(group = group_var), color = "grey85", linewidth = 0.5) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") + 
  xlab("Trees") + 
  ylab("LOOCV MAPE (%)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## Save plot
ggsave(plot     = p5                      ,
       filename = "plot_loocv_rf_mape.pdf",
       path     = path_output             , 
       device   = "pdf"                   ,
       width    = 7                       ,
       height   = 6                       ,
       units    = "in"                    )
  
