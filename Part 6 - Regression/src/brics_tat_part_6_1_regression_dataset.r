################################################################################
## Clear environment
################################################################################

rm(list = ls())
gc(full = TRUE)

################################################################################
## libraries
################################################################################

library(writexl    )  ## Write excel files
library(readxl     )  ## Read excel files
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
db_ctxl <- read_excel(file.path(path_data, "table_countries.xlsx"                 ))
db_attr <- read_excel(file.path(path_data, "table_touristic_attraction_index.xlsx"))
db_dist <- read_excel(file.path(path_data, "table_country_distance.xlsx"          ))
db_data <- read_excel(file.path(path_data, "table_data_360_indicators.xlsx"       ))

################################################################################
## Data manipulation
################################################################################

## Select relevant data
db_ctxl <- db_ctxl %>% select(-`Link TA`)
db_dist <- db_dist %>% select(country_origin, country_dest, population_origin, population_dest, common_border)
db_data <- db_data %>% select(COUNTRY_NAME, INDICATOR_NAME, VALUE)

## Filter relevant data
db_data <- db_data %>% filter(INDICATOR_NAME %in% c(
  
  "GDP per capita, PPP (current international $)"                       ,
  "Unemployment, total (% of total labor force) (modeled ILO estimate)" ,
  "Foreign direct investment, net inflows (% of GDP)"                   ,
  "Inflation, consumer prices (annual %)"                               ,
  "Logistics performance index: Overall (1=low to 5=high)"              ,
  "Press Freedom Index Score"                                           ,
  "Control of Corruption: Estimate"                                     ,
  "Government Effectiveness: Estimate"                                  ,
  "Rule of Law: Estimate"                                               ,
  "Voice and Accountability: Estimate"                                  ,
  "Official exchange rate (LCU per US$, period average)"                 
  
))

## Pivot data
db_data <- db_data %>% pivot_wider(names_from = INDICATOR_NAME, values_from = VALUE)

## Change names
db_ctxl <- db_ctxl %>% rename(country = Countries, g20 = G20, brics = BRICS)
db_attr <- db_attr %>% rename(tourism_attractiveness_index = score)
db_dist <- db_dist %>% rename(origin = country_origin, destination = country_dest)
db_data <- db_data %>% rename(
  country                  = "COUNTRY_NAME"                                                        ,                            
  gdp_pp_ppp               = "GDP per capita, PPP (current international $)"                       ,
  unemployment             = "Unemployment, total (% of total labor force) (modeled ILO estimate)" ,
  fdi                      = "Foreign direct investment, net inflows (% of GDP)"                   ,
  inflation                = "Inflation, consumer prices (annual %)"                               ,
  lpi                      = "Logistics performance index: Overall (1=low to 5=high)"              ,
  press_freedom            = "Press Freedom Index Score"                                           ,
  control_corruption       = "Control of Corruption: Estimate"                                     ,
  government_effectiveness = "Government Effectiveness: Estimate"                                  ,
  rule_of_law              = "Rule of Law: Estimate"                                               ,
  voice_accountability     = "Voice and Accountability: Estimate"                                  ,
  exchange_rate            = "Official exchange rate (LCU per US$, period average)"                 
)

## Change case
db_attr <- db_attr %>% mutate(destination = tolower(destination), origin = tolower(origin))
db_dist <- db_dist %>% mutate(destination = tolower(destination), origin = tolower(origin))
db_data <- db_data %>% mutate(country = tolower(country))
db_ctxl <- db_ctxl %>% mutate(country = tolower(country))

## Change country names
db_attr <- db_attr %>% mutate(destination = case_match(
  destination                                     ,
  "egypt"                   ~ "egypt, arab rep."  ,
  "iran"                    ~ "iran, islamic rep.",
  c("korea", "south korea") ~ "korea, rep."       ,
  "russia"                  ~ "russian federation",
  c("turkiye", "turkey")    ~ "türkiye"           , 
  .default                  = destination
))

db_attr <- db_attr %>% mutate(origin = case_match(
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

db_data <- db_data %>% mutate(country = case_match(
  country                                         ,
  "egypt"                   ~ "egypt, arab rep."  ,
  "iran"                    ~ "iran, islamic rep.",
  c("korea", "south korea") ~ "korea, rep."       ,
  "russia"                  ~ "russian federation",
  c("turkiye", "turkey")    ~ "türkiye"           ,
  .default                  = country              
))

db_ctxl <- db_ctxl %>% mutate(country = case_match(
  country                                         ,
  "egypt"                   ~ "egypt, arab rep."  ,
  "iran"                    ~ "iran, islamic rep.",
  c("korea", "south korea") ~ "korea, rep."       ,
  "russia"                  ~ "russian federation",
  c("turkiye", "turkey")    ~ "türkiye"           ,
  .default                  = country              
))

################################################################################
## Create dataset
################################################################################

## Regression dataset
db_reg <- db_attr %>% left_join(db_dist)

## Add origin data
db_reg <- db_reg %>%
  left_join(select(db_data, country, gdp_pp_ppp, unemployment, exchange_rate), by = join_by(origin == country)) %>%
  rename(gdp_pp_ppp_origin = gdp_pp_ppp, unemployment_origin = unemployment, exchange_rate_origin = exchange_rate)

## Add destination data
db_reg <- db_reg %>%
  left_join(db_data, by = join_by(destination == country)) %>%
  rename(gdp_pp_ppp_destination               = gdp_pp_ppp               ,
         fdi_destination                      = fdi                      ,
         inflation_destination                = inflation                ,
         lpi_destination                      = lpi                      ,
         press_freedom_destination            = press_freedom            ,
         control_corruption_destination       = control_corruption       ,
         government_effectiveness_destination = government_effectiveness ,
         rule_of_law_destination              = rule_of_law              ,
         voice_accountability_destination     = voice_accountability     ,
         exchange_rate_destination            = exchange_rate            )

## Add contextual data
db_reg <- db_reg %>% 
  left_join(db_ctxl, by = join_by(destination == country)) %>% 
  rename(g20_destination = g20, brics_destination = brics) %>% 
  left_join(db_ctxl, by = join_by(origin == country)) %>% 
  rename(g20_origin = g20, brics_origin = brics)

## Create bilateral data
db_reg <- db_reg %>% mutate(
  exchange_rate_ratio = exchange_rate_origin / exchange_rate_destination,
  
  origin_status = case_when(
    g20_origin == "Yes" & brics_origin == "Yes" ~ "g20_brics",
    g20_origin == "Yes" & brics_origin == "No"  ~ "g20"      ,
    g20_origin == "No"  & brics_origin == "Yes" ~ "brics"    
  ),
  
  dest_status = case_when(
    g20_destination == "Yes" & brics_destination == "Yes" ~ "g20_brics",
    g20_destination == "Yes" & brics_destination == "No"  ~ "g20"      ,
    g20_destination == "No"  & brics_destination == "Yes" ~ "brics"    
  ),
  
  pair_composition = paste(origin_status, dest_status, sep = "_to_")
)

## Criar dummies
db_reg <- db_reg %>% mutate(
  g20_to_g20           = ifelse(pair_composition == "g20_to_g20"            , 1, 0),
  g20_to_brics         = ifelse(pair_composition == "g20_to_brics"          , 1, 0),
  g20_to_g20brics      = ifelse(pair_composition == "g20_to_g20_brics"      , 1, 0),
  brics_to_g20         = ifelse(pair_composition == "brics_to_g20"          , 1, 0),
  brics_to_brics       = ifelse(pair_composition == "brics_to_brics"        , 1, 0),
  brics_to_g20brics    = ifelse(pair_composition == "brics_to_g20_brics"    , 1, 0),
  g20brics_to_g20      = ifelse(pair_composition == "g20_brics_to_g20"      , 1, 0),
  g20brics_to_brics    = ifelse(pair_composition == "g20_brics_to_brics"    , 1, 0),
  g20brics_to_g20brics = ifelse(pair_composition == "g20_brics_to_g20_brics", 1, 0)
)

db_reg <- db_reg %>% select(
  "origin"                              ,
  "destination"                         ,
  "tourism_attractiveness_index"        ,
  "gdp_pp_ppp_origin"                   ,
  "population_origin"                   ,
  "unemployment_origin"                 ,
  "gdp_pp_ppp_destination"              ,
  "fdi_destination"                     ,
  "inflation_destination"               ,
  "lpi_destination"                     ,
  "press_freedom_destination"           ,
  "control_corruption_destination"      ,
  "government_effectiveness_destination",
  "rule_of_law_destination"             ,
  "voice_accountability_destination"    ,
  "common_border"                       ,
  "exchange_rate_ratio"                 ,
  "g20_to_g20"                          ,
  "g20_to_brics"                        ,
  "g20_to_g20brics"                     ,
  "brics_to_g20"                        ,
  "brics_to_brics"                      ,
  "brics_to_g20brics"                   ,
  "g20brics_to_g20"                     ,
  "g20brics_to_brics"                   ,
  "g20brics_to_g20brics"                 
)

## Save dataset
write_xlsx(db_reg, file.path(path_output, "table_regression_dataset.xlsx"))
