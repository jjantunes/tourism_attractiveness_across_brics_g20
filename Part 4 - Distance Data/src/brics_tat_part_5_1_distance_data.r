################################################################################
## Distance data
## Last Modification: 2025-11-23
################################################################################

################################################################################
## Clear environment
################################################################################

rm(list = ls())
gc(full = TRUE)

################################################################################
## libraries
################################################################################

library(rvest      )  ## Read html from webpages
library(writexl    )  ## Write excel files
library(readstata13)  ## Read STATA 13 data
library(tidyverse  )  ## Data manipulation
library(pbapply    )  ## Progress bar lapply

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
## Read geonames table
################################################################################

## Geonames table
url_geonames <- "https://www.geonames.org/countries/"

## Get table
db_geonames <- session(url_geonames) %>% read_html() %>% html_table()
db_geonames <- db_geonames[[2]]

## Select only relevant data
db_geonames <- db_geonames %>% select(
  `ISO-3166alpha3`,
  Country         ,
  `Area in km²`   ,
  Population       
)

## Rename
db_geonames <- db_geonames %>% rename(
  code       = `ISO-3166alpha3`,
  country    = Country         ,
  area_km2   = `Area in km²`   ,
  population = Population       
)

## Remove unused data
rm(url_geonames)

################################################################################
## Read data
################################################################################

## Read data
db_cult  <- read.dta13(file.path(path_data, "cultural_distance_PSW2024.dta"  ))
db_geo   <- read.dta13(file.path(path_data, "geodist_PSW24.dta"              ))
db_relig <- read.dta13(file.path(path_data, "religious_distance_PSW2024.dta" ))
db_lang  <- read.dta13(file.path(path_data, "linguistic_distance_PSW2024.dta"))

################################################################################
## Data manipulation
################################################################################

## Filter data to last year
db_cult  <- db_cult  %>% filter(year == max(year))
db_relig <- db_relig %>% filter(year == max(year))

## Select relevant data
db_cult  <- db_cult  %>% select(countrycode_1, countrycode_2, cultdist)
db_geo   <- db_geo   %>% select(countrycode_1, countrycode_2, border, avg_distance_km)
db_relig <- db_relig %>% select(countrycode_1, countrycode_2, reldist_weighted)
db_lang  <- db_lang  %>% select(countrycode_1, countrycode_2, lingdist_tree_weighted)

## Rename data
db_cult  <- db_cult  %>% rename(code_origin = countrycode_1, code_dest = countrycode_2, cultural_dist   = cultdist              )
db_geo   <- db_geo   %>% rename(code_origin = countrycode_1, code_dest = countrycode_2, geographic_dist = avg_distance_km       )
db_relig <- db_relig %>% rename(code_origin = countrycode_1, code_dest = countrycode_2, religious_dist  = reldist_weighted      )
db_lang  <- db_lang  %>% rename(code_origin = countrycode_1, code_dest = countrycode_2, linguistic_dist = lingdist_tree_weighted)

## Join databases
db_dist <- db_cult %>% 
  full_join(db_geo) %>%
  full_join(db_relig) %>%
  full_join(db_lang)

## Join origin data with geonames
db_dist <- db_dist %>% left_join(db_geonames, by = join_by(code_origin == code))

## Rename
db_dist <- db_dist %>% rename(
  country_origin    = country   ,
  area_origin       = area_km2  ,
  population_origin = population
)

## Join destination data with geonames
db_dist <- db_dist %>% left_join(db_geonames, by = join_by(code_dest == code))

## Rename
db_dist <- db_dist %>% rename(
  country_dest    = country   ,
  area_dest       = area_km2  ,
  population_dest = population
)

## Change types
db_dist <- db_dist %>% mutate(
  area_origin       = as.numeric(gsub(",", "", area_origin      )),
  population_origin = as.numeric(gsub(",", "", population_origin)),
  area_dest         = as.numeric(gsub(",", "", area_dest        )),
  population_dest   = as.numeric(gsub(",", "", population_dest  ))
)

## Remove same country pairs
db_dist <- db_dist %>% filter(code_origin != code_dest)

## Remove unused data
db_dist <- db_dist %>% select(-code_origin, -code_dest)

## Rename
db_dist <- db_dist %>% rename(common_border = border)

## Relocate data
db_dist <- db_dist %>% relocate(
  country_origin, area_origin, population_origin, 
  country_dest  , area_dest  , population_dest  ,
  common_border
)

## Save data
write_xlsx(db_dist, file.path(path_output, "table_country_distance.xlsx"))

