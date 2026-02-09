################################################################################
## Visa restriction data
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

library(writexl  )  ## Write excel files
library(httr     )  ## Request API data
library(jsonlite )  ## JSON files 
library(tidyverse)  ## Data manipulation
library(pbapply  )  ## Progress bar lapply

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
path_output <- paste0(path, "/output/")

## Seed
set.seed(100)

################################################################################
## Download Henley Passaport Power Data from API
################################################################################

## URL with API endpoint
url_hpp <- "https://api.henleypassportindex.com/api/v2/hpp"

## Get response
res_hpp <- GET(url_hpp)

## Check status
cat("Status:", res_hpp$status_code)

## Coerce to data frame
data_hpp <- fromJSON(content(res_hpp, as = "text", encoding = "UTF-8"))

## Select only relevant data
data_hpp <- data_hpp %>% select(name, code, hpi_score, hpp)

## Change names
data_hpp <- data_hpp %>% rename(
  country = name     ,
  code    = code     ,
  hpi     = hpi_score,
  hpp     = hpp       
)

## Coerce to data frame
data_hpp <- data_hpp %>% data.frame

## Remove unused variables
rm(url_hpp    )
rm(res_hpp    )

################################################################################
## Download data for a specific country from Henley API
################################################################################

## Function to download visa data
fn_download_visa_country <- function(code, path_save, pause = 1) {
  
  ## URL with API endpoint
  url_visa <- "https://api.henleypassportindex.com/api/v3/visa-single/"
  
  ## URL for country
  url_visa_country <- paste0(url_visa, code)
  
  ## Get response
  res_visa <- GET(url_visa_country)
  
  ## Coerce to data frame
  data_country <- fromJSON(content(res_visa, as = "text", encoding = "UTF-8"))

  ## File name
  file_i <- data_country$country
  file_i <- gsub(" ", "_", file_i)
  file_i <- tolower(paste0("visa_", file_i, ".json"))
  
  ## Save json
  write_json(data_country, path = file.path(path_save, file_i))
  
  ## Pause to avoid rate limit
  Sys.sleep(pause)

}

## output folder
path_countries <- file.path(path_output, "countries_json")

## Create output folder
if(!dir.exists(file.path(path_countries))) dir.create(path_countries)

## Download visa data
pblapply(data_hpp$code, fn_download_visa_country, path_save = path_countries)

################################################################################
## Create dataset from Countries json files 
################################################################################

## Function to create database from json files
fn_data_countries <- function(file_json, data_hpp) {
  
  ## Coerce to data frame
  data_country <- fromJSON(file_json, flatten = TRUE)
  
  ## Visa Data
  db_visa <- data.frame(rbind(
    mutate(data_country$visa_on_arrival                , visa = "visa_on_arrival"                ),
    mutate(data_country$electronic_travel_authorisation, visa = "electronic_travel_authorisation"),
    mutate(data_country$visa_online                    , visa = "visa_online"                    ),
    mutate(data_country$visa_required                  , visa = "visa_required"                  ),
    mutate(data_country$visa_free_access               , visa = "visa_free_access"               )
  ))
  
  ## Add other variables
  db_country <- data.frame(data_country$country, data_country$code, db_visa)
  
  ## Change names
  colnames(db_country) <- c("country_origin", "code_origin", "code_dest", "country_dest", "visa")
  
  ## Add HPI e HPP values for origin country
  db_country <- db_country %>%
    left_join(select(data_hpp, -country), by = join_by(code_origin == code))
  
  ## Change names
  db_country <- db_country %>% rename(hpi_origin = hpi, hpp_origin = hpp)
  
  ## Add HPI e HPP values for origin country
  db_country <- db_country %>%
    left_join(select(data_hpp, -country), by = join_by(code_dest == code))
  
  ## Change names
  db_country <- db_country %>% rename(hpi_dest = hpi, hpp_dest = hpp)
  
  ## Relocate 
  db_country <- db_country %>% relocate(
    country_origin,
    code_origin   ,
    hpi_origin    ,
    hpp_origin    ,
    country_dest  ,
    code_dest     ,
    hpi_dest      ,
    hpp_dest      ,
    visa           
  )
  
  ## Return data frame
  return(db_country)
  
}

## List json files
files_country <- list.files(path_countries, pattern = ".json", full.names = TRUE)

## Visa data
res_visa <- pblapply(files_country, fn_data_countries, data_hpp = data_hpp)

## Coerce data to a single data frame
db_country_visa <- bind_rows(res_visa)

## Save data
write_xlsx(db_country_visa, file.path(path_output, "table_countries_visa.xlsx"))


