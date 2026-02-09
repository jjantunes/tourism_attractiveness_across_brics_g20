################################################################################
## Last Modification: 2025-09-01
################################################################################

################################################################################
## Clear environment
################################################################################

rm(list = ls())
gc(full = TRUE)

################################################################################
## libraries
################################################################################

library(readxl   )  ## Read Excel files
library(writexl  )  ## Write Excel files
library(arrow    )  ## I/O Parquet files
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
path_data    <- paste0(path, "/Data/"   )
path_results <- paste0(path, "/Results/")

## Seed
set.seed(123)

################################################################################
## Read data
################################################################################

## Read countries
setwd(path_results)

## Read countries
db_country <- read_excel("Table Variables Worldbank.xlsx", sheet = "COUNTRIES")
db_vars    <- read_excel("Table Variables Worldbank.xlsx", sheet = "SELECTED" )

## Country Names and Codes
country_code <- db_country$`Country Code`
country_name <- db_country$Country

## Variables
vars <- db_vars$VARIABLE

## Databases
dbs <- unique(db_vars$DB)

## List files in path
files_parquet <- list.files(path_data, full.names = TRUE, pattern = ".parquet")

## Select only used db
files_parquet <- grep(paste(dbs, collapse = "|"), files_parquet, value = TRUE)

## Create consolidated data base
db <- NULL

## Remove unused variables
rm(db_country)
rm(db_vars   )

################################################################################
## Read FAO_AS
################################################################################

## Read parquet
db_i <- read_parquet(grep("FAO_AS", files_parquet, value = TRUE))

## Filter variables
db_i <- db_i %>% filter(INDICATOR_LABEL %in% vars)

## Filter countries
db_i <- db_i %>% filter(REF_AREA %in% country_code)

## Filter last year
db_i <- db_i %>% filter(TIME_PERIOD == max(TIME_PERIOD))

## Select only relevant data
db_i <- db_i %>% select(REF_AREA        ,
                        REF_AREA_LABEL  ,
                        TIME_PERIOD     ,
                        INDICATOR       ,
                        INDICATOR_LABEL ,
                        OBS_VALUE       )

## Change names
colnames(db_i) <- c("COUNTRY_CODE"  ,
                    "COUNTRY_NAME"  ,
                    "YEAR"          ,
                    "INDICATOR_ID"  ,
                    "INDICATOR_NAME",
                    "VALUE"         )

## Add in data base
db <- rbind(db, db_i)

## Remove unused variables
rm(db_i)

################################################################################
## Read RWB_PFI
################################################################################

## Read parquet
db_i <- read_parquet(grep("RWB_PFI", files_parquet, value = TRUE))

## Filter variables
db_i <- db_i %>% filter(INDICATOR_NAME %in% vars)

## Filter countries
db_i <- db_i %>% filter(REF_AREA_ID %in% country_code)

## Filter last year
db_i <- db_i %>% filter(TIME_PERIOD == max(TIME_PERIOD))

## Select only relevant data
db_i <- db_i %>% select(REF_AREA_ID     ,
                        REF_AREA_NAME   ,
                        TIME_PERIOD     ,
                        INDICATOR_ID    ,
                        INDICATOR_NAME  ,
                        OBS_VALUE       )

## Change names
colnames(db_i) <- c("COUNTRY_CODE"  ,
                    "COUNTRY_NAME"  ,
                    "YEAR"          ,
                    "INDICATOR_ID"  ,
                    "INDICATOR_NAME",
                    "VALUE"         )

## Add in data base
db <- rbind(db, db_i)

## Remove unused variables
rm(db_i)

################################################################################
## Read WB_WDI
################################################################################

## Read parquet
db_i <- read_parquet(grep("WB_WDI", files_parquet, value = TRUE))

## Filter variables
db_i <- db_i %>% filter(INDICATOR_LABEL %in% vars)

## Filter countries
db_i <- db_i %>% filter(REF_AREA %in% country_code)

## Filter last year per indicator
db_i <- db_i                              %>% 
  group_by(INDICATOR, REF_AREA)           %>%
  filter(TIME_PERIOD == max(TIME_PERIOD)) %>% 
  ungroup

## Select only relevant data
db_i <- db_i %>% select(REF_AREA        ,
                        REF_AREA_LABEL  ,
                        TIME_PERIOD     ,
                        INDICATOR       ,
                        INDICATOR_LABEL ,
                        OBS_VALUE       )

## Change names
colnames(db_i) <- c("COUNTRY_CODE"  ,
                    "COUNTRY_NAME"  ,
                    "YEAR"          ,
                    "INDICATOR_ID"  ,
                    "INDICATOR_NAME",
                    "VALUE"         )

## Add Inflation in Argentina
db_i  <- db_i                                                      %>% 
  filter(COUNTRY_CODE == "ARG", INDICATOR_ID == "WB_WDI_CC_EST")   %>%
  mutate(YEAR = 2024)                                              %>% 
  mutate(INDICATOR_ID = "WB_WDI_FP_CPI_TOTL_ZG")                   %>% 
  mutate(INDICATOR_NAME = "Inflation, consumer prices (annual %)") %>%
  mutate(VALUE = 117.8)                                            %>%
  bind_rows(db_i)                                                        

## Add in data base
db <- rbind(db, db_i)

## Remove unused variables
rm(db_i)

################################################################################
## Read FAO_AS
################################################################################

## Read parquet
db_i <- read_parquet(grep("WEF_TTDI", files_parquet, value = TRUE))

## Filter variables
db_i <- db_i %>% filter(INDICATOR_LABEL %in% vars)

## Filter countries
db_i <- db_i %>% filter(REF_AREA %in% country_code)

## Filter last year
db_i <- db_i                              %>% 
  group_by(INDICATOR, REF_AREA)           %>%
  filter(TIME_PERIOD == max(TIME_PERIOD)) %>% 
  ungroup

## Filter Counts only
db_i <- db_i %>% filter(COMP_BREAKDOWN_1 == "WEF_TTDI_VAL")

## Select only relevant data
db_i <- db_i %>% select(REF_AREA        ,
                        REF_AREA_LABEL  ,
                        TIME_PERIOD     ,
                        INDICATOR       ,
                        INDICATOR_LABEL ,
                        OBS_VALUE       )

## Change names
colnames(db_i) <- c("COUNTRY_CODE"  ,
                    "COUNTRY_NAME"  ,
                    "YEAR"          ,
                    "INDICATOR_ID"  ,
                    "INDICATOR_NAME",
                    "VALUE"         )

## Add missing data for russia and ethiopia
db_i <- db_i %>% slice(1:4) %>% mutate(
  COUNTRY_CODE   = c("RUS", "ETH", "RUS", "ETH"),
  COUNTRY_NAME   = c("Russian Federation", "Ethiopia", "Russian Federation", "Ethiopia"),
  INDICATOR_ID   = c(rep("WEF_TTDI_CULTHERITGSITE", 2), rep("WEF_TTDI_NATHERITGSITE" , 2)),
  INDICATOR_NAME = c(rep("Number of World Heritage cultural sites", 2), rep("Number of World Heritage natural sites" , 2)),
  VALUE          = c(21, 9, 11, 3)
) %>% rbind(db_i)

## Add in data base
db <- rbind(db, db_i)

## Remove unused variables
rm(db_i)

################################################################################
## Save database
################################################################################

## Path
setwd(path_results)

## Save database
write_xlsx(db, "Table Contextual Variables.xlsx")
