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

library(writexl  )  ## Write Excel files
library(arrow    )  ## Write Parquet files
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
## Read files
################################################################################

## List files in path
files_parquet <- list.files(path_data, full.names = TRUE, pattern = ".parquet")

## Data base names
db_names <- gsub(
  pattern     = ".parquet"                                                     , 
  replacement = ""                                                             , 
  x           = list.files(path_data, full.names = FALSE, pattern = ".parquet") 
)

## Data frame with indicators
db_indicators <- NULL

## Read data
for(i in 1:144) {
  
  ## Read parquet
  db_i <- read_parquet(files_parquet[i])
  
  ## Select indicators
  try(db_i <- db_i %>% select(INDICATOR   , INDICATOR_LABEL), silent = TRUE)
  try(db_i <- db_i %>% select(INDICATOR_ID, INDICATOR_NAME ), silent = TRUE)
  
  ## Unique values
  db_i <- db_i %>% unique
  
  ## Add file
  db_i <- db_i %>% mutate(FILE = db_names[i])
  
  ## Rename
  colnames(db_i) <- c("INDICATOR", "INDICATOR_LABEL", "FILE")
  
  ## Print evaluation
  cat("db:", i, "-" , "indicators:", nrow(db_i), "\n")
  
  ## Add in indicators database
  db_indicators <- rbind(db_indicators, db_i)
  
  ## Clean memory
  rm(db_i       )
  gc(full = TRUE)
  
}

## Remove unused variables
rm(files_parquet)
rm(db_names     )

################################################################################
## Save indicators names
################################################################################

## Path
setwd(path_results)

## Save database
write_xlsx(db_indicators, "Table Data 360 Indicators.xlsx")

