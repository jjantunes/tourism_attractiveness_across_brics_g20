################################################################################
## Clean memory
################################################################################

rm(list = ls())
gc(full = TRUE)

################################################################################
## Libraries
################################################################################

library(writexl  )   ## Write Excel files
library(readxl   )   ## Read Excel files
library(Hmisc    )   ## Statistics
library(tidyverse)   ## Data manipulation

################################################################################
## Paths, seeds and options
################################################################################

## Automatic path library
library(this.path)  

## Root path
path <- this.dir()
path <- rev(rev(strsplit(path, "/")[[1]])[-1])
path <- paste(path, collapse = "/")

## Paths
path_base    <- paste0(path, "/Data/"   )
path_results <- paste0(path, "/Results/")
path_source  <- paste0(path, "/Codes/"  )

## Seed
set.seed(100)

################################################################################
## Database manipulation
################################################################################

## Path
setwd(path_results)

## Read data
db_reviews <- read_excel("Database - Reviews Tripadvisor Sentiment.xlsx")

## Group data by index/review
db_res <- db_reviews                                                      %>% 
  group_by(Index)                                                         %>% 
  summarise(Sentiment_Index   = mean  (Sentiment_Index  ),
            Attraction_Weight = unique(Attraction_Weight))                %>% 
  left_join(db_reviews %>% select(Index, Country, G20, BRICS) %>% unique) %>% 
  ungroup()

## Descriptive statistics
db_res_desc <- db_res %>% group_by(Country, G20, BRICS) %>% summarise(
  sentiment = wtd.mean(Sentiment_Index, Attraction_Weight)      ,
  sd        = wtd.var (Sentiment_Index, Attraction_Weight)^(0.5),
  cv        = sd / sentiment                                           
)

## Save table
write_xlsx(db_res_desc, file.path(path_results, "table_country_sentiment.xlsx"))
