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
## Review analysis
################################################################################

## Read data
db_reviews <- read_excel(file.path(path_data, "table_reviews_tripadvisor_sentiment.xlsx"))

## Select relevant data
db_reviews <- db_reviews %>% select(Rate, Sentiment_Index)

## Rate transformation
db_reviews <- db_reviews %>% mutate(Rate = (Rate - 3)/2)

## Correlation (pearson)
cor.test(x           = db_reviews$Rate           ,
         y           = db_reviews$Sentiment_Index,
         alternative = "two.sided"               ,
         exact       = TRUE                      ,
         method      = "pearson"                 )

## Correlation (spearman)
cor.test(x           = db_reviews$Rate           ,
         y           = db_reviews$Sentiment_Index,
         alternative = "two.sided"               ,
         exact       = TRUE                      ,
         method      = "spearman"                )

## Metrics
db_reviews %>% summarise(RMSE = mean(   (Rate - Sentiment_Index)^2)^0.5,
                         MAE  = mean(abs(Rate - Sentiment_Index)  )    )








