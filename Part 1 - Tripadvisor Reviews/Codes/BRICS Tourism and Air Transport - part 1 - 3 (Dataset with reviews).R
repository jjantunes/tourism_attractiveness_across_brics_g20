################################################################################
## Clean memory
################################################################################

rm(list = ls())
gc(full = TRUE)

################################################################################
## Libraries
################################################################################

library(digest   )   ## Hash values
library(arrow    )   ## Write parquet files
library(jsonlite )   ## JSON files
library(tidyverse)   ## Data manipulation
library(lubridate)   ## Date manipulation
library(cld2     )   ## Language detection

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

## Other paths
path_results_rev <- paste0(path_results, "/Reviews/")

## Seed
set.seed(100)

################################################################################
## Database manipulation
################################################################################

## JSON Files
files_json <- list.files(path_results_rev, full.names = TRUE, pattern = ".json")

## Read jsons
db_reviews <- lapply(files_json, fromJSON, flatten = TRUE)

## Select data
db_reviews <- lapply(db_reviews, \(x) x[setdiff(names(x), c("Profiles"))])

## Coerce to data frame
db_reviews <- bind_rows(lapply(db_reviews, bind_rows))

## Rename
db_reviews <- db_reviews %>% rename(Country = Countries,  
                                    Rate    = Stars_rev,      
                                    Title   = Titles   ,  
                                    Text    = Texts    ,  
                                    Date    = Dates    )  

## Correct types
db_reviews <- db_reviews %>% mutate(
  Index      = as.character(Index     ),
  Country    = as.character(Country   ),
  G20        = as.character(G20       ),
  BRICS      = as.character(BRICS     ),
  Categories = as.character(Categories),
  Attraction = as.character(Attraction),
  Qnt_rev    = as.character(Qnt_rev   ),
  Qnt_5      = as.character(Qnt_5     ),
  Qnt_4      = as.character(Qnt_4     ),
  Qnt_3      = as.character(Qnt_3     ),
  Qnt_2      = as.character(Qnt_2     ),
  Qnt_1      = as.character(Qnt_1     ),
  Rate       = as.character(Rate      ),
  Title      = as.character(Title     ),
  Text       = as.character(Text      ),
  Date       = as.character(Date      ) 
)

## Remove missing values
db_reviews <- na.omit(db_reviews)

## Correct format
db_reviews <- db_reviews %>% mutate(
  Date    = mdy       (gsub("Written ", "" , db_reviews$Date                          )),
  Qnt_rev = as.numeric(gsub(","       , "" , sapply(strsplit(Qnt_rev, " "), \(x) x[1]))), 
  Qnt_5   = as.numeric(gsub(","       , "" , Qnt_5                                    )), 
  Qnt_4   = as.numeric(gsub(","       , "" , Qnt_4                                    )), 
  Qnt_3   = as.numeric(gsub(","       , "" , Qnt_3                                    )), 
  Qnt_2   = as.numeric(gsub(","       , "" , Qnt_2                                    )), 
  Qnt_1   = as.numeric(gsub(","       , "" , Qnt_1                                    )), 
  Rate    = as.numeric(gsub(","       , ".", sapply(strsplit(Rate, " "), \(x) x[1])   ))
)

## Create mean rate
db_reviews <- db_reviews %>% group_by(Index) %>% mutate(
  Rate_mean = (5*Qnt_5 + 4*Qnt_4 + 3*Qnt_3 + 2*Qnt_2 + 1*Qnt_1) / (Qnt_5 + Qnt_4 + Qnt_3 + Qnt_2 + Qnt_1)
)

## Ungroup data
db_reviews <- db_reviews %>% ungroup() 

## Add new index
db_reviews$Index <- db_reviews                    %>%
  select(Country, Attraction, Title, Date, Text)  %>%
  data.frame                                      %>%
  apply(1, \(x) digest(paste(x, collapse = "|")))      

## Unique values
db_reviews <- db_reviews %>% filter(!duplicated(Index))
db_reviews <- unique(db_reviews)

## Detect languagens
db_reviews$language <- detect_language(text = db_reviews$Text, plain_text = TRUE)

## Select only english reviews
db_reviews <- db_reviews %>% filter(language == "en")

################################################################################
## Save data
################################################################################

## Path
setwd(path_results)

## Save dataset
write_parquet(db_reviews, "Database - Reviews Tripadvisor.parquet")
