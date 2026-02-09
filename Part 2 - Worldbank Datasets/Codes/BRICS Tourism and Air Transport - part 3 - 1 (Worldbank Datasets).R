################################################################################
## Corruption Paper
## Last Modification: 2025-08-25
################################################################################

################################################################################
## Clear environment
################################################################################

rm(list = ls())
gc(full = TRUE)

################################################################################
## libraries
################################################################################

library(arrow    )  ## Write Parquet files
library(chromote )  ## Browser automation
library(rvest    )  ## Webscraping
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
## Get database links from data360
################################################################################

## Vector with links
vec_links <- c()

## URL
url <- "https://data360.worldbank.org/en/search?tab=dataset"

## Start chromote
b <- ChromoteSession$new()

## Acess URL
b$Page$navigate(url)
b$Page$loadEventFired()

## Loop until complete
while(TRUE) {
  
  ## Wait page load
  Sys.sleep(5)
  
  ## Get html
  html_content <- b$Runtime$evaluate('document.documentElement.outerHTML')
  html_content <- html_content$result$value
  
  ## Read html in rvest
  html_page <- read_html(html_content)
  
  ## Selectors
  sel_dbs   <- "#reactapp > div > div.aem-Grid > div > div > section > div.aem-GridColumn.aem-GridColumn--default--8.aem-GridColumn--desktop--8.aem-GridColumn--tablet--12.aem-GridColumn--phone--12.aem-GridColumn--xl-4.aem-GridColumn--xs-12 > div > div > div > div > div.search-inforamtion.section-spacing-sm > div.search-inforamtion-left.aem-GridColumn--default--3.aem-GridColumn--desktop--3.aem-GridColumn--tablet--3.aem-GridColumn--phone--12.space-0 > span > strong:nth-child(3)"
  sel_links <- "#tabs-by-dataset-tabpanel > div > div > div > a.download-pdf"
  sel_next  <- "#tabs-by-dataset-tabpanel > div > div.pagination-section.section-spacing-sm.aem-GridColumn--default--12.aem-GridColumn--desktop--12.aem-GridColumn--tablet--12.aem-GridColumn--phone--12.space-0 > div > nav > ul > li:nth-child(11) > a > i"
  
  ## Number of databases
  n_db <- html_page %>% html_nodes(sel_dbs) %>% html_text2 %>% as.numeric
  
  ## Links
  page_links <- html_page %>% html_nodes(sel_links) %>% html_attr("href")
  
  ## Add in vec links
  if(length(page_links) > 0) vec_links <- c(vec_links, page_links)
  
  ## Unique values
  vec_links <- unique(vec_links)
  
  ## print evaluation
  cat("Number of links:", length(vec_links), "/", n_db, "\n")
  
  ## Stop criterion
  if(length(vec_links) >= n_db) break
  
  ## ID to next page
  select_id <- b$DOM$querySelector(b$DOM$getDocument()$root$nodeId, sel_next)$nodeId
  
  ## Scroll page to next page
  b$DOM$scrollIntoViewIfNeeded(nodeId = select_id)
  
  ## Get coordinates
  coords    <- b$DOM$getBoxModel(select_id)$model$content
  
  ## Click in next page
  b$Input$dispatchMouseEvent(
    type       = "mousePressed"                                     ,
    x          = unlist(coords[seq(1, 7, 2)]) %>% range() %>% mean(),
    y          = unlist(coords[seq(2, 8, 2)]) %>% range() %>% mean(),
    button     = "left"                                             ,
    clickCount = 1                                                  ,
  )
  b$Input$dispatchMouseEvent(
    type       = "mouseReleased"                                    ,
    x          = unlist(coords[seq(1, 7, 2)]) %>% range() %>% mean(),
    y          = unlist(coords[seq(2, 8, 2)]) %>% range() %>% mean(),
    button     = "left"                                             ,
    clickCount = 0                                                   
  )
  
  ## Remove variables
  rm(select_id   )
  rm(sel_dbs     )
  rm(sel_links   )
  rm(sel_next    )
  rm(page_links  )
  rm(html_content)
  rm(html_page   )
  rm(n_db        )
  rm(coords      )
  
}

## Close
b$close()

## Remove variables
rm(url         )
rm(sel_dbs     )
rm(sel_links   )
rm(sel_next    )
rm(page_links  )
rm(html_content)
rm(html_page   )
rm(n_db        )
rm(b           )

################################################################################
## Download worldbank data
################################################################################

## File names
file_names <- sapply(strsplit(vec_links, split = "/"), \(x) rev(x)[1])
file_names <- paste0(path_data, file_names)

## Data frame with links and file names
df_files <- data.frame(Filename = file_names, link = vec_links)

## Loop downloding data
for(i in 1:nrow(df_files)) download.file(
  url      = df_files$link    [i],
  destfile = df_files$Filename[i],
  method   = "curl"               
)

## Remove unused variables
rm(i         )
rm(vec_links )
rm(file_names)
rm(df_files  )

################################################################################
## Check files
################################################################################

## List files in path
files_csv <- list.files(path_data, full.names = TRUE, pattern = ".csv")

## Check sizes
files_csv_info <- file.info(files_csv)

## Files to remove with lengh 0
files_rem <- files_csv[files_csv_info$size == 0]

## Remove files
file.remove(files_rem)

## Remove unused variables
rm(files_csv     )
rm(files_csv_info)
rm(files_rem     )

################################################################################
## Coerce to parquet
################################################################################

## List files in path
files_csv <- list.files(path_data, full.names = TRUE, pattern = ".csv")

## Loop converting data
for(i in 1:length(files_csv)){
  
  ## Read data
  data_i <- read.csv(files_csv[i])
  
  ## Save as parquet
  write_parquet(data_i, gsub(".csv", ".parquet", files_csv[i]))
  
  ## Remove original file
  file.remove(files_csv[i])
  
  ## Print evaluation
  cat("File:", i, "/", length(files_csv), "\n")
  
  ## Clean memory
  rm(data_i     )
  gc(full = TRUE)
  
} 

## Remove unused variables
rm(i        )
rm(files_csv)
