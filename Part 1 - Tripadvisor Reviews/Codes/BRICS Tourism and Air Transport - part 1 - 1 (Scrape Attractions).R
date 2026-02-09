################################################################################
## Clean memory
################################################################################

rm(list = ls())
gc(full = TRUE)

################################################################################
## Libraries
################################################################################

library(processx   )   ## Open and close process
library(digest     )   ## Hash
library(websocket  )   ## Browser automation
library(later      )   ## Run scheduled actions
library(rvest      )   ## HTML functions
library(jsonlite   )   ## JSON Files
library(readxl     )   ## Read excel files
library(tidyverse  )   ## Data manipulation

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
path_base    <- paste0(path, "/Data/"               )
path_results <- paste0(path, "/Results/Attractions/")
path_source  <- paste0(path, "/Codes/"              )

## Create path
if(!dir.exists(path_results)) dir.create(path_results)

## Seed
set.seed(100)

################################################################################
## Functions
################################################################################

## Function to get the reviews links using websocket
fn_links <- function(link, ntry = 10, xpath_rev) {
  
  ## Create environment
  env <- new.env()

  ## Open Browser
  p <- process$new(
    command = "C:/Program Files/Google/Chrome/Application/chrome.exe",
    args    = c("--remote-debugging-port=9222", "--incognito")       ,
  )

  ## Get info from browser devtool
  info_devtools <- fromJSON("http://localhost:9222/json")
  
  ## Connection with the websocket
  url_ws <- info_devtools$webSocketDebuggerUrl[1]
  
  ## Connect with WebSocket
  ws <- WebSocket$new(url_ws)

  ## Wait loading
  while(ws$readyState() == 0) run_now(timeoutSecs = 0.3)
  
  ## Function to proccess the responses
  ws$onMessage(function(event) {
    
    ## Get json
    msg <- fromJSON(event$data)
    
    ## Verify the page load
    if(!is.null(msg$method) && msg$method == "Page.loadEventFired") env$load <- TRUE
    
    ## Get html
    env$html <- msg$result$result$value
    
  })

  ## Enable callback from browser
  ws$send(toJSON(auto_unbox = TRUE, x = list(
    id = 1,
    method = "Page.enable"
  )))
  
  ## Navigate to page
  ws$send(toJSON(auto_unbox = TRUE, x = list(
    id     = 2               ,
    method = "Page.navigate" ,
    params = list(url = link)
  )))
  
  ## Wait page to load
  for(i in 1:ntry) if(isTRUE(env$load)) break else Sys.sleep(0.5)
  
  ## Verify if page was loaded
  if(isFALSE(env$load)) return(list(load = FALSE, links = NULL))
  
  ## Capture page's html
  for(i in 1:ntry) {
    
    ## Send command
    ws$send(toJSON(auto_unbox = TRUE, x = list(
      id     = 3                                                      ,
      method = "Runtime.evaluate"                                     ,
      params = list(expression = "document.documentElement.outerHTML")
    )))
    
    ## run now
    run_now()
    
    ## Wait page load
    Sys.sleep(1)
    
    ## Check state
    if(is.null(env$html)) next
    
    ## Get links
    page_links <- env$html          %>% 
      read_html                     %>%
      html_nodes(xpath = xpath_rev) %>%
      html_attr("href")  
    
    ## Check state
    if(!is.null(page_links)) break
    
  }
  
  ## Close browser
  p$kill_tree()
  
  ## Return
  return(list(load = TRUE, links = page_links))
  
}

################################################################################
## Data
################################################################################

## Path
setwd(path_base)

## Read dataset
df_country <- read_excel("Dataset tripadvisor.xlsx", sheet = "Countries" )
df_cat     <- read_excel("Dataset tripadvisor.xlsx", sheet = "Categories")

## Read downloaded files
files_down <- list.files(path_results, pattern = ".json")

################################################################################
## Create links
################################################################################

## Data frame with links
df_links <- bind_rows(lapply(1:nrow(df_country), \(i) data.frame(
  Countries  = df_country$Countries[i]                                          ,
  G20        = df_country$G20      [i]                                          ,
  BRICS      = df_country$BRICS    [i]                                          ,
  Categories = df_cat$`Category Type`                                           ,
  Link       = sapply(df_cat$Code, \(x) gsub("oa0", x, df_country$`Link TA`[i])),
  row.names  = NULL                                                                       
)))

## Add index
df_links$Index <- as.character(sapply(df_links$Link, digest))

## Remove downloaded data
df_links <- filter(df_links, !(Index %in% gsub("links_|.json", "", files_down)))

################################################################################
## Get attractions links
################################################################################

## xpath of reviews links
xpath_rev <- '//*[@id="lithium-root"]/main/div/div/div[2]/div[2]/div[2]/div[2]/div/div/div[2]/div/div[2]/div/div/section/div/div/div/div/article/div[2]/header/div/div/div/a[2]'

## Loop in links
for(i in 1:nrow(df_links)) {
  
  ## Evaluation time
  eval_time <- system.time({ while(TRUE){
    
    ## try to load page
    try_load <- try(res_links <- fn_links(link      = df_links$Link[i],
                                          ntry      = 10              ,
                                          xpath_rev = xpath_rev       ))
    
    ## Interrupt loop
    if(!is(try_load, "try-error")) break
    
  }})

  
  ## Create list
  res_links <- list(Index      = df_links$Index     [i],
                    Countries  = df_links$Countries [i],
                    G20        = df_links$G20       [i],
                    BRICS      = df_links$BRICS     [i],
                    Categories = df_links$Categories[i],
                    Load       = res_links$load        ,
                    Links      = res_links$links       )
  
  ## File name
  file_i <- paste0(path_results, "links_", df_links$Index[i], ".json")
  
  ## Save json with links
  write_json(res_links, path = file_i, auto_unbox = TRUE)
  
  ## Print evaluation
  print(sprintf(
    "%d/%d (%.2f%%); Attractions: %d; Time: %.2fs",
    i, nrow(df_links), 100*i/nrow(df_links), length(res_links$Links), eval_time[3]
  ))
  
  ## Remove variables
  rm(eval_time)
  rm(res_links)
  rm(file_i   ) 

}
