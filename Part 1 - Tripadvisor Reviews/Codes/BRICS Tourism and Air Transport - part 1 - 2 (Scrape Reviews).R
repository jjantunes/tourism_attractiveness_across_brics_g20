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
path_base    <- paste0(path, "/Data/"   )
path_results <- paste0(path, "/Results/")
path_source  <- paste0(path, "/Codes/"  )

## Other paths
path_results_att <- paste0(path_results, "/Attractions/")
path_results_rev <- paste0(path_results, "/Reviews/"    )

## Create path
if(!dir.exists(path_results_rev)) dir.create(path_results_rev)

## Seed
set.seed(100)

################################################################################
## Functions
################################################################################

## Function to get the reviews links using websocket
fn_links <- function(link, ntry = 10) {
  
  ## xpaths
  xpath_title     <- '//*[@id="lithium-root"]/main/div/div[1]/div/div/div/a/span/span'
  xpath_qnt_rev   <- '//*[@id="tab-review-content"]/div/div[3]/div[1]/div/div[1]/div[2]/span'
  xpath_qnt_stars <- '//*[@id="tab-review-content"]/div/div[3]/div[1]/div/div[2]/div/div/div/div[3]/div'
  
  ## Selectors
  sel_profiles <- "#tab-review-content > div > div.LbPSX > div > div > div > div > div.mwPje.f.M.k > div.hcVjp.f.u.o > div.QIHsu.Zb > span > a"
  sel_stars    <- "#tab-review-content > div > div.LbPSX > div > div > div > div > div:nth-child(2) > svg"
  sel_titles   <- "#tab-review-content > div > div.LbPSX > div > div > div > div > div.biGQs._P.fiohW.qWPrE.ncFvv.fOtGX > a > span"
  sel_texts    <- "#tab-review-content > div > div.LbPSX > div > div > div > div > div._T.FKffI > div.fIrGe._T.bgMZj > div > span > span"
  sel_dates    <- "#tab-review-content > div > div.LbPSX > div > div > div > div > div.TreSq > div.biGQs._P.pZUbB.ncFvv.osNWb"
  sel_pags     <- "div.Yzhnw.P a"

  ## Create environment
  env <- new.env()
  
  ## List with results
  list_res <- list()
  
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
    
    ## Get title
    attraction <- env$html            %>% 
      read_html                       %>%
      html_nodes(xpath = xpath_title) %>%
      html_text()                     %>% 
      rev                             %>%
      .[1]
    
    ## Get Quantity of Reviews
    qnt_rev <- env$html                 %>% 
      read_html                         %>%
      html_nodes(xpath = xpath_qnt_rev) %>%
      html_text()                       
    
    ## Get quantity of stars, from 5 to 1
    qnt_stars <- env$html                 %>% 
      read_html                           %>%
      html_nodes(xpath = xpath_qnt_stars) %>%
      html_text()   
    
    ## Quantity per stars
    qnt_5 <- qnt_stars[1]
    qnt_4 <- qnt_stars[2]
    qnt_3 <- qnt_stars[3]
    qnt_2 <- qnt_stars[4]
    qnt_1 <- qnt_stars[5]
    
    ## Get profiles
    profiles <- env$html       %>% 
      read_html                %>%
      html_nodes(sel_profiles) %>%
      html_attr("href")        
    
    ## Get stars
    stars_rev <- env$html   %>% 
      read_html             %>%
      html_nodes(sel_stars) %>%
      html_text()  
    
    ## Get titles
    titles <- env$html       %>% 
      read_html              %>%
      html_nodes(sel_titles) %>%
      html_text()  
    
    ## Get reviews
    texts <- env$html       %>% 
      read_html             %>%
      html_nodes(sel_texts) %>%
      html_text()          
    
    ## Get dates
    dates <- env$html       %>% 
      read_html             %>%
      html_nodes(sel_dates) %>%
      html_text() 
    
    ## Get links
    links_pages <- env$html                    %>% 
      read_html                                %>%
      html_nodes(sel_pags)                     %>%
      html_attr("href")                        %>%
      paste0("https://www.tripadvisor.com", .)
    
    ## Maximum of 4 pages
    n_pages <- min(4, length(links_pages)) 
    
    ## Add in result list
    list_res[[1]] <- list(Attraction = attraction,
                          Qnt_rev    = qnt_rev   ,
                          Qnt_5      = qnt_5     ,
                          Qnt_4      = qnt_4     ,
                          Qnt_3      = qnt_3     ,
                          Qnt_2      = qnt_2     ,
                          Qnt_1      = qnt_1     ,
                          Profiles   = profiles  ,
                          Stars_rev  = stars_rev ,
                          Titles     = titles    ,
                          Texts      = texts     ,
                          Dates      = dates     )
    
    ## Check state
    if(!is.null(env$html)) break
    
  }
  
  ## Remove variables
  rm(profiles )
  rm(stars_rev)
  rm(titles   )
  rm(texts    )
  rm(dates    )
  
  ## Loop per page
  if(n_pages > 0) for(j in 1:n_pages) {
    
    ## New environment
    env <- new.env()
    
    ## Navigate to page
    ws$send(toJSON(auto_unbox = TRUE, x = list(
      id     = 2               ,
      method = "Page.navigate" ,
      params = list(url = links_pages[j])
    )))
    
    ## Wait page to load
    for(i in 1:ntry) if(isTRUE(env$load)) break else Sys.sleep(0.5)
    
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
      
      ## Get profiles
      profiles <- env$html       %>% 
        read_html                %>%
        html_nodes(sel_profiles) %>%
        html_attr("href")        
      
      ## Get stars
      stars_rev <- env$html   %>% 
        read_html             %>%
        html_nodes(sel_stars) %>%
        html_text()  
      
      ## Get titles
      titles <- env$html       %>% 
        read_html              %>%
        html_nodes(sel_titles) %>%
        html_text()  
      
      ## Get reviews
      texts <- env$html       %>% 
        read_html             %>%
        html_nodes(sel_texts) %>%
        html_text()          
      
      ## Get dates
      dates <- env$html       %>% 
        read_html             %>%
        html_nodes(sel_dates) %>%
        html_text() 
      
      ## Add in result list
      list_res[[j+1]] <- list(Attraction = attraction,
                              Qnt_rev    = qnt_rev   ,
                              Qnt_5      = qnt_5     ,
                              Qnt_4      = qnt_4     ,
                              Qnt_3      = qnt_3     ,
                              Qnt_2      = qnt_2     ,
                              Qnt_1      = qnt_1     ,
                              Profiles   = profiles  ,
                              Stars_rev  = stars_rev ,
                              Titles     = titles    ,
                              Texts      = texts     ,
                              Dates      = dates     )
      
      ## Check state
      if(!is.null(env$html)) break
      
    }
    
  }
  
  ## Close browser
  p$kill_tree()
  
  ## Create final list
  list_res <- list(
    Attraction = attraction                                     ,
    Qnt_rev    = qnt_rev                                        ,
    Qnt_5      = qnt_5                                          ,
    Qnt_4      = qnt_4                                          ,
    Qnt_3      = qnt_3                                          ,
    Qnt_2      = qnt_2                                          ,
    Qnt_1      = qnt_1                                          ,
    Profiles   = do.call(c, lapply(list_res, \(x) x$Profiles  )),
    Stars_rev  = do.call(c, lapply(list_res, \(x) x$Stars_rev )),
    Titles     = do.call(c, lapply(list_res, \(x) x$Titles    )),
    Texts      = do.call(c, lapply(list_res, \(x) x$Texts     )),
    Dates      = do.call(c, lapply(list_res, \(x) x$Dates     ))
  )
  
  ## Return final list
  return(list_res)
  
}

################################################################################
## Data
################################################################################

## Read downloaded reviews files
files_rev <- list.files(path_results_rev, pattern = ".json", full.names = FALSE)

## Read downloaded attraction files
files_att <- list.files(path_results_att, pattern = ".json", full.names = TRUE)

## Attraction dataframe
db_att <- bind_rows(lapply(files_att, \(x) bind_rows(fromJSON(x, flatten = TRUE))))

################################################################################
## Create links
################################################################################

## Add site in links
db_att$Links <- paste0("https://www.tripadvisor.com", db_att$Links)

## Add index
db_att <- db_att %>% mutate(
  Index = as.character(sapply(paste(db_att$Countries, db_att$Categories, db_att$Links, sep = "|"), digest))
)

## Remove downloaded data
db_att <- db_att %>% filter(!(Index %in% gsub("reviews_|.json", "", files_rev)))

################################################################################
## Get reviews
################################################################################

## Loop in links
for(i in 1:nrow(db_att)) {
  
  ## Evaluation time
  eval_time <- system.time({ while(TRUE){
    
    ## try to load page
    try_load <- try(res_links <- fn_links(link = db_att$Links[i], ntry = 10))
    
    ## Interrupt loop
    if(!is(try_load, "try-error")) break
    
  }})
  
  ## Create list
  res_links <- list(Index      = db_att$Index     [i],
                    Countries  = db_att$Countries [i],
                    G20        = db_att$G20       [i],
                    BRICS      = db_att$BRICS     [i],
                    Categories = db_att$Categories[i],
                    Attraction = res_links$Attraction,
                    Qnt_rev    = res_links$Qnt_rev   ,
                    Qnt_5      = res_links$Qnt_5     ,
                    Qnt_4      = res_links$Qnt_4     ,
                    Qnt_3      = res_links$Qnt_3     ,
                    Qnt_2      = res_links$Qnt_2     ,
                    Qnt_1      = res_links$Qnt_1     ,
                    Profiles   = res_links$Profiles  ,
                    Stars_rev  = res_links$Stars_rev ,
                    Titles     = res_links$Titles    ,
                    Texts      = res_links$Texts     ,
                    Dates      = res_links$Dates     )
  
  ## File name
  file_i <- paste0(path_results_rev, "reviews_", res_links$Index, ".json")
  
  ## Save json with links
  write_json(res_links, path = file_i, auto_unbox = TRUE)
  
  ## Print evaluation
  print(sprintf(
    "%d/%d (%.2f%%); Reviews: %d; Time: %.2fs",
    i, nrow(db_att), 100*i/nrow(db_att), length(res_links$Texts), eval_time[3]
  ))
  
  ## Remove variables
  rm(eval_time)
  rm(res_links)
  rm(file_i   ) 
  
}

