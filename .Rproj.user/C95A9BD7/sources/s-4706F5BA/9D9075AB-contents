### Extract and Save Data
### Load Packages -------------------------------------------

## Load Libraries
library(tidyverse)
library(rvest)
library(here)
library(snowfall)

### Scrape Team Stats -------------------------------------------

## get conferences
conferences <- read_html("https://www.sports-reference.com/cfb/years/2019.html") %>% 
  html_nodes(".stats_table") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  data.frame(links = .) %>% 
  filter(grepl("conferences",links)) %>% 
  pull(links) %>% 
  gsub("/cfb/conferences/(.+)/2019.html","\\1",.)

## get schools in conferences
schools <- conferences %>% 
  map_dfr(function(conf){
    url <- file.path("https://www.sports-reference.com/cfb/conferences",conf,"2019.html")
    read_html(url) %>% 
      html_nodes("#all_standings") %>% 
      html_nodes(".stats_table") %>% 
      html_nodes("a") %>% 
      html_attr("href") %>% 
      data.frame(links = .) %>% 
      filter(grepl("schools",links)) %>% 
      pull(links) %>% 
      gsub("/cfb/schools/(.+)/2019.html","\\1",.) %>% 
      tibble(schools = ., conference = conf)
  })

list_seasons <- function(school_info){
  url <- file.path("https://www.sports-reference.com/cfb/schools",school_info$schools)
  tab <- read_html(url) %>% 
    html_table() %>% 
    purrr::pluck(1)
  years <- tab %>% filter(Year != "Year") %>% 
    select(Year, W, L, `T`)
  tibble(years, school_info)
}

season_statistics <- function(school_info, min_year = 1980){
  school_info <- school_info %>% 
    filter(Year >= min_year)
  
  message(paste("School:", unique(school_info$schools)))
  
  years <- school_info$Year
  
  map(years,function(year){
    print(year)
    url <- file.path("https://www.sports-reference.com/cfb/schools",unique(school_info$schools),paste0(year,".html"))
    tab <- read_html(url) %>% 
      html_table() %>% 
      purrr::pluck(1)
    
    if(!is.null(tab)){
      new_colnames <- janitor::make_clean_names(gsub("\\s","",paste(colnames(tab),unlist(tab[1,]))))
      tab <- tab[-1,]
      colnames(tab) <- new_colnames
    }
    return(tab)
  }) %>% 
    tibble(season_data = ., school_info) -> school_info
  
  school_info %>% 
    unnest(season_data) %>% 
    select( school = schools, conference, Year, W,L,`T`,everything())
}

School_seasons <- schools %>% 
  split(.$schools) %>% 
  map(~list_seasons(.x))

sfInit(parallel = TRUE, cpus = 8)
sfLibrary(tidyverse)
sfLibrary(rvest)

active_NCAA_stats_list <- School_seasons %>% 
  sfLapply(season_statistics)

active_NCAA_stats <- active_NCAA_stats_list %>% 
  sfLapply(function(x){
    x %>% mutate_all(as.character)
  }) %>% 
  bind_rows()

sfStop()

saveRDS(active_NCAA_stats, here("college/active_NCAA_stats_1980.RDS"))
