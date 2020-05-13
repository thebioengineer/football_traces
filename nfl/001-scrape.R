### Extract and Save Data
### Load Packages -------------------------------------------

## Load Libraries
library(tidyverse)
library(rvest)
library(here)
library(snowfall)

### Scrape Team Stats -------------------------------------------

## get Teams in NFL
teams <- "https://www.pro-football-reference.com/teams/" %>%
  read_html() %>%
  html_nodes("#all_teams_active") %>%
  html_nodes(".stats_table") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  data.frame(links = .) %>%
  filter(grepl("teams", links)) %>%
  pull(links) %>%
  gsub("/teams/(.+)/", "\\1", .) %>%
  tibble(team = .)

list_seasons <- function(team_info){
  url <- file.path("https://www.pro-football-reference.com/teams/",team_info$team)
  tab <- read_html(url) %>% 
    html_table() %>% 
    purrr::pluck(1)
  
  if(!is.null(tab)){
    new_colnames <- janitor::make_clean_names(gsub("\\s","",paste(colnames(tab),unlist(tab[1,]))))
    tab <- tab[-1,]
    colnames(tab) <- new_colnames
  }
  
  years <- tab %>% filter( ! year %in%  c("","Year")) %>% 
    select(Year = year, W = w, L = l, `T` = t, team_name = `tm`) %>% 
    mutate( team_name = first(team_name))
  tibble(years, team_info)
}

season_statistics <- function(team_info, min_year = 1980){
  team_info <- team_info %>% 
    filter(Year >= min_year)
  
  message(paste("Team:", unique(team_info$team)))
  
  years <- team_info$Year
  
  map(years,function(year){
    print(year)
    url <- file.path("https://www.pro-football-reference.com/teams",unique(team_info$team),paste0(year,".htm"))
    tab <- read_html(url) %>% 
      html_table() %>% 
      purrr::pluck(1)
    
    if(!is.null(tab)){
      new_colnames <- janitor::make_clean_names(gsub("\\s","",paste(colnames(tab),unlist(tab[1,]))))
      tab <- tab[-1,]
      colnames(tab) <- new_colnames
    }
    
    tab %>% 
      mutate(player = 
               case_when(
                 player == "Team Stats" ~ "Offense",
                 player == "Opp. Stats"  ~ "Defense")
             ) %>% 
      filter( !is.na(player) ) %>% 
    return()
  }) %>% 
    tibble(season_data = ., team_info) -> team_info
  
  team_info %>% 
    unnest(season_data) %>% 
    select( team = team, team_name, Year, W,L,`T`,everything())
}

team_seasons <- teams %>% 
  split(.$team) %>% 
  map(~list_seasons(.x))

sfInit(parallel = TRUE, cpus = 8)
sfLibrary(tidyverse)
sfLibrary(rvest)

active_NFL_stats_list <- team_seasons %>% 
  sfLapply(season_statistics)

active_NFL_stats_list <- active_NFL_stats_list %>% 
  sfLapply(function(x){
    x %>% mutate_all(as.character)
  }) %>% 
  bind_rows() %>% 
  rename( split  = player )

sfStop()

saveRDS(active_NFL_stats_list, here("nfl/active_NFL_stats_1980.RDS"))

## NFL Logos URL 

team_logo_section <- "https://www.sportslogos.net/teams/list_by_league/7/National_Football_League/NFL/logos/" %>% 
  read_html() %>% 
  html_node(".section") %>% 
  html_nodes("a")

team_logos <- tibble(
  team = html_attr(team_logo_section,"href") %>% 
    gsub(".+/\\d+/(.+)/","\\1",.) %>% 
    grep("/",., value = TRUE,invert = TRUE) %>% 
    gsub("_"," ",.),
  logo_url = team_logo_section %>% 
    html_nodes("img") %>% 
    html_attr("src")
)
  
saveRDS(team_logos, here("nfl/team_logos.RDS"))

