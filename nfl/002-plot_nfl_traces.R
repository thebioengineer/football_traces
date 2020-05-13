### Plot Data
### Load Packages -------------------------------------------

## Load Libraries
library(tidyverse)
library(here)
library(cowplot)

## Load Data

active_NFL_stats <- readRDS(here("nfl/active_NFL_stats_1980.RDS"))
NFL_Logos <- readRDS(here("nfl/team_logos.RDS"))

### Data Munging -------------------------------------------

stats_of_interest <- active_NFL_stats %>%
  select(
    team,
    team_name,
    Year = Year,
    Split = split,
    "Wins" = W,
    "Losses" = L,
    "Ties" = `T`,
    "Passing Attempts" = passing_att,
    "Rushing Attempts" = rushing_att,
    "Passing Yards" = passing_yds,
    "Rushing Yards" = rushing_yds,
    "Passing Touchdowns" = passing_td,
    "Rushing Touchdowns" = rushing_td
  ) %>%
  mutate_at(
    vars(
      `Passing Attempts`,
      `Rushing Attempts`,
      `Passing Yards`,
      `Rushing Yards`,
      `Passing Touchdowns`,
      `Rushing Touchdowns`,
      Wins,
      Losses,
      Ties
    ),
    as.numeric
  ) %>%
  mutate(
    ngames = Wins + Losses + Ties
  ) %>% 
  mutate_at(
    vars(
      `Passing Attempts`,
      `Rushing Attempts`,
      `Passing Yards`,
      `Rushing Yards`,
      `Passing Touchdowns`,
      `Rushing Touchdowns`
    ),
   .funs = list( ~ `/`(., ngames ))
  ) %>% 
  mutate(
    "Win Percent" = ((Wins + (Ties * .5)) / (Wins + Losses + Ties)),
    Year = as.numeric(Year)
  ) %>%  # keep only the start year) %>%
  gather(
    "Statistic",
    "Value",
    `Passing Attempts`,
    `Rushing Attempts`,
    `Passing Yards`,
    `Rushing Yards`,
    `Passing Touchdowns`,
    `Rushing Touchdowns`,
    `Win Percent`
  ) %>%
  mutate(Statistic = factor(
    Statistic,
    levels = c(
      "Passing Touchdowns",
      "Rushing Touchdowns", 
      "Passing Attempts",
      "Rushing Attempts",
      "Passing Yards",
      "Rushing Yards",
      "Win Percent"
    )),
    Split = factor(
      Split,
      levels = c(
        "Offense",
        "Defense"
      ))
  )


###


### Create Plot -------------------------------------------

TOI <- "sfo"
Nice_name <- active_NFL_stats %>% 
  filter(team == TOI) %>% 
  pull(team_name) %>% 
  unique %>% 
  gsub("[*]","",.)
stopifnot(nchar(Nice_name)!=0)
Trace_color <- "#AA0000"

logo_url <- NFL_Logos %>% 
  filter(team == Nice_name) %>% 
  pull(logo_url)

win_perc_data <- stats_of_interest %>% 
  filter(Statistic == "Win Percent") %>% 
  select(-Split) %>% 
  distinct()

plotting_data <- stats_of_interest %>%
  filter( ! Statistic %in% c("Wins","Losses","Ties","Win Percent")) %>% 
  filter( Split != "Difference")

Wins_Hist_Plot <- win_perc_data %>% 
  ggplot(aes(
    x = Year, y = Value
  )) + 
  geom_line(aes(
    color = team == TOI,
    group = team,
    alpha = ifelse(team == TOI, 1.2, 0.1),
    size  = ifelse(team == TOI, 1.2, 0.5)
  )) + 
  scale_y_continuous(labels = scales::percent) +
  scale_size_identity() +
  scale_alpha_identity() +
  scale_color_manual(values = c("light grey", Trace_color)) +
  facet_wrap(
    ~ Statistic,
    ncol = 1,
    scales = "free_y"
  ) +
  labs(x = NULL, y = NULL,
       title = "NFL Historical Statistics",
       subtitle = Nice_name) +
  theme(panel.background = element_rect(fill = "#333333", color = "#333333"),
        plot.background = element_rect(fill = "#333333", color = "#333333"),
        panel.grid.major = element_line(color = "#333333"),
        panel.grid.minor = element_line(color = "#333333"),
        panel.border = element_rect(color = "white", fill = NA),
        legend.position = "none",
        axis.text.x = element_text(color = "white", 
                                   face = "bold", 
                                   size = 12),
        axis.text.y = element_text(color = "white",
                                   face = "bold", 
                                   size = 12),
        axis.title.x = element_text(color = "white",
                                    face = "bold", 
                                    size = 13,
                                    vjust = 2),
        plot.title = element_text(color = "white",
                                  size = 20),
        plot.subtitle = element_text(color = "white", 
                                     size = 14),
        plot.caption = element_text(color = "white",
                                    face = "bold"),
        panel.grid.major.y = element_line(color = "#e6e6e6"),
        panel.grid.major.x = element_blank())

Wins_Hist_Plot

  
hist_plot <- plotting_data %>% 
  
  ## Set aes for any data to be added to data
  ggplot(aes(x = Year,
             y = Value)) +
  
  ## Add lines, alpha and coloring are for the "ghosting" of lines
  geom_line(aes(
    color = team == TOI,
    group = team,
    alpha = ifelse(team == TOI, 1.2, 0.1),
    size  = ifelse(team == TOI, 1.2, 0.5)
  )) + 
  
  ## Set the colors for where we don't match or do match the TOI
  scale_color_manual(values = c("light grey", Trace_color)) +
  scale_size_identity() +
  scale_alpha_identity() +
  
  ## Set the Labels
  labs(x = "Season",
       y = "",
       caption = "Source: https://pro-football-reference.com/") +
  
  scale_y_continuous(labels = scales::comma) + 
  
  facet_wrap(
    ~ Split + Statistic,
    ncol = 2,
    scales = "free_y"
  ) +
  
  ## Setting themes
  theme(panel.background = element_rect(fill = "#333333", color = "#333333"),
        plot.background = element_rect(fill = "#333333", color = "#333333"),
        panel.grid.major = element_line(color = "#333333"),
        panel.grid.minor = element_line(color = "#333333"),
        panel.border = element_rect(color = "white", fill = NA),
        legend.position = "none",
        axis.text.x = element_text(color = "white", 
                                   face = "bold", 
                                   size = 12),
        axis.text.y = element_text(color = "white",
                                   face = "bold", 
                                   size = 12),
        axis.title.x = element_text(color = "white",
                                    face = "bold", 
                                    size = 13,
                                    vjust = 2),
        plot.title = element_text(color = "white",
                                  size = 20),
        plot.subtitle = element_text(color = "white", 
                                     size = 14),
        plot.caption = element_text(color = "white",
                                    face = "bold"),
        panel.grid.major.y = element_line(color = "#e6e6e6"),
        panel.grid.major.x = element_blank())

hist_plot


combined_plot <- plot_grid(
  Wins_Hist_Plot,hist_plot,
  nrow = 2, ncol = 1,
  rel_heights = c(1,5),
  align = "h"
)

combined_plot

ggdraw() +
  draw_plot(combined_plot) +
  draw_image(logo_url,  x = .43, y = 0.465, scale = .1,)


ggsave((
  paste0(
    "nfl/",
    TOI,
    "_Trace.png"
  )
),
height = 20,
width = 15,
units = "in")

