## load impact data 

library(tidyverse)
library(here)
library(rio)
library(fs)
library(colorblindr)
library(gghighlight)
library(forcats)
library(ggrepel)
library(gt)
library(knitr)
library(kableExtra)
library(reactable)
library(plotly)


files <- dir_ls(here::here("ImPACT_data"), glob = "*.csv")

impact_2 <- map_df(files, read_csv, .id = "file")

impact00_10 <- import(here("ImPACT_data", "2000_2010_impact.csv"),
                 setclass = "tbl_df") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character)

impact11 <- import(here("ImPACT_data", "2011_impact.csv"),
                   setclass = "tbl_df") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character)

impact12 <- import(here("ImPACT_data", "2012_impact.csv"),
                   setclass = "tbl_df") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character)

impact13 <- import(here("ImPACT_data", "2013_impact.csv"),
                   setclass = "tbl_df") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character)

impact14 <- import(here("ImPACT_data", "2014_impact.csv"),
                   setclass = "tbl_df") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character)

impact15 <- import(here("ImPACT_data", "2015_impact.csv"),
                   setclass = "tbl_df") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character)

impact16 <- import(here("ImPACT_data", "2016_impact.csv"),
                   setclass = "tbl_df") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character)

impact17 <- import(here("ImPACT_data", "2017_impact.csv"),
                   setclass = "tbl_df") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character)

impact18 <- import(here("ImPACT_data", "2018_impact.csv"),
                   setclass = "tbl_df") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character)

impact19 <- import(here("ImPACT_data", "2019_impact.csv"),
                   setclass = "tbl_df") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character)

impact20 <- import(here("ImPACT_data", "2020_impact.csv"),
                   setclass = "tbl_df") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character)

impact <- bind_rows("2000-2010" = impact00_10, 
                    "2011" = impact11, 
                    "2012" = impact12, 
                    "2013" = impact13,
                    "2014" = impact14,
                    "2015" = impact15,
                    "2016" = impact16,
                    "2017" = impact17,
                    "2018" = impact18,
                    "2019" = impact19,
                    "2020" = impact20,
                      .id = "year") 

glimpse(impact)


ggplot(impact, aes(user_number_of_concussions)) +
  geom_histogram()


