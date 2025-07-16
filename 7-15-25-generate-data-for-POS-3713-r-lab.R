library(tidyverse)
library(ggrepel)
library(rvest)
library(naniar)
library(broom)


#So, you'll notice that there are a few more packages here
#than We've used before

#You'll want to install the packages first


#cbs QB stats from 2024

#get data from CBS website

cbsQB2024 <- read_html("https://www.cbssports.com/fantasy/football/stats/QB/2024/season/stats/ppr/") %>% 
  html_nodes(".TableBase") %>% 
  html_table(header = TRUE)


cbsQB2024 <- cbsQB2024 %>% 
  as.data.frame(cbsQB2024$tibble) %>%
  mutate_all(~ gsub(x = ., pattern = "QB", "")) %>% 
  glimpse()


cbsQB2024_clean <- cbsQB2024[-c(1),] %>%
  mutate(Pos ="QB") %>%
  mutate(FantasyPoints = readr::parse_number(Misc.1)) %>% 
  mutate(PassTD = readr::parse_number(Passing.4)) %>% 
  mutate(PassYD = readr::parse_number(Passing.2)) %>% 
  mutate(Player_clean_1 = str_remove_all(Var.1, "[\\r\\n]")) %>%
  mutate(Player_clean_2 = strtrim(Player_clean_1, 20)) %>%
  mutate(Player =str_trim(Player_clean_2)) %>% 
  mutate(Team_1 = str_trunc(Player_clean_1, 3, side="left", ellipsis="")) %>%
  mutate(Team = str_trim(Team_1)) %>% 
  mutate(Turnovers = readr::parse_number(Passing.5)+ readr::parse_number(Misc)) %>% 
  mutate(PassAtt = readr::parse_number(Passing)) %>%
  mutate(RushTD =readr::parse_number(Rushing.3)) %>% 
  mutate(RushYD =readr::parse_number(Rushing.1)) %>%
  mutate(RushAtt = readr::parse_number(Rushing)) %>%
  mutate(RecTD = 0) %>% 
  mutate(RecYD = 0) %>% 
  mutate(Rec =0) %>%
  #dplyr::filter(is.na(FantasyPoints)==FALSE) %>%
  replace_na_with(0) %>% 
  glimpse()


#select the clean data

cbsQB2024_clean <- cbsQB2024_clean %>%
  dplyr::select(c(Player,Pos,Team, FantasyPoints, PassTD, PassYD, PassAtt,Turnovers, RushTD, RushYD, RushAtt, RecTD, RecYD, Rec)) %>% 
  glimpse()


#Rinse and Repeat

#rinse and repeat

cbsRB2024 <- read_html("https://www.cbssports.com/fantasy/football/stats/RB/2024/season/stats/ppr/") %>% 
  html_nodes(".TableBase") %>% 
  html_table(header = TRUE)

cbsRB2024 <- cbsRB2024 %>% 
  as.data.frame(cbsRB2024$tibble) %>% 
  glimpse()

cbsRB2024_clean <- cbsRB2024[-c(1),] %>% 
  mutate_all(~ gsub(x = ., pattern = "RB", "")) %>%
  mutate(Pos = "RB") %>%
  mutate(FantasyPoints = readr::parse_number(Misc.1)) %>%
  mutate(Turnovers = readr::parse_number(Misc)) %>% 
  mutate(RushTD =readr::parse_number(Rushing.3)) %>% 
  mutate(RushYD =readr::parse_number(Rushing.1)) %>%
  mutate(RushAtt = readr::parse_number(Rushing)) %>% 
  mutate(RecTD = readr::parse_number(Receiving.5)) %>% 
  mutate(RecYD = readr::parse_number(Receiving.2)) %>% 
  mutate(Rec =readr::parse_number(Receiving.1)) %>%
  mutate(PassAtt = 0) %>%
  mutate(PassTD = 0) %>%
  mutate(PassYD = 0) %>% 
  mutate(Player_clean_1 = str_remove_all(Var.1, "[\\r\\n]")) %>%
  mutate(Player_clean_2 = strtrim(Player_clean_1, 20)) %>%
  mutate(Player =str_trim(Player_clean_2)) %>% 
  mutate(Team_1 = str_trunc(Player_clean_1, 3, side="left", ellipsis="")) %>%
  mutate(Team = str_trim(Team_1)) %>%
  replace_na_with(0) %>%
  glimpse()


cbsRB2024_clean <- cbsRB2024_clean %>% 
  dplyr::select(c(Player,Pos,Team, FantasyPoints, PassTD, PassYD, PassAtt,Turnovers, RushTD, RushYD, RushAtt, RecTD, RecYD, Rec)) %>% 
  glimpse()


#Now WRs

cbsWR2024 <- read_html("https://www.cbssports.com/fantasy/football/stats/WR/2024/season/stats/ppr/") %>% 
  html_nodes(".TableBase") %>% 
  html_table(header = TRUE)

cbsWR2024 <- cbsWR2024 %>% 
  as.data.frame(cbsWR2024$tibble) %>%
  mutate_all(~ gsub(x = ., pattern = "WR", "")) %>% 
  glimpse()

cbsWR2024_clean <-cbsWR2024[-c(1),] %>% 
  mutate(Pos = "WR") %>%
  mutate(FantasyPoints = readr::parse_number(Misc.1)) %>%
  mutate(Turnovers = readr::parse_number(Misc)) %>% 
  mutate(RushTD =readr::parse_number(Rushing.3)) %>% 
  mutate(RushYD =readr::parse_number(Rushing.1)) %>%
  mutate(RushAtt = readr::parse_number(Rushing)) %>% 
  mutate(RecTD = readr::parse_number(Receiving.5)) %>% 
  mutate(RecYD = readr::parse_number(Receiving.2)) %>% 
  mutate(Rec =readr::parse_number(Receiving.1)) %>%
  mutate(PassAtt = 0) %>%
  mutate(PassYD = 0) %>% 
  mutate(PassTD = 0) %>% 
  mutate(Player_clean_1 = str_remove_all(Var.1, "[\\r\\n]")) %>%
  mutate(Player_clean_2 = strtrim(Player_clean_1, 20)) %>%
  mutate(Player =str_trim(Player_clean_2)) %>% 
  mutate(Team_1 = str_trunc(Player_clean_1, 3, side="left", ellipsis="")) %>%
  mutate(Team = str_trim(Team_1)) %>%
  replace_na_with(0) %>%
  glimpse()


#clean it up
cbsWR2024_clean <- cbsWR2024_clean %>% 
  dplyr::select(c(Player,Pos,Team, FantasyPoints, PassTD, PassYD, PassAtt,Turnovers, RushTD, RushYD, RushAtt, RecTD, RecYD, Rec)) %>% 
  glimpse()


#Now TEs

cbsTE2024 <- read_html("https://www.cbssports.com/fantasy/football/stats/TE/2024/season/stats/ppr/") %>% 
  html_nodes(".TableBase") %>% 
  html_table(header = TRUE)

cbsTE2024 <- cbsTE2024 %>% 
  as.data.frame(cbsTE2024$tibble) %>%
  mutate_all(~ gsub(x = ., pattern = "TE", "")) %>% 
  glimpse()

cbsTE2024_clean <-cbsTE2024[-c(1),] %>% 
  mutate(Pos = "TE") %>%
  mutate(FantasyPoints = readr::parse_number(Misc.1)) %>%
  mutate(Turnovers = readr::parse_number(Misc)) %>% 
  mutate(RushTD =0) %>% 
  mutate(RushYD =0) %>%
  mutate(RushAtt = 0) %>% 
  mutate(RecTD = readr::parse_number(Receiving.5)) %>% 
  mutate(RecYD = readr::parse_number(Receiving.2)) %>% 
  mutate(Rec =readr::parse_number(Receiving.1)) %>%
  mutate(PassAtt = 0) %>%
  mutate(PassYD = 0) %>% 
  mutate(PassTD = 0) %>% 
  mutate(Player_clean_1 = str_remove_all(Var.1, "[\\r\\n]")) %>%
  mutate(Player_clean_2 = strtrim(Player_clean_1, 20)) %>%
  mutate(Player =str_trim(Player_clean_2)) %>% 
  mutate(Team_1 = str_trunc(Player_clean_1, 3, side="left", ellipsis="")) %>%
  mutate(Team = str_trim(Team_1)) %>%
  replace_na_with(0) %>%
  glimpse()


#clean it up
cbsTE2024_clean <- cbsTE2024_clean %>% 
  dplyr::select(c(Player,Pos,Team, FantasyPoints, PassTD, PassYD, PassAtt,Turnovers, RushTD, RushYD, RushAtt, RecTD, RecYD, Rec)) %>% 
  glimpse()


cbs2024 <- rbind(cbsQB2024_clean, cbsRB2024_clean, cbsWR2024_clean, cbsTE2024_clean) %>%
  glimpse()

write_rds(cbs2024, "2024-fantasy-football.RDS")
