library(tidyverse)
library(haven)
library(broom)
library(ggExtra)
library(margins)
library(interplot)
library(modelsummary)
library(ggrepel)
library(rvest)
library(stringr)
library(naniar)


#cbs projected QB stats

#get data from CBS website

cbsQBproj <- read_html("https://www.cbssports.com/fantasy/football/stats/QB/2024/season/projections/ppr/") %>% 
  html_nodes(".TableBase") %>% 
  html_table(header = TRUE)

#make data frame

cbsQBproj <- cbsQBproj %>% 
  as.data.frame(cbsQBproj$tibble) %>%
  mutate_all(~ gsub(x = ., pattern = "QB", "")) %>% 
  glimpse()

#clean data

cbsQBproj <- cbsQBproj[-c(1),] %>%
  mutate(Pos ="QB") %>%
  mutate(FantasyPoints = readr::parse_number(Misc.1)) %>% 
  mutate(PassTD = readr::parse_number(Passing.4)) %>% 
  mutate(Player = strtrim(Var.1, 15)) %>% 
  mutate(Turnovers = readr::parse_number(Passing.5)+ readr::parse_number(Misc)) %>% 
  mutate(PassAtt = readr::parse_number(Passing)) %>%
  mutate(RushTD =readr::parse_number(Rushing.3)) %>% 
  mutate(RushYD =readr::parse_number(Rushing.1)) %>%
  mutate(RushAtt = readr::parse_number(Rushing)) %>%
  mutate(RecTD = 0) %>% 
  mutate(RecYD = 0) %>% 
  mutate(Rec =0) %>% 
  glimpse()

#select the clean data

cbsQBproj <- cbsQBproj[-c(1:16)] %>%
  glimpse()

#fun graph  

cbsQBproj %>%
  filter(PassAtt>175) %>% 
  ggplot(aes(x=Turnovers, y=FantasyPoints, label=Player))+
  geom_text_repel()+
  geom_smooth(method = "lm")+
  labs(caption ="Minimum of 175 Projected Passing Attempts, data from https://www.cbssports.com/fantasy/football/stats/QB/2021/season/projections/nonppr/",
       x="Turnovers",
       title="2024 Projected Fantasy Points vs Turnovers")+
  theme_minimal()



#CBS RB projections

#rinse and repeat

cbsRBproj <- read_html("https://www.cbssports.com/fantasy/football/stats/RB/2024/season/projections/ppr/") %>% 
  html_nodes(".TableBase") %>% 
  html_table(header = TRUE)

cbsRBproj <- cbsRBproj %>% 
  as.data.frame(cbsRBproj$tibble) %>% 
  glimpse()

cbsRBproj <- cbsRBproj[-c(1),] %>% 
  mutate_all(~ gsub(x = ., pattern = "RB", "")) %>%
  mutate(Pos = "RB") %>%
  mutate(FantasyPoints = readr::parse_number(Misc.1)) %>%
  mutate(Player = strtrim(Var.1, 15)) %>%
  mutate(Turnovers = readr::parse_number(Misc)) %>% 
  mutate(RushTD =readr::parse_number(Rushing.3)) %>% 
  mutate(RushYD =readr::parse_number(Rushing.1)) %>%
  mutate(RushAtt = readr::parse_number(Rushing)) %>% 
  mutate(RecTD = readr::parse_number(Receiving.5)) %>% 
  mutate(RecYD = readr::parse_number(Receiving.2)) %>% 
  mutate(Rec =readr::parse_number(Receiving.1)) %>%
  mutate(PassAtt = 0) %>%
  mutate(PassTD = 0) %>% 
  glimpse()

cbsRBproj <- cbsRBproj[-c(1:15)] %>% 
  glimpse()


cbsRBproj %>%
  filter(RushAtt>150) %>% 
  ggplot(aes(x=RushYD, y=RushTD, label=Player))+
  geom_text_repel()+
  geom_smooth(method = "lm")+
  theme_minimal()


#CBS WR Projections

cbsWRproj <- read_html("https://www.cbssports.com/fantasy/football/stats/WR/2024/season/projections/ppr/") %>% 
  html_nodes(".TableBase") %>% 
  html_table(header = TRUE)

cbsWRproj <- cbsWRproj %>% 
  as.data.frame(cbsWRproj$tibble) %>%
  mutate_all(~ gsub(x = ., pattern = "WR", "")) %>% 
  glimpse()

cbsWRproj <- cbsWRproj[-c(1),] %>% 
  mutate(Pos = "WR") %>%
  mutate(FantasyPoints = readr::parse_number(Misc.1)) %>%
  mutate(Player = strtrim(Var.1, 15)) %>%
  mutate(Turnovers = readr::parse_number(Misc)) %>% 
  mutate(RushTD =readr::parse_number(Rushing.3)) %>% 
  mutate(RushYD =readr::parse_number(Rushing.1)) %>%
  mutate(RushAtt = readr::parse_number(Rushing)) %>% 
  mutate(RecTD = readr::parse_number(Receiving.5)) %>% 
  mutate(RecYD = readr::parse_number(Receiving.2)) %>% 
  mutate(Rec =readr::parse_number(Receiving.1)) %>%
  mutate(PassAtt = 0) %>%
  mutate(PassTD = 0) %>% 
  glimpse()

cbsWRproj <- cbsWRproj[-c(1:15)] %>% 
  glimpse()

#CBS TE Projections

cbsTEproj <- read_html("https://www.cbssports.com/fantasy/football/stats/TE/2024/season/projections/ppr/") %>% 
  html_nodes(".TableBase") %>% 
  html_table(header = TRUE)

cbsTEproj <- cbsTEproj %>% 
  as.data.frame(cbsWRproj$tibble) %>%
  mutate_all(~ gsub(x = ., pattern = "WR", "")) %>% 
  glimpse()

cbsTEproj <- cbsTEproj[-c(1),] %>% 
  mutate(Pos = "TE") %>%
  mutate(FantasyPoints = readr::parse_number(Misc.1)) %>%
  mutate(Player = strtrim(Var.1, 15)) %>%
  mutate(Turnovers = readr::parse_number(Misc)) %>% 
  mutate(RecTD = readr::parse_number(Receiving.5)) %>% 
  mutate(RecYD = readr::parse_number(Receiving.2)) %>% 
  mutate(Rec =readr::parse_number(Receiving.1)) %>% 
  mutate(RushTD = 0) %>% 
  mutate(RushYD = 0) %>%
  mutate(RushAtt = 0) %>%
  mutate(PassAtt = 0) %>%
  mutate(PassTD = 0) %>% 
  glimpse()

cbsTEproj <- cbsTEproj[-c(1:11)] %>% 
  glimpse()


#join them together

cbsproj <- rbind(cbsQBproj, cbsRBproj, cbsWRproj, cbsTEproj) %>%
  glimpse()

#average draft position

AvgDraftPos <-read_html("https://www.cbssports.com/fantasy/football/draft/averages/") %>% 
  html_nodes(".TableBase") %>% 
  html_table(header = TRUE)

AvgDraftPos <- AvgDraftPos %>% 
  as.data.frame(AvgDraftPos$tibble) %>%
  mutate_all(~ gsub(x = ., pattern = "DST", NA)) %>%
  mutate(Player = strtrim(Player, 15)) %>%
  mutate(Avg.Pos = readr::parse_number(Avg.Pos)) %>% 
  glimpse()

cbsproj <- cbsproj %>% 
  left_join(AvgDraftPos) %>%
  mutate(real_points = FantasyPoints - (Rec * 0.5)) %>% #correct for half ppr
  glimpse()


cbsproj_1 <- cbsproj %>% 
  group_by(Pos) %>% 
  mutate(POS_RANK = n() - rank(real_points)+1) %>%
  mutate(player_name = gsub(x = Player, pattern = "\n", " ")) %>%
  mutate(player_name = str_trim(player_name)) %>% 
  ungroup() %>% 
  glimpse()


points_over_replacement <- function(position, playername){
  
  player_points <- cbsproj_1 %>%
    filter(Pos == position) %>% 
    filter(player_name==playername) %>% 
    dplyr::select(real_points)
  
  spot <- player_points$real_points[[1]]
  
  temp <- cbsproj_1 %>% 
    filter(Pos==position) %>% 
    mutate(points_over_replacement= spot - real_points) %>% 
    dplyr::select(player_name, points_over_replacement)
  
  return(temp)
  
}


points_over_replacement(position = "QB", playername = "C. Stroud")

points_over_replacement(position = "WR", playername = "T. Hill")


check <- points_over_replacement(position = "WR", playername = "Q. Johnston")

