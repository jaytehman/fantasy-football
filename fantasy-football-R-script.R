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

cbsQBproj <- read_html("https://www.cbssports.com/fantasy/football/stats/QB/2021/season/projections/nonppr/") %>% 
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
       title="2022 Projected Fantasy Points vs Turnovers")+
  theme_minimal()



#CBS RB projections

#rinse and repeat

cbsRBproj <- read_html("https://www.cbssports.com/fantasy/football/stats/RB/2021/season/projections/nonppr/") %>% 
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

cbsWRproj <- read_html("https://www.cbssports.com/fantasy/football/stats/WR/2021/season/projections/nonppr/") %>% 
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

cbsTEproj <- read_html("https://www.cbssports.com/fantasy/football/stats/TE/2021/season/projections/nonppr/") %>% 
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
  mutate(DraftToPointsRatio = (FantasyPoints)/((Avg.Pos))) %>%
  mutate(DraftToPointsRatio2 = (FantasyPoints)/((172-Avg.Pos))) %>%
  mutate(ValFunct = (FantasyPoints)/((172-Avg.Pos)^2)) %>% 
  mutate(Log = log(DraftToPointsRatio)) %>% 
  write_rds("cbsproj.rds") %>%
  glimpse()

#if I'm correct, the higher the value of log, the better it is to draft them early. 

cbsproj %>% 
  filter(Avg.Pos<20) %>% 
  ggplot(aes(y=FantasyPoints, x=Log, label=Player))+
  geom_text_repel()+
  geom_smooth(method = "lm")

#2021 stats

data21 <- read.csv("https://raw.githubusercontent.com/fantasydatapros/data/master/yearly/2021.csv")
glimpse(data21)


data21 <- data21 %>% 
  mutate(turnovers = (FumblesLost+Int)) %>% 
  glimpse()

data21QB <- data21 %>% 
  filter(Pos=="QB")

ols <- lm(FantasyPoints ~ Age + turnovers + PassingAtt + PassingYds, data=data21QB)
summary(ols)


data21 %>%
  filter(Pos=="QB") %>%
  filter(PassingAtt>175) %>%
  ggplot(aes(x=turnovers, y=FantasyPoints, label=Player))+
  geom_label_repel()+
  geom_smooth(method = "lm")+
  labs(caption ="Minimum of 175 Passing Attempts, data from https://raw.githubusercontent.com/fantasydatapros/data/master/yearly/2021.csv",
       x="Turnovers",
       title="Turnovers Cause Fantasy Points, or an example of a spurious effect")+
  theme_minimal()
  
ggsave("joke_graph.jpeg")
