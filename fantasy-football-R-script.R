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


#cbs projected QB stats


cbsQBproj <- read_html("https://www.cbssports.com/fantasy/football/stats/QB/2021/season/projections/nonppr/") %>% 
  html_nodes(".TableBase") %>% 
  html_table(header = TRUE)

cbsQBproj <- cbsQBproj %>% 
  as.data.frame(cbsQBproj$tibble) %>%
  mutate_all(~ gsub(x = ., pattern = "QB", "")) %>% 
  glimpse()

cbsQBproj <- cbsQBproj[-c(1),] %>%
  mutate(GM = readr::parse_number(Var.2)) %>%
  mutate(FantasyPoints = readr::parse_number(Misc.1)) %>% 
  mutate(PassTD = readr::parse_number(Passing.4)) %>% 
  mutate(Player = strtrim(Var.1, 10)) %>% 
  mutate(turnovers = readr::parse_number(Passing.5)+ readr::parse_number(Misc)) %>% 
  mutate(attemps = readr::parse_number(Passing)) %>% 
  glimpse()

cbsQBproj %>%
  filter(attemps>175) %>% 
  ggplot(aes(x=turnovers, y=FantasyPoints, label=Player))+
  geom_text_repel()+
  geom_smooth(method = "lm")+
  labs(caption ="Minimum of 175 Projected Passing Attempts, data from https://www.cbssports.com/fantasy/football/stats/QB/2021/season/projections/nonppr/",
       x="Turnovers",
       title="2022 Projected Fantasy Points vs Turnovers")+
  theme_minimal()



#CBS RB projections


cbsRBproj <- read_html("https://www.cbssports.com/fantasy/football/stats/RB/2021/season/projections/nonppr/") %>% 
  html_nodes(".TableBase") %>% 
  html_table(header = TRUE)

cbsRBproj <- cbsRBproj %>% 
  as.data.frame(cbsRBproj$tibble) %>% 
  glimpse()

cbsRBproj <- cbsRBproj[-c(1),] %>% 
  mutate_all(~ gsub(x = ., pattern = "RB", "")) %>%
  mutate(GM = readr::parse_number(Var.2)) %>%
  mutate(FantasyPoints = readr::parse_number(Misc.1)) %>%
  mutate(Player = strtrim(Var.1, 10)) %>%
  mutate(FumblesLost = readr::parse_number(Misc)) %>% 
  mutate(RushTD =readr::parse_number(Rushing.3)) %>% 
  mutate(RushYD =readr::parse_number(Rushing.1)) %>%
  mutate(RushAtt = readr::parse_number(Rushing)) %>% 
  mutate(RecTD = readr::parse_number(Receiving.5)) %>% 
  mutate(RecYD = readr::parse_number(Receiving.2)) %>% 
  mutate(Rec =readr::parse_number(Receiving.1)) %>% 
  glimpse()


cbsRBproj %>%
  filter(RushAtt>150) %>% 
  ggplot(aes(x=RushYD, y=RushTD, label=Player))+
  geom_text_repel()+
  geom_smooth(method = "lm")+
  theme_minimal()


#2021 Stats

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
