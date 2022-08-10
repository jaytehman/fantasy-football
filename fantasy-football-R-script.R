library(tidyverse)
library(haven)
library(broom)
library(ggExtra)
library(margins)
library(interplot)
library(modelsummary)
library(ggrepel)



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
