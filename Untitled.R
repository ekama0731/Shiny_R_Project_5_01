library(dplyr)
library(ggplot2)

# reading my players.csv into r
nba = read.csv('~/Documents/NYC Data Science Academy/nba-players-stats-since-1950/Players.csv', stringsAsFactors = F)
str(nba)

# removing birth_state NA's
nba_state = nba %>% filter(., nba$birth_state != "") %>%
  arrange(born)

#  mutating generalitional column

nba_gens = nba_state %>%
  group_by(nba_state$born) %>%
  mutate(., generations)

# Creating generations ####
gen_1 <- nba_state %>% filter(., nba_state$born >= 1915 & nba_state$born <= 1919) %>% mutate(., generation = 1)
gen_2 <- nba_state %>% filter(., nba_state$born >= 1920 & nba_state$born <= 1924) %>% mutate(., generation = 2)
gen_3 <- nba_state %>% filter(., nba_state$born >= 1925 & nba_state$born <= 1929) %>% mutate(., generation = 3)
gen_4 <- nba_state %>% filter(., nba_state$born >= 1930 & nba_state$born <= 1934) %>% mutate(., generation = 4)
gen_5 <- nba_state %>% filter(., nba_state$born >= 1935 & nba_state$born <= 1939) %>% mutate(., generation = 5)
gen_6 <- nba_state %>% filter(., nba_state$born >= 1940 & nba_state$born <= 1944) %>% mutate(., generation = 6)
gen_7 <- nba_state %>% filter(., nba_state$born >= 1945 & nba_state$born <= 1949) %>% mutate(., generation = 7)
gen_8 <- nba_state %>% filter(., nba_state$born >= 1950 & nba_state$born <= 1954) %>% mutate(., generation = 8)
gen_9 <- nba_state %>% filter(., nba_state$born >= 1955 & nba_state$born <= 1959) %>% mutate(., generation = 9)
gen_10 <- nba_state %>% filter(., nba_state$born >= 1960 & nba_state$born <= 1964) %>% mutate(., generation = 10)
gen_11 <- nba_state %>% filter(., nba_state$born >= 1965 & nba_state$born <= 1969) %>% mutate(., generation = 11)
gen_12 <- nba_state %>% filter(., nba_state$born >= 1970 & nba_state$born <= 1974) %>% mutate(., generation = 12)
gen_13 <- nba_state %>% filter(., nba_state$born >= 1975 & nba_state$born <= 1979) %>% mutate(., generation = 13)
gen_14 <- nba_state %>% filter(., nba_state$born >= 1980 & nba_state$born <= 1984) %>% mutate(., generation = 14)
gen_15 <- nba_state %>% filter(., nba_state$born >= 1985 & nba_state$born <= 1989) %>% mutate(., generation = 15)
gen_16 <- nba_state %>% filter(., nba_state$born >= 1990 & nba_state$born <= 1994) %>% mutate(., generation = 16)
gen_17 <- nba_state %>% filter(., nba_state$born >= 1995 & nba_state$born <= 1999) %>% mutate(., generation = 17)

# putting all the gens together into a dataframe
nba_gens = rbind(gen_1, gen_2,gen_3,gen_4,gen_5,gen_6,gen_7,gen_8,gen_9,gen_10,gen_11,gen_12,gen_13,gen_14,gen_15,gen_16,gen_17)

# map 

states_count = nba_gens %>% 
  group_by(birth_state) %>%
  summarise(., count=n())

