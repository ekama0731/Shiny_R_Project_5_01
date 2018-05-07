library(dplyr)
library(ggplot2)
library (tidyr)
library(usmap)
library (plotly)

# 1.0 Setting up my dataframe ####
## reading my players.csv into r
nba = read.csv('~/Documents/NYC Data Science Academy/Shiny_Project/nba-players-stats-since-1950/Players.csv', stringsAsFactors = F)
## loading in season stats csv
season_stats = read.csv('~/Documents/NYC Data Science Academy/Shiny_Project/nba-players-stats-since-1950/Seasons_Stats.csv')

## removing birth_state NA's
nba_state = nba %>% filter(., nba$birth_state != "") %>%
  arrange(born)



# 2.0 Creating generations for groups of players ####
## Finding the number of seasons each player, played
season_gen = season_stats %>% 
  group_by(Player) %>%
  summarise(., num_season = n_distinct(Year)) %>%
  filter(., Player != "") 

nba_gens_1 = nba_gens
nba_gens_1$Player = as.factor(nba_gens_1$Player)

##Joining nba_gens with season_gens
nba_gen_season =  inner_join(x=nba_gens_1, y=season_gen, by='Player')

## Count of each birth state in each generation data set
nba_gen_season = nba_gen_season %>% 
  group_by(birth_state,generation) %>%
  summarise(mean_generation = mean(num_season), count = n()) %>%
  filter(count > 3)

#creating a join with our nba gen season with all states to visualize all the different generations
nba_generations =nba_gen_season 
colnames(nba_generations)=c('region','generations','mean_gen','num_player')
nba_generations$region = tolower(nba_generations$region)

# # mutating generalitional column
nba_gens = nba_state %>%
  group_by(nba_state$born) %>%
  mutate(., generations)

# 3.0 Creating generations ####
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
states_count = nba_gens %>%
  group_by(birth_state) %>%
  mutate(., count=n()) %>%
  arrange(count)


# 3.0 Data frame to summarise for map ####
# I first wanted to see where the majority of players have come from since the birth of the first NBA player.
nba_count_state = states_count %>%
  group_by(birth_state) %>%
  summarise(., num_players=n()) %>%
  arrange(desc(num_players))

nba_count_state_top20 = head(nba_count_state,20, decreasing=T) # Showing the top states that produce players

# 4.0 Creating a heat map on a US Map #### 
# Creating the graph above but putting it on a U.S. map

all_states = map_data('state') # extracted the states map from the library 'maps'
nba_count_state$birth_state=tolower(nba_count_state$birth_state) # formatted all the value to be lower case
colnames(nba_count_state) = c('region','born') # changed the column names
nba_states_map = inner_join(x=nba_count_state, y = all_states, by='region') #inner joined out states map with nba count to create a map

# 5.0 Looking into the split between US vs Foregin players####
## created a list of states
states = list("Alabama","Alaska",'District of Columbia',"Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")

## created a new dataframe that added a new column to check if a player was born in the US or outside
inUs = nba_gens %>%
  mutate(., in_us = ifelse(birth_state %in% states, 1, 0)) 

inUs$born = as.character(inUs$born)

## Analysis and manipulation to get a graph that shows the trend between US vs World
popChange = inUs %>%
  group_by(born) %>%
  summarise(.,total=sum(in_us) , count = n(), perUs = (total/count)*100, perWorld = 100-perUs ) 

x= popChange %>%
  select(., born, perChange =perUs ) %>% mutate(us='United States')
y=popChange %>%
  select(., born, perChange = perWorld) %>% mutate(us='International')

percentChange = rbind(x,y)

percentChange$perChange = format(percentChange$perChange, digits = 5, format = 'f')

percentChange = percentChange %>%
  filter(born>1941)



#Plotly graph ####
a <- list(title = "Trend Bewtween US vs World NBA PLayers",titlefont = f1,showticklabels = TRUE,tickfont = f2,exponentformat = "E")
b <- list(title='Year',autotick=TRUE,ticks='inside',ticklen=10,tickwidth=3,zeroline = TRUE,showline = TRUE,showgrid = FALSE)
c <- list(title = 'Percent Difference',autotick = FALSE,ticks = "inside",tick0 = 0,dtick = 12,ticklen = 10,tickwidth = 3,zeroline = TRUE,showline = TRUE,showgrid = FALSE)
m <- list(l = 50, r = 75, b = 75, t = 50, pad = 1)

plot_ly(data = percentChange, type = 'scatter', x= ~born, y= ~perChange, color= ~us, hoverinfo = 'text',
        text = ~paste('Percent Change: ', perChange,
                      '<br> Location: ', us )) %>%
        layout(title = 'Trend Bewtween US vs World NBA PLayers',titlefont = 'mono',xaxis=b, yaxis=c,autosize = F, width = 500, height = 500, margin = m)


#created a dataframe that shows us all the players that are not from the US ####
outsideUs = inUs %>%
  group_by(in_us) %>%
  filter(., in_us !='US')

# a dataframe that shows you where players outside the US are coming from ####
foreignState = outsideUs %>%
  group_by(birth_state, generation) %>%
  mutate(., state_count = n()) 

foreignStateTop10 = foreignState %>%
  filter(in_us == 0) %>% 
  group_by(birth_state) %>%
  summarise(., country_count = n()) %>%
  arrange(desc(country_count)) %>%
  filter(country_count >= 9)
  




