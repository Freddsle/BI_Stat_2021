# Set working directory to the data folder:
# setwd("/BI_Stat_2021/olimpic_games/data")
library(tidyverse)

# Open all .csv 
all_participants <-
  list.files(path = "../data/", pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(.))

#Check Sex column
all_participants %>% filter(!Sex %in% c('F', 'M', NA))
all_participants %>% filter(ID == '79609' | ID == '79630')

all_participants <- all_participants %>% mutate(Sex = ifelse(ID == '79609', 'M', Sex))
all_participants <- all_participants %>% 
  mutate(Sex = ifelse(!Sex %in% c('F', 'M', NA), 'NA', Sex))

#Check Sport column
all_participants %>% group_by(Sport) %>% summarise()
all_participants %>% filter(str_detect(Sport, "Footba")) %>% group_by(Sport) %>% 
  summarise()
all_participants <- all_participants %>% 
  mutate(Sport = ifelse(Sport == "Footba", 'Football', Sport))

#Check Name column
ID <- all_participants %>% group_by(ID, Name) %>% summarise() %>% ungroup() %>% 
  mutate(RowNumber = 1:n()) %>% tail(1)
ID

all_participants %>% filter(ID == (all_participants %>% group_by(ID, Name) %>% summarise() %>% ungroup() %>% 
                                     mutate(RowNumber = 1:n()) %>% filter(RowNumber != ID) %>% head(1)[1,1]))

all_participants <- all_participants %>% 
  mutate(Name = ifelse((Name == 'Pietro Spec' & ID == '113716'), 'Pietro Speciale', Name))

#test
'all_participants %>% filter(ID == 135571)

all_participants <- all_participants %>% 
  mutate(Name = ifelse((ID == 135571 & Age == 34), 'Tomas', Name))'

# if multiple

'f_names <- function(all_participants) {
  all_participants %>% group_by(ID, Name) %>% summarise() %>% ungroup() %>% 
    mutate(RowNumber = 1:n()) %>% filter(RowNumber != ID) %>% head(1)[1,1,1]
}

for (i in c(1:(ID[3] - ID[1])[1,1])){
  all_participants <- all_participants %>% 
    mutate(Name = ifelse((ID == f_names(all_participants)) & !is.na(f_names(all_participants)),
                         toString(i), Name))
}'

#Check Height column


all_participants %>% filter(ID %in% (all_participants %>% 
                                       filter(Height > 272) %>%  
                                       .[,1,1]) &
                            Age %in% (all_participants %>% 
                                        filter(Height > 272) %>%  
                                        .[,4,1]))

all_participants <- all_participants %>% 
  mutate(Height = ifelse((ID == '23549' & Height > 272), 176, Height))

# if multiple:
all_participants <- all_participants %>% 
  mutate(Height = ifelse(Height > 272, NA, Height))



# Check Age column
all_participants %>% filter(Age > 122)
all_participants %>% filter(ID %in% (all_participants %>% 
                                    filter(Age > 122) %>%  
                                    .[,1,1]) &
                            Year %in% (all_participants %>% 
                                      filter(Age > 122) %>%  
                                      .[,10,1]))

all_participants <- all_participants %>% 
  mutate(Age = ifelse((ID == '23459' & Age == 240), 24, Age))

all_participants <- all_participants %>% 
  mutate(Age = ifelse(Age > 122, NA, Age))

all_participants %>% filter(ID %in% (all_participants %>% 
                                     filter(Age < 8) %>%  
                                     .[,1,1]) &
                            Year %in% (all_participants %>% 
                                       filter(Age < 8) %>%  
                                       .[,10,1]))





# Check Weight column

all_participants %>% filter(ID %in% (all_participants %>% 
                                     filter(Weight < 17) %>%  
                                     .[,1,1]))

all_participants <- all_participants %>% 
  mutate(Weight = ifelse((ID == '68370' & Weight == 7), 77, Weight))

all_participants <- all_participants %>% 
  mutate(Weight = ifelse(Weight < 17, NA, Weight))


all_participants %>% filter(ID %in% (all_participants %>% 
                                       filter(Weight > 445) %>%  
                                       .[,1,1]))

all_participants <- all_participants %>% 
  mutate(Weight = ifelse(Weight > 445, NA, Weight))


# Check Games column
all_participants %>% 
  filter(!(grepl('Summer', Games, fixed = TRUE) | 
         grepl('Winter', Games, fixed = TRUE) |
         is.na(Games)))

all_participants <- all_participants %>% 
  mutate(Games=sub('S+.*$', 'Summer', Games)) %>% 
  mutate(Games=sub('W+.*$', 'Winter', Games))


# Check Year column
all_participants <- all_participants %>% 
  mutate(Year = ifelse((is.na(Year) & !is.na(Games)), 
                       str_extract(Games,'^[[:digit:]]{4}'), 
                       Year))


# Check Season column
all_participants <- all_participants %>% 
  mutate(Season = ifelse((is.na(Season) & !is.na(Games)),
                         str_extract(Games,'[[:alpha:]]+$'), 
                         Season))


# Check City column
all_participants %>% group_by(City) %>%  summarise(n())
all_participants %>% filter(is.na(City))

# Check Event column
all_participants %>% group_by(Event) %>%  summarise(n())
all_participants %>% filter(is.na(Event))

# Check Medal Column

all_participants %>% filter(!(Medal %in% c('Gold', 'Silver', 'Bronze', NA)))



#Age of the youngest athletes of both genders at the 1992 Olympics

youngest_men_1992 <- all_participants %>% filter(Year == 1992 & Sex == 'M') %>% 
  filter(Age == min(Age, na.rm = TRUE))

youngest_woman_1992 <- all_participants %>% filter(Year == 1992 & Sex == 'F') %>% 
  filter(Age == min(Age, na.rm = TRUE))

youngest_men_1992[1,4,1]
youngest_woman_1992[1,4,1]

#Рассчитайте среднее значение и стандартное отклонение переменной Height для
#спортсменов каждого пола. (2 балла)

mean_woman <- all_participants %>% filter(Sex == 'F') %>% 
  summarise(mean(Height, na.rm = TRUE))
mean_woman[1,1,1]

sd_woman <- all_participants %>% filter(Sex == 'F') %>% 
  summarise(sd(Height, na.rm = TRUE))
sd_woman[1,1,1]


mean_man <- all_participants %>% filter(Sex == 'M') %>% 
  summarise(mean(Height, na.rm = TRUE))
round(mean_man[1,1,1], 2)

sd_man <- all_participants %>% filter(Sex == 'M') %>% 
  summarise(sd(Height, na.rm = TRUE))
sd_man[1,1,1]


# Calculate the mean and standard deviation of the Height variable for
# tennis players (sex = F) at the 2000 Olympics. 
# Round your answer to the first character after dot. (2 points)

mean_woman_2000 <- all_participants %>% filter(Sex == 'F'& Year == 2000) %>% 
  summarise(mean(Height, na.rm = TRUE))
sprintf("%.1f", round(mean_woman_2000[1,1,1], 1))

sd_woman_2000 <- all_participants %>% filter(Sex == 'F' & Year == 2000) %>% 
  summarise(sd(Height, na.rm = TRUE))
sprintf("%.1f", round(sd_woman_2000[1,1,1], 1))


# What sport was the 2006 Olympics heaviest athlete in? (2 points)

heaviest_2006 <- all_participants %>% 
  filter(Year == 2006) %>% 
  filter(Weight == max(Weight, na.rm = TRUE))
  
heaviest_2006 %>% select(Sport) %>% .[1,1,1]
heaviest_2006 %>% select(Weight) %>% .[1,1,1]


# How many gold medals were won by women from 1980 to 2010? (2 points)

golds_1980_2010 <- all_participants %>% 
  filter(Sex == 'F' & (Year >= 1980 & Year <= 2010) & Medal == 'Gold') 

golds_1980_2010 %>% summarise(n()) %>% .[1,1,1]

ggplot(golds_1980_2010, aes(y = Games)) +
  geom_histogram(stat="count", fill='Blue', alpha = 0.3) +
  geom_text(aes(label = ..count.., fontface = "bold"), stat = "count", position = position_stack(vjust = 0.5)) +
  labs(x='Number of medals',
       y='Games',
       title="Number of Gold medals received by women from 1980 to 2010") + 
  theme_set(theme_bw(base_size = 15))


# How many times has athlete John Aalberg competed in the Olympics over the years? (2 points)
 
john_games <- all_participants %>% filter(grepl('John Aalberg', Name))
john_games %>% group_by(Year) %>% select(Name, Games) %>% mutate(N = n())%>% unique()

john_games %>% summarise(n()) %>% .[1,1,1]
john_games %>% filter(Games == '1994 Winter')%>% summarise(n()) %>% .[1,1,1]
john_games %>% filter(Games == '1992 Winter')%>% summarise(n()) %>% .[1,1,1]

# Identify the least and most represented (by number of participants) age groups 
# for athletes at the 2008 Olympic Games. Possible age groups: 
# 1 - [15-25), 2 - [25-35), 3 - [35-45), 4 - [45-55].

age_groups_2006 <- all_participants %>% 
  filter(Year == 2008) %>% 
  select(ID, Sex, Age) %>% 
  distinct() %>% 
  mutate(Age_group = NA) %>% 
  mutate(Age_group = ifelse(Age < 15, '14 and less', Age_group)) %>% 
  mutate(Age_group = ifelse(Age > 14 & Age < 25, '15-24', Age_group)) %>% 
  mutate(Age_group = ifelse(Age > 24 & Age < 35, '25-34', Age_group)) %>% 
  mutate(Age_group = ifelse(Age > 34 & Age < 45, '35-44', Age_group)) %>% 
  mutate(Age_group = ifelse(Age > 44 & Age < 56, '45-55', Age_group)) %>% 
  mutate(Age_group = ifelse(Age > 55, '56 and more', Age_group)) %>% 
  filter(!is.na(Age_group))
  
age_groups_2006 %>% group_by(Age_group) %>% summarise('Group size' = n())

ggplot(age_groups_2006, aes(y = Age_group)) +
  geom_histogram(stat="count", alpha = 0.3) +
  geom_text(aes(label = ..count.., fontface = "bold"), stat = "count", position = position_stack(vjust = 0.5)) +
  labs(x='Number of participants',
       y='Age group',
       title="Number of participants in different age groups") + 
  theme_set(theme_bw(base_size = 15))

age_groups_2006 %>% 
  filter(Age_group != '14 and less' & Age_group != '56 and more') %>% 
  group_by(Age_group) %>% 
  summarise(Group_size = n()) %>% 
  filter(Group_size == min(Group_size)) %>% .[1,1,1]

age_groups_2006 %>% 
  filter(Age_group != '14 and less' & Age_group != '56 and more') %>% 
  group_by(Age_group) %>% 
  summarise(Group_size = n()) %>% 
  filter(Group_size == max(Group_size)) %>% .[1,1,1]


# How has the number of sports in the 2002 Olympics changed from the 1994 Olympics?

# dinamics of winter sports
sports_from_94_to_02 <- all_participants %>% 
  filter(Year >= 1994 & Year <= 2002) %>% 
  filter(Season == 'Winter') %>% 
  select(Year, Season, Sport) %>% 
  distinct()

ggplot(sports_from_94_to_02, aes(y = Year)) +
  geom_histogram(stat="count", alpha = 0.3, fill = 'Red') +
  geom_text(aes(label = ..count.., fontface = "bold"), stat = "count", position = position_stack(vjust = 0.5)) +
  labs(x='Number of sports',
       y='Year',
       title="Number of sports at the Winter Olympics 1994-2002") + 
  theme_set(theme_bw(base_size = 15))

sports_answer <- sports_from_94_to_02 %>% 
  filter(Year == 2002 | Year == 1994) %>% 
  group_by(Year)%>% summarise(n = n())

min_year_sports <- sports_answer %>% filter(n == min(n)) %>% .[1,1,1]

sports_answer <- sports_answer %>% 
  mutate(direction = if_else(min_year_sports == 1994, 'increased', 'decreased'))

max(sports_answer[,2,]) - min(sports_answer[,2,]) 
(max(sports_answer[,2,]) / min(sports_answer[,2,]) - 1) * 100

sports_answer[1,3,1]

# Output for the winter and summer Olympiads separately the top 3 countries for 
# each type of medal.

#Winter
winter_medals <- all_participants %>% 
  mutate(Team = ifelse(Team == 'Unified Team', 'Soviet Union', Team)) %>% 
  filter(Season == 'Winter' & !is.na(Medal)) %>% 
  mutate(Team = sub('-{1}[[:digit:]]{1}$', '', Team))


winter_medals %>% 
  group_by(Team) %>% 
  summarise(n = n()) %>% 
  filter(n > 10) %>% 
  ggplot(aes(y = reorder(Team, desc(n)), x = n)) +
  geom_histogram(stat="identity", alpha = 0.3, fill = 'Blue') +
  geom_text(aes(label = n, fontface = "bold"), position = position_stack(vjust = 0.5)) +
  labs(x='Number of medals (all types) in different countries',
       y='Сountry',
       title="Medals in different countries (if more than 10). Winter Games",
       subtitle = 'The name of the country in the year of receiving') + 
  theme_set(theme_bw())


winter_medals %>% 
  filter(Medal == 'Gold') %>% 
  group_by(Team) %>% 
  summarise(Medals = n()) %>% 
  top_n(n=3)

winter_medals %>% 
  filter(Medal == 'Silver') %>% 
  group_by(Team) %>% 
  summarise(Medals = n()) %>%
  arrange(-Medals) %>% 
  top_n(n=3)

winter_medals %>% 
  filter(Medal == 'Bronze') %>% 
  group_by(Team) %>% 
  summarise(Medals = n()) %>%
  arrange(-Medals) %>% 
  top_n(n=3)

# Summer
summer_medals <- all_participants %>% 
  mutate(Team = ifelse(Team == 'Unified Team', 'Soviet Union', Team)) %>% 
  filter(Season == 'Summer' & !is.na(Medal)) %>% 
  mutate(Team = sub('-{1}[[:digit:]]{1}$', '', Team))

summer_medals %>% 
  group_by(Team) %>% 
  summarise(n = n()) %>% 
  filter(n > 200) %>% 
  ggplot(aes(y = reorder(Team, desc(n)), x = n)) +
  geom_histogram(stat="identity", alpha = 0.3, fill = 'Blue') +
  geom_text(aes(label = n, fontface = "bold"), position = position_stack(vjust = 0.5)) +
  labs(x='Number of medals (all types) in different countries',
       y='Сountry',
       title="Medals in different countries (if more than 200). Summer Games",
       subtitle = 'The name of the country in the year of receiving') + 
  theme_set(theme_bw())

summer_medals %>% 
  filter(Medal == 'Gold') %>% 
  group_by(Team) %>% 
  summarise(Medals = n()) %>% 
  arrange(-Medals) %>% 
  top_n(n=3)

summer_medals %>% 
  filter(Medal == 'Silver') %>% 
  group_by(Team) %>% 
  summarise(Medals = n()) %>%
  arrange(-Medals) %>% 
  top_n(n=3)

summer_medals %>% 
  filter(Medal == 'Bronze') %>% 
  group_by(Team) %>% 
  summarise(Medals = n()) %>%
  arrange(-Medals) %>% 
  top_n(n=3)


summer_medals_teams <- summer_medals %>% 
  group_by(NOC, Team) %>% 
  summarise(n = n())

# Create a new variable Height_z_scores and store the values of the 
# Height variable in it after standardizing it.

all_participants <- all_participants %>%  
  mutate(Height_z_scores = (Height - 
                              mean(Height, na.rm = TRUE)) / sd(Height, na.rm = TRUE))


plot_height <- ggplot(all_participants, aes(Height)) +
  geom_density() +
  theme_bw() +
  labs(title="Height of athletes", x = 'Height, cm')

plot_height_z <- ggplot(all_participants, aes(Height_z_scores)) +
  geom_density() +
  theme_bw() +
  labs(title="Height of athletes", x = 'Standardized Height')

gridExtra::grid.arrange(plot_height, plot_height_z, ncol=2)

# Create a new variable Height_min_max_scaled and store the values of the Height 
# variable in it after applying min-max normalization to it (you will need to figure 
# out how it works).

all_participants <- all_participants %>%  
  mutate(Height_min_max_scaled = (Height - 
                                  min(Height, na.rm = TRUE)) / 
                                 (max(Height, na.rm = TRUE) - 
                                  min(Height, na.rm = TRUE)))

plot_height_minmax <- ggplot(all_participants, aes(Height_min_max_scaled)) +
  geom_density() +
  theme_bw() +
  labs(title="Height of athletes", x = '"Min-max" standardized Height')

gridExtra::grid.arrange(plot_height, plot_height_minmax, ncol=2)

# Compare the height, weight and ages of men and women who competed in the Winter 
# Olympics. Please format the results so that we can immediately use them for 
# the article.

task_14 <- all_participants %>% 
  filter(Season == 'Winter') %>% 
  select(Height, Weight, Age, Sex)

## Sex-Height

ggplot(task_14, aes(Height, color=Sex)) +
  geom_density() +
  scale_color_hue(labels = c("female", "male")) +
  theme_bw() +
  labs(title="Height of athletes")

t_test_height <- t.test(Height~Sex, data = task_14)

## Sex-Weight

ggplot(task_14, aes(Weight, color=Sex)) +
  geom_density() +
  scale_color_hue(labels = c("female", "male")) +
  theme_bw() +
  labs(title="Weight of athletes")

t_test_height <- t.test(Weight~Sex, data = task_14)

## Sex-Age

ggplot(task_14, aes(Age, color=Sex)) +
  geom_density() +
  scale_color_hue(labels = c("female", "male")) +
  theme_bw() +
  labs(title="Age of athletes")

t_test_age <- t.test(Age~Sex, data = task_14)


# Нас особенно интересуют переменные Team и Medal. 
# Что ты можешь про них сказать? 
# Есть ли у нас основания предполагать, что они могут быть взаимосвязаны?
# Как ты это определил?

# We are especially interested in the variables Team and Medal. 
# What can you say about them? 
# Do we have any reason to believe that they might be interrelated? 
# How did you define it?