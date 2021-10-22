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
    mutate(Name = ifelse((ID == f_names(all_participants)), toString(i), Name))
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


# Рассчитайте среднее значение и стандартное отклонение переменной Height у
# теннисисток (sex = F) на Олимпиаде 2000 года. Округлите ответ до первого знака
# после точки. (2 балла)


