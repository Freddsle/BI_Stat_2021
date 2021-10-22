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




f_names <- function(all_participants) {
  all_participants %>% group_by(ID, Name) %>% summarise() %>% ungroup() %>% 
    mutate(RowNumber = 1:n()) %>% filter(RowNumber != ID) %>% head(1)[1,1]
}

for (wrong_names in c(1:(ID[3] - ID[1])[1,1])){
  all_participants %>% filter(ID == f_names(.))
}



#Check Height column
all_participants %>% filter(Height > 272)
all_participants %>% filter(ID=='23549')

all_participants <- all_participants %>% 
  mutate(Height = ifelse((ID == '23549' & Height > 272), 176, Height))

# Check Age column
all_participants %>% filter(Age > 122)
all_participants %>% filter(ID=='23459' & Year == 1912)

all_participants <- all_participants %>% 
  mutate(Age = ifelse((ID == '23459' & Age == 240), 24, Age))

# Check Weight column
all_participants %>% filter(Weight == min(Weight, na.rm = TRUE))
all_participants %>% filter(ID=='68370', Age == 25)

all_participants <- all_participants %>% filter(!(Weight == 7 & ID == '68370'))

# Check Games column
Games <-  all_participants %>% group_by(Games) %>%  summarise(n())

all_participants <- all_participants %>% 
  mutate(Games = ifelse((Games == '2000 Su' & !is.na(Games)), '2000 Summer', Games))
all_participants <- all_participants %>% 
  mutate(Games = ifelse((Games == '2004 Summe' & !is.na(Games)), '2004 Summer', Games))

# Check Year column
Year <-  all_participants %>% group_by(Year) %>%  summarise(n())
all_participants %>% filter(is.na(Year))

# Check Season column
Season <-  all_participants %>% group_by(Season) %>%  summarise(n())
all_participants %>% filter(is.na(Season))

# Check City column
City <-  all_participants %>% group_by(City) %>%  summarise(n())
all_participants %>% filter(is.na(City))

# Check Event column
Event <-  all_participants %>% group_by(Event) %>%  summarise(n())
all_participants %>% filter(is.na(Event))


