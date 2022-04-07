library(tidyverse)
library(readxl)

set.seed(1234)


# Import .xls data

mouse <-
  list.files(path = "../data/", pattern = "*.xls", full.names = TRUE) %>% 
  map_df(~read_excel(.))

head(mouse)
ncol(mouse)
nrow(mouse)

# Data description

## mouses
mouse %>%
  separate(MouseID, c("MouseID", "NumberExperiment"), "_") %>% 
  distinct(MouseID) %>% 
  count() %>% .[1,1,1]

## number classes
mouse %>% distinct(class) %>% count() %>% .[1,1,1]

## mice in each class
mouse %>%
  separate(MouseID, c("MouseID", "NumberExperiment"), "_") %>% 
  group_by(class) %>% 
  summarise(mice_count = n())

# Genotypes and treatment and Behavior
mouse %>% distinct(Genotype) %>% count() %>% .[1,1,1]
mouse %>% distinct(Treatment) %>% count() %>% .[1,1,1]
mouse %>% distinct(Behavior) %>% count() %>% .[1,1,1]

mouse %>%
  separate(MouseID, c("MouseID", "NumberExperiment"), "_") %>% 
  group_by(Treatment, Behavior, Genotype) %>% 
  summarise('Records count' = n())

# NA?
mouse %>% filter_all(any_vars(is.na(.))) %>%  
  separate(MouseID, c("MouseID", "NumberExperiment"), "_") %>% 
  group_by(Treatment, Behavior, Genotype) %>% 
  summarise('Records count' = n())

# number na
mouse %>% filter_all(any_vars(is.na(.))) %>% count() %>% .[1,1,1]
# total observations
mouse %>% count() %>% .[1,1,1]

# delete na in groups

mouse_wt_na_M_CS <- mouse %>% 
  filter(Treatment == "Memantine" & Behavior == "C/S") %>% 
  separate(MouseID, c("MouseID", "NumberExperiment"), "_")

mouse_wt_na_M_CS %>% select(where(function(x) any(!is.na(x)))) %>%  group_by(Treatment, Behavior, Genotype) %>% 
  summarise('Records count' = n())
mouse_wt_na_M_CS %>% filter_all(any_vars(is.na(.))) %>%  group_by(Treatment, Behavior, Genotype) %>% 
  summarise('Records count' = n())

mouse_wt_na_M_SC <- mouse %>% 
  filter(Treatment == "Memantine" & Behavior == "S/C") %>% 
  separate(MouseID, c("MouseID", "NumberExperiment"), "_") 

mouse_wt_na_M_SC %>% select(where(function(x) any(!is.na(x)))) %>%  group_by(Treatment, Behavior, Genotype) %>% 
  summarise('Records count' = n())
mouse_wt_na_M_SC %>% filter_all(any_vars(is.na(.))) %>%  group_by(Treatment, Behavior, Genotype) %>% 
  summarise('Records count' = n())



mouse_wt_na_S_CS <- mouse %>% 
  filter(Treatment == "Saline" & Behavior == "C/S") %>% 
  separate(MouseID, c("MouseID", "NumberExperiment"), "_")

mouse_wt_na_S_CS %>% select(where(function(x) any(!is.na(x)))) %>%  group_by(Treatment, Behavior, Genotype) %>% 
  summarise('Records count' = n())
mouse_wt_na_S_CS %>% filter_all(any_vars(is.na(.))) %>%  group_by(Treatment, Behavior, Genotype) %>% 
  summarise('Records count' = n())

mouse_wt_na_S_SC <- mouse %>% 
  filter(Treatment == "Saline" & Behavior == "S/C") %>% 
  separate(MouseID, c("MouseID", "NumberExperiment"), "_") 

mouse_wt_na_S_SC %>% select(where(function(x) any(!is.na(x)))) %>%  group_by(Treatment, Behavior, Genotype) %>% 
  summarise('Records count' = n())
mouse_wt_na_S_SC %>% filter_all(any_vars(is.na(.))) %>%  group_by(Treatment, Behavior, Genotype) %>% 
  summarise('Records count' = n())



# PLOTS in files

function_name <- function(column) {
  
  p <- ggplot(stand_mouse, aes_string(y = column, x = 'class')) +
  geom_violin() +
  stat_boxplot(geom ='errorbar', width=0.5) +
  geom_boxplot(width=0.05) +
  theme_bw() +
  labs(title="Boxplot for Protein in each class",
       x ="Class")
  
  png(paste('../data/plots/', column, '.png', sep=''))
  print(p)
  dev.off()
}

stand_mouse <- mouse[unlist(lapply(mouse, is.numeric))]
num_cols_names <-  names(stand_mouse)
stand_mouse <- as.data.frame(lapply(stand_mouse, scale))
stand_mouse$class <- mouse$class

lapply(num_cols_names, function_name)

