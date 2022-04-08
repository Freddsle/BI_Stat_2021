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


# Are there differences in the level of BDNF_N production depending on the class in the experiment?
mouse %>% 
  filter(!is.na(BDNF_N)) %>% 
  group_by(class) %>% 
  summarise(n_BDNF_N = n())

ggplot(mouse, aes(BDNF_N, color=class)) +
  geom_density() +
  theme_bw() +
  labs(title="Boxplot for Protein in each class",
       x ="Class")

ggplot(mouse, aes(y = BDNF_N, x = class)) +
  geom_violin() +
  stat_boxplot(geom ='errorbar', width=0.7) +
  geom_boxplot(width=0.2) +
  stat_summary(geom="text", fun=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.45), size=3.5) +
  theme_bw() +
  labs(title="Boxplot for Protein in each class",
       x ="Class")

## выбросы для каждого класса

quants <- c(0, 0.25, 0.50, 0.75, 1)

### 1
quant <- mouse_BDNF_N %>% 
  filter(class == 'c-CS-m') %>% 
  select(BDNF_N) %>%  
  apply(., 2, quantile, probs = quants) 

iqr <- (quant[4,1] - quant[2,1]) * 1.75

mouse_wo_outl <- mouse_BDNF_N %>% filter(class == 'c-CS-m' & !BDNF_N > quant[4,1] + iqr)
mouse_wo_outl <- mouse_wo_outl %>% filter(class == 'c-CS-m' & !BDNF_N < quant[2,1] - iqr)

### 2
quant <- mouse_BDNF_N %>% 
  filter(class == 'c-CS-s') %>% 
  select(BDNF_N) %>%  
  apply(., 2, quantile, probs = quants) 

iqr <- (quant[4,1] - quant[2,1]) * 1.75

mouse_wo_outl <- rbind(mouse_wo_outl, mouse_BDNF_N %>% 
                         filter(class == 'c-CS-s' & !(BDNF_N < (quant[2,1] - iqr) | BDNF_N > quant[4,1] + iqr)))

### 3
quant <- mouse_BDNF_N %>% 
  filter(class == 'c-SC-m') %>% 
  select(BDNF_N) %>%  
  apply(., 2, quantile, probs = quants) 

iqr <- (quant[4,1] - quant[2,1]) * 1.75

mouse_wo_outl <- rbind(mouse_wo_outl, mouse_BDNF_N %>% 
                         filter(class == 'c-SC-m' & !(BDNF_N < (quant[2,1] - iqr) | BDNF_N > quant[4,1] + iqr)))

### 4
quant <- mouse_BDNF_N %>% 
  filter(class == 'c-SC-s') %>% 
  select(BDNF_N) %>%  
  apply(., 2, quantile, probs = quants) 

iqr <- (quant[4,1] - quant[2,1]) * 1.75

mouse_wo_outl <- rbind(mouse_wo_outl, mouse_BDNF_N %>% 
                         filter(class == 'c-SC-s' & !(BDNF_N < (quant[2,1] - iqr) | BDNF_N > quant[4,1] + iqr)))

### 5
quant <- mouse_BDNF_N %>% 
  filter(class == 't-CS-m') %>% 
  select(BDNF_N) %>%  
  apply(., 2, quantile, probs = quants) 

iqr <- (quant[4,1] - quant[2,1]) * 1.75

mouse_wo_outl <- rbind(mouse_wo_outl, mouse_BDNF_N %>% 
                         filter(class == 't-CS-m' & !(BDNF_N < (quant[2,1] - iqr) | BDNF_N > quant[4,1] + iqr)))


### 6
quant <- mouse_BDNF_N %>% 
  filter(class == 't-CS-s') %>% 
  select(BDNF_N) %>%  
  apply(., 2, quantile, probs = quants) 

iqr <- (quant[4,1] - quant[2,1]) * 1.75

mouse_wo_outl <- rbind(mouse_wo_outl, mouse_BDNF_N %>% 
                         filter(class == 't-CS-s' & !(BDNF_N < (quant[2,1] - iqr) | BDNF_N > quant[4,1] + iqr)))

### 7
quant <- mouse_BDNF_N %>% 
  filter(class == 't-SC-m') %>% 
  select(BDNF_N) %>%  
  apply(., 2, quantile, probs = quants) 

iqr <- (quant[4,1] - quant[2,1]) * 1.75

mouse_wo_outl <- rbind(mouse_wo_outl, mouse_BDNF_N %>% 
                         filter(class == 't-SC-m' & !(BDNF_N < (quant[2,1] - iqr) | BDNF_N > quant[4,1] + iqr)))


### 8
quant <- mouse_BDNF_N %>% 
  filter(class == 't-SC-s') %>% 
  select(BDNF_N) %>%  
  apply(., 2, quantile, probs = quants) 

iqr <- (quant[4,1] - quant[2,1]) * 1.75

mouse_wo_outl <- rbind(mouse_wo_outl, mouse_BDNF_N %>% 
                         filter(class == 't-SC-s' & !(BDNF_N < (quant[2,1] - iqr) | BDNF_N > quant[4,1] + iqr)))





## plot

ggplot(mouse_wo_outl, aes(y = BDNF_N, x = class)) +
  geom_violin() +
  stat_boxplot(geom ='errorbar', width=0.7) +
  geom_boxplot(width=0.2) +
  theme_bw() +
  labs(title="Boxplot for BDNF_N Protein in each class",
       x ="Class")


mouse_wo_outl %>% 
  group_by(class) %>% 
  summarise(n_BDNF_N = n())

##SW
mouse_wo_outl$class = as.factor(mouse_wo_outl$class)

mod_BDNF <- lm(BDNF_N ~ class, data = mouse_wo_outl)
summary(mod_BDNF)

ggplot(mouse_wo_outl, aes(x = mod_BDNF$residuals)) +
  geom_histogram(bins=30, fill = 'blue', alpha = 0.5, color = 'black') +
  theme_bw() +
  labs(title = 'Histogram of Residuals', 
       x = 'Residuals', 
       y = 'Frequency')

spariro_res <- shapiro.test(mod_BDNF$residuals)

ggplot(data = mouse_wo_outl,
       aes(sample = mod_BDNF$residuals)) +
  geom_qq() +
  theme_bw() +
  geom_qq_line(colour = "red") +
  labs(title = "Quantile plot of residuals")

ggplot(mod_BDNF, 
       aes(x = class, y = .stdresid)) + 
  geom_violin() + 
  theme_bw() +
  geom_boxplot(width = 0.1) + 
  labs(title = "Plot of residuals",
       x = "Experiment class",
       y = 'St.d. of the residuals')

### ANOVA
library(car)
anova_test_BDNF <- Anova(mod_BDNF)
anova_test_BDNF

library(multcomp)
post_hoch_BDNF <- glht(mod_BDNF, linfct = mcp(class = "Tukey"))
result_posthoch <- summary(post_hoch_BDNF)

result_posthoch$test$tstat['c-CS-s - c-CS-m']


# Попробовать построить линейную модель, способную предсказать уровень продукции белка ERBB4_N 
# на основании данных о других белках в эксперименте (15 баллов) 
# -- провести диагностику полученной линейной модели 
# -- объяснить, почему это является хорошим/не хорошим решением 


# rows with many NA
df1 <- sapply(mouse, function(y) is.na(y))
mouse[rowSums(df1, na.rm=TRUE) > 20,]
mouse_naom <- mouse[rowSums(df1, na.rm=TRUE) < 20,]

# columns where more then half rows are NA
na_count <- sapply(mouse_naom, function(y) sum(length(which(is.na(y)))))
na_count[na_count > 100]

mouse_naom <- mouse_naom %>% dplyr::select(-names(na_count[na_count > 100]))
mouse_naom <- mouse_naom[rowSums(sapply(mouse_naom, function(y) is.na(y)), na.rm=TRUE) < 1,]

mouse_naom <- mouse_naom %>% dplyr::select(where(is.numeric))

## Linear model
# 2. Divide the data into training and test samples

library(caTools)
require(caret)

sample = sample.split(mouse_naom$ERBB4_N, SplitRatio = .75)
train = subset(mouse_naom, sample == TRUE)
test  = subset(mouse_naom, sample == FALSE)

# 3. Normalize the train and test data

norm.train <- train
temp <- scale(train[, -1])
norm.train[, -1] <- temp

normParam <- caret::preProcess(train[, -1])
norm.test <- predict(normParam, test)


# LM
model <- lm(ERBB4_N ~ ., data = norm.train)
model_summary <- summary(model)
model_summary

round(sd(model_summary$residuals), 3)
nrow(table(model_summary$residuals)) - ncol(norm.test) + 1 # degrees of freedom

model_summary$r.squared # Multiple R-squared:  
model_summary$adj.r.squared # , Adjusted R-squared

round(model_summary$fstatistic, 0)[1] #F-statistic:
round(model_summary$fstatistic, 0)[2] # DF
round(model_summary$fstatistic, 0)[3] # p-value: <2e-16.

pred_y_lm <-  predict(model, dplyr::select(norm.test, -ERBB4_N))
mean(abs(norm.test$ERBB4_N - pred_y_lm))

## Inspect model
par(mfrow=c(1,1))
plot(model, 5)

par(mfrow = c(2, 2))
plot(model)
par(mfrow=c(1,1))

library(lmtest)
bptest(model)


#  PCA

library(vegan)

temp_pca_train <- rda(dplyr::select(train, -ERBB4_N), scale = TRUE)
biplot(temp_pca_train)

ncol(temp_pca_train$CA$u)

Scree_Plot_PCA <- temp_pca_train
screeplot(Scree_Plot_PCA, type = "lines", bstick = TRUE)


pca_summary <- summary(temp_pca_train)
pca_result <- as.data.frame(pca_summary$cont)
plot_data <- as.data.frame(t(pca_result[c("Cumulative Proportion"),]))
plot_data$component <- rownames(plot_data)

plot_data %>% 
  filter(`Cumulative Proportion` <= 0.91) %>% 
  tail(1)


plot_data %>% 
  filter(`Cumulative Proportion` <= 0.95)

# Getting Principal Component Scores and Data Transformation

pca_scores_17 <- as.data.frame(scores(temp_pca_train, display = "species", 
                                      choices = c(1:17), scaling = 0))

matrix_mult_train_7 <- function (pca_scores_7)  {
  as.matrix(norm.train[, -1]) %*% pca_scores_7
}

pca_train_7 <- as.data.frame(apply(pca_scores_7, 2, matrix_mult_train_7))
pca_train_7 <- cbind(critical_temp=norm.train[, 1], pca_train_7)


matrix_mult_train <- function (pca_scores_17)  {
  as.matrix(dplyr::select(norm.train, -ERBB4_N)) %*% pca_scores_17
}

pca_train_17 <- as.data.frame(apply(pca_scores_17, 2, matrix_mult_train))
pca_train_17 <- cbind(ERBB4_N=dplyr::select(norm.train, ERBB4_N), pca_train_17)

# Test data transformation

matrix_mult_test <- function (pca_scores_17)  {
  as.matrix(dplyr::select(norm.test, -ERBB4_N)) %*% pca_scores_17
}

pca_test_17 <- as.data.frame(apply(pca_scores_17, 2, matrix_mult_test))
pca_test_17 <- cbind(ERBB4_N=dplyr::select(norm.train, ERBB4_N), pca_test_17)

# New linear regression after PCA

model_after_pca_17 <- lm(ERBB4_N ~ ., data = pca_train_17)
pca_model_summary_17 <- summary(model_after_pca_17)

summary(model_after_pca_17)
pca_test_17$new_ERBB4_N <- predict(model_after_pca_17, dplyr::select(pca_test_17, -ERBB4_N))

## 3D plot

pca_scores_3 <- as.data.frame(scores(temp_pca_train, display = "species", 
                                      choices = c(1:3), scaling = 0))

plot(pca_scores_3)

library(vegan3d)

ordiplot3d(temp_pca_train, scaling = 3)

pca_scores_3$PC1


install.packages("plot3D")
library(plot3D)
plot3D::scatter3D(pca_scores_3$PC1, pca_scores_3$PC2, pca_scores_3$PC3, theta = 15, d = 2, phi = 16,
                  xlab = "PC1", ylab ="PC2", zlab = "PC3")

# 
