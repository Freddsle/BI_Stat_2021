library(tidyverse)

temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00464/superconduct.zip", temp)

file_one <- read.csv('./Data/train.csv')
file_two <- read.csv('./Data/unique_m.csv')

unlink(temp)
file_two <- file_two %>% select(-material) %>% relocate(critical_temp)
file_one <- file_one %>% relocate(critical_temp)

new_table <- cbind(file_two, file_one[, c(2:82)])

#delete columns with all zeros
new_table <- new_table[,colSums(new_table != 0) != 0]
write.csv(new_table, './Data/all_in_one.csv', row.names = FALSE)


# В этой задаче мы хотим научиться предсказывать критическую температуру 
# сверхпроводника (колонка critical_temp) по различным характеристикам вещества 
# и по его составу (все остальные колонки).
require(caTools)
set.seed(42) 
sample = sample.split(new_table$critical_temp, SplitRatio = .75)
train = subset(new_table, sample == TRUE)
test  = subset(new_table, sample == FALSE)

## Разделите данные на обучающую и тестовую выборки, 
# на первой вы будете оценивать коэффициенты, на второй вычислять метрику. 


###
# install.packages('caret')
library(caret)
norm.train <- train
temp <- scale(train[, -1])
norm.train[, -1] <- temp

normParam <- preProcess(train[, -1])
norm.test <- predict(normParam, test)


# write.csv for kernel pca
#write.csv(norm.train, 'norm.train.csv', row.names = FALSE)
#write.csv(norm.test, 'norm.test.csv', row.names = FALSE)


# Постройте линейную модель, 
# которая по всем имеющимся у вас признакам предсказывает критическую температуру, 
# посмотрите на adjusted R-squared.  

model <- lm(critical_temp ~ ., data = norm.train)
model_summary <- summary(model)
model_summary$adj.r.squared    # 0.7668411
model_summary$r.squared    # 0.7690963
model_summary$fstatistic
model_summary$residuals

## Хорошая ли получилась модель?
# ... So-so
pred_y_lm <-  predict(model, select(norm.test, -critical_temp))
mean(abs(norm.test$critical_temp - pred_y_lm))

#cor(norm.test$critical_temp, norm.test$new_critical_temp)
#install.packages('Metrics')
#Metrics::mae(norm.test$critical_temp, norm.test$new_critical_temp)
#mean(abs(train$critical_temp - norm.train$critical_temp))
#norm.test %>% select(critical_temp, new_critical_temp) %>% head

# PCA

library(vegan)

temp_pca_train <- rda(norm.train[, -1], scale = TRUE)
head(summary(temp_pca_train))

# to many components to visualize
biplot(temp_pca_train, expand=10)
biplot(temp_pca_train, scaling = "species", display = "species")
biplot(temp_pca_train, scaling = "sites", display = "sites")

# screeplot
eigenvals(temp_pca_train)
bstick(temp_pca_train)
screeplot(temp_pca_train, type = "lines", bstick = TRUE) # график собственных чисел

# summary() Proportion Explained

pca_summary <- summary(temp_pca_train)
pca_result <- as.data.frame(pca_summary$cont)
plot_data <- as.data.frame(t(pca_result[c("Cumulative Proportion"),]))
plot_data$component <- rownames(plot_data)

plot_data %>%
  filter(`Cumulative Proportion` < 0.91) %>% 
  mutate(component = fct_reorder(component, -`Cumulative Proportion`)) %>%
  ggplot(aes(y = component, x = `Cumulative Proportion`)) + 
  geom_bar(stat = "identity") + 
  theme_bw()

plot_data %>% 
  filter(`Cumulative Proportion` <= 0.953) %>% 
  tail(1)


## Component interpretation 

### Train data transformation
pca_scores_7 <- as.data.frame(scores(temp_pca_train, display = "species", 
                     choices = c(1:7), scaling = 0))

matrix_mult_train_7 <- function (pca_scores_7)  {
  as.matrix(norm.train[, -1]) %*% pca_scores_7
}

pca_train_7 <- as.data.frame(apply(pca_scores_7, 2, matrix_mult_train_7))
pca_train_7 <- cbind(critical_temp=norm.train[, 1], pca_train_7)

#norm.test_wt169 <- norm.test[, -169]


### Test data transformation

matrix_mult_test_7 <- function (pca_scores_7)  {
  as.matrix(norm.test[, -c(1, 160)]) %*% pca_scores_7
}

pca_test_7 <- as.data.frame(apply(pca_scores_7, 2, matrix_mult_test_7))
pca_test_7 <- cbind(critical_temp=norm.test[, 1], pca_test_7)

## 78 principal components
pca_scores_67 <- as.data.frame(scores(temp_pca_train, display = "species", 
                                      choices = c(1:67), scaling = 0))

### Train data transformation
matrix_mult_train_67 <- function (pca_scores_67)  {
  as.matrix(norm.train[, -1]) %*% pca_scores_67
}

pca_train_67 <- as.data.frame(apply(pca_scores_67, 2, matrix_mult_train_67))
pca_train_67 <- cbind(critical_temp = norm.train[, 1], pca_train_67)

### Test data transformation
matrix_mult_test_67 <- function (pca_scores_67)  {
  as.matrix(norm.test[, -c(1, 160)]) %*% pca_scores_67
}

pca_test_67 <- as.data.frame(apply(pca_scores_67, 2, matrix_mult_test_67))
pca_test_67 <- cbind(critical_temp=norm.test[, 1], pca_test_67)

# 7-new linear regression
model_after_pca_7 <- lm(critical_temp ~ ., data = pca_train_7)
pca_model_summary_7 <- summary(model_after_pca_7)
pca_model_summary_7$adj.r.squared    # 0.7668411 ----> 0.5840986
pca_model_summary_7$r.squared    # 0.7690963   -------> 0.5842768

## Хорошая ли получилась модель?
# A bit better
pca_test_7$new_pca_temp <- predict(model_after_pca_7, select(pca_test_7, -critical_temp))
mean(abs(pca_test_7$critical_temp - pca_test_7$new_pca_temp))
# 12.61392 ---> 17.3107 --- NO

# 67-new linear regression
model_after_pca_67 <- lm(critical_temp ~ ., data = pca_train_67)
pca_model_summary_67 <- summary(model_after_pca_67)
pca_model_summary_67$adj.r.squared    # 0.7668411 ----> 0.6677988
pca_model_summary_67$r.squared    # 0.7690963   -------> 0.6691614

## Хорошая ли получилась модель?
# No
pca_test_67$new_pca_temp <- predict(model_after_pca_67, 
                                    select(pca_test_67, -critical_temp))
mean(abs(pca_test_67$critical_temp - pca_test_67$new_pca_temp))
# 12.61392  ---> 14.74913

# Снижаем количество главных компонент с 67 до ???

data_select_train = pca_train_67 %>% select(-PC27, -PC49, -PC29, 
                                      -PC47, -PC41, -PC7,
                                      -PC26, -PC33, -PC34,
                                      -PC36, -PC31, -PC37, 
                                      -PC53, -PC66, -PC19,
                                      -PC40, -PC64)

data_select_test = pca_test_67 %>% select(-PC27, -PC49, -PC29, 
                                          -PC47, -PC41, -PC7,
                                          -PC26, -PC33, -PC34,
                                          -PC36, -PC31, -PC37, 
                                          -PC53, -PC66, -PC19,
                                          -PC40, -PC64)

model_after_pca_67_minus <- lm(critical_temp ~ ., data = data_select_train)

data_select_test$new_pca_temp <- predict(model_after_pca_67_minus, 
                                    select(data_select_test, -critical_temp))

mean(abs(data_select_test$critical_temp - data_select_test$new_pca_temp))
summary(model_after_pca_67_minus)$r.squared


# 50

data_select_train = pca_train_67 %>% 
  select(-PC43, -PC46, -PC38, -PC20, -PC25, -PC45,
         -PC62, -PC30, -PC57, -PC48, -PC56, -PC59,
         -PC29, -PC24)

data_select_test = pca_test_67 %>% 
  select(-PC43, -PC46, -PC38, -PC20, -PC25, -PC45,
         -PC62, -PC30, -PC57, -PC48, -PC56, -PC59,
         -PC29, -PC24)

model_after_pca_67_minus <- lm(critical_temp ~ ., data = data_select_train)

summary(model_after_pca_67_minus)

data_select_test$new_pca_temp <- predict(model_after_pca_67_minus, 
                                         select(data_select_test, -critical_temp))

mean(abs(data_select_test$critical_temp - data_select_test$new_pca_temp))



# Principal Component Regression (PCR)
#install.packages("pls")
library(pls)
# using all PC

model_pcr <- pcr(critical_temp ~ ., data = norm.train, scale=TRUE, validation="CV")

summary(model_pcr)

norm.test.pcr <- as.data.frame(predict(model_pcr, as.matrix(norm.test[, -1])))
names(norm.test.pcr)[1] <- 'new_temp'
norm.test.pcr$critical_temp <- norm.test$critical_temp

mean(abs(norm.test.pcr$critical_temp - norm.test.pcr$new_temp))   # 20.4927


model_pcr.r2 <- as.data.frame(predict(model_pcr, norm.train[, -1], ncomp = 67))
names(model_pcr.r2)[1] <- 'new_temp'
model_pcr.r2$critical_temp <- norm.train[, 1]
mean(abs(model_pcr.r2$critical_temp - model_pcr.r2$new_temp))  # 14.83362

rss <- sum((model_pcr.r2$new_temp - model_pcr.r2$critical_temp) ^ 2)  ## residual sum of squares
tss <- sum((model_pcr.r2$critical_temp - mean(model_pcr.r2$critical_temp)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
rsq      # 0.6691614        

               
# Using the kernel PCA

#install.packages("kernlab")
library(kernlab)

kpca <-  kpca(~., data = norm.train[, -1], kernel = 'rbfdot')

# kpca_158 <-  kpca(~., data = norm.train[, -1], kernel = 'rbfdot', features=158)

norm.train.kpca <- as.data.frame(predict(kpca, select(norm.train, -critical_temp)))
norm.train.kpca$critical_temp <- norm.train$critical_temp

norm.test.kpca <-  as.data.frame(predict(kpca, select(norm.test, -critical_temp)))
norm.test.kpca$critical_temp <- norm.test$critical_temp

lm_kpca <- lm(formula = critical_temp ~ ., 
              family = gaussian, 
              data = norm.train.kpca)

summary(lm_kpca)

#Predicting the test set results
prob_pred <-  as.data.frame(predict(lm_kpca, 
                                    newdata  = select(norm.test.kpca, 
                                                      -critical_temp)))
prob_pred$critical_temp <- norm.test$critical_temp

#prod_pred <- read.csv('./kpca_r/prod_pred_kpca.csv')
#prod_pred <- prod_pred %>% rename(new_temp = predict.classifier..newdata...select.norm.test.kpca...critical_temp..)


mean(abs(prod_pred$critical_temp - prod_pred$new_temp))

#### pythin run:
y_pred <- read.csv('y_pred.csv', head=FALSE)
y_pred$critical_temp <- prod_pred$critical_temp
mean(abs(y_pred$critical_temp - y_pred$V1))

rss_kern <- sum((y_pred$V1 - y_pred$critical_temp) ^ 2)  ## residual sum of squares
tss_kern <- sum((y_pred$critical_temp - mean(y_pred$critical_temp)) ^ 2)  ## total sum of squares
rsq_kern <- 1 - rss_kern/tss_kern
rsq_kern      # 0.9702866 


# dowload from telegram
#install.packages("reticulate")
#library(reticulate)
#np <- import("numpy")
#mat <- np$load("X_transform_kPCA.npy")


# sc-RNA seq data visualization
scRNAseq <- read.csv('scRNAseq_CITEseq.txt', sep="\t")

## t-SNE - t-Distributed Stochastic Neighbor Embedding (t-SNE)

't-Distributed Stochastic Neighbor Embedding (t-SNE) is a technique for 
dimensionality reduction that is particularly well suited for 
the visualization of high-dimensional datasets. '

'tSNE is used on scRNA-seq because this type of seq gives us expression values 
on a cell-wise basis, so, tSNE is one of many methods that looks for 
relationships between these cells and attempts to assign groups of cells 
into cell populations that way. A similar thing is performed in CyTOF analysis.'

install.packages('Seurat')
library(Seurat)
library(Rtsne)
library(ggplot2)

pbmc1 <- CreateSeuratObject(counts = scRNAseq[, -977])
pbmc1


#### Quality Control (QC)
pbmc1[["percent.mt"]] <- PercentageFeatureSet(pbmc1, pattern = "^MT-")

FeatureScatter(pbmc1, feature1 = "nCount_RNA", feature2 ="percent.mt")
FeatureScatter(pbmc1, feature1 = "nCount_RNA", feature2 ="nFeature_RNA")

pbmc1 <- subset(pbmc1, subset = nFeature_RNA > 200  & nCount_RNA < 60000)

#### Data Normalization
pbmc1<- NormalizeData(pbmc1)

##### Identification of Highly Variable Features
pbmc1<- FindVariableFeatures(pbmc1)

VariableFeaturePlot(pbmc1)

#### Data Scaling
pbmc1 <- ScaleData(pbmc1)

#### Visualization of scRNA-seq Data Using t-SNE
##### Principal Component Analysis (PCA)

pbmc1<- RunPCA(pbmc1, features = VariableFeatures(object = pbmc1))
ElbowPlot(pbmc1, ndims = 50)

##### Cell Clustering
######## See note 2
pbmc1<- FindNeighbors(pbmc1, dims = 1:20)
pbmc1<- FindClusters(pbmc1, resolution = 1)

##### Running t-SNE

pbmc1<- RunTSNE(pbmc1, dims = 1:20, tsne.method = "Rtsne")


#### Visualization of Single Cell RNA-seq Data Using t-SNE or PCA

DimPlot(object = pbmc1, reduction = "pca", label = TRUE)
DimPlot(object = pbmc1, reduction = "tsne", label = TRUE)

TSNEPlot(object = pbmc1, label = TRUE)

plot_tsne <- DimPlot(object = pbmc1, reduction = "tsne", label = TRUE)


## UMAP

pbmc1 <- RunUMAP(pbmc1, dims = 1:20)
DimPlot(pbmc1, reduction = "umap", label = TRUE)
DimPlot(pbmc1, reduction = "umap", split.by = "seurat_clusters")

plot_tsne <- DimPlot(object = pbmc1, reduction = "tsne", label = TRUE)
plot_umap <- DimPlot(pbmc1, reduction = "umap", label = TRUE)

library(gridExtra)
grid.arrange(plot_tsne, plot_umap, ncol=2)
