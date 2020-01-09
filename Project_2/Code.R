
#Load Packages
if (!require(dplyr)) install.packages('dplyr')
library(dplyr) # for data manipulation
if (!require(stringr)) install.packages('stringr')
library(stringr) # for data manipulation
if (!require(caret)) install.packages('caret')
library(caret) # for sampling
if (!require(caTools)) install.packages('caTools')
library(caTools) # for train/test split
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2) # for data visualization
if (!require(corrplot)) install.packages('corrplot')
library(corrplot) # for correlations
if (!require(Rtsne)) install.packages('Rtsne')
library(Rtsne) # for tsne plotting
if (!require(DMwR)) install.packages('DMwR')
library(DMwR) # for smote implementation
if (!require(ROSE)) install.packages('ROSE')
library(ROSE)# for ROSE sampling
if (!require(rpart)) install.packages('rpart')
library(rpart)# for decision tree model
if (!require(Rborist)) install.packages('Rborist')
library(Rborist)# for random forest model
if (!require(xgboost)) install.packages('xgboost')
library(xgboost) # for xgboost model


#Load data

df<- read.csv("creditcard.csv")

#Basic exploration
dim(df)
str(df)
head(df)
summary(df)

#Check for null values
colSums(is.na(df))

#Check class imbalance
table(df$Class)
prop.table(table(df$Class))


#Visualize class imbalance

common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(data = df, aes(x = factor(Class), 
                      y = prop.table(stat(count)), fill = factor(Class),
                      label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_x_discrete(labels = c("no fraud", "fraud"))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = 'Class', y = 'Percentage') +
  ggtitle("Distribution of class labels") +
  common_theme

#Data Visualization

#Distribution of 'Time' variable by Class

df %>%
  ggplot(aes(x = Time, fill = factor(Class))) + geom_histogram(bins = 100)+
  labs(x = 'Time in seconds since first transaction', y = 'No. of transactions') +
  ggtitle('Distribution of time of transaction by class') +
  facet_grid(Class ~ ., scales = 'free_y') + common_theme


#Distribution of Amount by Class

ggplot(df, aes(x = factor(Class), y = Amount)) + geom_boxplot() + labs(x = 'Class', y = 'Amount') +
ggtitle("Distribution of transaction amount by class") + common_theme

#Correlation of Anonymised Variables and Amount

correlations <- cor(df[,-1], method = 'pearson')
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")

#Visualization of transactions using t-SNE

# Use 10% of data to compute t-SNE
tsne_subset <- 1:as.integer(0.1*nrow(df))
tsne <- Rtsne(df[tsne,-c(1, 31)], perplexity = 20, theta = 0.5, pca = F, verbose = T, max_iter = 500, check_duplicates = F)

classes <- as.factor(df$Class)
tsne_mat <- as.data.frame(tsne$Y)
ggplot(tsne_mat, aes(x = V1, y = V2)) + geom_point(aes(color = classes)) + theme_minimal() + common_theme + 
ggtitle("t-SNE visualisation of transactions") + scale_color_manual(values = c("#E69F00", "#56B4E9"))

#Data Preparation

#Remove 'Time' variable
df <- df[,-1]

#Change 'Class' variable to factor
df$Class <- as.factor(df$Class)
levels(df$Class) <- c("Not_Fraud", "Fraud")

#Scale numeric variables

df[,-30] <- scale(df[,-30])

head(df)

#Split into train and test sets

set.seed(123)
split <- sample.split(df$Class, SplitRatio = 0.7)
train <-  subset(df, split == TRUE)
test <- subset(df, split == FALSE)

#Choose sampling technique

table(train$Class)


set.seed(9560)
down_train <- downSample(x = train[, -ncol(train)],
                         y = train$Class)
table(down_train$Class)  


set.seed(9560)
up_train <- upSample(x = train[, -ncol(train)],
                     y = train$Class)
table(up_train$Class)  


set.seed(9560)
smote_train <- SMOTE(Class ~ ., data  = train)

table(smote_train$Class)  


set.seed(9560)
rose_train <- ROSE(Class ~ ., data  = train)$data 

table(rose_train$Class) 


#CART Model Performance on imbalanced data
set.seed(5627)

orig_fit <- rpart(Class ~ ., data = train)

#Evaluate model performance on test set
pred_orig <- predict(orig_fit, newdata = test, method = "class")

roc.curve(test$Class, pred_orig[,2], plotit = TRUE)

set.seed(5627)
# Build down-sampled model


down_fit <- rpart(Class ~ ., data = down_train)


set.seed(5627)
# Build up-sampled model


up_fit <- rpart(Class ~ ., data = up_train)


set.seed(5627)
# Build smote model


smote_fit <- rpart(Class ~ ., data = smote_train)

set.seed(5627)
# Build rose model


rose_fit <- rpart(Class ~ ., data = rose_train)

pred_down <- predict(down_fit, newdata = test)

print('Fitting downsampled model to test data')
roc.curve(test$Class, pred_down[,2], plotit = FALSE)

pred_up <- predict(up_fit, newdata = test)

print('Fitting upsampled model to test data')
roc.curve(test$Class, pred_up[,2], plotit = FALSE)

pred_smote <- predict(smote_fit, newdata = test)

print('Fitting smote model to test data')
roc.curve(test$Class, pred_smote[,2], plotit = FALSE)

pred_rose <- predict(rose_fit, newdata = test)

print('Fitting rose model to test data')
roc.curve(test$Class, pred_rose[,2], plotit = FALSE)


### GLM Fit



glm_fit <- glm(Class ~ ., data = up_train, family = 'binomial')

pred_glm <- predict(glm_fit, newdata = test, type = 'response')

roc.curve(test$Class, pred_glm, plotit = TRUE)



### RF Fit (we use the Rborist package)


x = up_train[, -30]
y = up_train[,30]

rf_fit <- Rborist(x, y, ntree = 1000, minNode = 20, maxLeaf = 13)


rf_pred <- predict(rf_fit, test[,-30], ctgCensus = "prob")
prob <- rf_pred$prob

roc.curve(test$Class, prob[,2], plotit = TRUE)



### XGB Fit


#COnvert class labels from factor to numeric

labels <- up_train$Class

y <- recode(labels, 'Not_Fraud' = 0, "Fraud" = 1)


xgb <- xgboost(data = data.matrix(up_train[,-30]), 
               label = y,
               eta = 0.1,
               gamma = 0.1,
               max_depth = 10, 
               nrounds = 300, 
               objective = "binary:logistic",
               colsample_bytree = 0.6,
               verbose = 0,
               nthread = 7,
               seed = 42
)


xgb_pred <- predict(xgb, data.matrix(test[,-30]))

roc.curve(test$Class, xgb_pred, plotit = TRUE)


names <- dimnames(data.matrix(up_train[,-30]))[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])










