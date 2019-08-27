---
layout: page
title: Machine Learning
---

My language of choice for supervised and unsupervised learning is R; however, I run models in Spark (using Scala) for larger projects. Currently, I am playing around with deep learning models in Python.

## Projects

The following scripts can be found on my GitHub [account](https://github.com/jbeustice/machineLearning).

#### R
* K-nearest neighbors trained by k-fold cross-validation using housing data (from Zillow)
* Discriminant analysis (both LDA and QDA) using housing data (from Zillow)
* Random forests (and boosting) using FIFA 2019 player data (from Kaggle) ... also can be viewed below.
* K-means clustering, support vector machines, and regression (e.g., ridge, lasso, etc.) using housing data (from Zillow)
* Ensemble methods (e.g., stacking) using Seattle rainfall data (from Kaggle)

#### Spark (Scala)
* Linear regression using housing data (from Zillow)
* Binary classification (i.e., Logistic regression) trained by k-fold cross-validation using misquito disease data (from Kaggle) ... also can be viewed below.
* K-means clustering using housing data (from Zillow)
* Principle Component Analysis using FIFA 2019 player data (from Kaggle)

#### Python
* Bag-of-words using Naive Bayes and support vector machines to classifiy IMDb movie reviews (compiled by [Andrew Mass](http://ai.stanford.edu/~amaas/data/sentiment/))
* Artificial neural network using market segmentation data (from Kaggle)
* Convolutional neural network using synthesized digits (from Kaggle)

## Code snippets


The following R code predicts the overall score assigned to each player in the FIFA 2019 database via random forest regression both with and without boosting.
``` r
## This program predicts the overall player score of soccor players in
## the FIFA 2019 database using random forests and boosting.

setwd("/Users/Bradley/Dropbox/...")

## load required libraries
library(randomForest)
library(gbm)

# load and clean data
f_dataAll <- read.csv("fifa_data.csv",header = T)
str(f_dataAll)
f_data <- f_dataAll[,c(8,4,15,27,28,55:(ncol(f_dataAll)-1))]
f_data <- na.omit(f_data)
f_data$Weight <- as.character(f_data$Weight)
f_data$Weight <- as.numeric(substr(f_data$Weight,1,nchar(f_data$Weight)-3))
f_data$Height <- as.character(f_data$Height)
f_data$Height <- (as.numeric(substr(f_data$Height,1,1)) * 12) +
                  as.numeric(substr(f_data$Height,3,5))
f_data$Preferred.Foot <- droplevels(f_data$Preferred.Foot)
str(f_data)
rows <- nrow(f_data)

# split data --> training and test
split <- sample.int(n=rows,size=floor(0.8*rows),replace=F)
training <- f_data[split, ]
test  <- f_data[-split, ]

# plot subset of data to see general correlations
look1 <- test[,1:10]
look2 <- test[,c(1,11:20)]
look3 <- test[,c(1,21:ncol(test))]
pairs(look1)
pairs(look2)
pairs(look3)

############
## Random forests
############

# determine number of trees
rf <- randomForest(Overall~.,data=training)
rf
plot(rf)
which.min(rf$mse)

# choose mtry: # of vars to consider each split
oob.err=double(20)
for(i in 1:20){
  fit=randomForest(Overall~.,data=training,mtry=i,ntree=400)
  oob.err[i]=fit$mse[400]
  cat(i," ")
}

# plot mtry data
plot(1:i,oob.err,pch=19,col="red",type="b",ylab="Mean Squared Error",
     xlab="mtry values 1 to 20",main="Out-of-bag Error")
which.min(oob.err)

# validate on test data with tunned parameters
rf_test <- randomForest(Overall~.,data=training,xtest=test[,-1],
                        ytest=test$Overall,mtry=7,ntree=500,importance=T)
rf_test

# view variable importance
varImpPlot(rf_test)


############
## Boosting
############

# create grid to tune parameters
hyper_grid <- expand.grid(shrinkage=c(.001,.01,.1),interaction.depth=c(1,3,5),
                          n.minobsinnode=c(10,20))

# grid search 
for(i in 1:nrow(hyper_grid)){
  
  # train model
  boost.tune <- gbm(Overall~.,data=training,distribution="gaussian",n.trees=10000,
                    interaction.depth=hyper_grid$interaction.depth[i],
                    shrinkage=hyper_grid$shrinkage[i],
                    n.minobsinnode=hyper_grid$n.minobsinnode[i],train.fraction=.75)
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(boost.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(boost.tune$valid.error))
  cat(i," ")
}

# view top models
hyper_grid[order(hyper_grid$min_RMSE),]

# build best model and validate on test data
boost <- gbm(Overall~.,data=training,distribution="gaussian",n.trees=4600,
             shrinkage=0.1,interaction.depth=5)
summary(boost)

# plot RMSE results
num.trees=seq(from=100,to=4600,by=100)
test.pred <- predict(boost,newdata=test,n.trees=num.trees)
test.err <- with(test,apply(sqrt((test.pred-Overall)^2),2,mean))
plot(num.trees,test.err,pch=19,col="red",type="b",ylab="Root Mean Squared Error",
     xlab="# Trees",main="Boosting Test Error")

# compare best model to test data
which.min(test.err)
min(test.err)

# best model RMSE for test data
test.err[length(num.trees)]
```
-----
The following Spark (Scala) code predicts the presence of West Nile virus among misquito populations in Chicago via logistic regression (classification). The data is trained by k-fold cross-validation.
``` scala
/**
 * Logistic regression with k-fold cross-validation using Spark (Scala)
 */

import org.apache.spark.sql.SparkSession

// see less warnings
import org.apache.log4j._
Logger.getLogger("org").setLevel(Level.ERROR)

// start Session
val spark = SparkSession.builder().getOrCreate()

// load West Nile data
val wn_data = (spark.read.option("header","true")
                       .option("inferSchema","true")
                       .csv("westnile_data.csv"))

// see data
wn_data.printSchema()
wn_data.head(1)

// create categorical variables
import org.apache.spark.ml.feature.{StringIndexer,OneHotEncoderEstimator}

val transform_data = (new StringIndexer()
                    .setInputCol("RESULT")
                    .setOutputCol("resultIndex")
                    .fit(wn_data)
                    .transform(wn_data))

val ready_dataAll = transform_data.select(transform_data("resultIndex").as("label"),
                             $"TRAP_TYPE",$"SPECIES",$"WEEK",$"NUMBER OF MOSQUITOES")

val ready_data = ready_dataAll.na.drop()

val trapIndexer = (new StringIndexer().setInputCol("TRAP_TYPE")
                                      .setOutputCol("traptypeIndex"))
val speciesIndexer = (new StringIndexer().setInputCol("SPECIES")
                                         .setOutputCol("speciesIndex"))

val encoder = (new OneHotEncoderEstimator()
              .setInputCols(Array("traptypeIndex","speciesIndex"))
              .setOutputCols(Array("traptypeVec","speciesVec")))

// joins multiple feature columns into a single column of an array of feature values
// (label,features)
import org.apache.spark.ml.feature.VectorAssembler
import org.apache.spark.ml.linalg.Vectors

// dependent var must be titled as "label"; the independent vars as "features"
val assemble = (new VectorAssembler()
               .setInputCols(Array("traptypeIndex","speciesIndex","NUMBER OF MOSQUITOES"))
               .setOutputCol("features"))

// split data
val Array(training, test) = ready_data.randomSplit(Array(0.75, 0.25))

// run k-fold cv for logistic regression
import org.apache.spark.ml.evaluation.BinaryClassificationEvaluator
import org.apache.spark.ml.classification.LogisticRegression
import org.apache.spark.ml.tuning.{ParamGridBuilder, CrossValidator}
import org.apache.spark.ml.Pipeline

val lr = new LogisticRegression().setMaxIter(10)

val paramGrid = new ParamGridBuilder().addGrid(lr.regParam,Array(0.1, 0.01)).build()

// cv requires an Estimator, a set of Estimator ParamMaps, and an Evaluator
// 5-fold cv
val cv = (new CrossValidator()
         .setEstimator(lr)
         .setEvaluator(new BinaryClassificationEvaluator)
         .setEstimatorParamMaps(paramGrid)
         .setNumFolds(5))

val pipeline = new Pipeline().setStages(Array(trapIndexer,speciesIndexer,
                                              encoder,assemble,cv))

// run cv and choose the best set of parameters.
val cvModel = pipeline.fit(training)

// evaluation --> need to convert to RDD (from df)
import org.apache.spark.mllib.evaluation.MulticlassMetrics

val predictionAndLabels = cvModel.transform(test)
                                 .select($"prediction",$"label")
                                 .as[(Double, Double)]
                                 .rdd

val outcome = new MulticlassMetrics(predictionAndLabels)

// confusion matrix
println("Confusion matrix:")
println(outcome.confusionMatrix)
```
-----
The following Python code develops multiple bag-of-words models to classify IMDb movie reviews as positive or negative using Naive Bayes and support vector machines.
``` python

## This program develops multiple bag-of-words models to classify IMDb movie
## reviews as positive or negative. A sample set of 50k reviews are used to
## train and validate the models (NB and SVM) using both word occurrence and
## TF-IDF algorithms. 

# import library
import os

# set working directory
os.chdir('/Users/Bradley/Dropbox/...')
cwd = os.getcwd()


##########
## Read in the data and split into training/test
##########

# import libraries
import pandas as pd
from sklearn.model_selection import train_test_split

# function --> reads in individual text files in a folder and attaches sentiment
def f_ReadIn(folder,sentiment):
    result = []
    os.chdir(os.path.join(cwd,folder))
    f_cwd = os.getcwd()
    for f in os.listdir(f_cwd):
        if os.path.isfile(f) and f.endswith(".txt"):
           with open(os.path.join(f_cwd,f),'r') as file:
                content = file.read()
           result.append([content,sentiment])
    return pd.DataFrame(result)

# call function f_ReadIn() and combines into one df
neg = f_ReadIn('negative',0)
pos = f_ReadIn('positive',1)
df = pd.concat([neg,pos],ignore_index=True)
df.columns = ['review','sentiment']
df.head()

# free up memory
del neg, pos

# reset working directory
os.chdir(cwd)

# split data into train and test sets --> 70/30
X_train, X_test, y_train, y_test = train_test_split(df['review'],df['sentiment'],test_size = 0.3)


##########
## Create vector of features using the count and TF-IDF algorithms
##########

# import libraries
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer

# count occurrence
c_vectorizer = CountVectorizer(stop_words='english',max_features=1000)
c_train = c_vectorizer.fit_transform(X_train)
c_test= c_vectorizer.transform(X_test)

# TF-IDF
tf_vectorizer = TfidfVectorizer(stop_words='english',max_features=1000)
tf_train = tf_vectorizer.fit_transform(X_train)
tf_test = tf_vectorizer.transform(X_test)


##########
## Build and tune models (Naive Bayes and SVM)
##########

# import libraries
from sklearn.naive_bayes import MultinomialNB
from sklearn import svm
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import accuracy_score

# function --> tune NB
def f_tune_nb(X,y):
    alphas = [0.1,0.5,1]
    param_grid = {'alpha':alphas}
    grid_search = GridSearchCV(MultinomialNB(),param_grid,cv=5)
    grid_search.fit(X,y)
    return grid_search.best_params_

# function --> tune SVM (not feasible on a local computer)
def f_tune_svm(X,y):
    costs = [0.1,1,10]
    gammas = [0.1,1,4]
    param_grid = {'C':costs,'gamma':gammas}
    grid_search = GridSearchCV(svm.SVC(),param_grid,cv=5)
    grid_search.fit(X,y)
    return grid_search.best_params_

# print tuned parameters
print('')
print('c_nb --> %a' % f_tune_nb(c_train,y_train))
print('tf_nb --> %a' % f_tune_nb(tf_train,y_train))
print('c_svm --> %a' % f_tune_svm(c_train,y_train))
print('tf_svm --> %a' % f_tune_svm(tf_train,y_train))


##########
## Run tuned models (Naive Bayes and SVM) and compare results
##########

# NB count with tuned parameter
c_nb = MultinomialNB(alpha=0.1).fit(c_train,y_train)
c_nb_pred = c_nb.predict(c_test)

# NB TF-IDF with tuned parameter
tf_nb = MultinomialNB(alpha=0.5).fit(tf_train,y_train)
tf_nb_pred = tf_nb.predict(tf_test)

# SVM count with tuned parameters
c_svm = svm.SVC(C=1,gamma=0.1)
c_svm.fit(c_train,y_train)  
c_svm_pred = c_svm.predict(c_test)

# SVM TF-IDF with tuned parameters
tf_svm = svm.SVC(C=1,gamma=0.1)
tf_svm.fit(tf_train,y_train)  
tf_svm_pred = tf_svm.predict(tf_test)

# print model accuracy
print('')
print('Using Count Vectorizer and Naive Bayes')
print('Accuracy: %0.4f' % (accuracy_score(y_test,c_nb_pred)))
print('')
print('Using TF-IDF Vectorizer and Naive Bayes')
print('Accuracy: %0.4f' % (accuracy_score(y_test,tf_nb_pred)))
print('')
print('Using Count Vectorizer and SVM')
print('Accuracy: %0.4f' % (accuracy_score(y_test,c_svm_pred)))
print('')
print('Using TF-IDF Vectorizer and SVM')
print('Accuracy: %0.4f' % (accuracy_score(y_test,tf_svm_pred)))
```
