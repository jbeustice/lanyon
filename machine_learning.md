---
layout: page
title: Machine Learning
---

My language of choice for supervised and unsupervised learning is R; however, I run models in Spark (using Scala) for larger projects. Currently, I am familiarizing myself with implementing machine learning models in Python.

## Projects

The following scripts can be found on my GitHub [account](https://github.com/jbeustice/machineLearning).

#### R
* K-nearest neighbors trained with k-fold cross-validation using housing data (from Zillow)
* Discriminant analysis (both LDA and QDA) using housing data (from Zillow)
* Random forests (and boosting) using FIFA 2019 player data (from Kaggle) ... also can be viewed below.
* K-means clustering, support vector machines, and regression (e.g., ridge, lasso, etc.) using housing data (from Zillow)

#### Spark (Scala)
* Linear regression using housing data (from Zillow)
* Binary classification (i.e., Logistic regression) trained with k-fold cross-validation using misquito disease data (from Kaggle) ... also can be viewed below.
* K-means clustering using housing data (from Zillow)
* Priciple Components Analysis using FIFA 2019 player data (from Kaggle)

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
f_data$Height <- (as.numeric(substr(f_data$Height,1,1))*12)+as.numeric(substr(f_data$Height,3,5))
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

val paramGrid = new ParamGridBuilder().addGrid(lr.regParam, Array(0.1, 0.01)).build()

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
