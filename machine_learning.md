---
layout: page
title: Machine Learning
---

My language of choice for supervised and unsupervised learning is R; however, I run models in Spark (using Scala) for larger projects. Currently, I am familiarizing myself with implementing machine learning models in Python.

## Projects

The following projects can be found on my GitHub [account](https://github.com/jbeustice/machineLearning).

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

The following Spark (Scala) code reshapes and merges multiple files into one longitudinal dataset.
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

val trapIndexer = new StringIndexer().setInputCol("TRAP_TYPE").setOutputCol("traptypeIndex")
val speciesIndexer = new StringIndexer().setInputCol("SPECIES").setOutputCol("speciesIndex")

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

val pipeline = new Pipeline().setStages(Array(trapIndexer,speciesIndexer,encoder,assemble,cv))

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

The following .do file [Stata] merges datasets together to create a contiguity weighting matrix used in spatial analysis.
``` stata
// This script creates a contiguity weighting matrix by
// merging the original dataset with the US Census TIGER
// shapefile

import delimited "/Users/Bradley/Dropbox/..."

//  unzip TIGER file and generate fips pre merge
copy ~/Downloads/tl_2016_us_county.zip .
unzipfile tl_2016_us_county.zip
spshape2dta tl_2016_us_county
use tl_2016_us_county, clear
describe
list in 1/2
generate long fips = real(STATEFP + COUNTYFP)
bysort fips: assert _N==1
assert fips != .
spset fips, modify replace
list in 1/2
save, replace

// format original data pre merge
format year %tq
assert fips!=.
assert year!=.
bysort fips year: assert _N==1
xtset, clear
xtset fips year
spbalance
spset fips
save "/Users/Bradley/Dropbox/..."

// merge orignial data with TIGER by fips
use dataA, clear
describe
merge m:1 fips using tl_2016_us_county
keep if _merge==3
drop _merge
drop STATEFP COUNTYFP COUNTYNS GEOID NAME NAMELSAD LSAD CLASSFP
drop MTFCC CSAFP CBSAFP METDIVFP FUNCSTAT ALAND AWATER INTPTLAT INTPTLON
save "/Users/Bradley/Dropbox/..."

// create contiguity matrix
spmatrix create contiguity W if year == 2007
```
