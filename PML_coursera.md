Prediction of weight lifting style using random forest 
========================================================

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement ??? a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. 

## data
The training data for this project can be found here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

## test data
The test data for this project can be found here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

## reading in the data


```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(randomForest)
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
setwd("~/Desktop/DataScience_part3/Assignments/Machine Learning")
data_name1 <- "Practical Machine Learning _ Coursera.csv"
data_name2 <- "pml-testing.csv"
rawdata1 <- read.csv(data_name1, na.strings = c("", "NA"))
predict_rawdata <- read.csv(data_name2)
```


## data processing

```r
# turn each blank or na entry into NA
checker = is.na(rawdata1)
checker2 <- colSums(checker)

# remove those entries that have many NA's
checker3 <- checker2 > 15000
# cleaning data
cleandata1 <- rawdata1[!checker3]
# no more missing entries
sum(is.na(cleandata1))
```

```
## [1] 0
```

```r
# find out how many types of each classes are there
summary(cleandata1$classe)
```

```
##    A    B    C    D    E 
## 5580 3797 3422 3216 3607
```

```r

# apply the same to the predict_rawdata
predict_cleandata = predict_rawdata[!checker3]

# remove the unneccessary rows
cleandata2 <- cleandata1[, c(7:60)]
predict_cleandata2 <- predict_cleandata[, c(7:60)]
# change to factor variable
cleandata2$classe <- factor(cleandata2$classe)
```


## create a data partition

```r
data_partition <- createDataPartition(y = cleandata2$classe, p = 0.75, list = FALSE)
# trainInds <- sample(nrow(cleandata2), 3000)
# trainingdata<-cleandata2[trainInds,]
trainingdata <- cleandata2[data_partition, ]
testdata <- cleandata2[-data_partition, ]
```


## Creating Model 

```r
model <- train(classe ~ ., data = trainingdata, method = "rf", prox = TRUE, 
    trControl = trainControl(method = "cv", number = 4))
```


## in sample accuracy

```r
training_pred <- predict(model, trainingdata)
confusionMatrix(training_pred, trainingdata$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 4184    2    0    0    0
##          B    0 2845    0    0    0
##          C    0    0 2567    1    0
##          D    0    1    0 2411    1
##          E    1    0    0    0 2705
## 
## Overall Statistics
##                                     
##                Accuracy : 1         
##                  95% CI : (0.999, 1)
##     No Information Rate : 0.284     
##     P-Value [Acc > NIR] : <2e-16    
##                                     
##                   Kappa : 0.999     
##  Mcnemar's Test P-Value : NA        
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             1.000    0.999    1.000    1.000    1.000
## Specificity             1.000    1.000    1.000    1.000    1.000
## Pos Pred Value          1.000    1.000    1.000    0.999    1.000
## Neg Pred Value          1.000    1.000    1.000    1.000    1.000
## Prevalence              0.284    0.194    0.174    0.164    0.184
## Detection Rate          0.284    0.193    0.174    0.164    0.184
## Detection Prevalence    0.284    0.193    0.174    0.164    0.184
## Balanced Accuracy       1.000    0.999    1.000    1.000    1.000
```

so we can see that it has accuracy 1 

## out of sample 
this is estimated by using data parition with 25%.

```r
# outsampleaccuracy
training_pred <- predict(model, testdata)
confusionMatrix(training_pred, testdata$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1395    0    0    0    0
##          B    0  949    2    0    0
##          C    0    0  853    1    0
##          D    0    0    0  803    0
##          E    0    0    0    0  901
## 
## Overall Statistics
##                                     
##                Accuracy : 0.999     
##                  95% CI : (0.998, 1)
##     No Information Rate : 0.284     
##     P-Value [Acc > NIR] : <2e-16    
##                                     
##                   Kappa : 0.999     
##  Mcnemar's Test P-Value : NA        
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             1.000    1.000    0.998    0.999    1.000
## Specificity             1.000    0.999    1.000    1.000    1.000
## Pos Pred Value          1.000    0.998    0.999    1.000    1.000
## Neg Pred Value          1.000    1.000    1.000    1.000    1.000
## Prevalence              0.284    0.194    0.174    0.164    0.184
## Detection Rate          0.284    0.194    0.174    0.164    0.184
## Detection Prevalence    0.284    0.194    0.174    0.164    0.184
## Balanced Accuracy       1.000    1.000    0.999    0.999    1.000
```


We can see that the out of sample accuracy is also close to 1
## assignment problem

```r
answers <- predict(model, predict_cleandata2)
answers <- as.character(answers)
answers
```

```
##  [1] "B" "A" "B" "A" "A" "E" "D" "B" "A" "A" "B" "C" "B" "A" "E" "E" "A"
## [18] "B" "B" "B"
```

```r


pml_write_files = function(x) {
    n = length(x)
    for (i in 1:n) {
        filename = paste0("problem_id_", i, ".txt")
        write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
            col.names = FALSE)
    }
}

pml_write_files(answers)
```


we are able to achieve 100% test prediction with this model. 

## references

[wle]: Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.

[caret]: Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, Chris Keefer, Allan Engelhardt, Tony Cooper, Zachary Mayer and the R Core Team. Caret package.

