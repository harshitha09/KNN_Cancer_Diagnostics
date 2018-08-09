---
title: "K-NN Cancer_Diagnosis"
author: "HARSHITHA MEKALA"
date: "9 August 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#loading packages
```{r}
set.seed(123)

require(dplyr)
require(ggplot2)
require(class)
require(gmodels)
```

# loading data
```{r}

cancer_data <- read.csv("E://Machine Learning/MechineLearning Text book codes/breastcancer.csv",
                        header =  F,
                        stringsAsFactors = F)

names(cancer_data)[1:12] = c("id", "diagnosis", "radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean",
                       "compactness_mean", "concavity_mean", "concavepoints_mean", "symmetry_mean", "fractaldimensions_mean")

names(cancer_data)[13:22] = c("radius_SE", "texture_SE", "perimeter_SE", "area_SE", "smoothness_SE",
                             "compactness_SE", "concavity_SE", "concavepoints_SE", "symmetry_SE", "fractaldimensions_SE")

names(cancer_data)[23:32] = c("radius_worst", "texture_worst", "perimeter_worst", "area_worst", "smoothness_worst",
                              "compactness_worst", "concavity_worst", "concavepoints_worst", "symmetry_worst", "fractaldimensions_worst")

View(cancer_data)
class(cancer_data)
str(cancer_data)

```

# removing id column as it does not contribute to the prediction
```{r}
cancer_data <- cancer_data[-1]
```

# diagnosis indicates Malignant(M) and Benign(B). from this the outcome of our interest can be predicted
```{r}
table(cancer_data$diagnosis)
```

# giving the full-form of B and M
```{r}
cancer_data$diagnosis <- factor(cancer_data$diagnosis, levels = c("B", "M"), labels = c("Benign", "Maligant"))

round(prop.table(table(cancer_data$diagnosis))*100, digits = 1)

summary(cancer_data[c("radius_mean", "area_mean","smoothness_mean")])

"here we find smoothess value ranges from 0.005 to 0.16 and the values of area ranges from 143.5 to 2501. Hence the values of area has greater impact while calculating the distance. so we nees to normalize the data to overcome the impact"
```

# normalising the data
```{r}
normalise <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

cancer_data_norm <- as.data.frame(lapply(cancer_data[2:31], normalise))
summary(cancer_data_norm$area_mean)
```

# data preparation - creating training and testing data
```{r}
cancer_data_train <- cancer_data_norm[1:469,]
cancer_data_test <- cancer_data_norm[470:569,]
nrow(cancer_data_train)
nrow(cancer_data_test)

```

# we take the diagnosis colun same as training and testing so that we can use this to evaluate our classifier
```{r}
cancer_train_labels <- cancer_data[1:469,1]
cancer_test_labels <- cancer_data[470:569,1]
nrow(cancer_train_labels)
nrow(cancer_test_labels)

# applying knn() with k = 21 
sqrt(nrow(cancer_data_train))

cancer_data_pred <- knn(train = cancer_data_train, test = cancer_data_test, cl = cancer_train_labels, k = 21)

# evaluating the model performance
CrossTable(x = cancer_test_labels, y = cancer_data_pred, prop.chisq = FALSE)

# here our model predicts 2% values as false negative which is an error. 

```

# improving model preformance
```{r}
"lets improve the model performance by 1.rescaling the data using Z-transformayion and 2. we will try with different values of K"

cancer_data_z <- as.data.frame(scale(cancer_data[-1]))
summary(cancer_data_z$area_mean)

# now dividing the Z transformed data into test and train and again check the evaluation 
cancer_data_train_z <- cancer_data_z[1:469,]
cancer_data_test_z <- cancer_data_z[470:569,]

cancer_data_train_zlabels <- cancer_data[1:469,1]
cancer_data_test_zlabels <- cancer_data[470:569,1]

cancer_data_pred_z <- knn(train = cancer_data_train_z, test = cancer_data_test_z, cl = cancer_data_train_zlabels, k = 21)

CrossTable(x = cancer_data_test_zlabels, y = cancer_data_pred_z, prop.chisq = F)


## Here the result is same.
```

