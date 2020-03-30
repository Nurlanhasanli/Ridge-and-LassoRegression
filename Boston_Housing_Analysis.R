# Load some packages for data manipulation: 
library(highcharter)
library(readr)
library(tidyverse)
library(magrittr)
library(caret)

boston <- read.csv('C:/Users/HP/Desktop/DSA Bootcamp/r studio/week 8/boston_data.csv')
boston$chas<-as.factor(boston$chas)
summary(boston)
glimpse(boston)
colnames(boston)
hchart(cor(boston %>% 
             mutate_if(is.character,as.factor) %>% 
             mutate_if(is.factor,as.numeric)) %>% 
         round(.,2),label = T)

# Use data: 
set.seed(1)
id <- createDataPartition(y = boston$chas, p = 0.75, list = FALSE)
df_train <- boston [id, ]
df_test <- boston[-id, ]


# Activate h2o package for using: 
library(h2o)
h2o.init(nthreads = 16, max_mem_size = "16g")

# Convert to h2o Frame and identify inputs and output: 
test <- as.h2o(df_test)
train <- as.h2o(df_train)

response <- "chas"
predictors <- setdiff(names(train), response)


#===================================
#   Pure, Lasso and Ridge Logistic
#===================================

# Train Logistic Model: 
pure_logistic <- h2o.glm(family= "binomial", 
                         x = predictors, 
                         y = response, 
                         lambda = 0, 
                         training_frame = train)


# Function Shows the coefficients table: 

show_coeffs <- function(model_selected) {
  model_selected@model$coefficients_table %>% 
    as.data.frame() %>% 
    mutate_if(is.numeric, function(x) {round(x, 3)}) %>% 
    filter(coefficients != 0) %>% 
    knitr::kable()
}


# Use this function: 
show_coeffs(pure_logistic)

# Lasso Logistic Model: 

lasso_logistic <- h2o.glm(family = "binomial", 
                          alpha = 1,
                          seed = 1988, 
                          x = predictors, 
                          y = response, 
                          training_frame = train)

show_coeffs(lasso_logistic)

# Ridge Logistic Model: 
ridge_logistic <- h2o.glm(family = "binomial", 
                          alpha = 0,
                          seed = 1988, 
                          x = predictors, 
                          y = response, 
                          training_frame = train)

show_coeffs(ridge_logistic)

# Function shows model performance on test data: 

my_cm <- function(model_selected) {
  pred <- h2o.predict(model_selected, test) %>% 
    as.data.frame() %>% 
    pull(1)
  confusionMatrix(pred, df_test$chas, positive = "1") %>% 
    return()
}

lapply(list(pure_logistic, lasso_logistic, ridge_logistic), my_cm)
