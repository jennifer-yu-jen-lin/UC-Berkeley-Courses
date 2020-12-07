## 0
library(datasets)
library(boot)
library(dplyr)
library(ggplot2)

## 1
#### Load the iris dataset, and find the mean and SEM of the Sepal.Length, Sepal.Width, Petal.Length, Petal.Width.
data(iris)
variableList <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
for(i in 1:4){
  vec <- iris[ , variableList[i]]
  vec_mean <- mean(vec)
  vec_SEM <- sd(vec)/sqrt(length(vec))
  print(paste0(variableList[i], ": mean=", round(vec_mean,4), ", SEM=", round(vec_SEM,4)))
}

## 2
#### Calculate the variance of Sepal.Length, Sepal.Width, Petal.Length, Petal.Width.
for(i in 1:4){
  vec <- iris[ , variableList[i]]
  vec_variance <- var(vec)
  print(paste0(variableList[i], ": variance=", round(vec_variance,4)))
}

## 3
#### Using the values calculated above, obtain the 95% confidence interval of the mean for Sepal.Length, Sepal.Width, Petal.Length, Petal.Width. 
#### What determines the range of the confidence interval?
for(i in 1:4){
  vec <- iris[ , variableList[i]]
  vec_SEM <- sd(vec)/sqrt(length(vec))
  # 95% CI using normal distribution
  CI_n_lower <- mean(vec)-qnorm(0.975)*vec_SEM
  CI_n_upper <- mean(vec)+qnorm(0.975)*vec_SEM
  # 95% CI using t distribution
  CI_t_lower <- mean(vec)-qt(0.975,df=length(vec)-1)*vec_SEM
  CI_t_upper <- mean(vec)+qt(0.975,df=length(vec)-1)*vec_SEM  
  print(paste0(variableList[i], ": 95% CI (using normal distribution)=", round(CI_n_lower,4), "~",round(CI_n_upper,4), ", 95% CI (using t distribution)=", round(CI_t_lower,4), "~",round(CI_t_upper,4)))
}



## 4
#### Obtain 500 bootstrap samples out of Sepal.Length, Sepal.Width, Petal.Length, Petal.Width:
#### (a) Create a histogram of the bootstrap sample means 
#### (b) Find the 95% bootstrap confidence intervals
#### (c) Write a function for your own estimator (something other than the mean - could be for instance the median or skew) and compute the 95% confidence interval for it.
for(i in 1:4){
  vec <- iris[ , variableList[i]]
  ### BOOTSTRAP #############################################
  vec_bt_mean <- c()
  vec_bt_median <- c()
  for (j in 1:500){
    vec_bt_mean <- c(vec_bt_mean, mean(sample(vec,length(vec),replace=T)))
    vec_bt_median <- c(vec_bt_median, mean(sample(vec,length(vec),replace=T)))
  } 
  ### HISTOGRAM #############################################
  # mean
  p_CI_bt_mean <- ggplot() + aes(vec_bt_mean) + geom_histogram(colour="white") +
    labs(title=paste0("Bootstrap Sample Mean of ", variableList[i])) +
    xlab("bootstrap sample mean") +
    theme(plot.title = element_text(hjust = 0.5))
  # median
  p_CI_bt_median <- ggplot() + aes(vec_bt_median) + geom_histogram(colour="white") +
    labs(title=paste0("Bootstrap Sample Median of ", variableList[i])) +
    xlab("bootstrap sample median") +
    theme(plot.title = element_text(hjust = 0.5))
  ### 95% CI ################################################
  # mean
  CI_bt_mean_lower <- quantile(vec_bt_mean, 0.025)
  CI_bt_mean_upper <- quantile(vec_bt_mean, 0.975)
  # median
  CI_bt_median_lower <- quantile(vec_bt_median, 0.025)
  CI_bt_median_upper <- quantile(vec_bt_median, 0.975)
  ### PRINT RESULTS #########################################
  print(p_CI_bt_mean)
  print(paste0(variableList[i], ": 95% CI of Mean = ", round(CI_bt_mean_lower,4), "~",round(CI_bt_mean_upper,4)))
  print(p_CI_bt_median)
  print(paste0(variableList[i], ": 95% CI of Median = ", round(CI_bt_median_lower,4), "~",round(CI_bt_median_upper,4)))
}

## 5
#### Perform a t-test between all combinations of the four datasets (that would be a total of 4 choose 2 = 6 combinations).
#### See if there is any statistically significant relationship between any of the four parameters defining the iris.
# create all combinations
variableList_combn <- combn(variableList,2)
print(variableList_combn)
# t-test
for(i in 1:length(variableList_combn[1,])){
  x <- iris[ , variableList_combn[1,i] ]
  y <- iris[ , variableList_combn[2,i] ]
  tTestResult <- t.test(x,y)
  print(paste0("T-test results of ", variableList_combn[1,i], " and ", variableList_combn[2,i]))
  print(tTestResult)
  if(tTestResult$p.value <= 0.05){
    print(paste0("P value is smaller than (or equal to) 0.05 and thus we can reject the null hypothesis.  Consequently, the means of ", variableList_combn[1,i], " and ", variableList_combn[2,i], " are significantly differnet from each other."))
  }else{
    print(paste0("P value is larger than 0.05 and thus we can NOT reject the null hypothesis.  Consequently, the means of ", variableList_combn[1,i], " and ", variableList_combn[2,i], " are NOT significantly differnet from each other."))
  }
}

##################
# LAB SESSION
data <- as.data.frame(iris$Sepal.Length)

mean_fun <- function(data,i){
  return(mean(data[i,]))
}

results <- boot(data = data, R = 10, statistic = mean_fun)

plot(results)
summary(results)