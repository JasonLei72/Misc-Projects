########################################
##   FRE6871 R in Finance - Project   ##
##          Peng Lei   PL1736         ##
########################################

### -------------- Analysis of "German Credit Risk" -------------- ###
### Original Data: https://www.kaggle.com/uciml/german-credit/home
# Contents:
  # Missing Data
  # Basic Statistics
  # Linear Regression
  # ANOVA
  # Classification (Machine Learning)
  # Summary

# Initial Setup ------------------------------------------------------
  rm(list=ls())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  options(digits=7, scipen=0)

# Load and explore the data ------------------------------------------
  mydata <- read.csv("Peng_Lei_german_credit_data.csv")
  str(mydata)
    # 1000 obs. of 11 variables
    # $ X               : int  0 1 2 3 4 5 6 7 8 9 ...
    # $ Age             : int  67 22 49 45 53 35 53 35 61 28 ...
    # $ Sex             : Factor w/ 2 levels "female","male": ...
    # ...
  head(mydata)
  mydata <- mydata[, -1] # exclude the first indexing column
  summary(mydata)
  # It has NA values.
  # Other than that, it's pretty clean - no blank entries, no 0s.
  
# Dealing with missing data -----------------------------------------
  # Explore missing data visually
  # Refer to Chapter 18.3
  library(VIM)
  aggr(x = mydata, prop = FALSE, number = TRUE)
  # From the plot and summary, 'Saving.accounts' has 183 NAs,
  # and 'Checking.account' has 394 NAs. Among all entries, 99 rows
  # have both missing.
  
  # Use correlations to explore missing values
  # Refer to Chapter 18.3.3
  temp_x <- as.data.frame(abs(is.na(mydata)))
  temp_y <- temp_x[which(apply(temp_x,2,sum)>0)]
  cor(temp_y)
  # Their occurrence of missing value has low correlations.
  
  # Does omitting missing data have a large impact?
  mean(!complete.cases(mydata))
  # About 47.8% of all rows contains one or more NAs.
  # So we can't do complete analysis like 18.6.
  # We must use multiple imputation (MI) to simulate missing data.
  
  # Multiple Imputation (MI)
  # Refer to Chapter 18.7
  library(mice)
  imp <- mice(mydata, seed=1234)
  mydata <- complete(imp, action=2)
  # MICE assumes the missing data are Missing at Random, so we assume
  # they are. This technique bases on the probability of appearance on 
  # observed values to predict the missing values 
  
  summary(mydata)
  # Now there are no missing data.

  attach(mydata)
  
# Basic plots of data -------------------------------------------------
  # Check the class of each column
  unique(sapply(mydata, class))
    # "integer" "factor"

  # Draw histogram for each numeric variables in one plot
  # Refer to Chpater 3.5 - combining plots
  par(mfrow=c(2,2))
  hist(Age); hist(Job); hist(Credit.amount); hist(Duration)

  # Realize that "Job" should better be a factor.
  mydata$Job <- factor(mydata$Job)
  
  # Draw boxplots for Age, Credit.amount, and Duration
  layout(matrix(c(1,1,2,3), 2,2, byrow = TRUE))
  # Use sapply loop
  boxplot(Age, main="Age"); 
  boxplot(Credit.amount, main="Credit Amount"); 
  boxplot(Duration, main="Duration")
  
  # Correlation Plot between the 3 numeric varaibles
  library(corrgram)
  corrgram(mydata, order=TRUE, lower.panel=panel.pie,
           upper.panel=panel.cor, text.panel=panel.txt,
           main="Corrgram of intercorrelations")  

# Basic data analysis -----------------------------------------------
  # Data aggregation of Credit amount by Job and Sex
  aggregate(Credit.amount, by=list(Sex=Sex, Job=Job), mean)
    #      Sex Job        x
    # 1 female   0 2065.667
    # 2   male   0 3560.500
    # 3 female   1 2352.938
    # 4   male   1 2361.147
    # 5 female   2 2644.929
    # 6   male   2 3264.797
    # 7 female   3 5288.730
    # 8   male   3 5484.414
  # Job 3 has the highest credit amount. Female with job 0 have the
  # the lowest credit amount, but male with job 0 have higher credit
  # than job 1 and 2.
  
  # More descriptive stats using by() to study Credit amount and 
  # Savings accounts - refer to Chapter 7.2
  mystats <- function(x, na.omit=FALSE){
    m <- mean(x)
    n <- length(x)
    s <- sd(x)
    skew <- sum((x-m)^3/s^3)/n
    kurt <- sum((x-m)^4/s^4)/n - 3
    return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
  }
  by(data=Credit.amount, Saving.accounts, mystats)
  
  # Plot credit amount of each level of Savings.accounts and Checking
  # accounts using ggplot() - refer to Chapter 19.4
  library(ggplot2)
  ggplot(data=mydata, aes(x=Credit.amount, fill=Saving.accounts)) +
    geom_density(alpha=.3)
  ggplot(data=mydata, aes(x=Credit.amount, fill=Checking.account)) +
    geom_density(alpha=.3)
  # It seems that the rich tends to need lower credit amount.
  
  # Separate boxplot between Age and Job (as we don't know what jobs)
  boxplot(Age~Job, main="Age of People in Each Job")
  # Not a particular job corresponds to a very different range of age.
  # All jobs range from about 20 something to above 60.
  
  # A table of two related variables - savings and checkings
  # Can also do a frequency and contingency tables - refer to 7.2
  SavCheck_table1 <- table(Saving.accounts, Checking.account)
  # Equivalent of doing this
  xtabs(~ Saving.accounts+Checking.account, data=mydata)
    #                Checking.account
    # Saving.accounts little moderate rich
    #      little        371      288   79
    #      moderate       21       92    9
    #      quite rich     28       43   14
    #      rich           10       35   10
  # People with little checkings tend to have little savings as well.
  
  # We can study the marginal probabilities - e.g. by column.
  # Given someone has little checkings, what's the probability of 
  # that one has little savings as well?
  SavCheck_table2 <- prop.table(SavCheck_table1, margin = 2)
  addmargins(SavCheck_table2, margin = 1) 
    # Checking.account
    # Saving.accounts     little   moderate       rich
    #      little     0.86279070 0.62882096 0.70535714
    #      moderate   0.04883721 0.20087336 0.08035714
    #      quite rich 0.06511628 0.09388646 0.12500000
    #      rich       0.02325581 0.07641921 0.08928571
    #      Sum        1.00000000 1.00000000 1.00000000
  # Regardless of which type of people with little, moderate, or rich
  # checkings, they all tend to have little savings.

# Simple Linear Regression -------------------------------------------
  # Of the 3 meaningful continuous numerical variables, only the rela-
  # tionship between Credit Amount and Duration seems meaningful.
  plot(Credit.amount ~ Age)  # renders nothing
  plot(Credit.amount ~ Duration) # seems to have some relationship
  
  # Credit Amount against Duration
  lm1 <- lm(Credit.amount ~ Duration)
  summary(lm1)
    # Intercept is nonsignificant, but the slope of Duration is.
  par(mfrow=c(2,2))
  plot(lm1)
    # Normal Q-Q plot shows that the normality assumption is violated.
    # Scale-Location plot also shows that the homoscedasticity
    # assumption is violated as well. Try transforming y-axis data.
  
  lm2 <- lm(log(Credit.amount) ~ Duration)
  summary(lm2)
    # Both intercept and slope of Duration are significant.
  par(mfrow=c(2,2))
  plot(lm2)
    # Normal Q-Q plot looks good. Residuals looks OK, scattered around
    # 0. Scale-Location plot shows constant variance.
    # All assumptions are met.
  # log(Credit.amount) = 6.926183 + 0.041262*Duration
  # Credit.amount = exp(6.926183) + exp(0.041262*Duration)
  # However, adjusted R-squread  is only 41%.
  
  dev.off()
  plot(log(Credit.amount) ~ Duration, main="Log Credit vs Duration")
  abline(lm2, col="red")
  # Draw a Lowess smoothed line - Refer to Chapter 11.1
  lines(lowess(log(Credit.amount)~Duration), col="blue")
  
  # We could try adding non-linear features
  lm3 <- lm(log(Credit.amount) ~ Duration + I(Duration^2))
  summary(lm3)
  # All coefficients are significant, but the adjusted R-squared
  # improved very little. So we stick to lm2 for simplicity.
  
# ANOVA --------------------------------------------------------------
  # We might want to try to fit ANOVA to this dataset as we can treat
  # 'Credit.amount' as the dependent variable and other categorial
  # variables as group separation.
  
  # Refer to Chapter 9.3
  # Fit a one-way ANOVA for credit amount for male/female
  fit_1 <- aov(Credit.amount ~ Sex)                             
  summary(fit_1)  
    # > summary(fit_1)
    #              Df    Sum Sq  Mean Sq F value  Pr(>F)   
    # Sex           1 6.956e+07 69561082   8.798 0.00309 **
    # Residuals   998 7.890e+09  7906127      
  # p-value < 0.05, so we reject null hypothesis. The population means
  # of credit amount for male and female are different.
  
  library(gplots)
  plotmeans(Credit.amount ~ Sex, xlab="Sex", ylab="Credit Amount",
            main="Mean Plot\nwith 95% CI")
  
  # Fit a one-way ANOVA for credit amount for Savings/Checkings
  # Add an interaction term
  fit_2 <- aov(Credit.amount ~ Saving.accounts*Checking.account)
  summary(fit_2)
    # ...                                Pr(>F)
    # Saving.accounts                     0.160
    # Checking.account                 4.29e-06
    # Saving.accounts:Checking.account    0.916
  # The interaction is nonsignificant, supporting the assumption of
  # slope equality. Checking.account is significant.
  
  # Tukey HSD pairwise group comparison -only include Checking.account
  # Refer to Listing 9.2
  fit_3 <- aov(Credit.amount ~ Checking.account)
  plot(TukeyHSD(fit_3))
  # The population mean of credit amout in descending order:
  # Moderate > Little > Rich checking account
  # All group mean differences are significant as all confidence
  # intervals do not contain 0,
  
  # However, as we start checking ANOVA assumption, it fails badly.
  library(car)
  qqPlot(lm(Credit.amount ~ Checking.account, data=mydata),
           simulate=TRUE, main="Q-Q Plot", labels=FALSE)
  # Recall that we plotted some histogram of Credit Amount in various
  # grouping methods, and it all shows a significant skewness to the 
  # right. And many credit models are in such skewness. It's not 
  # normal distribution. So we can't do ANOVA.
  
# Classification ----------------------------------------------------
# This is truly the real objective of such dataset about credit.
# We want to classify whether a customer can be granted credit.
# Various methods can be used.
  
  # First, split data into training set and validation set (70%/30%)
  set.seed(1234)
  train <- sample(nrow(mydata), 0.7*nrow(mydata))
  mydata.train <- mydata[train,]
  mydata.validate <- mydata[-train,]
  table(mydata.train$Risk)
    # bad good
    # 201  499
  table(mydata.validate$Risk)
    # bad good
    # 99  201
  
  # Logistic regression (as we're doing a binary classification) -----
  # Refer to Chapter 17.2
  fit.logit <- glm(Risk ~ ., data=mydata.train, family=binomial())
  summary(fit.logit)
    
  fit.logit2 <- glm(Risk ~ Age + Saving.accounts + Checking.account+ 
                   Duration, data=mydata.train, family=binomial())
  summary(fit.logit2)
  # Most of the coefficients are significant.
  
  # Define the target variables to be "good" or "bad"
  prob <- predict(fit.logit2, mydata.validate, type="response")
  logit.pred <- factor(prob > .5, levels=c(FALSE, TRUE), 
                       labels=c("bad", "good"))
  logit.perf <- table(mydata.validate$Risk, logit.pred,
                      dnn=c("Actual", "Predicted"))
  logit.perf
    #       Predicted
    # Actual good bad
    #   bad    14  85
    #   good   10 191
  # Accuracy = (14+191) / 300 = 68.3%.
  
  # Try this - to use a smaller model (a smaller AIC)
  logit.fit.reduced <- step(fit.logit)
  prob <- predict(logit.fit.reduced, mydata.validate, type="response")
  logit.perf <- table(mydata.validate$Risk, logit.pred, 
                      dnn=c("Actual", "Predicted"))
  logit.perf
    #       Predicted
    # Actual good bad
    #   bad    20  79
    #   good   8  193
  # Accuracy = (20+193) / 300 = 71%.
  
  # Decision Tree ----------------------------------------------------
  # Refer to Chapter 17.3
  library(rpart)
  set.seed(1234)
  dtree <- rpart(Risk ~ ., data=mydata.train, method="class",
                 parms=list(split="information"))
  dtree$cptable                 
  plotcp(dtree)  
    #           CP nsplit rel error    xerror       xstd
    # 1 0.04477612      0 1.0000000 1.0000000 0.05955294
    # 2 0.03482587      2 0.9104478 0.9800995 0.05919323
    # 3 0.02736318      3 0.8756219 0.9651741 0.05891570
    # 4 0.01492537      5 0.8208955 0.9104478 0.05783966
    # 5 0.01119403      6 0.8059701 0.9104478 0.05783966
    # 6 0.01000000     12 0.7363184 0.9353234 0.05834032
  # xerror + xstd --> 0.9353234 + 0.05834032 = 0.99367
  # So choose the tree with 2 splits.
  
  # Prune the tree at level 2
  dtree.pruned <- prune(dtree, cp=0.03482587)

  library(rpart.plot)
  prp(dtree.pruned, type = 2, extra = 104,  
      fallen.leaves = TRUE, main="Decision Tree")  
  
  # Classifies new cases
  dtree.pred <- predict(dtree.pruned, mydata.validate, type="class")
  dtree.perf <- table(mydata.validate$Risk, dtree.pred, 
                      dnn=c("Actual", "Predicted"))
  dtree.perf
    #       Predicted
    # Actual bad good
    #  bad    15   84
    #  good   18  183
  # Accuracy = (15+183)/300 = 66%.
  
  # Random Forest ----------------------------------------------------
  # Refer to Chapter 17.5
  library(randomForest)
  set.seed(1234)
  # Grow the forest
  fit.forest <- randomForest(Risk~., data=mydata.train,        
                             na.action=na.roughfix,
                             importance=TRUE)             
  fit.forest
  
  # Determines variable importance
  importance(fit.forest, type=2)
  
  # Classifies new cases
  forest.pred <- predict(fit.forest, mydata.validate)         
  forest.perf <- table(mydata.validate$Risk, forest.pred, 
                       dnn=c("Actual", "Predicted"))
  forest.perf
    #       Predicted
    # Actual bad good
    #  bad    25   74
    #  good   17  184
  # Accuracy = (25+184)/300 = 69.67%.
  
  # SVM --------------------------------------------------------------
  # Refer to Chapter 17.6
  library(e1071)
  set.seed(1234)
  fit.svm <- svm(Risk ~., data=mydata.train)
  fit.svm
  # Tuning an RBF support vector machine
  # This will take a while
  tuned <- tune.svm(Risk ~ ., data=mydata.train,
                    gamma=10^(-6:1), cost=10^(-10:10))
  tuned
    # - best parameters:
    #   gamma cost
    #     0.1    1
  # Fits the model with these parameters
  fit.svm <- svm(Risk~., data=mydata.train, gamma=.1, cost=1)  
  
  # Evaluates the cross-validation performance
  svm.pred <- predict(fit.svm, mydata.validate)
  svm.perf <- table(mydata.validate$Risk,
                    svm.pred, dnn=c("Actual", "Predicted"))
  svm.perf  
    #       Predicted
    # Actual bad good
    #   bad    9   90
    #   good   2  199
  # Accuracy = (9+199)/300 = 69.3%

# Choosing the best predictive solution ------------------------------
# by writing function to return true positive, true negative, false
# positive, and false negative
# Refer to Chapter 17.9
  performance <- function(table, n=2){
    if(!all(dim(table) == c(2,2)))
      stop("Must be a 2 x 2 table")
    # Extracts frequencies
    tn = table[1,1]
    fp = table[1,2]
    fn = table[2,1]
    tp = table[2,2]
    
    #Calculates statistics  
    sensitivity = tp/(tp+fn)
    specificity = tn/(tn+fp)
    ppp = tp/(tp+fp)
    npp = tn/(tn+fn)
    hitrate = (tp+tn)/(tp+tn+fp+fn)  # Accuracy
    
    # Prints results
    result <- paste("Sensitivity = ", round(sensitivity, n),
                    "\nSpecificity = ", round(specificity, n),
                    "\nPositive Predictive Value = ", round(ppp, n),
                    "\nNegative Predictive Value = ", round(npp, n),
                    "\nAccuracy = ", round(hitrate, n), "\n", sep="")
    cat(result)
  }
  
  # Performance of breast cancer data classifiers
  performance(logit.perf)
  performance(dtree.perf)
  performance(forest.perf)
  performance(svm.perf)
  
  # Summary
  # 1. All have similar accuracy, but among all logistic regression
  #    (the simplest one) has the highest accuracy.
  # 2. SVM has the highest sensitivity (predicting what's really true)
  # 3. But all 4 classifications have very low specificity, which is 
  #    a real problem because this is the power of test. We prefer a
  #    classifier detecting what is truly bad over one detecting what
  #    is truly good.

# Final words --------------------------------------------------------
# This dataset may be manipulated by the contributer for the purpose of
# applying data mining techniques as the original data has no NAs and 
# more irrelevant variables like a questionaire. True credit screening
# process is more complicated, and many datasets in this area have
# masked all attributes and values into meaningless symbols.