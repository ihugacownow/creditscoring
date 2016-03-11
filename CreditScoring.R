input <- read.csv("~/Desktop/Wealthfront/input.csv", header = TRUE)
# Check if the data has been read in correctly 
head(input)

# Clean the data for statuses, changing it to a "boolean" type  
table(filteredInput$loan_status)
bad_indicators <- c("Late (16-30 days)", "Late (31-120 days)", "Default", "Charged Off")
# Remove invalid loan statuses 
filteredInput <- input[input$loan_status != "",]
#Append isBad indicators to the table, note "NA" if no loan status  
filteredInput$is_bad <- ifelse(filteredInput$loan_status %in% bad_indicators, 1, 0)

# Get rid of fields which are mostly NA
poor_coverage <- sapply(filteredInput, function(x) {
  coverage <- 1 - sum(is.na(x)) / length(x)
  coverage < 0.7
})
filteredInput <- filteredInput[,poor_coverage==FALSE]


#Modify Data Types - Numeric
filteredInput$interest_rate <- gsub("[%]", "", filteredInput$interest_rate)
filteredInput$interest_rate <- as.numeric(filteredInput$interest_rate)
filteredInput$open_acc = as.numeric(filteredInput$open_acc)
filteredInput$revol_bal= as.numeric(filteredInput$revol_bal)
filteredInput$total_acounts = as.numeric(filteredInput$total_acounts)
#Modify Data Types - Create Ratios
filteredInput$fundToLoan <- filteredInput$loan_amount / filteredInput$annual_income
filteredInput$installmentToAnnualincome <- filteredInput$installment/ filteredInput$annual_income
filteredInput$interestRateToAnnualincome <- filteredInput$interest_rate / filteredInput$annual_income
filteredInput$installmentToLoan <- filteredInput$installment/ filteredInput$loan_amount
#Modify Data Types - Simplify Factors 
filteredInput$termFac <- as.factor(ifelse(filteredInput$term == "60 months", "long term", 
                                  "short term"))
filteredInput$termNum <- as.numeric(ifelse(filteredInput$term == "60 months", "60", 
                                          "36"))
filteredInput$emp_lengthFac <- as.factor(ifelse((filteredInput$emp_length == "< 1 year"), "transient", 
                                                ifelse(filteredInput$emp_length == "1 year", "transient",
                                                  ifelse(
                                                     filteredInput$emp_length == "10+ years", "stable", "Middle"
                                                   ) 
                                          )))
filteredInput$emp_lengthNum <- as.numeric(ifelse((filteredInput$emp_length == "< 1 year"), "1", 
                                                ifelse(filteredInput$emp_length == "1 year", "1",
                                                       ifelse(
                                                         filteredInput$emp_length == "10+ years", "3", "2"
                                                       ) 
                                                )))
filteredInput$home_ownerFac <- as.factor(ifelse((filteredInput$home_owner == "MORTGAGE"), "mortgage", 
                                                ifelse(filteredInput$home_owner == "OWN", "own", "rent"
                                                )))
filteredInput$home_ownerNum <- as.numeric(ifelse((filteredInput$home_owner == "MORTGAGE"), "1", 
                                                ifelse(filteredInput$home_owner == "OWN", "2", "3"
                                                )))

filteredInput$annual_incomeFac <- as.factor(ifelse((filteredInput$annual_income < 20000), "low", 
                                                ifelse(filteredInput$annual_income < 60000, "middle",
                                                       ifelse(
                                                         filteredInput$annual_income < 100000, "higher", "highest"
                                                       ) 
                                                )))
filteredInput$annual_incomeNum <- as.numeric(ifelse((filteredInput$annual_income < 20000), "1", 
                                                   ifelse(filteredInput$annual_income < 60000, "2",
                                                          ifelse(
                                                            filteredInput$annual_income < 100000, "3", "4"
                                                          ) 
                                                   )))

filteredInput$delinq_2yrs <- as.numeric(filteredInput$delinq_2yrs)
filteredInput$delinq_2yrsFac <- as.factor(ifelse((filteredInput$delinq_2yrs == 0), "low", 
                                                   ifelse(filteredInput$delinq_2yrs == 1, "middle",
                                                          ifelse(
                                                            filteredInput$delinq_2yrs == 2, "higher", "highest"
                                                          ) 
                                                   )))

#FEATURE SELECTION 
#figure out which columns are numeric (and hence we can look at the distribution)
numeric_cols <- sapply(filteredInput, is.numeric)
library(reshape2)
library(ggplot2)
filteredInput.lng <- melt(filteredInput[,numeric_cols], id="is_bad")
#plot the distribution for bads and goods for each variable
p <- ggplot(aes(x=value, group=is_bad, colour=factor(is_bad)), data=filteredInput.lng)
#Visualize density
p + geom_density() +
  facet_wrap(~variable, scales="free")

#SPLIT TRAIN AND TEST DATA
#Get training data 
goods <- filteredInput[filteredInput$is_bad == '0',]
bads <- filteredInput[filteredInput$is_bad == '1',]
#goodRows <- sort(sample(nrow(goods), 8003))
#filteredGoods <- goods[goodRows,]
#small <- rbind(filteredGoods, bads)
trainingRows <- sort(sample(nrow(filteredInput), nrow(filteredInput)*0.7))
train <- filteredInput[trainingRows,]
test <- filteredInput[-trainingRows,]

#MODEL 1: PLOT GLM
contModelBigger.logit <- glm(factor(is_bad) ~ 
                               emp_length + 
                               home_ownerFac + 
                               annual_income + 
                               dti + 
                               delinq_2yrsFac + 
                               fundToLoan + 
                               installmentToAnnualincome +
                               interestRateToAnnualincome + 
                               installmentToLoan +
                               outstanding_principal +
                               total_received_interest +
                               total_received_principal, 
                             family = binomial(link = "logit"), data = train, na.action=na.omit)
summary(contModelBigger.logit)

#Run a prediction on the test values 
test$contModelBigger.yhat <- predict(contModelBigger.logit, newdata = test, type = "response")

#Visualize the accuracy of the glm 
library(ROCR)
contModelBigger.scores <- prediction(test$contModelBigger.yhat, test$is_bad)

#Check with KS AND AUC statistic 
contModelBigger.perf <- performance(contModelBigger.scores, "tpr", "fpr")
ks1.logit <- max(attr(contModelBigger.perf, "y.values")[[1]] - (attr(contModelBigger.perf, "x.values")[[1]]))
contModelBigger.auc = performance(contModelBigger.scores, "auc")

#MODEL 2: Tree
t1.weighted <- rpart(factor(is_bad) ~ 
                    emp_length + 
                    home_ownerFac + 
                    annual_income + 
                    dti + 
                    delinq_2yrsFac + 
                    fundToLoan + 
                    installmentToAnnualincome +
                    interestRateToAnnualincome + 
                    installmentToLoan +
                    outstanding_principal +
                    total_received_interest +
                    total_received_principal, 
                  , data = train, parms = list(prior = c(0.5, 0.5)), na.action=na.omit)

test$t1.yhat <- predict(t1.weighted, newdata = test, type = "prob")

t1.scores <- prediction(test$t1.yhat[,2], test$is_bad)
t1.perf <- performance(t1.scores, "tpr", "fpr")

# AUC for the decision tree
t1.auc <- performance(t1.scores, "auc")
# KS statistic
ks1.tree <- max(attr(t1.perf, "y.values")[[1]] - (attr(t1.perf, "x.values")[[1]]))
ks1.tree


#MODEL 3: Random Forest
library(randomForest)
rf <- randomForest(factor(is_bad) ~   emp_length + 
                     home_ownerFac + 
                     annual_income + 
                     dti + 
                     delinq_2yrsFac + 
                     fundToLoan + 
                     installmentToAnnualincome +
                     interestRateToAnnualincome + 
                     installmentToLoan +
                     outstanding_principal +
                     total_received_interest +
                     total_received_principal, 
                   type="classification", data=train, importance=TRUE, na.action=na.omit)

test$rf.yhat <- predict(rf, newdata = test, type = "prob")
rf.scores <- prediction(test$rf.yhat[,2], test$is_bad)
rf.perf <- performance(rf.scores, "tpr", "fpr")

# KS statistic
ks1.rf <- max(attr(rf.perf, "y.values")[[1]] - (attr(rf.perf, "x.values")[[1]]))
ks1.rf
# AUC 
rf.auc <- performance(rf.scores, "auc")

#PLOT COMPARISON OF ALL 3 MODELS
plot(contModelBigger.perf, col = "red", lwd = 1.5)
plot(t1.perf, col = "green", lwd = 1.5, add = TRUE)
plot(rf.perf, col = "blue", lwd = 1.5, add = TRUE)
abline(0, 1, lty = 8, col = "grey")
legend("bottomright", legend = c("Tree", "glm", "rf"), col = c("green", "red", "blue"), lwd = c(1 ,1,1))

