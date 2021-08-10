# Telecom Customer churn Prediction:
#Find out the most striking behaviour of customers through EDA and later on use some 
#of the predictive analytics techniques to determine the customers who are most likely to churn.

df <- read.csv('churn.csv')

summary(df)
str(df)

require(readr)
require(dplyr)
require(ggplot2)

colnames(df)
#[1] "customerID"       "gender"           "SeniorCitizen"    "Partner"          "Dependents"      
# [6] "tenure"           "PhoneService"     "MultipleLines"    "InternetService"  "OnlineSecurity"  
# [11] "OnlineBackup"     "DeviceProtection" "TechSupport"      "StreamingTV"      "StreamingMovies" 
# [16] "Contract"         "PaperlessBilling" "PaymentMethod"    "MonthlyCharges"   "TotalCharges"    
# [21] "Churn"  


#"customerID" 
n_distinct(df$customerID)
sum(is.na(df))
nrow(df)
df$churn <- if_else(df$Churn=='Yes','1','0')
df$Churn <- NULL

#"gender" 
table(df$gender)

ggplot(df,aes(x=gender,fill = churn)) + geom_bar(color='black')

#"SeniorCitizen" 
table(df$SeniorCitizen)
str(df$SeniorCitizen)
# changing datatype( int -> character)
df$SeniorCitizen <- as.character(df$SeniorCitizen)
ggplot(df,aes(x=SeniorCitizen,fill = churn)) + geom_bar(color='black')

#"Partner" 
table(df$Partner)
ggplot(df,aes(x=Partner,fill = churn)) + geom_bar(color='black')


#"Dependents"
table(df$Dependents)
ggplot(df,aes(x=Dependents,fill = churn)) + geom_bar(position='fill',color='black')

#"tenure" 
str(df$tenure)
summary(df$tenure)

hist(df$tenure)
ggplot(df,aes(x=tenure)) + geom_histogram(binwidth = 5,fill='yellow',color='black')


#PhoneService" 
table(df$PhoneService)
ggplot(df,aes(x=PhoneService,fill = churn)) + geom_bar(color='black')


# "MultipleLines" 
table(df$MultipleLines)
df$MultipleLines <- if_else(df$MultipleLines=='No phone service','No',df$MultipleLines)


#"InternetService" 
table(df$InternetService)

#   DSL         Fiber optic          No 
#  2421        3096                 1526 


#"OnlineSecurity" 
table(df$OnlineSecurity)

# No          No internet service                 Yes 
# 3498                1526                       2019 
df$OnlineSecurity <- if_else(df$OnlineSecurity=='No internet service','Yes',df$OnlineSecurity)

#"OnlineBackup" 
table(df$OnlineBackup)
# No            No internet service                 Yes 
# 3088                1526                          2429 

df$OnlineBackup <- if_else(df$OnlineBackup=='No internet service','No',df$OnlineBackup)


#"DeviceProtection" 
table(df$DeviceProtection)

df$DeviceProtection <- if_else(df$DeviceProtection=='No internet service','No',df$DeviceProtection)

#"TechSupport"
table(df$TechSupport)
df$TechSupport <- if_else(df$TechSupport=='No internet service','No',df$TechSupport)


#"StreamingTV" 
table(df$StreamingTV)
df$StreamingTV <- if_else(df$StreamingTV=='No internet service','No',df$StreamingTV)

#"StreamingMovies" 
table(df$StreamingMovies)
df$StreamingMovies <- if_else(df$StreamingMovies=='No internet service','No',df$StreamingMovies)


#"Contract"
table(df$Contract)

ggplot(df,aes(x=Contract,fill = churn)) + geom_bar(color='black')


#"PaperlessBilling" 
table(df$PaperlessBilling)


#PaymentMethod" 
table(df$PaymentMethod)

ggplot(df,aes(x=PaymentMethod,fill=churn))+geom_bar(color='darkgreen')


#"MonthlyCharges"
str(df$MonthlyCharges)

summary(df$MonthlyCharges)
quantile(df$MonthlyCharges,seq(0,1,0.01))
plot(quantile(df$MonthlyCharges,seq(0,1,0.01)))

#"TotalCharges" 

summary(df$TotalCharges)

quantile(df$TotalCharges,seq(0,1,0.01),na.rm = T)
plot(quantile(df$TotalCharges,seq(0,1,0.01),na.rm = T))

#outlier treatment
df$TotalCharges <- if_else(df$TotalCharges>8039.8830,8039.8830,df$TotalCharges)

# whose totalcharges is NA, -> these are new customers bcz tenure of all these are zero,
df1 <- df[which(is.na(df$TotalCharges)),]
View(df1)
df_tenure1 <- df[which(df$tenure==1),]
View(df_tenure1)


# there is only 11 NA values and all of them are new customers(bcz they are not completed there 1st month),
# so we can remove them or can assign zero or can replace the NA value with their monthlycharges(if they left within one month).
# all these customers contract>= 1 year

# now setting all NA value to 0, bcz all these customers are not completed the first month yet.
df$TotalCharges <- if_else(is.na(df$TotalCharges),0,df$TotalCharges)
sum(is.na(df$TotalCharges))

#"Churn" 
table(df$churn)

ggplot(df,aes(x=churn,fill = churn)) + geom_bar(color='black')

#SMOTE
# Data is unbalanced, out of 7043, 1869 customers churn , so going to balance the data
#set.seed(19)
#require(smotefamily)
#df_1 <- SMOTE(df[,-c(1,21)],df$churn,knn)
#df_1 <- SMOTE(churn~.df,perc.over=600,perc.under=100)

#library(DMwR2)
#df_1 <- SMOTE(churn~ df[,-c(1,21)],df,perc.over=100,perc.under=100,k=5,learner=NULL)

#library(ROSE)
#df[,c(1)] <- NULL
#over <- sample(churn~.,data=df,method = "over",N=10348)$data
#table(df$churn)
 
df$customerID <- NULL

??upSample

library(caret)
str(df)
df$churn <- as.factor(df$churn)

new.df <-upSample(df,df$churn)
str(new.df)  

table(new.df$churn)
  


hist(new.df$TotalCharges, col= 'orange')


#Scaling
#df[, c(6,19:20)] <- sapply(new.df[, c(6,19:20)],scale)
new.df[, c(5,18,19)] <- sapply(new.df[, c(5,18,19)],scale)

#corr
str(df)
# par(mfrow=c(1,1))
cor_mat <- cor(df[,c(5,18:19)])
require(corrplot)
corrplot(cor_mat)



str(new.df)
new.df$Class <- NULL
 

#dummies
require(dummies)
new.df$churn <- as.integer(new.df$churn)
final_df <- as.data.frame(new.df)
final_df <- dummy.data.frame(final_df)
View(final_df)

final_df$churn <- if_else(final_df$churn==2,1,0)


head(final_df)
str(final_df)
final_df[,c(2,4,6,8,11,13,16,18,20,22,24,26,28,31,33,37)] <- NULL
View(final_df) # 24 cols

#splitting
require(caTools)  
set.seed(44)
i <- sample.split(final_df$churn, SplitRatio = 0.80) 
trn <- final_df[i,]
val <- final_df[!i,]
nrow(trn)
nrow(val)

prop.table(table(trn$churn))
prop.table(table(final_df$churn))
prop.table(table(val$churn))

str(new.df)

#--- Logistic Regression---
model_1 <- glm(churn ~.,data = trn, family = 'binomial')
#model_1 <- glm(churn ~.,data = trn, family = 'binomial',maxit = 25,trace = TRUE, epsilon = 1e-14)
summary(model_1)

#---Step reduction---
model_2 <- step(model_1)
summary(model_2)

#Checking VIF---
require(car)
sort(vif(model_2)) 

model_3 <- glm(formula = churn ~ SeniorCitizen0 + DependentsNo + tenure + 
                 MultipleLinesNo + InternetServiceDSL + OnlineSecurityNo + TechSupportNo + 
                 StreamingTVNo + StreamingMoviesNo + 
                 `ContractMonth-to-month` + `ContractOne year` + PaperlessBillingNo + 
                 `PaymentMethodElectronic check` + MonthlyCharges, 
               family = "binomial", data = trn)


model_3 <- glm(formula = churn ~ SeniorCitizen0 + DependentsNo + tenure + 
      MultipleLinesNo + InternetServiceDSL +  
      OnlineSecurityNo + TechSupportNo + StreamingTVNo + StreamingMoviesNo + 
      `ContractMonth-to-month` + `ContractOne year` + PaperlessBillingNo + 
      `PaymentMethodElectronic check` + MonthlyCharges, 
    family = "binomial", data = trn)



summary(model_3)
sort(vif(model_3)) 



#prediction
val$pred_prob <- predict(model_3,newdata = val,type = 'response')
val$pred_churn <- if_else(val$pred_prob > 0.50,1,0)

require(caret)
actual <- as.factor(val$churn) # need to convert into factor bcz confusion matrix works on catagorical data
predicted <- as.factor(val$pred_churn)
confusionMatrix(predicted,actual,positive = '0')

summary(val$pred_prob)

___________________________


#prediction with threshold==35%
val$pred_prob <- predict(model_3,newdata = val,type = 'response')
val$pred_churn <- if_else(val$pred_prob > 0.35,1,0)

actual <- as.factor(val$churn) # need to convert into factor bcz confusion matrix works on catagorical data

predicted <- as.factor(val$pred_churn)
confusionMatrix(predicted,actual,positive = '0')



#Decision Tree---
require(caret)
require(rpart) 
dt_model <- rpart(churn~.,trn)
summary(dt_model)

require(rpart.plot)
prp(dt_model)
rpart.plot(dt_model)

dt_pred <- predict(dt_model, newdata = val)
summary(dt_pred)
dt_labels <- as.factor(ifelse(dt_pred > 0.46,'1','0'))

confusionMatrix(dt_labels,as.factor(val$churn),positive = '1')






#Accuracy : 0.7608           
#Sensitivity : 0.7460          
#Specificity : 0.7662 


#RandomForest 

require(randomForest)
rf_model <- randomForest(churn~.,trn,ntree=800,do.trace=20,importance=T)
#RANDOM FOREST IS not accepting space and special char in colnames

names(trn)[c(9)] <- c('InternetServiceFiberoptic')
names(trn)[c(16)] <- c('ContractMonthtomonth')
names(trn)[c(17)] <- c('ContractOneyear')
names(trn)[c(19)] <- c('PaymentMethodBanktransferautomatic')
names(trn)[c(20)] <- c('PaymentMethodCreditcardautomatic')
names(trn)[c(21)] <- c('PaymentMethodElectroniccheck')

names(val)[c(9)] <- c('InternetServiceFiberoptic')
names(val)[c(16)] <- c('ContractMonthtomonth')
names(val)[c(17)] <- c('ContractOneyear')
names(val)[c(19)] <- c('PaymentMethodBanktransferautomatic')
names(val)[c(20)] <- c('PaymentMethodCreditcardautomatic')
names(val)[c(21)] <- c('PaymentMethodElectroniccheck')

varImp(rf_model)
varImpPlot(rf_model)

rf_pred <- predict(rf_model,newdata = val,type = 'response')

summary(rf_pred)
rf_prom <- as.factor(ifelse(rf_pred> 0.613,'1','0'))
confusionMatrix(rf_prom,as.factor(val$churn),positive = '1')


#'Finally I got 89% accuracy with Rzndom forest'


#Accuracy : 0.8966
#Sensitivity : 0.8986          
#Specificity : 0.8947 


 

