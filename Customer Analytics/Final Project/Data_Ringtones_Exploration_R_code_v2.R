setwd("C:\\Users\\Aakash\\Desktop\\UW Courses\\Customer Analytics\\Final Project")

rm(list=ls())
cat('\014')

library(readxl)
library(caTools)
library(corrplot)
library(dplyr)
library(leaps)
library(randomForest)

data.input <- read.csv("Ringtones_Final.csv")

str(data.input)

data.input$Date <- as.POSIXct(data.input$Date, format="%m/%d/%Y", tz="UTC")

data_mod <- data.input[c(1:length(data.input))]

cor(data_mod[sapply(data_mod,is.numeric)])

corrplot(cor(data_mod[sapply(data_mod,is.numeric)]),        # Correlation matrix
         method = "shade", # Correlation plot method
         type = "full",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         title = "",       # Main title
         col = NULL)       # Color palette

set.seed(123)
#Splitting into Train/ Test
split <- sample.split(data_mod$Profitability, SplitRatio = 0.7)
dataTrain <- data.input[split, ]
dataTest <- data.input[!split, ]

CTR_lmmodel_1 <- lm(CTR ~ Keyword_ringtone + log(Keyword_WordCount) + log(Headline_WordCount) 
                    + Headline_Number + Headline_Readability + Line.1_Time 
                    + log(Line1_WordCount) + Line.2_Action + log(Line2_WordCount) 
                    + log(Position) + KeywordinHeadline 
                    + KeywordinBody + as.factor(Competition) + Headline_Specific + as.factor(Volume), data = dataTrain)

summary(CTR_lmmodel_1)

#define intercept-only model
intercept_onlymodel_1 <- lm(CTR ~ 1, data=dataTrain)

#define model with all predictors
allmodel_1 <- CTR_lmmodel_1

#perform backward stepwise regression
bothmodel_1 <- step(intercept_onlymodel_1, direction='both', scope=formula(allmodel_1), trace=0)
summary(bothmodel_1)

Predicted_CTR_model1 <- predict(bothmodel_1,dataTest)

SSE = sum((dataTest$CTR - Predicted_CTR_model1)^2)
SST = sum((dataTest$CTR - mean(dataTrain$CTR))^2)

Test_RSquare_lmmodel1 <- 1 - SSE/SST
Test_RSquare_lmmodel1

#Log on position
CTR_lmmodel_2 <- lm(CTR ~ Keyword_ringtone + Keyword_WordCount + Headline_WordCount 
                    + Headline_Number + Headline_Readability + Line.1_Time 
                    + Line1_WordCount + Line.2_Action + Line2_WordCount 
                    + log(Position) + KeywordinHeadline 
                    + KeywordinBody + Competition + Headline_Specific + Volume, data = dataTrain)

summary(CTR_lmmodel_2)

#define intercept-only model
intercept_onlymodel_2 <- lm(CTR ~ 1, data=dataTrain)

#define model with all predictors
allmodel_2 <- CTR_lmmodel_2

#perform backward stepwise regression
bothmodel_2 <- step(intercept_onlymodel_2, direction='both', scope=formula(allmodel_2), trace=0)
summary(bothmodel_2)


Predicted_CTR_lmmodel1=2 <- predict(bothmodel_2,dataTest)

SSE = sum((dataTest$CTR - Predicted_CTR_lmmodel2)^2)
SST = sum((dataTest$CTR - mean(dataTrain$CTR))^2)

Test_RSquare_lmmodel2 <- 1 - SSE/SST
Test_RSquare_lmmodel2

CTR_rfmodel_1 <- randomForest(CTR ~ Keyword_ringtone + Keyword_WordCount + Headline_WordCount 
                              + Headline_Number + Headline_Readability + Line.1_Time 
                              + Line1_WordCount + Line.2_Action + Line2_WordCount 
                              + Position + KeywordinHeadline 
                              + KeywordinBody + Competition + Headline_Specific + Volume, 
                              data = dataTrain, mtry = 5, importance = TRUE, na.action = na.omit)

Predicted_CTR_rfmodel1 <- predict(CTR_rfmodel_1,dataTest)

SSE = sum((dataTest$CTR - Predicted_CTR_rfmodel1)^2)
SST = sum((dataTest$CTR - mean(dataTrain$CTR))^2)

Test_RSquare_rfmodel1 <- 1 - SSE/SST
Test_RSquare_rfmodel1

print(importance(CTR_rfmodel_1,type = 2))


!is.na(dataTrain$actioncount / dataTrain$Clicks)
CR_lmmodel_1 <- lm(Conversion ~ log(Keyword_WordCount) + log(Headline_WordCount) 
                    + Headline_Number + Headline_Readability + Line.1_Time 
                    + log(Line1_WordCount) + Line.2_Action + log(Line2_WordCount) 
                    + Impressions + Clicks + log(Position) + KeywordinHeadline 
                    + KeywordinBody + as.factor(Competition) + Headline_Specific, data = dataTrain[!is.na(dataTrain$actioncount / dataTrain$Clicks)])

summary(CR_lmmodel_1)

#define intercept-only model
intercept_onlymodel_1 <- lm(CTR ~ 1, data=dataTrain)

#define model with all predictors
allmodel_1 <- CTR_lmmodel_1

#perform backward stepwise regression
bothmodel_1 <- step(intercept_onlymodel_1, direction='both', scope=formula(allmodel_1), trace=0)
summary(bothmodel_1)



Clicks_model_1 <- glm(Clicks_b ~ Keyword_ringtone + Keyword_WordCount + Headline_WordCount 
                      + Headline_Number + Headline_Readability + Line.1_Time 
                      + Line1_WordCount + Line.2_Action + Line2_WordCount 
                      + Position + KeywordinHeadline 
                      + KeywordinBody + Competition + Headline_Specific + Volume, data = dataTrain, family = binomial)

summary(Clicks_model_1)

Predicted_Clicks_model1 <- predict(Clicks_model_1, type="response", newdata=dataTest)

table(dataTest$Clicks_b, Predicted_Clicks_model1 > 0.5) # Creates confusion matrix for t=0.5

sum(diag(table(dataTest$Clicks_b, Predicted_Clicks_model1 > 0.5)))/sum(table(dataTest$Clicks_b, Predicted_Clicks_model1 > 0.5))


Clicks_model_2 <- glm(Clicks_b ~ Keyword_ringtone + log(Keyword_WordCount) + log(Headline_WordCount) 
                      + Headline_Number + Headline_Readability + Line.1_Time 
                      + log(Line1_WordCount) + Line.2_Action + log(Line2_WordCount) 
                      + log(Position) + KeywordinHeadline 
                      + KeywordinBody + Competition + Headline_Specific + Volume, data = dataTrain, family = binomial)

summary(Clicks_model_2)

Predicted_Clicks_model2 <- predict(Clicks_model_2, type="response", newdata=dataTest)

table(dataTest$Clicks_b, Predicted_Clicks_model2 > 0.5) # Creates confusion matrix for t=0.5

sum(diag(table(dataTest$Clicks_b, Predicted_Clicks_model2 > 0.5)))/sum(table(dataTest$Clicks_b, Predicted_Clicks_model1 > 0.5))


#Run from here

#Click through rate:
  
##Model 1:
  
CTR.lm.model.1 <- lm(CTR ~ log(Position) + as.factor(Competition) + as.factor(Volume) + Keyword_ringtone + Keyword_WordCount + Headline_WordCount + Headline_Number + Headline_Readability + Line.1_Time + Line1_WordCount + Line.2_Action + Line2_WordCount + KeywordinHeadline + KeywordinBody + Headline_Specific, data=dataTrain)

##Model 2:
  
CTR.lm.model.2 <- lm(CTR ~ log(Position) + Keyword_ringtone +
                    log(Keyword_WordCount) +log(Headline_WordCount) + Headline_Number + Headline_Readability + Line.1_Time + log(Line1_WordCount) + Line.2_Action + log(Line2_WordCount) +Line_2_Time+ KeywordinHeadline + KeywordinBody + Headline_Specific + Volume + Competition,data=dataTrain)

##Model 3

CTR.lm.model.3 <- lm(CTR ~ Keyword_ringtone + log(Keyword_WordCount) + log(Headline_WordCount)
                    + Headline_Number + Headline_Readability + Line.1_Time
                    + log(Line1_WordCount) + Line.2_Action + log(Line2_WordCount)
                    + log(Position) + KeywordinHeadline
                    + KeywordinBody + as.factor(Competition) + Headline_Specific + as.factor(Volume), data = dataTrain)

#Conversion:
CR.lm.model.1 <- lm(Conversion ~ log(Position) + as.factor(Competition) + as.factor(Volume) + Headline_WordCount + Headline_Number + Headline_Readability + Line.1_Time + Line1_WordCount + Line.2_Action + Line2_WordCount + KeywordinHeadline + KeywordinBody + Headline_Specific, data=dataTrain)

#Conversion:
CR.lm.model.2 <- lm(Conversion ~ as.factor(Competition)+Headline_Number + KeywordinHeadline, data=dataTrain)




#Profit:
Profit.lm.model.1 <- lm(Profit ~ log(Position) + as.factor(Competition) + as.factor(Volume), data=dataTrain)

summary(CTR.lm.model.1)
summary(CTR.lm.model.2)
summary(CTR.lm.model.3)
summary(CR.lm.model.1)
summary(CR.lm.model.2)

summary(Profit.lm.model.1)


#Test RSquares
Predicted_CTR.lm.model.1 <- predict(CTR.lm.model.1,dataTest)
Predicted_CTR.lm.model.2 <- predict(CTR.lm.model.2,dataTest)
Predicted_CTR.lm.model.3 <- predict(CTR.lm.model.3,dataTest)
Predicted_CR.lm.model.1 <- predict(CR.lm.model.1,dataTest)

dataTest_NArem <- dataTest[!is.na(dataTest$Conversion),]

Predicted_CR.lm.model.2 <- predict(CR.lm.model.2,dataTest_NArem)

Predicted_Profit.lm.model.1 <- predict(Profit.lm.model.1,dataTest)


SSE_CTR_1 = sum((dataTest$CTR - Predicted_CTR.lm.model.1)^2)
SSE_CTR_2 = sum((dataTest$CTR - Predicted_CTR.lm.model.2)^2)
SSE_CTR_3 = sum((dataTest$CTR - Predicted_CTR.lm.model.3)^2)
SSE_CR_1 = sum((dataTest$Conversion - Predicted_CR.lm.model.1)^2)


SSE_CR_2 = sum((dataTest_NArem$Conversion - Predicted_CR.lm.model.2)^2)

SSE_Profit_1 = sum((dataTest$Profit - Predicted_Profit.lm.model.1)^2)

SST_CTR = sum((dataTest$CTR - mean(dataTrain$CTR))^2)
SST_CR = sum((dataTest_NArem$Conversion - mean(dataTrain$Conversion,na.rm=T))^2)

Test_RSquare_CTR_lmmodel1 <- 1 - SSE_CTR_1/SST
Test_RSquare_CTR_lmmodel1

Test_RSquare_CTR_lmmodel2 <- 1 - SSE_CTR_2/SST
Test_RSquare_CTR_lmmodel2

Test_RSquare_CTR_lmmodel3 <- 1 - SSE_CTR_3/SST
Test_RSquare_CTR_lmmodel3

Test_RSquare_CR_lmmodel1 <- 1 - SSE_CR_1/SST
Test_RSquare_CR_lmmodel1

Test_RSquare_CR_lmmodel2 <- 1 - SSE_CR_2/SST_CR
Test_RSquare_CR_lmmodel2


Test_RSquare_Profit_lmmodel1 <- 1 - SSE_Profit_1/SST
Test_RSquare_Profit_lmmodel1



