# Clear All Variables & Clear the Screen
rm(list=ls())
cat("\014")

# Read in the Data
sea = read.csv("ringtones_Final.csv")


sea$Competition = as.factor(sea$Competition)
sea$Volume = as.factor(sea$Volume)
sea$logPosition=log(sea$Position)



# Creating Training and Testing Sets

library(caTools)
set.seed(123)
split = sample.split(sea$Profitability, SplitRatio = 0.7)
Train = subset(sea, split==TRUE)
Test = subset(sea, split==FALSE)


#CTR Model

lm_CTR <- lm(CTR ~ logPosition + Competition + Volume  + Keyword_WordCount + Headline_WordCount  + Headline_Readability + Line.1_Time + Line1_WordCount  + Line2_WordCount  , data=Train)
summary(lm_CTR)

Predicted_lmctr <- predict(lm_CTR,Test)

SSE = sum((Test$CTR - Predicted_lmctr)^2)
SST = sum((Test$CTR - mean(Train$CTR))^2)

Test_RSquare_lmctr <- 1 - SSE/SST
Test_RSquare_lmctr




#Conversion Model

Test_1 <- Test[!is.na(Test$Conversion),]

lm_conv<-lm(Conversion ~  Headline_Number +  KeywordinHeadline, data=Train)
summary(lm_conv)

Predicted_lm_conv <- predict(lm_conv,Test_1)
SSE = sum((Test_1$Conversion - Predicted_lm_conv)^2)
SST = sum((Test_1$Conversion- mean(Train$Conversion,na.rm=T))^2)

Test_RSquare_lm_conv <- 1 - SSE/SST
Test_RSquare_lm_conv




#Profit Model

#With Headline Number

lm_PH<-lm(Profit ~ logPosition + Competition + Headline_Number, data=Train)
summary(lm_PH)
Predicted_lm_PH <- predict(lm_PH,Test)

SSE = sum((Test$Profit - Predicted_lm_PH)^2)
SST = sum((Test$Profit - mean(Train$Profit))^2)

Test_RSquare_lm_PH <- 1 - SSE/SST
Test_RSquare_lm_PH

#Without Headline Number

lm_P<-lm(Profit ~ logPosition + Competition , data=Train)
summary(lm_P)
Predicted_lm_P <- predict(lm_P,Test)

SSE = sum((Test$Profit - Predicted_lm_P)^2)
SST = sum((Test$Profit - mean(Train$Profit))^2)

Test_RSquare_lm_P <- 1 - SSE/SST
Test_RSquare_lm_P


