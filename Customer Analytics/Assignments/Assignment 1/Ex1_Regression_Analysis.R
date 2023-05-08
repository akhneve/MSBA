library(stringr)

#Loading the dataset
setwd("C:\\Users\\Aakash\\Downloads")
rdb <- read.csv("Ex1_Data_R-1.csv")

#Discovering

summary(rdb)


#modeling

y <- as.numeric(gsub("(\\$)|(\\s)", "", rdb$Sales))

x <- rdb[,3:ncol(rdb)]

#Mod1 having all the parameters
mod1 <- lm(y ~ x$Offline...Print + x$Offline...TV + x$Online...Ad.Network + x$Online...Google.SEO + x$Online...Paid.Search + x$Online...Facebook + x$Online...Twitter + x$Online...Instagram)

summary(mod1)

#Mod2 having only significant parameters identified
mod2 <- lm(y ~ x$Offline...TV + x$Online...Facebook)

summary(mod2)

#Observations:

print("1. The combination of the Offline and Online platforms drive the sales of the product. However, the most significant channels are Offline - TV and Online - Facebook")
print("2. Online - Facebook and Offline - TV channels drive the sales (as the intercept of parameters in the model is high) and also, they are statistically more significant (p value < 0.05). Hence, it makes more sense to spend on advertising in these two channels.")
