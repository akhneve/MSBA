
setwd("C:\\Users\\Aakash\\Desktop\\UW Courses\\Customer Analytics\\Final Project")

rm(list=ls())
cat('\014')

library(readxl)
library(corrplot)
library(dplyr)

data.input <- read_excel("C:\\Users\\Aakash\\Desktop\\UW Courses\\Customer Analytics\\Final Project\\Data_Ringtones.xlsx")

str(data.input)


data_mod <- data.input

#Calculating Click through and conversion rates
data_mod$CTR <- data_mod$Clicks / data_mod$Impressions
data_mod$Conversion <- ifelse(data_mod$Clicks!=0, (data_mod$actioncount / data_mod$Clicks), 0)
  
cor(data_mod[sapply(data_mod,is.numeric)])

corrplot(cor(data_mod[sapply(data_mod,is.numeric)]), method = "number",
         title = "method = 'number'",
         tl.pos = "n") 

corrplot(cor(data_mod[sapply(data_mod,is.numeric)]),        # Correlation matrix
         method = "shade", # Correlation plot method
         type = "full",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         title = "",       # Main title
         col = NULL)       # Color palette

#Find the list of unique keywords
keywords <- c()
for (i in data_mod$Keyword)
{
  keywords<- append(keywords, strsplit(i, " "))
}

keywords_ls <- unlist(keywords, recursive = FALSE)
keyword_list <- unique(keywords_ls)

# extract the list of matching words
x <- sapply(keyword_list, function(x) grepl(tolower(x), tolower(data_mod$Headline)))

# paste the matching words together
data_mod$Headline_KW_Words <- apply(x, 1, function(i) paste0(names(i)[i], collapse = ","))

# count the number of matching words
data_mod$Headline_KW_Count <- apply(x, 1, function(i) sum(i))

KeywordinHeadline <- c()
for(i in c(1:nrow(data_mod)))
{
  keywords <- lapply(strsplit(data_mod$Keyword[i], " "), tolower)
  headline_sp <- lapply(strsplit(data_mod$Headline[i], " "), tolower)
  
  cnt_mt <- length(intersect(keywords[[1]], headline_sp[[1]]))
  
  KeywordinHeadline <- append(KeywordinHeadline, (cnt_mt / length(keywords[[1]])))
}

data_mod$KeywordinHeadline <- KeywordinHeadline

KeywordinBody <- c()
for(i in c(1:nrow(data_mod)))
{
  keywords <- lapply(strsplit(data_mod$Keyword[i], " "), tolower)
  headline_bd <- lapply(strsplit(c(data_mod$`Line 1`[i],data_mod$`Line 2`[i]), " "), tolower)

  cnt_mt <- length(intersect(keywords[[1]], headline_bd[[1]]))
  
  KeywordinBody <- append(KeywordinBody, (cnt_mt / length(keywords[[1]])))
}

data_mod$KeywordinBody <- KeywordinBody


tagged_data <- read.csv("ringtones_Tagged.csv")

list_keyword_vars <- colnames(tagged_data[c(17,24:62)])

model1 <- lm(CTR ~ ., tagged_data[c(17,24:62)])
summary(model1)

write.csv(data_mod,"Data_Ringtones_Modified.csv")

