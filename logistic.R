library(xlsx)
library(caTools)
library(nnet) 
library(caret)
library(dplyr)

adidas <- read.xlsx("Adidas US Sales Datasets.xlsx", 1)
str(adidas)
adidas$Retailer <- as.factor(adidas$Retailer)
adidas$Region <- as.factor(adidas$Region)
adidas$State <- as.factor(adidas)
adidas$City <- as.factor(adidas$City)
adidas$Product <- as.factor(adidas$Product)
adidas$Sales.Method <- as.factor(adidas$Sales.Method)
summary(adidas)

sales_group <- vector(mode="character", length=length(adidas$Total.Sales))
sales_group[adidas$Total.Sales<4254] <- "low"
sales_group[adidas$Total.Sales>=4254 & adidas$Total.Sales<150000] <-"medium"
sales_group[adidas$Total.Sales>=150000] <- "high"
sales_group <- factor(sales_group,levels=c("low", "medium", "high"), ordered = TRUE)
adidas <- cbind(adidas, sales_group)

x <- adidas[,1:13]
y <- adidas[,14]
p_value <- sapply(x, function(f) chisq.test(f,y)$p.value)
p_value
df <- subset(adidas, select = c(1, 4, 5, 6, 7, 8, 9, 13, 14))

set.seed(123)
split <- sample.split(df$sales_group, SplitRatio = 0.7)
train_data <- subset(df, split == TRUE)
test_data <- subset(df, split == FALSE)

logit_model <- multinom(sales_group ~ ., data = train_data)
predictions <- predict(logit_model, newdata = test_data, type = "class")
cm <- confusionMatrix(test_data$sales_group,predictions)
cm
predictions1 <- predict(logit_model, newdata = train_data, type = "class")
cm1 <- confusionMatrix(train_data$sales_group,predictions1)
cm1

new_data <- data.frame(Retailer="Amazon", Region="South", State="Texas", City="Dallas",
                       Product="Men's Apparel", Price.per.Unit=50, Units.Sold=80, Sales.Method="Online")
new_prediction <- predict(logit_model, newdata = new_data, type = "class")
new_prediction
