install.packages("readxl")
library("readxl")
library(xlsx)
MishMash_train <- read_excel("C://Users//mukti//Desktop//ML-Problem//MishMash//Training-Data-Sets.xlsx")
MishMash_train[,-c(1,2)] = scale(MishMash_train[,-c(1,2)], center = TRUE, scale = TRUE)
MishMash_test <- read_excel("C://Users//mukti//Desktop//ML-Problem//MishMash//Test dataset v1.xlsx")
MishMash_test[,-c(1,2)] = scale(MishMash_test[,-c(1,2)], center = TRUE, scale = TRUE)
View(MishMash_train)
View(MishMash_test)
summary(MishMash_train)
cor(MishMash_train_scale)
MishMash_model=lm(EQ~Social_Search_Impressions+Median_Rainfall+Inflation+pct_PromoMarketDollars_Category+pct_PromoMarketDollars_Subcategory+EQ_Category+EQ_Subcategory,data = MishMash_train)
summary(MishMash_model)
MishMash_Pred <- predict(MishMash_model, MishMash_test)
MishMash_Pred
actuals_preds <- data.frame(cbind(actuals=MishMash_test$EQ, predicteds=MishMash_Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)
View(actuals_preds)
correlation_accuracy
write.xlsx(as.data.frame(actuals_preds), file = "C://Users//mukti//Desktop//ML-Problem//MishMash//results.xlsx")
# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy
# MAPE Calculation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape