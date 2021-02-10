#load necessary packages
library(ggplot2)
library(caTools)

#downloading the csv file for analysis
url <- "https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv"
dataset <- read.csv(url)
print("Data has been downloaded successfully")
cat("\n")

#view data
print(head(dataset,10))
cat("\n")

#plotting the data - checking for linearity
plot <- ggplot()+
  geom_point(aes(x=dataset$Hours,y=dataset$Scores),col="blue")+
  scale_x_continuous(breaks = c(1:9),limits = lim_x)+
  scale_y_continuous(breaks = seq(0,100,by=10),limits = lim_y)+
  xlab("Hours Studied")+ylab("Percentage Score")+
  ggtitle("Hours vs Percentage")+
  theme(plot.title = element_text(hjust = 0.5))
print(plot)
print("The variables have been plotted - See Plots Tab")
cat("\n")

#saving plot in file "task1.png"
dev.copy(png,"task1.png")
dev.off()

#correlation between dependent and independent variable
correlation <- cor(dataset$Hours,dataset$Scores)
cat("Correlation between Hours and Scores : \n")
cat(correlation)
cat("\n")

#creating dependent and independent variables
independent_var <- dataset$Hours
dependent_var <- dataset$Scores

#splitting test-train data
set.seed(0)
split = sample.split(dataset$Scores, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#training the simple linear model
regressor <- lm(formula = Scores ~ Hours,data = training_set)
cat("Training Complete\n\n")
stats <- summary(regressor)
print("Coefficients")
print(stats$coefficients[,c(1,4)])
cat("\n")

#predicting the test data
scores <- predict(regressor,newdata = test_set)
predicted_scores <- data.frame(test_set$Hours,test_set$Scores,scores)
names(predicted_scores) <- c("Hours","Scores","Predicted Scores")
rownames(predicted_scores) <- 1:nrow(predicted_scores)
print("Comparing the actual values and the preicted values of the test data")
print(predicted_scores)
cat("\n")

#plotting the regression line along dataset for the training set
plot <- ggplot()+
   geom_point(aes(x=training_set$Hours,y=training_set$Scores),col="blue")+geom_line(aes(x=training_set$Hours,y=predict(regressor,newdata = training_set),colour='red'))+     xlab("Hours Studied")+ylab("Percentage Score")+
   ggtitle("Hours vs Percentage - Regression Line")+
   theme(plot.title = element_text(hjust = 0.5))+
   labs(colour='RegressionLine')
print(plot)
print("The Regression Line has been plotted - See Plots Tab")
cat("\n")

#saving plot in file "task1.1.png"
dev.copy(png,"task1.1.png")
dev.off()

#custom prediction
cat("Custom Prediction\n")
sample <- data.frame(Hours=9.25)
print("Hours = 9.25")
predicted_val <- predict(regressor,sample)
c <- paste("Predicted value = ",predicted_val)
print(c)
cat("\n")

#evaluating metrics
cat("Evaluating the Regression Model\n")
metrics <- data.frame(matrix(ncol = 2,nrow = 5))
names(metrics) <- c("Metric","Value")
mae <- MAE(training_set$Scores,predict(regressor,training_set))
mape <- MAPE(predict(regressor,training_set),training_set$Scores)
mnames <- c("R-Squared","Adj. R-Squared","MeanAbsoluteError","MeanAbsolutePercentageError","ResidualStandardError")
mvalues <- c(stats$r.squared,stats$adj.r.squared,mae,mape,stats$sigma)
metrics$Metric <- mnames
metrics$Value <- mvalues
print(metrics)
