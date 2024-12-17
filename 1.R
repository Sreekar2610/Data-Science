#LoGISTIC 
# Load the data
url <- "https://raw.githubusercontent.com/Sreekar2610/Data-Science/refs/heads/main/admission.csv"
data <- read.csv(url)
View(data)
head(data)
summary(data)
str(data)
# Convert 'rank' to factor
data$rank <- factor(data$rank)
# Fit the logistic regression model
mylogit <- glm(admit ~ gre + gpa + rank, data = data, family = "binomial")
summary(mylogit)
# Prepare new data for prediction
newdata <- data.frame(gre = 700, gpa = 10, rank = factor(4, levels = levels(data$rank)))
# Make prediction
predict(mylogit, newdata, type = "response")

#KNN
library('class')
install.packages('caret')
library('caret') 
diabetes<-read.csv("https://raw.githubusercontent.com/Sreekar2610/Data-Science/refs/heads/main/diabetes%20(1).csv")
View(diabetes)
str(diabetes)
diabetes[,"Outcome"]<-factor(diabetes[,"Outcome"])
summary(diabetes)
mean(diabetes$Age)
train=diabetes[1:500,]
test=diabetes[501:768,]
pred_test<-knn(train[,-9],test[,-9],train$Outcome,k=2)
pred_test
confusion=table(pred_test,test$Outcome)
confusion
sum(diag(confusion))/nrow(test)
confusionMatrix(pred_test,test$Outcome)
###muliplt linear regression
# Load the data
data <- mtcars[, c("mpg", "disp", "drat", "hp")]
# Display the first few rows
head(data)
# Scatterplot matrix
pairs(data, pch = 0, col = "black")
# Build the multiple linear regression model
model <- lm(mpg ~ drat + disp + hp, data = data)
# Histogram of residuals
hist(residuals(model), col = "steelblue", main = "Residuals Histogram", xlab = "Residuals")
# Model summary
summary(model)
# New data for prediction
newdata1 <- data.frame(drat = 2.7, disp = 170.0, hp = 100)
# Predict MPG for new data
predicted_mpg <- predict(model, newdata1, type = "response")
# Display the prediction
predicted_mpg
model$coefficients

##Linear regression



##kmeeans
# Install necessary packages
install.packages('cluster')
install.packages('factoextra')

library(factoextra)
library(cluster)

# Load dataset and clean it
data <- USArrests
df <- na.omit(data[sapply(data, is.numeric)]) # Keep numeric data only
df <- scale(df)  # Scale the data
# Determine number of unique rows
n_unique <- nrow(unique(df))
# Visualize optimal number of clusters
fviz_nbclust(df, kmeans, method = "wss", k.max = min(n_unique, 10)) 
# Limit k.max
km<-kmeans(df,centers = 4,nstart = 25)
km
fviz_cluster(km,data = df)
aggregate(USArrests,by=list(clusters=km$cluster),mean)
final_data<-cbind(USArrests,clusters=km$cluster)
head


# Load the iris dataset
data(iris)

# Create a binary outcome (1 if species is "setosa", 0 otherwise)
iris$Species_binary <- ifelse(iris$Species == "setosa", 1, 0)

# Fit the logistic regression model
model <- glm(Species_binary ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width, 
             data = iris, family = binomial)

# View the summary of the logistic regression model
summary(model)

# Predict probabilities (logistic regression predictions)
predicted_probabilities <- predict(model, type = "response")

# Plotting predicted probabilities vs. Petal.Length
plot(iris$Petal.Length, predicted_probabilities, 
     main = "Logistic Regression Predictions (Setosa)", 
     xlab = "Petal Length", 
     ylab = "Predicted Probability (Setosa)", 
     pch = 19, col = rgb(0, 0, 0, 0.5))

# Add a smoother to represent the trend
library(ggplot2)
ggplot(iris, aes(x = Petal.Length, y = predicted_probabilities)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +
  labs(
    title = "Logistic Regression Curve",
    x = "Petal Length",
    y = "Predicted Probability (Setosa)"
  ) +
  theme_minimal()



#logistic regression in R   data <- read.csv("C:/Users/pc/Downloads/admission.csv")
str(data)
head(data)

# Convert 'rank' and 'admit' to factors
data$rank <- factor(data$rank)
data$admit <- factor(data$admit)

# Fit the logistic regression model
model <- glm(admit ~ gre + gpa + rank, data = data, family = "binomial")

# View summary of the model
summary(model)

# Load ggplot2 for plotting
library("ggplot2")

# Predict probabilities from the logistic regression model
predicted_prob <- predict(model, type = "response")

# Add predicted probabilities to the dataset
data$predicted_prob <- predicted_prob

# Plotting the logistic regression curve using ggplot2
ggplot(data, aes(x = gpa, y = predicted_prob, color = admit)) +
  geom_point() +  # Add scatter plot points
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Logistic regression curve
  labs(title = "Logistic Regression in R", x = "GPA", y = "Predicted Probability")