## Logistic regression

library(caTools)
#load data
split <- sample.split(data, SplitRatio=0.8)

training <- subset(data, split=="TRUE")
testing <- subset(data, split=="FALSE")

model=glm(y~., training, family="binomial") # where y is dependent categorical variable)

summary(model) #check residual deviance and AIC which should decrease with reduction in variable for better model output


res2 <- predict(model, testing, type="response")

table(Actualvalue=testing$colname, Predictedvalue=res>0.5)
#correct prediction divided by total observations

## Find threshold for the probability value
res1 <- predict(model, training, type="response") #use training dataset for threshold value

library(ROCR)

ROCRPred <- prediction(res, training$type)
ROCRPerf <-  preformance(ROCRPred, "tpr", "fpr")

plot(ROCRPerf, colorize=TRUE, print.cuttoff.at=seq(0.1, by=0.1))

##after checking the graph the threshold value can be changed for the best output
View(ROCRPerf)








