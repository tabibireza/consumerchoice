# Binary choice modeling 

install.packages("ggplot2")
install.packages("ggthemes")
install.packages("xtable")
install.packages("knitr")
install.packages("caret")
install.packages("e1071")
install.packages("pROC")

library("xtable") # processing of regression output
library("knitr") # used for report compilation and table display
library("ggplot2") # very popular plotting library ggplot2
library("ggthemes") # themes for ggplot2
library("caret") # confusion matrix
library("pROC") # confusion matrix

# Reading data
getwd ()
RFMdata <- read.csv(file = "RFMData.csv",row.names=1)
kable(head(RFMdata, 5), row.names = TRUE)

# a scatter plot of purchase occurrences (y-axis) by recency (x-axis)
# Purchasei = β0 + β1Recency
model <-lm(data=RFMdata, Purchase ~ Recency) #note, lm() automatically includes intercept
# coef(model)[1] is beta0
# coef(model)[2] is beta1
p <- ggplot(RFMdata, aes(Recency, Purchase))+ 
  geom_point(alpha=0.3)+ #draws points
  theme_bw()  # changes visual theme of the plot to make the look cleaner
# setting intercept of the line based on beta0
# setting slope of the line based on beta1
# annotating
p + geom_abline(intercept = coef(model)[1],
                slope = coef(model)[2]) + 
  annotate(label = sprintf("y = %.5f + %.5f x\nR² = %.3f", coef(model)[1],coef(model)[2], 
                           summary(model)$r.squared), 
           geom = "text", x = 75, y = 0.6, size = 4)

# A better choice model logit, which restricts the output values to lie in interval.
# P(Purchasei) =
# exp(β0 + β1Recencyi + β2Frequencyi + β3Monetaryi)/
# exp(β0 + β1Recencyi + β2Frequencyi + β3Monetaryi) + 1
# utility of choosing to buy is Vbi = β0 + β1Recencyi + β2Frequencyi + β3Monetaryi
# utility of choosing not to buy is normalized to zero
# Vni = 0 exp(Vn) = exp(0) = 1
model <- glm(Purchase ~ Recency + Frequency+ Monetary, data= RFMdata, family = "binomial")
output <- cbind(coef(summary(model))[,1:4], exp(coef(model)))
colnames(output) <- c("beta","SE","z val.","Pr(>z)",'exp(beta)')
kable(output, caption = "Logistic regression estimates")

# the likelihood ratio test H0: β1 = β2 = β3 = 0
reduced.model <- glm(Purchase~1,data=RFMdata, family= "binomial")
kable(xtable(anova(reduced.model, model, test = "Chisq")), caption = "Likelihood ratio test")

# Predicting probabilities for each individual in the data set
# calculate logit probabilities

RFMdata$Base.probability <- predict(model, RFMdata, type="response")
kable(head(RFMdata, 5), row.names = TRUE)

# If individual’s predicted probability is greater or equal to 0.5, we predict he will make a purchase.
RFMdata$Predicted.Purchase <- 1*(RFMdata$Base.probability >= 0.5)
kable(head(RFMdata, 5), row.name= TRUE)

# Evaluating the model
# confusion matrix between predicted purchases and actual purchase behavio
confusionMatrix(table(RFMdata$Predicted.Purchase, RFMdata$Purchase), positive = "1")

# plot the receiver operating characteristic (ROC) curve
# plotting the true positive rate (TPR) against the false positive rate (FPR) 
rocobj <- roc(RFMdata$Purchase, RFMdata$Base.Probability)
{plot(rocobj,legacy.axes=TRUE)
  text(0.5, 0.8, labels = sprintf("AUC = %.5f",rocobj$auc))}

# scenario that everyone’s Monetary variable went up by one unit
# calculate new logit probabilities (Monetary+1)
Rfmdata_new <- RFMdata
Rfmdata_new$Monetary <- Rfmdata_new$Monetary+1
RFMdata$New.probabilty <- predict(model, Rfmdata_new, type = "response")

# compare mean new probability across individuals to the mean of old probabilities, and also calculate the lift metric
# mean predicted base probability
mean (RFMdata$Base.probability)
# mean new predicted probability
mean (RFMdata$New.probabilty)
# lift
(mean(RFMdata$New.probabilty)-mean(RFMdata$Base.probability))/mean(RFMdata$Base.probability)

# remove predicted purchase variable
RFMdata$Predicted.Purchase <- NULL
# data
kable(head(RFMdata,5),row.names = TRUE)
 












  


