library(ISLR)
library(MASS)
library(tree)
library(randomForest)
library(gbm)
library(glmnet)
library(ggplot2)
library(Amelia)
library(e1071) 

#Check missing 

df = read.csv("C:/Users/fudf/Desktop/STAT 684 Project/NewBankData.csv")
df[61,5]=NA
missmap(df)

##Logtransform of Y and normal plot
df_c = df[,-1]

skewness(df$currentsalary)
shapiro.test(df$currentsalary)
skewness(log(df$currentsalary))
shapiro.test(log(df$currentsalary))

qqnorm(log(df$currentsalary), pch = 1, frame = FALSE)
qqline(log(df$currentsalary), col = "steelblue", lwd = 2)

#Correlation plot
library(corrplot)
df = read.csv("C:/Users/fudf/Desktop/STAT 684 Project/NewBankData.csv")
df_c = df[,-1]
df_num=df_c[,-c(2,6,7)]
c <- cor(df_num)
corrplot(c, method="circle")

#Edu startsalary vs Y
library(vioplot)
vioplot(currentsalary~education, data=df_num)

library(ggplot2)
library(hrbrthemes)
p3 <- ggplot(df_num, aes(x=startingsalary, y=currentsalary)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()
p3
fit <- lm(currentsalary~.,data=df)
summary(fit)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)


ggplot(df_c[1:10,], aes(x=reorder(Variables, MSE), y= MSE, label = round(MSE, 1))) +
  geom_segment( aes(x=reorder(Variables, MSE), xend=reorder(Variables, MSE), y=0, yend= MSE ), color=ifelse(df_c[1:10,]$Variables %in% c("Neighborhood", "MSSubClass"), "orange", "grey"), size=ifelse(df_c[1:10,]$Variables  %in% c("Neighborhood", "MSSubClass"), 1.5, 0.7) ) +
  geom_point( color=ifelse(df_c[1:10,]$Variables %in% c("Neighborhood", "MSSubClass"), "orange", "grey"), size=ifelse(df_c[1:10,]$Variables  %in% c("Neighborhood", "MSSubClass"), 11, 7) )


step <- stepAIC(fit, direction="both")
step$anova # display results

# Calculate Relative Importance for Each Predictor
library(relaimpo)
fit2=lm(y~x)
calc.relimp(fit2,type=c("lmg"),
            rela=TRUE)


train=sample(1:nrow(df),300)

# Lets fit a random forest and see how well it performs. 
# We will use the response `medv`, the median housing value (in \$1K dollars)
rf=randomForest(currentsalary~.,data=df_c)
rf
VI_F=importance(rf)
VI_F

library(caret)
varImp(rf)
varImpPlot(rf,type=2)


boost=gbm(currentsalary~.,data=df,distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost)
boost.boston=gbm(currentsalary~.,data=df_c[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4,cv.fold=10)
gbm.perf(boost.boston, method = "cv")#587
boost=gbm(currentsalary~.,data=df_c,distribution="gaussian",n.trees=439,shrinkage=0.01,interaction.depth=4)
summary(boost)


df = read.csv("C:/Users/fudf/Desktop/STAT 684 Project/NewBankData.csv")
df = df[,-1]
x=model.matrix(currentsalary~.,df)[,-1] # take out the first column which are all 1's for intercept
y=log(df$currentsalary)
x=scale(x)
# Lasso Regression
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
lasso.best.lambda=cv.lasso$lambda.min # find the best lambda value corresponding to min cv.error
log(lasso.best.lambda)
min(cv.lasso$cvm) # min cv.error


# find the best lambda value corresponding to 1 standard error above the minimum MSE
# usually more conservative (fewer variables) solution than the minimum MSE
lasso.lambda.1se=cv.lasso$lambda.1se 
lasso_best <- glmnet(x, y, alpha = 1, lambda = lasso.lambda.1se)
a=coef(lasso_best)
plot(coef(lasso_best)[-1],xlab = "Variables", xaxt = "n",ylab = "Coefficient")
axis(1, at=1:15, labels=a@Dimnames[[1]][-1])

#Find outlier
library(bigutilsr)
df = read.csv("C:/Users/fudf/Desktop/STAT 684 Project/NewBankData.csv")
df_c = df[,-1]
df_num=df_c[,-c(2,6,7)]
X <- df_num
pca <- prcomp(X, scale. = TRUE, rank. = 10)
U <- pca$x
library(ggplot2)
theme_set(bigstatsr::theme_bigstatsr(0.8))
qplot(U[, 1], U[, 2]) + coord_equal()
apply(U, 2, function(x) which( abs(x - mean(x)) > (6 * sd(x)) ))

