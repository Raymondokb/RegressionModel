exlibrary(manipulate); library(UsingR)
library(ggplot2); data(galton)
myHist = function(mu){
     mse = mean((galton$child - mu)^2)
     g = ggplot(galton, aes(x=child)) + geom_histogram(fill = "salmon",
                                                       colour = "black",
                                                       binwidth = 1)
     g = g + geom_vline(xintercept = mu, size = 3)
     g = g + ggtitle(paste("mu = ", mu, ", MSE  = ", round(mse, 2), sep = ""))
     g
}
manipulate(myHist(mu), mu=slider(62, 74, step = 0.5))

####
#Linear Least Squares
# Least Squares Estimation
library(UsingR)
data(galton)
library(dplyr); library(ggplot2)
freqData = data.frame(table(galton$child, galton$parent))
names(freqData) = c("child", "parent", "freq")
freqData$child = as.numeric(as.character(freqData$child)) 
# ^ to unfactor it
freqData$parent = as.numeric(as.character(freqData$parent))
g = ggplot(filter(freqData, freq>0), aes(x=parent, y= child))
g = g + scale_size(range = c(2,20), guide = "none")
g = g + geom_point(colour = "grey50", aes(size = freq+20, show_guide=FALSE))
g = g + geom_point(aes(colour= freq, size = freq))
g = g + scale_colour_gradient(low = "lightblue", high="white")
g = g + geom_smooth(formula = y ~ x, method="lm")
g

y = galton$child
x = galton$parent
beta1 = cor(y, x) * sd(y) / sd(x)
beta0 = mean(y) - beta1*mean(x)
rbind(c(beta0, beta1), coef(lm(y~x)))

#centered
yc = y - mean(y);
xc = x - mean(x)
beta1 = sum(yc*xc)/ sum(xc^2)

lm(yc ~ xc - 1) 
#for without intercept


ggplot(galton, aes(x=parent, y= child)) + geom_point()
#^not the best

library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
myPlot <- function(x, y) {
     plot(x, y,
          xlab = "Father's height, normalized",
          ylab = "Son's height, normalized",
          xlim = c(-3, 3), ylim = c(-3, 3),
          bg = "lightblue", col = "black", cex = 1.1, pch = 21,
          frame = FALSE)
}
myPlot(x,y)
abline(0, 1) # if there were perfect correlation
abline(0, rho, lwd = 2) # father predicts son
abline(0, 1 / rho, lwd = 2) # son predicts father, son on vertical axis
abline(h = 0); abline(v = 0) # reference lines for no relathionship


######
library(UsingR)
data(diamond)
library(ggplot2)
g = ggplot(diamond, aes(x = carat, y = price))
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 6, colour = "black", alpha = 0.2)
g = g + geom_point(size = 5, colour = "blue", alpha = 0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g

fit= lm(price ~ carat, data = diamond)
coef(fit) # by default, includes an intercept
summary(fit)

#33721 dollar increase for every 1 unit increase in carat

#To mean center the variable
# Get a more interpretable intercept
#Use I() to make arithmetic operations
fit2 = lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)
#Note the slope stayed the same
# But intercept changed
# 500.1 is the expected price for the
# average sized diamond of the data

#To change units to one 10th of a carat
fit3 = lm(price ~ I(carat * 10), data = diamond)
coef(fit3)


# estimate the price
newx = c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2] * newx #or...
predict(fit, newdata = data.frame(carat= newx))
# need to name the df as x axis in fit model


# Residuals represent variation left unexplained by model
#Residuals are estimates of the errors.

data(diamond)
y = diamond$price;
x = diamond$carat;
n = length(y)
fit = lm(y ~ x)
e = resid(fit)
yhat = predict(fit)
max(abs(e - (y-yhat)))
max(abs(e - (y-coef(fit)[1] - coef(fit)[2] * x)))
sum(e)
#almost zero

sum(e *x)
#also has to be zero by principle
plot(diamond$carat, diamond$price,
     xlab="Mass(carat)",
     ylab="Price(SIN $)",
     bg="lightblue",
     col="black",cex=1.1,pch=21,frame=FALSE)
abline(fit, lwd = 2)
for(i in 1:n){
     lines(c(x[i],x[i]), c(y[i],yhat[i]),col="red",lwd=2)
} #residual errors visually

#plot residuals on vertical axis instead
plot(x, e,
     xlab="Mass (carats)",
     ylab="Residuals (SIN $)",
     bg="lightblue",
     col="black",cex=2,pch=21,frame=FALSE)
abline(h = 0, lwd = 2)
for(i in 1:n){
     lines(c(x[i],x[i]), c(e[i],0), col="red",lwd=2)
}
#residuals should not have a pattern

x = runif(100, -3, 3); y = x + sin(x) + rnorm(100,sd = 0.2);
library(ggplot2)
g = ggplot(data.frame(x=x, y=y), aes(x=x, y=y))
g = g + geom_smooth(method = "lm", colour = "black")
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g

##
g = ggplot(data.frame(x=x,y=resid(lm(y~x))),aes(x=x,y=y))
g = g + geom_hline(yintercept = 0, size = 2)
g = g + geom_point(size = 7, colour = "black", alpha = 0,4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g
#noticed a patter on residuals


#Now contrast using white space
x = runif(100, 0, 6); y = x + rnorm(100, mean = 0, sd = 0.001 *x)
g = ggplot(data.frame(x=x,y-y), aes(x=x,y=y))
g = g + geom_smooth(method = "lm", colour = "black")
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "blue", alpha = 0.4)
g
#seems okay right? But wait, lets look at the residuals

g = ggplot(data.frame(x=x,y=resid(lm(y~x))),aes(x=x,y=y))
g = g + geom_hline(yintercept = 0, size = 2)
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "blue", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g
#Now we see something abnormal
#So remember, always look at residual, even through graph looks perfect

#another one
diamond$e = resid(lm(price ~ carat, data = diamond))
g = ggplot(diamond, aes(x = carat, y = e))
g = g + xlab("Mass (carats)")
g = g + ylab('Residual price (SIN $)')
g = g + geom_hline(yintercept = 0, size = 2)
g = g + geom_point(size = 7, colour = "black", alpha = 0.5)
g = g + geom_point(size = 5, colour  = "blue", alpha = 0.2)
g

#Diamond residual plot
e = c(resid(lm(price ~ 1, data = diamond)),
      resid(lm(price ~ carat, data = diamond)))
fit = factor(c(rep("Itc", nrow(diamond)),
               rep("Itc, slope", nrow(diamond))))
g = ggplot(data.frame(e=e, fit=fit),aes(y=e,x=fit,fill=fit))
g = g + geom_dotplot(binaxis="y", size = 2, stackdir="center",binwidth=20)
g = g + xlab("Fitting approach")
g = g + ylab("Residual price")
g
#what we see here
# 
###
# Quiz time
###
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
#???

data(mtcars)
ff = lm(mtcars$mpg ~ mtcars$wt)
#average weight
mean(mtcars$wt)
ff$coefficients[1] + mean(mtcars$wt)*ff$coefficients[2]
#how to get the variance from the fitted model?

#weight if 3000 pounds, so in wt terms, its 3
ff$coefficients[1] + 3*ff$coefficients[2]
#again, how to construct a 95% confidence interval?


#redo#
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
summary(lm(y~x))$coef
fit = summary(lm(y~x))
fit$sigma #is the estimate of residual standard deviation

data(mtcars)
fit = lm(mpg ~ I(wt - mean(wt)), data = mtcars)
confint(fit, level = 0.95)

fit = lm(mpg ~ wt, data = mtcars)
predict(fit, newdata=data.frame(wt=3), interval = "prediction")

#try wt = 4, wt = 2 and compare the outcomes
four = fit$coefficients[1] + 2 * fit$coefficients[2]
two = fit$coefficients[1] + 4 * fit$coefficients[2]
four - two

fit1 = lm(mpg ~ wt - 1, data = mtcars)
fit2 = lm(mpg ~ wt, data = mtcars)
summary(fit1)$sigma / summary(fit2)$sigma


###
#Multiplie regression
###
n <- 100; x <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
y <- x + x2 + x3 + rnorm(n, sd = .1)
e <- function(a, b) a - sum( a * b ) / sum( b ^ 2) * b
ey <- e(e(y, x2), e(x3, x2))
ex <- e(e(x, x2), e(x3, x2))
sum(ey * ex) / sum(ex ^ 2)

coef(lm(y ~ x + x2 + x3 - 1)) #the -1 removes the intercept term

#Showing that order does not matter
ey <- e(e(y, x3), e(x2, x3))
ex <- e(e(x, x3), e(x2, x3))
sum(ey * ex) / sum(ex ^ 2)

coef(lm(y ~ x + x2 + x3 - 1)) #the -1 removes the intercept term

#####
#ctd: multivariable regression
####

n = 100; x= rnorm(n);x2 = rnorm(n); x3 = rnorm(n);
y = 1 + x + x2 + x3 + rnorm(n, sd = .01)
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))
sum(ey * ex) / sum(ex ^ 2) #regression to origin coef estimate
lm(ey ~ ex - 1) #actual regression to origin coef 
coef(lm(y ~ x + x2 + x3))

###
require(datasets)
data(swiss)

require(ggplot2); require(GGally)
# Function to return points and geom_smooth
# allow for the method to be changed
my_fn <- function(data, mapping, method="loess", ...){
     p <- ggplot(data = data, mapping = mapping) + 
          geom_point() + 
          geom_smooth(method=method, ...)
     p
}

# Default loess curve    
g = ggpairs(swiss, lower = list(continuous = my_fn))
g

summary(lm(Fertility ~ . , data = swiss))$coefficients #include all vars

#trying t-test
x = c(0.593, 0.142, 0.329, 0.691, 0.231, 0.793, 0.519, 0.392, 0.418)
#Evidenc emean is greather than 0.3?
#Null hypotheiss: mu = 0.3
#Alt hypothesis: mu > 0.3
t.test(x, alternative="greater", mu = 0.3)
# p-value of 0.029, less than 5%, so it is statistically significant

#Two sample t-test
#mu_1, mu_2
#NUll hypothesis: mu_1 - mu_2 = 0
#Alt hypothesis: mu_1 - mu_2 < 0
control = c(91, 87, 99,77, 88, 91); treat = c(101, 110, 103, 93, 99, 104)
t.test(control, treat, alternative = "less", var.equal=TRUE)
#^ assuming equal standard deviation

#if not assuming equal standard deviaton
t.test(control, treat, alternative = "less")
#both p-vlaues show that we can reject the null hypothesis (statistically significant)

#Paired t-test: a subject before/after treatment
reg = c(16, 20, 21, 22,23, 22, 27, 25, 27, 28)
prem = c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
t.test(prem, reg, alternative = "greater", paired = TRUE)
#t-stats is 4.47, p -value 0.0007
#p-value low, reject null hypotheiss
#string evidence of mean increase in gas mileage between regular and premium gasoline

c4 = mtcars[which(mtcars$cyl==4),1]; c6 = mtcars[which(mtcars$cyl==6),1]
t.test(c4, c6, alternative="greater")
#cars with cyl=4 gets greater mpg than with cyl=6, statistically significant
#reject null hypothesis that cyl=4,6 have same mpg
t.test(c4,mu=27)

#back to lesson
summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients
#agriculture is statistically significant in this case

#Regression: what variables to include
n = 100; x2 = 1:n; x1 = 0.01*x2 + runif(n, -0.01, 0.1); y = -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef

dat = data.frame(y = y, x1= x1, x2 = x2, ey = resid(lm(y~x2)), ex1 = resid(lm(x1 ~ x2)))
library(ggplot2)
g = ggplot(dat, aes(y=y, x=x1, colour = x2))
g = g + geom_point(colour = "grey50", size = 5)+ geom_smooth(method=lm,se=FALSE,colour="black")+geom_point(size=4)
g

g2 = ggplot(dat, aes(y=ey, x=ex1, colour = x2))
g2 = g2 + geom_point(colour = "grey50", size = 5)+ geom_smooth(method=lm,se=FALSE,colour="black")+geom_point(size=4)
g2
#resid y, resid x have a clear linear relation (slope +- -1)


#Sign reverses itself with inclusion of Examination and Education

#counter example
z = swiss$Agriculture + swiss$Education #no added value
lm(Fertility ~ . + z, data = swiss)
#z is NA, shows it's completely unnecessary, a linear combination of the other variables

#Dummy variables
#multi factor variable
require(datasets); data(InsectSprays); require(stats)
g = ggplot(data = InsectSprays, aes(y=count, x = spray, fill = spray))
g = g + geom_violin(colour = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g

summary(lm(count ~ spray, data = InsectSprays))$coef
#Now hard coding the dummy variables
summary(lm(count ~ I(1 * (spray=='B')) + I(1*(spray=='C'))+
                I(1*(spray=='D')) + I(1 * (spray == 'E')) + 
                I(1 * (spray =='F')), 
           data = InsectSprays))$coef

#now wrongly
summary(lm(count ~ I(1 * (spray=='B')) + I(1*(spray=='C'))+
                I(1*(spray=='D')) + I(1 * (spray == 'E')) + 
                I(1 * (spray =='F')) + I(1*(spray=='A')), 
           data = InsectSprays))$coef

summary(lm(count ~ spray - 1, data = InsectSprays))$coef
#also same interpretation as before
library(dplyr)
summarize(group_by(InsectSprays, spray), mn = mean(count))

# with factor variables
spray2 = relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef
#spray C is now the reference level

###
###
library(datasets); data(swiss)
library(dplyr)
hist(swiss$Catholic)
#creating a binary variable
swiss = mutate(swiss, CatholicBin = 1 + (Catholic > 50))
