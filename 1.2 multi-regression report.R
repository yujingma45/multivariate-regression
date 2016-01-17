R code
#Q1:Hotelling's T2 p214-215 PPT5
q1data <- read.csv("E:/RU courses/multivariate/report1/data1.csv", header=TRUE)
q1data
#Sample means
samplemeans = colMeans(q1data[5:10],na.rm=TRUE) round(samplemeans,2)
#covariance-variance matrix
S =cov(q1data[5:10],use = "complete.obs")#missing value round(S,2)
# Inverse of variance-covariance matrix
S.inv = solve(S)
round(S.inv,2)
# Observations
n = dim(q1data)[1]
n
# Variables
p = dim(q1data[5:10])[2]
p
# Hypothesized means
mu0 = matrix(c(1.1,1.5,1.69,2.43,1.95,2.02))
mu0
# Alpha and critical value
alpha = 0.05
alpha
critical = (((n - 1)*p)/(n - p))*qf(1-alpha,p,n-p)
round(critical,2)
#Hotelling's T2
T2 = n*t(samplemeans - mu0)%*%S.inv%*%(samplemeans - mu0) round(T2,2)
#T2 is xx, the critical values is xx. We reject the null hypothesis if T2>critical,
#therefore, in this example, we reject the null at the 10% level of significance.
#This data does not support one or more of the hypothesized values. ##T2 confidence interval
#V4
v4low = samplemeans[1] - (sqrt(critical)*sqrt(S[1,1]/n))
v4low
v4up = samplemeans[1] + (sqrt(critical)*sqrt(S[1,1]/n))
v4up
#V5
v5low = samplemeans[2] - (sqrt(critical)*sqrt(S[2,2]/n))
v5low
v5up = samplemeans[2] + (sqrt(critical)*sqrt(S[2,2]/n))
v5up
#V6
v6low =samplemeans[3] - (sqrt(critical)*sqrt(S[3,3]/n))
v6low
v6up = samplemeans[3] + (sqrt(critical)*sqrt(S[3,3]/n))
v6up
#V7
v7low = samplemeans[4] - (sqrt(critical)*sqrt(S[4,4]/n))
v7low
v7up = samplemeans[4] + (sqrt(critical)*sqrt(S[4,4]/n))
v7up
#V8
v8low = samplemeans[5] - (sqrt(critical)*sqrt(S[5,5]/n))
v8low
v8up = samplemeans[5] + (sqrt(critical)*sqrt(S[5,5]/n))
v8up
#V9
v9low = samplemeans[6] - (sqrt(critical)*sqrt(S[6,6]/n))
v9low
v9up = samplemeans[6] + (sqrt(critical)*sqrt(S[6,6]/n))
v9up
##Bonferroni method
critical1 = qt(1 - (alpha/(2*p)),n-1)
#V4
v4low1 = samplemeans[1] - (sqrt(critical1)*sqrt(S[1,1]/n))
v4low1
v4up1 = samplemeans[1] + (sqrt(critical1)*sqrt(S[1,1]/n))
v4up1
#V5
v5low1 = samplemeans[2] - (sqrt(critical1)*sqrt(S[2,2]/n)) v5low1
v5up1 = samplemeans[2] + (sqrt(critical1)*sqrt(S[2,2]/n)) v5up1
#V6
v6low1 =samplemeans[3] - (sqrt(critical1)*sqrt(S[3,3]/n)) v6low1
v6up1= samplemeans[3] + (sqrt(critical1)*sqrt(S[3,3]/n)) v6up1
#V7
v7low1 = samplemeans[4] - (sqrt(critical1)*sqrt(S[4,4]/n)) v7low1
v7up1 = samplemeans[4] + (sqrt(critical1)*sqrt(S[4,4]/n)) v7up1
#V8
v8low1 = samplemeans[5] - (sqrt(critical1)*sqrt(S[5,5]/n)) v8low1
v8up1 = samplemeans[5] + (sqrt(critical1)*sqrt(S[5,5]/n)) v8up1
#V9
v9low1 = samplemeans[6] - (sqrt(critical1)*sqrt(S[6,6]/n)) v9low1
v9up1 = samplemeans[6] + (sqrt(critical1)*sqrt(S[6,6]/n)) v9up1
##check the plausible
t2CI = data.frame(var = c("Family","Friend","Leisure time","Politics","Work","Religion"),
                  mu0 = c(1.1,1.5,1.69,2.43,1.95,2.02), lower=c(v4low,v5low,v6low,v7low,v8low,v9low),
                  upper=c(v4up,v5up,v6up,v7up,v8up,v9up)) t2CI$plausible = ifelse(t2CI$mu0>t2CI$lower & t2CI$mu0<t2CI$upper, "Yes","No")
t2CI
##check the plausible of Bonferroni method
BonCI = data.frame(var = c("Family","Friend","Leisure time","Politics","Work","Religion"),
                   mu0 = c(1.1,1.5,1.69,2.43,1.95,2.02), lower=c(v4low1,v5low1,v6low1,v7low1,v8low1,v9low1), upper=c(v4up1,v5up1,v6up1,v7up1,v8up1,v9up1))
BonCI$plausible = ifelse(BonCI$mu0>BonCI$lower & BonCI$mu0<BonCI$upper, "Yes","No")
BonCI
#Q2:large example
critical2 = qchisq(1-alpha,p)
v4low2 = samplemeans[1] - (sqrt(critical2)*sqrt(S[1,1]/n)) v4low2
v4up2 = samplemeans[1] + (sqrt(critical2)*sqrt(S[1,1]/n)) v4up2
#V5
v5low2 = samplemeans[2] - (sqrt(critical2)*sqrt(S[2,2]/n)) v5low2
v5up2 = samplemeans[2] + (sqrt(critical2)*sqrt(S[2,2]/n)) v5up2
#V6
v6low2 =samplemeans[3] - (sqrt(critical2)*sqrt(S[3,3]/n)) v6low2
v6up2= samplemeans[3] + (sqrt(critical2)*sqrt(S[3,3]/n)) v6up2
#V7
v7low2 = samplemeans[4] - (sqrt(critical2)*sqrt(S[4,4]/n)) v7low2
v7up2 = samplemeans[4] + (sqrt(critical2)*sqrt(S[4,4]/n)) v7up2
#V8
v8low2 = samplemeans[5] - (sqrt(critical2)*sqrt(S[5,5]/n)) v8low2
v8up2 = samplemeans[5] + (sqrt(critical2)*sqrt(S[5,5]/n)) v8up2
#V9
v9low2 = samplemeans[6] - (sqrt(critical2)*sqrt(S[6,6]/n)) v9low2
Appendix
v9up2 = samplemeans[6] + (sqrt(critical2)*sqrt(S[6,6]/n)) v9up2
q2 = data.frame(var = c("Family","Friend","Leisure time","Politics","Work","Religion"),
                mu0 = c(1.1,1.5,1.69,2.43,1.95,2.02), lower=c(v4low2,v5low2,v6low2,v7low2,v8low2,v9low2), upper=c(v4up2,v5up2,v6up2,v7up2,v8up2,v9up2))
q2$plausible = ifelse(q2$mu0>q2$lower & q2$mu0<q2$upper, "Yes","No")
q2
#Q3:MANOVA
mydata <- read.csv("E:/RU courses/multivariate/report1/data1.csv", header=TRUE) mydata
myvars <- c("Y002", "V96", "V97")
q3<- mydata[myvars]
q3$Y002=factor(q3$Y002)
q3 = na.omit(q3)
q3
######### #by hand# #########
# Estimating the population means
q3$pop.mean1 = mean(q3$V96)
q3$pop.mean2 = mean(q3$V97)
# Estimating the group means
q3$group.mean1 <- with(q3, tapply(V96,Y002, mean))[q3$Y002] q3$group.mean2 <- with(q3, tapply(V97,Y002, mean))[q3$Y002] # Treatment effects
q3$te1 <- q3$group.mean1 - q3$pop.mean1 q3$te2 <- q3$group.mean2 - q3$pop.mean2 # Residuals
q3$residuals1 <- q3$V96 - q3$group.mean1 q3$residuals2 <- q3$V97 - q3$group.mean2 # Deviations
q3$dev1 <- q3$V96 - q3$pop.mean1 q3$dev2 <- q3$V97 - q3$pop.mean2 q3
B = as.matrix(q3[c("te1","te2")])
B = t(B)%*%B round(B,2)
W = as.matrix(q3[c("residuals1","residuals2")]) W = t(W)%*%W
round(W,2)
BW = as.matrix(q3[c("dev1","dev2")])
BW = t(BW)%*%BW round(BW,2) #degree of freedom n = dim(q3)[1]
n
g = sum(table(levels(q3$Y002))) g
Bdf = g-1
Bdf
Wdf = n-g
Wdf
BWdf = n-1
BWdf
q3
#wilk's lambda
lambda = det(W)/det(BW) lambda
round(lambda,2)
alpha = 0.05
# Here consult J&W, table 6.3, p. 303
critical = qf(1-alpha,2*(g-1),2*(n-g-1)) round(critical,2)
F = ((1-sqrt(lambda))/sqrt(lambda))*((n-g-1)/(g-1)) round(F,2)
pval = 1 - pf(F,2*(g-1),2*(n-g-1))
round(pval,2)
#####################
# by Using manova() #
#####################
compare = manova(cbind(V96,V97)~Y002, data=q3) summary(compare, test="Wilks")
#Q4:principal components
mydata <- read.csv("E:/RU courses/multivariate/report1/data1.csv", header=TRUE) mydata
myvars <- c(125:133)
q4data<- mydata[myvars]
q4data = na.omit(q4data)
#scale the data
q4data= scale(q4data)
sigma =cov(q4data,use = "complete.obs")
sigma ##Variance-covariance matrix
#eigenvecors
# From variance-covariance to correlation matrix
rho = cov2cor(sigma)
rho
eigenvalues = eigen(rho)$values
round(eigenvalues,2)
eigenvectors = eigen(rho)$vectors
eigenvectors
round( eigenvalues/sum(eigenvalues) * 100, 2) # % Variance round( cumsum(eigenvalues)/sum(eigenvalues) * 100, 2) round( eigenvectors[,1]*sqrt(eigenvalues[1]), 3) # CorY1 and Zi pca= as.matrix(q4data)%*%eigenvectors # Estimating PCs head(pca)
cov(pca)
eigenvalues
###check:using princomp()
q4datad= princomp(q4data, cor=T)
summary(q4datad)
###Scree plot
# Helps select the number of components.
# Select components before the elbow (or bend).
# At the bend and after eigenvalues are small and same. plot(q4datad, type = "l",main="Democracy")#Q5:Factor analysis mydata <- read.csv("E:/RU courses/multivariate/report1/data1.csv", header=TRUE)
mydata
myvars <- c("V96","V97","V98","V99","V100","V101")
q5data<- mydata[myvars]
q5data = na.omit(q5data)
q5data
#1.Checking the correlation matrix
R = cor(q5data)
R
#2.standardize the data
q5data = scale(q5data, scale=FALSE)
#Note:1.Scale=False means don't divided by standard deviation #2.To normalize the data type scale(q5data)
colMeans(q5data)
#3.Estimating the correlation matrix.
#Extracting eigenvalues and eigenvectors.
eigenvalues = eigen(R)$values
round(eigenvalues,2)
round( eigenvalues/sum(eigenvalues) * 100, 2) # % of variance CPTSSC=round( cumsum(eigenvalues)/sum(eigenvalues) * 100, 2) # Cum. variance
eigenvectors = eigen(R)$vectors
eigenvectors
# Getting p
p = sum(diag(R))
p
# Estimated factor loadings
f1 = round ( sqrt(eigenvalues[1])*eigenvectors[,1], 2)
f1
f2 = round ( sqrt(eigenvalues[2])*eigenvectors[,2], 2)
f2
# Communalities
h2 = round ( f1^2 + f2^2, 2)
h2
# Specific variances (uniqueness)
psi=1-h2
psi
table=cbind(f1,f2,h2,psi)
round(eigenvalues,2)
table
CPTSSC
#4.The covariance structure of the (orthogonal) factor model L = as.matrix(cbind(f1,f2))
L
R.hat= round( L%*%t(L)+diag(psi), 2)
R.hat
##5.rotation
var.rot= varimax(L) # Rotation print(loadings(var.rot),cutoff=0)
var.rot$rotmat
round( L %*% var.rot$rotmat, 2)
## check the num of factor we chosen using scree plot fit <- princomp(q5data, cor=TRUE)
screeplot(fit)
library(psych)
scree(q5data, factors=TRUE, pc=TRUE, add=FALSE, main="Scree plot for q5 data")
#Q6:discriminant analysis
mydata <- read.csv("E:/RU courses/multivariate/report1/data1.csv", header=TRUE)
mydata
myvars <- c("V81","V238","V240","V242")
q6data<- mydata[myvars]
q6data
#recode V81
q6data$V81[q6data$V81 == 1] <- 0
q6data$V81[q6data$V81 == 2] <- 1
q6data$V81[q6data$V81 == 3] <- NA
#recode V240
q6data$V240[q6data$V240 == 1] <- 0
q6data$V240[q6data$V240 == 2] <- 1
#CLEAN DATA
q6data = na.omit(q6data)
str(q6data)
# Using the lda()
library(MASS)
lda1= lda(V81~ V238 + V240 + V242, data=q6data, prior=c(0.5,0.5)) lda1
# Using the qda()
qda1= qda(V81~ V238 + V240 + V242, data=q6data, prior=c(0.5,0.5))
qda1
###confusion matrix and apparent error rate
# Linear discriminant model
lda.pred= predict(lda1)
lda.table= table(actual = q6data$V81,predicted=lda.pred$class) lda.table
#apr round((374+194)/(390+374+194+223),2)
# Quadratic discriminant model
qda.pred= predict(qda1)
qda.table= table(actual = q6data$V81,predicted=qda.pred$class) qda.table
#apr
round((431+147)/(333+431+147+270),2)
###"HOLDOUT"
# Linear discriminant model
lda2= lda(V81~ V238 + V240 + V242, data=q6data, prior=c(0.5,0.5),CV=TRUE)
lda2.table = table(actual = q6data$V81,predicted = lda2$class) lda2.table
#aer
round((386+208)/(378+386+208+209),2)
# Quadratic discriminant model
qda2= qda(V81~ V238 + V240 + V242, data=q6data, prior=c(0.5,0.5), CV=TRUE)
qda2.table= table(actual = q6data$V81,predicted=qda2$class) qda2.table
#aer
round((436+158)/(328+436+158+259),2)
##comapre
# 1.Linear discriminant model prop.table(lda.table,1) prop.table(lda2.table,1) #Holdout
# 2.Quadratic discriminant model prop.table(qda.table,1) prop.table(qda2.table,1)#Holdout
###Testing for equality of variances in school data
#H0: equal variances.
library(biotools) boxM(cbind(q6data$V238,q6data$V240,q6data$V242),q6data$V81 )
##how to reponse
##V238-- Social class
#1-Upper class #2-Upper middle class
#3-Lower middle class #4-Working class 5##Lower class ##V240--SEX
##V242--AGE
response1=data.frame(V238=2, V240=1,V242=41) response2=data.frame(V238=2, V240=0,V242=41) response3=data.frame(V238=3, V240=1,V242=41) response4=data.frame(V238=3, V240=0,V242=41) ##1.using lda
lda.re1 = predict(lda1,response1)
lda.re1
lda.re2 = predict(lda1,response2)
lda.re2
lda.re3 = predict(lda1,response3)
lda.re3
lda.re4 = predict(lda1,response4)
lda.re4
##2.using qda
qda.rep1 = predict(qda1,response1)
qda.rep1
qda.rep2 = predict(qda1,response2)
qda.rep2
qda.rep3 = predict(qda1,response3)
qda.rep3
qda.rep4 = predict(qda1,response4)
qda.rep4
##3.using logit
logit = glm(V81~ V238 + V240 + V242, data=q6data, family=binomial(logit),
            na.action="na.exclude") summary(logit)
# test the logit model
# Difference in fit between a null and current model test = logit$null.deviance-logit$deviance
test
# Degrees of freedom of the difference test.df= logit$df.null-logit$df.residual test.df
# Significance of the statistic critical = qchisq(0.95, test.df) critical
pval= pchisq(test, test.df, lower.tail= FALSE) pval
# prediction
l1 <-predict(logit, response1, type="response") l1
l2<-predict(logit, response2, type="response") l2
l3<-predict(logit, response3, type="response") l3
l4<-predict(logit, response4, type="response")
l4
#Q7:cluster analysis
mydata <- read.csv("E:/RU courses/multivariate/report1/data1.csv", header=TRUE) mydata
myvars <- c(70:80,90:95,125:134) q7data<- mydata[myvars]
q7data = na.omit(q7data)
q7data
#single method
q7.cor = cor(q7data) # Correlation matrix q7.cor.d = dist(q7.cor)
q7.single.vars = hclust(q7.cor.d,method="single") plot(q7.single.vars)
#complete method
q7.comp.vars = hclust(q7.cor.d, method="complete") plot(q7.comp.vars)
#average method
q7.ave.vars = hclust(q7.cor.d,method="average") plot(q7.ave.vars)
#ward method
q7.ward.vars = hclust(q7.cor.d,method="ward.D2") plot(q7.ward.vars)
#compare
q7.vars = data.frame(
  vars = c("V69","V70","V71","V72","V73","V74","V75","V76","V77","V78","V7 9","V96","V97",
           "V98","V99","V100","V101","V131","V132","V133","V134","V135","V 136","V137","V138","V139","V140"),
  single = cutree(q7.single.vars, k=4), comp = cutree(q7.comp.vars, k=5), ave = cutree(q7.ave.vars, k=4), ward = cutree(q7.ward.vars, k7))
q7.vars
##CHECK the number of clusters
wss <- (nrow(q7data)-1)*sum(apply(q7data,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(q7data,centers=i)$withinss) plot(1:10, wss, type="b", xlab="Number of Clusters",
                                                                      ylab="Within groups sum of squares") abline(v=6, col="red")