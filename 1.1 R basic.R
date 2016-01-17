## Basic R 
setwd("C:/myfolder/files") 
# A vector of values are assigned to variable 'age'
age <- c(18, 25, 21, 33, 19)
ages
ages[3] # Extracting a specific value, here the third number
# A vector of city names are assigned to variable 'cities'
cities <- c("New York", "Chicago", "Boston", "San Francisco", "Seattle")
cities
cities[1] # Extracting only the first city in the list
A <- matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2)
B <- matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2, byrow=TRUE)
A2 <- 2*A
AplusB <- A+B
AxB <- A*B
C <- matrix(c(1,2,3,4,5,6,7,8), nrow=2, ncol=4)
AxC <- A%*%C
# See http://www.statmethods.net/advstats/matrix.html #####################################
mydata6080 <- read.csv("http://www.princeton.edu/~otorres/mydata6080.csv", header=TRUE) mydata9020 <- read.csv("http://www.princeton.edu/~otorres/mydata9020.csv", header=TRUE) mydatapol <- read.csv("http://www.princeton.edu/~otorres/mydatapol.csv", header=TRUE)
# Structure and summary
str(mydata6080)
summary(mydata6080)
# Append
mydataecon <- rbind(mydata6080, mydata9020)
table(mydataecon$year)
# Merge, will keep only perfect matches, notice the N
mydata <- merge(mydataecon, mydatapol, by=c("country","year"))
table(mydata$year)
str(mydata)
# Merging keeping only matches that are in the bigger file
# use the option all=TRUE to keep all data from both files
mydata <- merge(mydataecon, mydatapol, by=c("country","year"), all.x=TRUE) table(mydata$year)
str(mydata)
head(mydata)
tail(mydata)
summary(mydata)
# Nice summary statistics
library(stargazer)
stargazer(mydata, type="text", digits=1)
stargazer(mydata, type="text", digits=1, flip=TRUE)
# Removing cases
#mydata <- mydata[-4543:-nrow(mydata), ]
#Rescaling
mydata$export <- mydata$export/1000000
mydata$import <- mydata$import/1000000
summary(mydata[c("export","import")])
#Replacing to missing
mydata$laboredu[mydata$laboredu==0] <- NA
#Creating new variables
mydata$trade <- mydata$export + mydata$import
summary(mydata[c("trade")])
# 2 regime categories
mydata$stable <- ifelse(mydata$politics > 74, 1 , 0 )
str(mydata$stable)
summary(mydata[c("stable")])
# How many?
unique(mydata$country)
as.data.frame(levels(mydata$country))
# Creating id
library(plyr)
mydata$id <- id(mydata[c("country")], drop = TRUE)
summary(mydata[c("id","country")])
#summary(mydata[c("unemp","unempf", "unempm")])
#Identifying colnames number
as.matrix(colnames(mydata))
#Renaming
names(mydata)[names(mydata) == "old_name"] <- "new_name" #Multiple
names(mydata)[c(7,8)] <- c("new_name1","new_name2") library(plyr)
mydata <- rename(mydata, c("old_name"="new_name"))
mydata <- rename(mydata1 c("old_name1"="new_name1","old_name2"="new_name2"))
# Duplicates
duplicated(mydata[c("country","year")])
which(duplicated(mydata[c("country","year")]))
index <- which(duplicated(mydata[c("country","year")]))
mydata[-index, ] #Without duplicates
mydata1 <- mydata[-index, ] #Removing duplicates
#Selecting variables
select <- mydata[c("gdppc","trade","politics")]
summary(select)
#Subsetting
us <- subset(mydata, mydata$country=="United States")
summary(us)
us <- droplevels(us) #Removing unnecessary levels
summary(us)
# Means by group
aggregate(mydata[c("gdppc","unemp")], by=list(country=mydata$country), mean, na.rm=TRUE)
mean <- aggregate(mydata[c("gdppc","unemp")],by=list(country=mydata$country),mean, na.rm=TRUE) write.table(mean, file = "mean.txt", sep = "\t")
#mydata$mean.gdppc1 <- with(mydata, tapply(gdppc, country, mean))[mydata$country]
#Different stats
library(plyr)
ddply(mydata,'country', function(x) c(mean=mean(x$gdppc), median=median(x$gdppc)))
#Other options
mean(mydata$gdppc, na.rm=TRUE)
with(subset(mydata, country=="United States"), mean(gdppc))
#Sort
#Will print output
mydata[order(mydata$country,mydata$year),]
#Won't print output
attach(mydata)
mydata <- mydata[order(country,year), ]
detach(mydata)
#First differences per country (or group)
mydata$dif <- with(mydata, ave(gdppc, country, FUN=function(x) c(NA, diff(x)))) View(mydata[c("country","year","gdppc","dif")])
#First differences
mydata$dif1 <- ave(mydata$gdppc , FUN=function(x) c(NA,diff(x))) View(mydata[c("country","year","gdppc","dif","dif1")])
#Lag
mydata$lag <- ave(mydata$gdppc, FUN=function(x) c(NA, x[-length(x)])) View(mydata[c("country","year","gdppc","dif","dif1","lag")])
#Growth rates
mydata$growth <- with(mydata, ave(gdppc,country, FUN=function(x) c(NA, diff(x)/x[-length(x)]))) ### Scatterplots
# Using base commands
plot(us$import, us$gdppc)
plot(us$export, us$gdppc)
# Using -car- package
library(car)
scatterplot(gdppc ~ import, data=us)
scatterplot(gdppc ~ export, data=us)
scatter3d(gdppc ~ export + unemp, data=us)
# Using -ggplot2-
#install.packages("ggplot2")
#See http://docs.ggplot2.org/
library(ggplot2)
fig1 <- ggplot(us, aes(export, gdppc))
fig1 + geom_point()
# Scatterplots matrix
pairs(~gdppc + unemp + trade + +laboredu + politics, main="Variables of interest", data=mydata) pairs(~log(gdppc) + unemp + log(trade) + +laboredu + politics,main="Variables of interest (log version)",data=mydata)
# See http://www.r-bloggers.com/scatter-plot-matrices-in-r/
library(car)
scatterplotMatrix(~log(gdppc) + unemp + log(trade) + laboredu + politics, data=mydata) scatter3d(log(gdppc) ~ trade + unemp,id.n=3,data=mydata)
scatter3d(prestige ~ income + education,id.n=3,data=Duncan)
# Stars/spider/radius
w <- subset(mydata,
            country=="United States" | country=="Hong Kong SAR, China" | country=="Brazil" | country=="Belgium" | country=="Austria" | country=="Germany" | country=="Chile" | country=="UK" | country=="Mexico" | country=="Argentina")
w <- droplevels(w)
mean <- aggregate(w[c("gdppc","unemp","export","import","trade","politics","laboredu")],by=list(countr y=w$country),mean, na.rm=TRUE)
mean
mean$country <- as.character(mean$country)
stars(mean[c("gdppc", "unemp","trade", "politics","laboredu")],key.loc=c(- 0.5,7),labels=mean$country)
# Chernoff faces
# install.packages("aplpack")
library(aplpack)
faces(mean[c("gdppc", "unemp","trade", "politics","laboredu")],labels=mean$country) faces(mean[c("gdppc", "unemp","trade", "politics","laboredu")],labels=mean$country, face.type=2)
# Graphs
mydata$country.year <- paste(mydata$country, as.character(mydata$year))
head(mydata)
row.names(mydata) <- paste(mydata$country, as.character(mydata$year))
head(mydata)
plot(mydata$year,mydata$unemp, main="Unemployment", xlab="Year", ylab="Unemployment rate") identify(mydata$year,mydata$unemp,row.names(mydata)) # Interactive identification with(subset(mydata, unemp>37), text(year, unemp, labels=country.year, pos=3))
######## Two graphs separated
par(mfrow=c(1, 2))
with(subset(mydata, country=="United States"), plot(year, unemp, main="Unemployment, US", xlab="Year", ylab="Unemployment rate", ylim=c(0,12)))
with(subset(mydata, country=="United States"), lines(year, unemp, lwd=2))
with(subset(mydata, country=="United States"), abline(lm(unemp~year), col="red"))
grid()
with(subset(mydata, country=="United Kingdom"), plot(year, unemp, main="Unemployment, UK", xlab="Year", ylab="Unemployment rate", ylim=c(0,12)))
with(subset(mydata, country=="United Kingdom"), lines(year, unemp, lwd=2))
with(subset(mydata, country=="United Kingdom"), abline(lm(unemp~year), col="red"))
grid()
######## Two graphs in same region
par(mfrow=c(1, 1))
with(subset(mydata, country=="United States"), plot(year, unemp, xlab="Year", ylab="Unemployment rate", ylim=c(0,12)))
with(subset(mydata, country=="United States"), lines(year, unemp, lwd=2, col="green")) par(new=TRUE) #Previous plot stays
with(subset(mydata, country=="United Kingdom"), plot(year, unemp, main="Unemployment", xlab="Year", ylab="Unemployment rate", ylim=c(0,12), pch=8))
with(subset(mydata, country=="United Kingdom"), lines(year, unemp, lty=3, lwd=2, col="red")) grid()
legend(1963,6,c("US","UK"),lty=c(1,3),lwd=c(2,2),pch=c(1,8), col=c("green", "red"))
# Correlation matrix
cor(mydata$gdppc,mydata$unemp, use="complete.obs") # ?cor for more Pearson default cor.test(mydata$gdppc,mydata$unemp, use="complete.obs")
with(subset(mydata, country=="United States"), cor.test(gdppc,unemp, use="complete.obs")) with(subset(mydata, country=="Mexico"), cor.test(gdppc,unemp, use="complete.obs")) cor(mydata[c("gdppc","unemp","trade","politics")], use="complete.obs")
#Getting the p-values in the correlation matrix
library(psych)
corr.test(mydata[c("gdppc","unemp","trade","politics")])
#Regressions
reg1 <- lm(log(gdppc) ~ unemp + log(trade) + laboredu + politics, data=mydata) reg1
summary(reg1)
library(stargazer)
stargazer(reg1, type="text")
stargazer(reg1, type="html", out="reg1.htm") #Open with Word
confint(reg1)
gdppc.predicted <- fitted(reg1) # predicted values
mydata.residuals <- residuals(reg1) # residuals
as.data.frame(mydata.residuals)
### To add to data frame
reg2 <- lm(log(gdppc) ~ unemp + log(trade) + laboredu + politics, data=mydata, na.action="na.exclude")
summary(reg2)
mydata$gdppc.pred <- exp(fitted(reg2)) #Add to dataset View(mydata[c("country","year","gdppc","gdppc.pred")])
plot(reg1)
opar <- par()
par(mfrow = c(2, 2), oma = c(0, 0, 1.5, 0))
plot(reg1, las = 1)
residualPlots(reg1)
# Adding dummy variables
reg1a <- lm(log(gdppc) ~ unemp + log(trade) +
              factor(country), data=mydata)
stargazer(reg1,reg1a, type="text")
# Logit models
logit1 <- glm(stable ~ gdppc + unemp + trade,
              summary(logit1)
              exp(coef(logit1))
              exp(cbind(OR = coef(logit1), confint(logit1)))
              # Saving data in R format
              save(mydata,file="mydata.RData") # Saving data
              # Load data from web
              mydata <-url("http://dss.princeton.edu/training/mydata.RData")
              load(mydata)
              # Probability ~ long-term relative frequency - Die
              die <- data.frame(face = c(1:6), prob.e = rep("1/6",6), prob.e = rep(1/6,6)) die
              die$a <- ifelse(die$face>4,1,0)
              with(die, tapply(prob.e.1, a, sum))
              die1 <- sample(seq(1:6), 1000, replace=TRUE)
              table(die1)
              x <-table(die1)/1000
              d6 <- x[6]
              trials = 50
              d6 <- rep(0,trials)
              for (i in 1:trials) {
                die <- sample(seq(1:6), trials, replace=TRUE) x <-table(die)/trials
                d6[i]=x[6]
              }
              par(mfrow = c(1, 2), oma = c(0, 0, 1.5, 0))
              hist(d6, main = paste("Prob die = 6 in",trials,"trials"))
              plot(d6, type="l",ylim=c(0,0.4), main = paste("Prob die = 6 in",trials,"trials"))