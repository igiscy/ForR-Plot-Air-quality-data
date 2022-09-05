getwd()
library("MASS")
require("MASS")
library("survival")
database1<-read.csv("data.csv")
str(database1)     #Check the variable format
#View(database1)		#Check Dataset
dim(database1)		#Check Dataset (how many observations and variables)
colnames(database1)	#Check variable names
summary(database1)	#Get the summary statistics about the object
database1$year <- as.character(database1$year)
database1$month <- as.numeric(database1$month)
database1$country_e <- as.character(database1$country_e)
database1$station <- as.character(database1$station)
database1$NOx <- as.numeric(as.character(database1$NOx))
mean(database1$NOx,na.rm=TRUE)
mean(database1$NOx[database1$country_e == "Kaohsiung"],na.rm=TRUE)
mean(database1$NOx[database1$country_e == "Pingtung"],na.rm=TRUE)

max(database1$NOx,na.rm = TRUE)
median(database1$NOx,na.rm=TRUE)
median(database1$NOx[database1$country_e == "Kaohsiung"],na.rm=TRUE)
median(database1$NOx[database1$country_e == "Pingtung"],na.rm=TRUE)
min(database1$NOx,na.rm = TRUE)

#variance
var(database1$NOx,na.rm=TRUE)
var(database1$NOx[database1$country_e == "Kaohsiung"],na.rm=TRUE)
var(database1$NOx[database1$country_e == "Pingtung"],na.rm=TRUE)

#Standard Deviation
sd(database1$NOx,na.rm=TRUE)
sd(database1$NOx[database1$country_e == "Kaohsiung"],na.rm=TRUE)
sd(database1$NOx[database1$country_e == "Pingtung"],na.rm=TRUE)

#range
range(database1$NOx,na.rm = TRUE)
range(database1$NOx[database1$country_e == "Kaohsiung"],na.rm=TRUE)
range(database1$NOx[database1$country_e == "Pingtung"],na.rm=TRUE)

#IQR
IQR(database1$NOx,na.rm = TRUE)
IQR(database1$NOx[database1$country_e == "Kaohsiung"],na.rm=TRUE)
IQR(database1$NOx[database1$country_e == "Pingtung"],na.rm=TRUE)

#quantile
quantile(database1$NOx,0.25,na.rm = TRUE)

quantile(database1$NOx,0.50,na.rm = TRUE)

quantile(database1$NOx,0.75,na.rm = TRUE)
#min 25% 50% 75% max
fivenum(database1$NOx,na.rm = TRUE) 
fivenum(database1$NOx[database1$country_e == "Kaohsiung"],na.rm=TRUE)
fivenum(database1$NOx[database1$country_e == "Pingtung"],na.rm=TRUE)


#generate a histogram/boxplot 

#install a new library "lattice"
install.packages("lattice")

# How to load a package #
library("lattice")
require("lattice")

#expression() can change a letter into subscript or superscript.
#subscript:[]   superscript:^   blankspace:~   connection:*

densityplot(~NOx ,data =database1,group =country_e ,xlab = expression(NO[x]~(ppb)),ylab ="Probability" ,main="Distrubution of NOx",plot.points =FALSE ,auto.key =list(columns=2))

hist(database1$NOx[database1$country_e == "Kaohsiung"],main="2010-2019 year histogram in Kaohsiung", xlab = expression(NO[x]~(ppb)), ylab = "Count") 
qqnorm(database1$NOx[database1$country_e == "Kaohsiung"], main="Q-Q plot in Kaohsiung")
qqline(database1$NOx[database1$country_e == "Kaohsiung"])

hist(database1$NOx[database1$country_e == "Pingtung"],main="2010-2019 year histogram in Pingtung", xlab = expression(NO[x]~(μg/m^3)), ylab = "Count") 
qqnorm(database1$NOx[database1$country_e == "Pingtung"], main="Q-Q plot in Pingtung")
qqline(database1$NOx[database1$country_e == "Pingtung"])

hist(database1$NOx,main="2010-2019 year histogram", xlab = expression(NO[x]~(μg/m^3)), ylab = "Count") 

graph1 <-histogram(x= ~NOx|year,data =database1, xlab = expression(NO[x]~(μg/m^3)),ylab = "Count",layout=c(10,1), main="Yearly histogram") 
graph2 <-densityplot(~NOx ,data =database1,group =year ,xlab = expression(NO[x]~(μg/m^3)),ylab ="Probability" ,plot.points =FALSE ,auto.key =list(columns =5), main="Yearly Distribution")
plot(graph1 ,position =c(0,.6,1,1))
plot(graph2 ,position =c(0,0,1,.6) ,newpage =FALSE)

boxplot(formula= NOx~country_e ,data =database1, xlab = "City", ylab = expression(NO[x]~(μg/m^3)), col ="red", main="Box chart by city")

par(family="STKaiti")
boxplot(formula= NOx~station ,data =database1, xlab = "Station", ylab = expression(NO[x]~(μg/m^3)), col ="red", main="Box chart by station")


boxplot(formula= NOx~year,data =database1, xlab = "Year", ylab = expression(NO[x]~(μg/m^3)), col ="blue", main="Box chart by year")

boxplot(formula= NOx~month,data =database1, xlab = "Month", ylab = expression(NO[x]~(μg/m^3)), col ="blue", main="Box chart by month")

write.table(database1,file="output.csv",sep=",",row.names=F, na = "NA", fileEncoding = "Big-5")

