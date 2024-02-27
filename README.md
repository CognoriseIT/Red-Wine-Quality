# Red-Wine-Quality
#Firstly install ISLR Package
install.packages("ISLR")
library(ISLR)

red_wine=read.csv("C:/Users/HP/AppData/Local/Temp/Rar$DRa3384.38279/winequality-red.csv")
red_wine
ncol(red_wine)
names(red_wine)
nrow(red_wine)
summary(red_wine)

#Running first regression on two variables
#x-variable is quality, y-variable is alcohol
plot(red_wine$quality~red_wine$alcohol, xlab="Alcohol", ylab="Quality")
reg<-lm(quality~alcohol,data=red_wine)
reg
plot(reg)

abline(v=mean(red_wine$alcohol),col="green")
abline(h=mean(red_wine$quality),col="orange")
abline(reg)
#Intuition:- This shows a positive relation between the quality of red wine
#and the content of alcohol in red wine. So, As the quantity of alcohol increases
#by 1 lt. the estimated quality rises by 0.3608 percentage point. When there is no
#content of alcohol present, the estimated quality is 1.8750 which has no economic significance.


#Running second regression on two variables
#x-variable is pH level in red wine, y-variable is chlorides in red wine
plot(red_wine$chlorides~red_wine$pH, xlab="pH level",ylab = "chlorides")
reg<-lm(chlorides~pH,data = red_wine)
reg
plot(reg)
abline(v=mean(red_wine$pH),col="red")
abline(h=mean(red_wine$chlorides),col="blue")
abline(reg)
#Intuition:- As the pH level in red wine rises by 1, the estimated chlorides
#decreases by 0.08079mmol/L. When there is no ph level present in red wine 
#the chlorides will be 0.35499mmol/L which has no economic significance.


#Making histograms for better data visualisations
hist(red_wine$quality,
     main = "Quality of Red Wine",
     xlab = "Quality",
     ylab = "frequency",
     col="purple")
hist(red_wine$alcohol,
     main = "Alcohol present in Red Wine",
     xlab = "Alcohol (in per liter)",
     ylab = "frequency",
     col="green")
hist(red_wine$chlorides,
     main = "Chlorides present in Red Wine",
     xlab = "CHlorides (in millimoles per liter (mmol/L))",
     ylab = "frequency",
     col="pink")
hist(red_wine$pH,
     main = "PH level present in Red Wine",
     xlab = "ph level",
     ylab = "frequency",
     col="blue")

#Corrplot
install.packages("corrplot")
library(corrplot)
cor_mat<-cor(red_wine)
cor_mat
corrplot(cor_mat,type="upper",order="original",tl.col="black",tl.srt = 45)
corrplot(cor_mat,type="lower",order="original",tl.col="violet",tl.srt = 45)

#For better understanding of the data, we need to install ggplot2 package
library(ggplot2)
ggplot(data = red_wine, aes(x = pH, y = chlorides)) +
  geom_line() +
  labs(title = "Relationship between chlorides and ph level in Red Wine",
       x = "chlorides",
       y = "ph level")

ggplot(red_wine,mapping=aes(x=pH,y=chlorides))+
  geom_point()

ggplot(red_wine,mapping=aes(x=pH,y=chlorides))+
  geom_point()+
  geom_area()

ggplot(red_wine,mapping=aes(x=pH,y=chlorides),colour=clarity)+
  geom_point()+
  geom_area()+
  geom_boxplot()

ggplot(red_wine,aes(x=pH,y=chlorides))+
  geom_point()+
  theme_minimal()+
  labs(title="Relationship between ph level and chlorides in Red Wine",
       x="ph level",
       y="chlorides")+
  scale_colour_brewer(palette="PuRd")
