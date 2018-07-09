brand<-c("BMW","Mersedes")
brand<-factor(brand)
model<-c("Jeep","sedan")
model<-factor(model)
year<-c(2006,2008,2010,2012)
year<-factor(year)
color<-c("black","gray","red")
color<-factor(color)
my_list<-list(brand,model,year,color)
my_list

cand <- expand.grid(my_list)

library(AlgDesign)
set.seed(1)
# This will show 8 cars to each respondent
a1 <- optBlock(withinData= cand, blocksizes = 10, nRepeats = 100)
des <- as.data.frame(a1$design)

# Create a random data frame with 50 entries

kk <- do.call("rbind", replicate(50, des, simplify = FALSE))
kk$block <- factor(sort(rep(1:50, 8)))
kk$price <- rnorm(400,mean=22000, sd = 2500)
auto<-read.csv("auto_rep.csv")
auto$Driver<-factor(auto$Driver,levels = c("No","Yes"), labels =c("Not Driver","Driver" ))
auto$Sex<-factor(auto$Sex,levels=c("Female", "Male"), labels =c("Female","Male" ))
auto$Age<-factor(auto$Age,levels=c("18-23", "24-29" ,"30-35",">35"))
auto$Favorite.Color<-factor(auto$Favorite.Color,levels=c("Black", "Gray" ,"Red","None"))
str(auto)
auto_actual<-read.csv("auto.csv")
str(auto_actual)
View(auto)

library(ggplot2)
Cars<- c(1,2,3,4,5,6,7,8,9,10)
ActualPrice<-auto_actual$Actual.Price
Estimated_median<-auto_actual$Estimated_median
comp<-data.frame(Cars,ActualPrice,Estimated_median)
comp
library(reshape2)
# reshape your data into long format
nyxlong <- melt(comp, id=c("Cars"))

# make the plot
ggplot(nyxlong) +
  geom_bar(aes(x = Cars, y = value, fill = variable), 
           stat="identity", position = "dodge", width = 0.7) +
  scale_fill_manual("Result\n", values = c("red","blue"), 
                    labels = c("Actual Price", "Estimation")) +
  labs(x="\nCars",y="Price\n") +
  theme_bw(base_size = 14)


## before comparing estimations with actual price we shall check
#assumptions of normality 
qqnorm(auto$car.1)
qqline(auto$car.1)
## from plot we can see that there are points that are far from line,additionally
#we can do shapiro test
shapiro.test(auto$car.1)
## as we see that p-value is less than alpha we must reject Null hypothesis 
#and claim that sample is not normally distributed.
#in that case we can use Wilcox instead of one sample t.test

wilcox.test(auto$car.1, mu = 9200, alternative = "two.sided")
#Lets take  0.1 level of signiicance 
## P-values is greater than alpha, so we fail to reject Null hypothesis 
#and we can claim that people's estimation was  close to actual price.

##car.2 from now on  we will do wilcox test,
#The one-sample Wilcoxon signed rank test is a non-parametric alternative to 
#one-sample t-test when the data cannot be assumed to be normally distributed. 
#It's used to determine whether the median of the sample is equal to a known standard value
wilcox.test(auto$car.2,mu=58000,alternative = "two.sided")
## for estimation of car 2 we can assume that is far from real value.
#Actual Price is not equal to estimation
wilcox.test(auto$car.3,mu=20000,alternative = "two.sided")
#P`value is greater than alpha and we fail to reject 
#HO and we cannot claim that Estimation and actual prices are not equal. 
t.test(auto$car.4,mu=12000,alternative = "two.sided")
#P value is greater than alpha so we fail to reject null hypothesis and 
#we can claim that Actual price is not different from estimations.
wilcox.test(auto$car.5,mu=19900,alternative = "two.sided")
##From the result(P-value<Alpha) we see that variables are not equal.
wilcox.test(auto$car.6,mu=14000,alternative = "two.sided")
##P-value less than alpha and we reject Null hypothesis.
wilcox.test(auto$car.7,mu=16500,alternative="two.sided")
##p-value is greater than alpha and we can say that at this time values 
#are equal
wilcox.test(auto$car.8,mu=4200,alternative="two.sided")
##p-value is very small number and from that we may claim that our H0 is
#rejected
wilcox.test(auto$car.9,mu=6900,alternative="two.sided")
##P-value is  greater than alpha  and we can claim people
#estimated the price of car more or less right
wilcox.test(auto$car.10,mu=38000,alternative="two.sided")
##for last car estimation is far from the actual price.
#So, from the experiment we see that interestingly half of 
#people's estimation was close to the actual prices.

fit<-aov(Estimated_median~Model+Type+Color+Type:Color,data = auto_actual)
summary(fit)
##so from the result we can claim that our categorical variables have 
#affects and there is interaction.


