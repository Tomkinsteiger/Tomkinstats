#we will put aside zipcode, lat and long variables as these are special#

setwd("~/Desktop/Maths Project")
dir()

options(scipen = 999)

data = read.csv("kc_house_data.csv",header=TRUE)

summary(data)

data$bedrooms.factor = factor(data$bedrooms)
summary(data)

data$bathrooms.factor = factor(data$bathrooms)
summary(data)

hist(data$bedrooms)

histbathroom = hist(data$bathrooms, main='Histogram showing Bathrooms',
     xlab='Number of Bathrooms in the house', xlim=c(0,6),
     ylim=c(0,10000),las=1, breaks=6)

### Compare Distribution
hist(data$bathrooms, prob=TRUE, xlab="Number of Bathrooms in the house",
     xlim=c(0,6),
     ylim=c(0,0.6), las=1, breaks=6)
lines(density(data$bathrooms, adjust = 8) ,col="Red" , lwd=2)
bathroomnorm = rnorm(length(data$bathrooms), mean(data$bathrooms), 
                     sd(data$bathrooms))
lines(density(bathroomnorm, adjust=2.25) ,col="Blue" ,lwd=2)
legend(2.9,0.60, legend=c("Actual Distribution",
                          "Generated Normal Distribution"),
       col=c("red" ,"blue"), lty=1:1, bty="n")
###################################

histbedroom = hist(data$bedrooms, main='Histogram showing Bedrooms',
                    xlab='Number of Bedrooms in the house', xlim=c(0,8),
                    ylim=c(0,10000), las=1, breaks=8)

### Compare Distribution
hist(data$bedrooms, prob=TRUE, xlab="Number of Bathrooms in the house",
     xlim=c(0,8),
      ylim=c(0, 0.6) ,las=1, breaks=8)
lines(density(data$bedrooms, adjust = 6) ,col="Red" , lwd=2)
bedroomnorm = rnorm(length(data$bedrooms), mean(data$bedrooms), 
                    sd(data$bedrooms))
lines(density(bedroomnorm, adjust=2.25) ,col="Blue" ,lwd=2)
legend(4, 0.5, legend=c("Actual Distribution",
                         "Generated Normal Distribution"), 
       col=c("red" ,"blue"),
       lty=1:1)
####################

histprice = hist(data$price, main='Histogram Showing Price',
                 xlab='price',col='red',xlim=c(0,3000000), ylim=c(0,5000),
                 las=1, breaks=100)

#convert to prob from freq, density lines#
hist(data$price, prob=TRUE, xlab="price", xlim=c(0,2000000), las=0,
     breaks=100)
lines(density(data$price, adjust=4) ,col="Red" , lwd=2)
pricenorm = rnorm(length(data$price), mean(data$price), sd(data$price))
lines(density(pricenorm, adjust=2.25) ,col="Blue" ,lwd=2)
legend(900000, 1.5e-06, legend=c("Actual Distribution",
              "Generated Normal Distribution"), col=c("red" ,"blue"),
       lty=1:1)
### 
plot(data$price, data$bedrooms, xlim=c(0,3000000), ylim=c(0,12) ,pch=19,
     xlab='Price', ylab='Bedrooms', frame=FALSE, main='Price Vs Bedrooms')

### Log Price vs Bedroom
par(mfrow=c(2,1))
boxplot(data$price~data$bedrooms.factor, main="Bedrooms v Price",
        xlab="Number of Bedrooms",
        ylab="Price", ylim=c(0,6000000))
boxplot(log(data$price)~data$bedrooms.factor,main="Bedrooms Vs Log Price",
        xlab="Number of Bedrooms", ylab="Log Price")
###

boxplot(data$price~data$bathrooms.factor)
boxplot(log(data$price)~data$bathrooms.factor,
        main="Number of Bathrooms Vs Price")

table(data$bathrooms)

resultpricebedrooms <- lm(price~bedrooms, data=data)
summary(resultpricebedrooms)

par(mfrow=c(2,2))
plot(resultpricebedrooms)

par(mfrow=c(1,1))
hist(resid(resultpricebedrooms), nclass=200)

resultlogpricebedrooms <- lm(price.log~bedrooms, data=data)
par(mfrow=c(1,1))
hist(resid(resultlogpricebedrooms), nclass=200)

########### Graph Output
par(mfrow=c(1,2))
histbedroom = hist(data$bedrooms, main='Bedroom Histogram',
                   xlab='Number of Bedrooms in the house', xlim=c(0,8),
                   ylim=c(0,10000), las=0, breaks=8)

hist(data$bedrooms, 
     main="Bedroom Histogram with densities showing Distribution",
     prob=TRUE, xlab="Number of Bedrooms in the house", xlim=c(0,8),
     ylim=c(0, 0.6) ,las=1, breaks=8)
lines(density(data$bedrooms, adjust = 6) ,col="Red" , lwd=2)
bedroomnorm = rnorm(length(data$bedrooms), mean(data$bedrooms),
                    sd(data$bedrooms))
lines(density(bedroomnorm, adjust=2.25) ,col="Blue" ,lwd=2)
legend(3, 0.59, legend=c("Actual Distribution",
        "Generated Normal Distribution"), col=c("red" ,"blue"),
       lty=1:1, bty="n")

### correlation matrix
cormatrix <- data[3:21]
cor(cormatrix)
library(corrplot)
mycorrplot <- cor(cormatrix)
corrplot(mycorrplot)
###

#### scatter plots
a <- ggplot(data, aes(x=price, y=sqft_living)) + geom_pointdensity() + 
  geom_smooth(method='lm', formula=y~x) + xlim(0,4500000) +ylim(0,10000) +
  xlab("Price") + ylab("Sqft_Living") + 
  ggtitle('Sqft Living Scatter Density Plot')
+ theme(plot.title = element_text(hjust = 0.5))
b <- ggplot(data, aes(x=price, y=sqft_lot)) + geom_pointdensity() + 
  geom_smooth(method='lm', formula=y~x)  +xlim(0,4500000) +ylim(0,600000) + 
  ylab('Sqft_Lot') + xlab("Price") + 
  ggtitle('Sqft Living Scatter Density Plot')
+ theme(plot.title = element_text(hjust = 0.5))
c <- ggplot(data, aes(x=price, y=sqft_above)) + geom_pointdensity() + 
  geom_smooth(method='lm', formula=y~x) + xlab("Price") + xlim(0,4500000) + 
  ylab('Sqft_Above') + 
  ggtitle('Sqft Living Scatter Density Plot') + theme(plot.title = 
                                              element_text(hjust = 0.5))
d <- ggplot(data, aes(x=price, y=sqft_basement)) + geom_pointdensity() + 
  geom_smooth(method='lm', formula=y~x) + xlab("Price") + xlim(0,4500000) + 
  ylab('Sqft_Basement') +
  ylim(0,3000) + ggtitle('Sqft Living Scatter Density Plot') + 
  theme(plot.title = element_text(hjust = 0.5))

plot_grid(a,b, c, d)
###
par(mfrow=c(2,2))
boxplot(log(data$price)~data$bathrooms.factor, 
        main='Bathrooms vs Log Price',
        xlab = 'Number of Bathrooms', ylab='Log Price')
boxplot(log(data$price)~data$floors, main='Floors vs log Price',
        xlab=('Number of Floors'), ylab=('Log Price'))
boxplot(log(data$price)~data$grade, main=("Grade vs Log Price"),
        xlab =('Grade of House'), ylab=('Log Price'))
boxplot(log(data$price)~data$condition, 
        main=("Condition vs Log Price"),
        xlab =('Condition of House'), ylab=('Log Price'))

boxplot(log(data$price)~data$bedrooms.factor, 
        main='Bedrooms vs Log Price',
        xlab = 'Number of Bedrooms', ylab='Log Price')
boxplot(log(data$price)~data$view, main='View vs log Price',
        xlab=('View Rating'), ylab=('Log Price'))
boxplot(log(data$price)~data$grade, main=("Grade vs Log Price"), 
        xlab =('Grade of House'), ylab=('Log Price'))
boxplot(log(data$price)~data$condition, 
        main=("Condition vs Log Price"),
        xlab =('Condition of House'), ylab=('Log Price'))
