#PREPARING THE DATA
#Import the data set into R
#Document the steps for the import process and data preparation, processing, and cleaning procedures had to be done. 
#Any R code used in the process should be included.
getwd()#get the working dictionary, and put the file into the dictionary
CDC <- read.csv(file = "CAR DETAILS FROM CAR DEKHO.csv")#one way to import a dataset into R, the file is in the working dictionary 
head(CDC)#check the dataset
class(CDC)#is a data.frame
str(CDC)#the structure of the data

#check whether the type is numeric, all are numeric which is good
is.numeric(CDC$year)
is.numeric(CDC$km_driven)
is.numeric(CDC$selling_price)

#check the number of missing values, 
sum(is.na(CDC))#The output is 0 means no missing value in this dataset

#delete rows with missing values if there is any 
CDC <- na.omit(CDC)


library(prob)
#ANALYZING THE DATA
#Do the analysis as in Module 3 for at least one categorical variable and at least one numerical variable. Show appropriate plots and properly label the plots.
CDC_fuel <- CDC$fuel#categorical variable 
table(CDC_fuel)#information about this variable 

barplot(table(CDC_fuel)/length(CDC_fuel),main = "DENDITY FOR FUEL TYPE", xlab = "Fuel Type",ylab = "Density",ylim = c(0,0.6))#density for each kind of fuel 
barplot(table(CDC_fuel), col = "green", ylim = c(0,2500),
        xlab = "Fuel Type", ylab = "Frequency", main="CAR DETAILS: FUEL TYPE")#frequency of the type of fuel 
data <- sort(table(CDC_fuel),decreasing = TRUE)#percentage of each type of fuel 
data <- data[c(4,3,2,5,1)]#arrange the types for clear presentation 
slice.labels <- names(data)
slice.percents <- round(data/sum(data)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")
slice.labels# show the percentage in string 
pie(data, labels = slice.labels, main = "fraction for each fuel")#pie chart for percentage 


CDC_price <- CDC$selling_price#numeric variable
#summary for the varable 
mean(CDC_price,trim = 0.1)#mean for the variable 
var(CDC_price)#variance
sd(CDC_price)#standard deviation 
median(CDC_price)#median
table(CDC_price)#frequency of the price, check if there is any repeat prices
range(CDC_price)#height price and lowest price
diff(range(CDC_price))#difference between height price and lowest price 
summary(CDC_price)

barplot(CDC_price, xlab="Car", ylab = "Price",main = "PRICE FOR CARS", ylim = c(0,10000000))#general representation for price
barplot(table(CDC_price), xlab = "price",ylab = "number of cars", ylim = c(0,200))# This represents how many cars for each price

#explore price distribution for both automatic cars and manual cars 
par(mfrow=c(1,2))
hist(CDC_price[CDC$transmission == "Automatic"], main = "PRICE FREQUENCY: AUTOMATIC",ylab = "frequency",xlab = "price",  ylim = c(0,250))
hist(CDC_price[CDC$transmission == "Manual"], main = "PRICE FREQUENCY: MANUAL",ylab = "frequency",xlab = "price", ylim = c(0,1500))
par(mfrow=c(1,1))

#boxplot for the summary of prices
boxplot(CDC_price, horizontal = TRUE, xaxt = "n")
axis(side = 1, at = fivenum(CDC_price), labels = TRUE)
text(fivenum(CDC_price), rep(1.2,5), srt=90, adj=0,
     labels=c("Min","Lower Hinge", "Median", "Upper Hinge", "Max"))






#Do the analysis as in Module 3 for at least one set of two or more variables. Show appropriate plots for your data.
CDC_year <- CDC$year
CDC_type <- CDC$seller_type
CDC_y_t <- table(CDC_year,CDC_type)# The contingency table between the year and seller type variables

apply(CDC_y_t, 1, sum)#number of cars for each year
apply(CDC_y_t, 2, sum)#numbers of cars for each type of seller 
margin.table(CDC_y_t)#calculate the sum of all cars 
addmargins(CDC_y_t)#calculated sums

#conditional distribution 
prop.table(CDC_y_t, 1)
prop.table(CDC_y_t, 2)
#mosaic plot 
mosaicplot(CDC_y_t, color=c("red", "blue","green"), main = "SELLER TYPE and MODEL YEARS", xlab = "model year", ylab = "seller type")
#bar plot
barplot(CDC_y_t, xlab = "seller type", 
        beside = TRUE, legend.text = TRUE,
        main = "seller type and model year",
        ylim=c(0,350), col=rainbow(27))



#Draw various random samples (using at least 3 different sample sizes) of the data and show the 
#applicability of the Central Limit Theorem for at least one variable.
CDC_km <- CDC$km_driven#variable 
hist(CDC_km, prob = TRUE)#distribution of the data
mean(CDC_km)
sd(CDC_km)

samples <- 5000
xbar = numeric(5000)
sample.size.1 <- 10
sample.size.2 <- 100
sample.size.3 <- 500
sample.size.4 <- 2000
par(mfrow = c(2,2))
for (size in c(sample.size.1, sample.size.2,sample.size.3, sample.size.4)){
        for (i in 1 : samples){
                xbar[i] <- mean(sample(CDC_km, size = size, replace = TRUE))
        }
        hist(xbar, prob = TRUE, main = paste("Sample size =", size))
        
        cat("Sample Size = ", size, " Mean = ", mean(xbar), " SD = ", sd(xbar), "\n")
}

par(mfrow = c(1,1))


#Show how various sampling methods (using at least 3 sampling methods) can be applied on your data. 
#What are your conclusions if these samples are used instead of the whole dataset
library(sampling)
table(CDC$seller_type)#using seller_type variable
#(1) simple random sampling 
sz <- 2000#sample size
s <- srswr(sz, nrow(CDC))
rows <- (1:nrow(CDC))[s!= 0]
rows <- rep(rows,s[s != 0])
sample.1 <- CDC[rows, ]
table(sample.1$seller_type)
#(2)systematic sampling 
k <- floor(nrow(CDC)/ sz)
r <- sample(k,1)
x <- seq(r, by = k, length = sz)
sample.2 <- CDC[x,]
table(sample.2$seller_type)
#(3)stratified sample 
order.index <- order(CDC$seller_type)
data <- CDC[order.index,]
freq <- table(CDC$seller_type)
sizes <- round(sz * freq / sum(freq))
st <- strata(data, stratanames = c("seller_type"), size = sizes, method = "srswor")
sample.3 <- getdata(data, st)
table(sample.3$seller_type)
#calculate mean for CDC$selling_price
#compare by mean
mean(sample.1$selling_price)#simple random sampling 
mean(sample.2$selling_price)#systematic sampling 
mean(sample.3$selling_price)#stratified sampling 
mean(CDC$selling_price)#whole data
#compare by standard deviation 
sd(sample.1$selling_price)#simple random sampling 
sd(sample.2$selling_price)#systematic sampling 
sd(sample.3$selling_price)#stratified sampling 
sd(CDC$selling_price)#whole data
#compare by density distribution
par(mfrow = c(2,2))
hist(sample.1$selling_price,main = "simple random sampling ", col = "blue")
hist(sample.2$selling_price,main = "systematic sampling", col = "green")
hist(sample.3$selling_price,main = "stratified sampling ", col = "yellow")
hist(CDC$selling_price, main = "CDC", col = "pink")
par(mfrow = c(1,1))


#EXTRA WORK
#Implementation of additional feature(s) not mentioned above
#using R to implement a decision tree algorithm 
install.packages("rpart.plot")
install.packages("rpart")
library(rpart)
library(rpart.plot)

#prepare the data
ct <- data.frame(year = CDC$year, price = CDC$selling_price, km = CDC$km_driven, 
                 fuel = CDC$fuel, seller = CDC$seller_type, trans = CDC$transmission)
str(ct)


set.seed(111)

dt <- sample(2,nrow(ct),replac = TRUE, prob = c(0.8,0.2))#resample the data to get training dataset and test dataset of  8:2
train_set <- ct[dt == 1,]#train dataset 
test_set <- ct[dt == 2,]#test dataset 

train_model <- rpart(trans~. , data = ct, method = "class")#train the model 
rpart.plot(train_model)#plot the tree model 


#prediction 
p <- predict(train_model, test_set, type = 'class')#test the trained model with the test dataset 
con_matrix <- table(test_set$trans,p)#create confusion matrix to get the result
accuracy <- sum(diag(con_matrix)) / sum(con_matrix)# calculate the accuracy 
con_matrix#confusion matrix 
accuracy


























