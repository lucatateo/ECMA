## ---- Data import ----

# using the read.csv function
y <- read.csv("data/captures.csv",sep=";") 
class(y) # this is a data frame
head(y) # we can see the first rows of our data frame

# or from an excel file:
# we need a specific package in this case, and we need to install it:
# install.packages("xlsx") # the line is commented because you just need to run it once
library(xlsx)
# read in the worksheet named mysheet
mydata <- read.xlsx("data/database_esercizio.xls", sheetName = "captures")
class(mydata)
head(mydata)


## ---- Extract a column (i.e. a variable) -----
# we can do this using '$'
y$weight_g
# or subsetting the data frame, using square brackets '[ ]'
y["weight_g"]
y[,10] # the weight of the animals is stored in the 10th column
# Let's ask R what kind of variable we are dealing with, using the 'class' function again
class(y$weight_g)

w <- y$weight_g
w

# repeat using the data frame loaded from the Excel file
mydata$weight_g
class(mydata$weight_g) # why?



## ---- Plot the data ----

# histogram
# the function to plot an histogram is 'hist'
# let's see how it works
??histogram
?hist # this is the simplest way to get help in R! just a question mark!
hist(y$weight_g, main="", xlab="Animal weight (g)") # with default break
hist(y$weight_g, breaks=30, main="", xlab="Animal weigth (g)") # we specified a single number giving
# the number of cells for the histogram

# dotplots (or stripcharts)
# op <- par(mfrow=c(1,3))
?stripchart
stripchart(y$weight_g, xlab="Animal weigth (g)")
stripchart(y$weight_g, xlab="Animal weigth (g)", method="jitter")
stripchart(y$weight_g, xlab="Animal weigth (g)", method="stack")
# par(op)

# boxplot
boxplot(y$weight_g, ylab="Animal weigth (g)")
boxplot(y$weight_g ~ y$sex + y$age, ylab="Animal weigth (g)")
# exercise (by your own): do the same with the foot lenght
boxplot(y$footlength_mm, ylab="Foot length (mm)")

# RIPRENDERE DA QUI IL 5 APRILE 
## ---- Central tendency measures ----

## the mean and the median
weight <- na.omit(y$weight_g)
weight
# mean
mean(weight)
mean(y$weight_g)
# median
median(weight)
hist(weight,prob=T,ylim=c(0,0.05)) # prob=T for relative frequencies (density)
lines(density(rnorm(1000000,mean(weight),sd(weight))),col="red")
segments(mean(weight),0,mean(weight),0.047,col="blue")
segments(median(weight),0,median(weight),0.047,col="green")

## the mode
# R does not have a standard in-built function to calculate mode.
# So we create a user function to calculate mode of a data set in R.
# This function takes the vector as input and gives the mode value as output.
# Create the function.
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
getmode(weight)


## ---- Dispersion measures ----

## range
range(weight)

## quantile
quantile(weight) # in R, quartiles are the default for the quantile function
median(weight)
?boxplot # check the range argument and its default value
boxplot(weight, range=0)
boxplot(na.omit(y$footlength_mm))
boxplot(na.omit(y$footlength_mm), range=0)

# summary
summary(weight)

## variance
var(weight)

## standard deviation
sd(weight)
#ignora fino a 145
# why we square the differences?
m <- mean(weight)
w <- c(m+4,m+4,m-4,m-4)
op <- par(mfrow=c(1,2))
plot(w, pch=19, col="dark grey", ylim=c(22,40))
abline(h=mean(w),col="blue")
for (i in 1:length(w)) {
  segments(i,w[i],i,mean(w),col="red",lty=2)
}
differences <- w-mean(w)
differences
# sum the differences from the mean, and divide by the number of elements:
(sum(differences))/length(differences) # the negatives cancel the positives
# as the sign of differences seems to be a problem, we try and use absolute values:
sum(abs(differences))/length(differences) # this is the mean deviation
# let's try with another vector, same mean but more spread differences
w1 <- c(m+7,m+1,m-6,m-2)
plot(w1, pch=19, col="dark grey", ylim=c(22,40))
abline(h=mean(w1),col="blue")
for (i in 1:length(w1)) {
  segments(i,w1[i],i,mean(w1),col="red",lty=2)
}
par(op)
differences.w1 <- w1-mean(w1)
sum(abs(differences.w1))/length(differences.w1) # this is the mean deviation
# if we finally square the differences, the standard deviation is bigger when the differences are more spread out
sqrt(sum(differences^2)/length(differences))
sqrt(sum(differences.w1^2)/length(differences.w1))
## using R functions
var(w)
var(w1)
sd(w)
sd(w1)
# the results are slightly different because the R functions adopt a correction for finite samples

## standard error
sd(weight)/sqrt(length(weight))
sd(weight)/sqrt(119)


## ---- Outliers ----

# boxplot
op <- par(mfrow=c(1,2)) #due grafici visualizzati
boxplot(y$footlength_mm, col = "lightgray", ylim=c(10,30))
# boxplot(log(y$footlength_mm), col = "lightgray")
mtext("80", line=-1)
points(x=29)

# points()


# Cleveland plot/dotchart
dotchart(y$footlength_mm)
par(op)
par(mfrow=c(1,1))
# identify the outlier
plot(x=y$footlength_mm, y=y$capture_id)
identify(x=y$footlength_mm, y=y$capture_id)

# press Esc to stop the identify stuff
y[102,]

y$weight_g
(y$weight_g)[y$weight_g > 30]


## ---- Handling data in a data frame - common operations ----

# selecting subsets of data, according to their position or name
y[1,] # one row (the first one)
y[1:10,] # first ten rows
y[5:10,] # from 5th to 10th row
y["29",] # by row name, useful if we removed some rows from the dataset
y[c("29","45"),] # by row name, more than a row

y[,1] # one column (the first one)
y[,1:10] # first ten columns
y[,5:10] # from 5th to 10th column
y[,"chip"] # by column name (tendenzialmente colonna piu utile)
y[,c("chip","trap_id")] # by column name, more than a column (c= concateno)
y[9,c("chip","trap_id")]
# selecting subsets of data, according to their values
library(dplyr)#ci aiuta a manipolare i dati: filtrare (selezionare sulla base delle righe )
library(tidyverse)
filter(y, trap_id > 43)
# or
y %>% filter(trap_id > 43) #pipe ctrl shift n
filter(y, trap_id > 43 & occasion < 20)
filter(y, trap_id < 5 | trap_id > 65)
arrange(y, trap_id)
#ordina in base al valore di una variabile

# multiple operations
y[,c("chip","trap_id")] %>% filter(trap_id > 65) %>% arrange(trap_id)
select(y, chip, trap_id)
y %>% 
  select(chip, trap_id) %>% 
  filter(trap_id > 65) %>% 
  arrange(trap_id)

# summarizing data within groups
names(y)
y$age
y %>% group_by(age) %>% summarise(mean.w = mean(na.omit(weight_g)))
y$sex
y %>% group_by(age, sex) %>% summarise(mean.w = mean(na.omit(weight_g)))
y <-read.csv("data/captures.csv", sep=";")

