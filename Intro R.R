##Primeiro curso Harvard - intro R

library(dslabs)
library(tidyverse)

data("murders")

murders %>%
  ggplot(aes(population, total, label = abb, color = region)) + geom_label()


a <- 2
b <- -1
c <- -4

root_1 <- (-b + sqrt(b^2 - 4*a*c))/2*a
root_2 <- (-b - sqrt(b^2 - 4*a*c))/2*a

roots <- list( ( (-b + sqrt(b^2 - 4*a*c))/2*a), ((-b - sqrt(b^2 - 4*a*c)) / 2*a) )
roots


log(1024,4)




ls()

head(murders,20)

murders[,5] 
murders$total

class(murders)
str(murders)


data(movielens)

class(movielens)
str(movielens)

codes <- c(Italy = 380, Canada = 124, Egypt = 818)
codes

codes[1]
codes["Italy"]

class(codes)
str(codes)

codes[[1]]

names(codes)

seq(0,100,5)
a <- 1:10

x <- c(31,4,15,92,65)
sort(x)
order(x)
rank(x)


name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

tab = data.frame(name= name, distance = distance, time=time)

tab[,4] = tab["time"]/60


library(dplyr)
data("murders")

murders <- mutate(murders, rate = murders$total/murders$population*100000) # adds a column name rate to data frame murders
head(murders)
low <- filter(murders, rate <= 0.71) # filter a data table by a function that evaluate a logical vector
new_table <- select(murders,state,region, rate) # creates a subset of data table with selected columns
low_2 <- filter(new_table, rate <= 0.71)

## Pipe operator %>% ##

murders %>% select(state,region,rate) %>% filter(rate <= 0.71)

reg <- levels(murders$region)

ne <- filter(murders, region == "Northeast")
s <- filter(murders, region == "South")
nc<- filter(murders, region == "North Central")
w<- filter(murders, region == "West")

mean(ne$rate)
mean(s$rate)
mean(nc$rate)
mean(w$rate)

region_rate <- data_frame(NE = mean(ne$rate), S = mean(s$rate), NC = mean(nc$rate), W = mean(w$rate))

grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)

grades %>% select(names, exam_2) %>% filter(exam_2 >= 90)

grades %>% filter(exam_1 >= 90 | exam_2 >=90)

# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)

# a histogram of murder rates
hist(murders$rate)

# boxplots of murder rates by region
boxplot(rate~region, data = murders)

boxplot(rate~state, data = murders)


library(dslabs)
data(heights)
options(digits = 3) 


head(heights)
avg <- mean(heights$height)

library(dplyr)

tall <- heights %>% filter(height > avg)
nrow(tall)

tall_female <- heights %>% filter(height > avg & sex == "Female")
nrow(tall_female)

heights$sex == "Female"

mean(heights$sex == "Female")

min(heights$height)

match(min(heights$height),heights$height)

heights[1032,]

max(heights$height)

range(heights$height)



ma <- as.integer(max(heights$height))
mi <- as.integer(min(heights$height))
x <- mi:ma

num_in <- heights %>% filter(height %in% c(mi:ma)) %>% select(height)

sort(num_in[,1])
unique(sort(num_in[,1]))

x_1 <- x %in% unique(sort(num_in[,1]))
sum(!x_1) 
nrow(num_in)
nrow(heights)

nrow(heights) - nrow(num_in)
class(x)
class(heights$height)

sum(heights$height %in% x)
sum(heights$height != x)

heights <- heights %>% mutate(ht_cm = height * 2.54) 

heights[18,]

mean(heights$ht_cm)

heights2 <- heights %>% filter(sex == "Female")

mean(heights2$ht_cm)


library(dslabs)
data(olive)
head(olive)

plot(olive$palmitic, olive$palmitoleic)

hist(olive$eicosenoic)

boxplot(palmitic ~ region,olive)


## Basic Cinditions

ind <- which.min(murders$rate)
r <- 0.5

if (murders$rate[ind] < r){
  #message(murders$state[ind],"has murder rate bellow of", ind)
  print(paste0(murders$state[ind], " has murder rate bellow of ",ind))
} else {
  print("No State has a murder rate that low!")
}

data("na_example")
sum(is.na(na_example))

no_na <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_na))

x <-1:100

avg2 <- function(x, arithmetic = T){
  n <- length(x)
  ifelse(arithmetic,sum(x)/n,prod(x)^(1/n))
}

avg2(x)
avg2(x,F)

temp_conversion <- function(x,Fahreinheit = T){
  ifelse(Fahreinheit,5/9*(x-32),x)
}

temp <- c(32,40,45,50,55,60,65,70,75,80,85,90,95,100)

temp_c <- temp_conversion(32,F)

casaco <- function(x,Fahreinheit = T){
  if(Fahreinheit){
    c = round(5/9*(x-32),2)
    print(paste0(c, " Celsus"))
  } else {
    c = x
    print(paste0(c," Celsus"))
  }
}  


temp_f <- seq(20,110,5)
temp_c <- round(5/9*(temp_f-32),2)
temp_c2 <- round(1/2*(temp_f-32),2)
x <- 1:length(temp_f)

plot(x,temp_c,type="l",col="red",ylim = c(-10, 50))
lines(x,temp_c2,col="green")

dif_apro <- temp_c - temp_c2


plot(temp_f,dif_apro)


compute_sum_n = function(n){
  x <- 1:n
  sum(x)
}

compute_sum_n(100)
100*(100+1)/2


for (i in 1:10){
  print(i)
}

m <- 25
vec_sum <- c(rep(0,m))

for ( i in 1:m){
  vec_sum[i] <- compute_sum_n(i)
  print(vec_sum)
}

vec_sum


library(dslabs)
data(heights)
head(heights)

vec <- ifelse(heights$sex == "Male", 2,1)
sum(vec)

alt_1 <- ifelse(heights$height > 72, heights$height,0)
mean(alt_1)
 
inches_to_ft <- function(x){
  x/12
}


inches_to_ft(144)

library(dplyr)

h_5 <- heights %>% mutate(ft = inches_to_ft(heights$height)) %>% filter(ft < 5)
nrow(h_5)

