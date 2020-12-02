## Data Visualisation ##

library(dslabs)
data(murders)
head(murders)

prop.table(table(heights$sex))

# define x as vector of male heights
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]

# calculate the mean and standard deviation manually
average <- sum(x)/length(x)
SD <- sqrt(sum((x - average)^2)/length(x))

# built-in mean and sd functions - note that the audio and printed values disagree
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)

# calculate standard units
z <- scale(x)

# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)


library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
# We can estimate the probability that a male is taller than 70.5 inches with:
  
1 - pnorm(70.5, mean(x), sd(x))


# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


summary(heights)

p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)


percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]


theoretical_quantiles <- qnorm(p, 69, 3)

index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)
summary(x)

# proportion of data below 69.5
mean(x <= 69.5)

# calculate observed and theoretical quantiles
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))

# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

x1 <- c(2,2,4,4)
x2 <- c(1,1,6,4)

mean(x1)
mean(x2)

sd(x1)
sd(x2)

mad(x1)
mad(x2)


sum(abs(x1-mean(x1)))/4
sum(abs(x2-mean(x2)))/4


library(tidyverse)
library(dslabs)
data("murders")

library(ggplot2)

r <- murders %>% summarise(rate = sum(total)/sum(population)*10^6) %>% .$rate

#ggplot(data=murders)

p <- murders %>% ggplot(aes(x = population/10^6, y = total, label = abb)) + 
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10()  +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

p + geom_abline(intercept = log10(r),lty=2,color="darkgray") +
  geom_point(aes(col = region), size = 3) +
  scale_color_discrete(name = "Region")
  
library(ggthemes)

p + geom_abline(intercept = log10(r),lty=2,color="darkgray") +
  geom_point(aes(col = region), size = 3) +
  scale_color_discrete(name = "Region") +
  theme_economist()

# load libraries
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)

# define the intercept
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate

# make the plot, combining all elements
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  geom_smooth() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

library(reshape2)


murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  geom_smooth() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_hue(name = "Region") +
  theme_grey()

library(ggpubr)

murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  geom_smooth() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_hue(name = "Region") +
  theme_pubr()


# Basic violin plot
p <- ggviolin(ToothGrowth, x = "dose", y = "len", add = "none")
p
# Add median_iqr
add_summary(p, "mean_sd")                                  


p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(color = gear))
# Default plot
p
# Use theme_pubr()
p + theme_pubr()
# Format labels
p + labs_pubr()


library(tidyverse)
library(dslabs)
data(gapminder)
head(gapminder,25)

gapminder %>% as_tibble()

gapminder %>% 
  filter(year == 2015 & country %in% c("Sri Lanka","Turkey")) %>% 
  select(country, infant_mortality)


filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()


filter(gapminder, year == 1962) %>%
  ggplot( aes(fertility, life_expectancy, color = continent)) +
  geom_point() 


filter(gapminder, year%in%c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent~year)

years <- c(1960, 1970, 1980, 1990, 2000, 2010)
continents <- c("Europe", "Asia")
gapminder %>% 
  filter(year %in% years & continent %in% continents) %>%
  ggplot( aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year) 

library(tidyverse)
library(dslabs)

data(heights)
head(heights)

stats <- heights %>% filter(sex == "Male") %>% summarise(average = mean(height),sd = sd(height))

data("murders")
head(murders)

us_murder_rate <- murders %>% summarize(rate = sum(total) / sum(population) * 100000)
us_murder_rate

us_murder_rate2 <- murders %>% summarize(rate = sum(total) / sum(population * 100000)) %>% .$rate

class(us_murder_rate)
class(us_murder_rate2)

group_sex <- heights %>%
  group_by(sex) %>%
  summarize(average = mean(height), st_dev = sd(height))

murders <- murders %>%
  mutate(murder_rate = total/population * 100000)
murders %>%
  group_by(region) %>%
  summarize(median_rate = median(murder_rate))


murders %>% arrange(murder_rate) %>% head(25)

murders %>% arrange(desc(murder_rate)) %>% head(25)

murders %>% arrange(region,desc(murder_rate))

library(dslabs)
data(gapminder)
head(gapminder,50)

gapminder %>% 
  filter(year %in% c(1960,1990,2015) & country %in% c("Sri Lanka", "Turkey" , "Vietnam")) %>%
  select(country, infant_mortality,life_expectancy,fertility)


ds_theme_set()    # set plot theme
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()

filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color = region)) +
  geom_point()

# facet by continent and year
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = region)) +
  geom_point() +
  facet_grid(region ~ year)

# facet by year only
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = region)) +
  geom_point() +
  facet_grid(. ~ year)

# facet by year, plots wrapped onto multiple rows
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)

gapminder %>%
  filter(country == "Brazil") %>%
  ggplot(aes(year, fertility)) +
  geom_line()


countries <- c("South Korea", "Brazil","United States","Argentina")

gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line()


gapminder <- gapminder %>% 
  mutate(gdp_per_capita = gdp/population/365) 

past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(gdp_per_capita)) +
  geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled data
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(gdp_per_capita))) +
  geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled x-axis
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(gdp_per_capita)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")


data("gapminder")

# add dollars per day variable
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

# number of regions
length(levels(gapminder$region))

# boxplot of GDP by region in 1970
past_year <- 1970
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p + geom_boxplot()

# rotate names on x-axis
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
  ggplot(aes(region, dollars_per_day, fill = continent)) +    # color by continent
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")
p

# log2 scale y-axis
p + scale_y_continuous(trans = "log2")

# add data points
p + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)


west <- c("Western Europe", "Northen Europe", "Southern Europe", "Northern America", "Australia and New Zeland")

past_year <- 1970
present_year <-  2010
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)


gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
  summarize(n = n()) %>% knitr::kable()

p <- gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)


gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))



# define gapminder
library(tidyverse)
library(dslabs)
data(gapminder)

# add additional cases
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

# define a data frame with group average income and average infant survival rate
surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

# plot infant survival versus income, with transformed axes
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) 

## Slope Plot

library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 

library(ggrepel)

dat %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_point() +
  geom_line(aes(color = country), show.legend = TRUE) +
  geom_text_repel(label=dat$country) +
  xlab("") +
  ylab("Life Expectancy") 

library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")


# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")

# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")
#Code: Line plot of measles rate by year and state

# compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")


##Titanic##

options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
data("titanic_train")
head(titanic_train)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

?titanic_train
head(titanic)
str(titanic)

p <- titanic %>%
  group_by(Sex) 



p1 <- titanic %>%
  filter(Survived == 1) %>%
  group_by(Sex)

p1+ggplot(aes(Age))
count(p)
count(p1)



p_age <- p %>% filter(Age < 17) 

count(p)
count(p_age)

49/261
51/543

133/261
251/453

summarize(ratio = count(p)/count(p_age))
count(p1)
summary(p)
mean(p)

titanic %>%
  filter(!is.na(Age) & Survived == 1) %>%
  group_by(Sex) %>%
  ggplot(aes(Age, ..count..,fill=Sex)) +
  geom_density(alpha = 0.2, bw = 3.5) + facet_grid(Sex ~ .)


titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.5, bw=2)


params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>%
  ggplot() +
  geom_qq(aes(sample=Age)) +
  geom_abline(line=params)

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()

titanic %>%
  group_by(Sex) %>%
  ggplot(aes(Sex,Survived)) +
  geom_bar(stat="identity")


#plot 1 - survival filled by sex
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar()
# plot 2 - survival filled by sex with position_dodge
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())
#plot 3 - sex filled by survival
titanic %>%
  ggplot(aes(Sex, fill = Survived)) +
  geom_bar()

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(Age, fill = Survived)) +
  geom_density(alpha=0.2) +
  facet_grid(Sex ~ .)

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(Age, fill = Survived)) +
  geom_density(alpha=0.2)

titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived,Fare, fill=Sex)) +
  geom_boxplot() +
  geom_point() +
  geom_jitter() +
  scale_y_continuous(trans="log2") +
  facet_grid(Sex ~ .) 

titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived,Fare)) +
  geom_boxplot() +
  geom_point() +
  geom_jitter() +
  scale_y_continuous(trans="log2") 

  
#1 Bar plot Class filled by survival
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar() 
  
#2 Bar plot Class with position filled by survival
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill())

#versão position_dodge não é tão boa em representar a proporção que sobreviveu em cada classe
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_dodge())

#3 Bar plot Survival with position filled by class
titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())

##Exercício 
#Create a grid of density plots for age, 
#filled by survival status, with count on the y-axis, 
#faceted by sex and passenger class.

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(Age,..count.., fill = Survived)) +
  geom_density(alpha=0.2, position = position_fill()) +
  facet_grid(Sex ~ Pclass) 

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(Age,..count.., fill = Survived)) +
  geom_density(alpha=0.2,position = "stack") +
  facet_grid(Sex ~ Pclass) 

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(Age),..count..) +
  geom_density(alpha=0.2) +
  facet_grid(Sex ~ Pclass) 

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(Age,..count.., fill = Sex)) +
  geom_density(alpha=0.2, position = position_fill()) +
  facet_grid(. ~ Pclass) 


##Properties of Stars Exercises

library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)
head(stars)
str(stars)

mean(stars$magnitude)
sd(stars$magnitude)

stars %>%
  summarize(mean=mean(magnitude),sd = sd(magnitude) )

stars %>%
  ggplot(aes(magnitude)) +
  geom_density(alpha=0.2) 
  
  
stars %>%
  ggplot(aes(temp)) +
  geom_density(alpha=0.2) 

stars %>%
  ggplot(aes(temp,magnitude)) +
  geom_point() +
  #scale_y_continuous(trans="log2") +
  scale_x_log10()
  
stars %>%
  ggplot(aes(temp,magnitude, label= temp)) +
  geom_point() +
  geom_text(aes(label= temp)) +
  geom_text_repel()+
  scale_x_log10() 
  


stars %>%
  ggplot(aes(temp,magnitude, label= magnitude)) +
  geom_point() +
  geom_text(aes(label= temp)) +
  geom_text_repel()+
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse() 


stars %>%
  #filter( temp >= 5000) %>%
  ggplot(aes(temp,magnitude,label= type)) +
  geom_point() +
  #geom_text() +
  geom_text_repel()+
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse() 

stars %>%
  #filter( temp >= 5000) %>%
  ggplot(aes(temp,magnitude, color=type)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse() 


temp_g <- c(12140,9340,6100,6100,7400,7400,7700,3200,3340,4900,4900,4900,5150,4130,3750,4590,4590) 
mean(temp_g)


library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

head(temp_carbon)
tail(temp_carbon)
temp_carbon[,1]
temp_carbon %>%
  filter(!is.na(carbon_emissions)) 

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year, temp_anomaly)


p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line()

p <- p + geom_hline(aes(yintercept = 0), col = "blue")
p

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(x =year)) +
  geom_line(aes(y= temp_anomaly)) +
  geom_hline(aes(yintercept = 0), col = "blue") +
  geom_line(aes(y= land_anomaly), color = "brown") +
  geom_line(aes(y= ocean_anomaly), color = "green")
  
p
head(greenhouse_gases,50)




greenhouse_gases %>%
  ggplot(aes(year,concentration)) +
  geom_line() +
  facet_grid(. ~ gas, scales = "free") +
  geom_vline(xintercept = 1880, color = "red") +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

head(temp_carbon) 


temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y=carbon_emissions)) +
  geom_line(aes(y=carbon_emissions)) +
  geom_line(aes(y=carbon_emissions)) +

str(temp_carbon)
years <- c(1970, 2014)

p <- temp_carbon %>%
  filter(!is.na(carbon_emissions) & year %in% years)

p

9855/4053

p <- ggplot() +
  geom_line(data = temp_carbon, aes(x=year,y=carbon_emissions)) +
  geom_line(data = greenhouse_gases, aes(year,concentration, color = gas)) +
  scale_x_log10() 

p

greenhouse_gases %>%
  ggplot(aes(year,concentration, color = gas)) +
  geom_line() +
  scale_x_log10() 
  
  
  
temp_carbon[,1]

greenhouse_gases[,1]


data(historic_co2)

head(historic_co2,25)
historic_co2[,1]


co2_time <- historic_co2 %>%
  ggplot(aes(year,co2, colour = source)) +
  geom_line() 

co2_time

co2_time + xlim(-800000,-775000)


co2_time + xlim(1750,2018)