## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Import Tidyverse.
library(tidyverse)
library(visdat)

# Import the data set.
turtle_sales <- read.csv('turtle_sales.csv', header = TRUE)

# Print the data frame.
print(turtle_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
turtle_sales1 <- select(turtle_sales, -Ranking, -(Year:Publisher))

# View the data frame.
view(turtle_sales1)

# View the descriptive statistics.
summary(turtle_sales1)

# Use the glimpse() function.
glimpse(turtle_sales1)

# Convert 'Product' to factor (categorical variable).
turtle_sales2 <- mutate(turtle_sales1, Product = as.factor(Product))

# View the descriptive statistics.
summary(turtle_sales2)

# Check for missing values (NA).
vis_miss(turtle_sales2)

# Check for values == 0: NA_Sales = 17 entries; EU_Sales = 3 entries.
NA_Sales_0 <- filter(turtle_sales2, NA_Sales == 0)
EU_Sales_0 <- filter(turtle_sales2, EU_Sales == 0)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots:

# Relationships between sales in North America (NA) and in Europe (EU), in £M by
# platform.
qplot(NA_Sales, EU_Sales, data=turtle_sales2, colour=Platform)

## 2b) Histograms
# Create histograms:

# Global_Sales (sum of NA_sales, EU_sales and other sales), in £M.
qplot(Global_Sales, data=turtle_sales2, bins=20)

# Sales in North America (NA_sales), in £M.
qplot(NA_Sales, data=turtle_sales2, bins=20)

# Sales in Europe (EU_sales), in £M.
qplot(EU_Sales, data=turtle_sales2, bins=20)

## 2c) Boxplots
# Create boxplots:

# Global_Sales (sum of NA_sales, EU_sales and other sales; in £M). 
qplot(Global_Sales, data=turtle_sales2, geom='boxplot')
# Global_Sales by platform.
qplot(Global_Sales, Platform, data=turtle_sales2, geom='boxplot')

# Sales in North America (NA_sales; in £M). 
qplot(NA_Sales, data=turtle_sales2, geom='boxplot')
# Sales in North America by by platform.
qplot(NA_Sales, Platform, data=turtle_sales2, geom='boxplot')

# Sales in Europe (EU_sales; in £M).
qplot(EU_Sales, data=turtle_sales2, geom='boxplot')
# Sales in Europe by platform.
qplot(EU_Sales, Platform, data=turtle_sales2, geom='boxplot')
###############################################################################

# 3. Observations and insights

## Created scatterplot shows some positive moderate relationships between sales 
## in North America (NA) and in Europe (EU) with four obvious outliers: one is
## at the upper right corner and three are at the bottom center of the plot. The
## scatterplot has a cone shape meaning that the data might be heteroscedastic.
## We can see some sales for NA when EU sales=0 and vice versa; as well as the 
## data point when both NA and EU = 0 (possibly, it relates to 'other' sales).

## Global, NA and EU sales have right-skewed distribution. Majority of entries
## representing Global sales are lower than £5M. All three histograms show high 
## probable outliers (for Global and EU sales - located at the far right tale; 
## for NA - located at the middle and far right tale).

## Average EU sales are lower than average NA sales. Each set of boxplots by 
## platform (Global, NA and EU) includes a variation of symmetric, right-skewed, 
## and left-skewed unimodal data sets. For Global sales per platform: SNES has 
## the highest average sales; NES has the largest range and IQR (overall spread 
## between data and the middle 50% of the data); 59% of platforms have outliers; 
## GEN and 2600 platforms need to be investigated further as might have one 
## recorded sale and possibly are outliers. For NA sales per platform: NES has 
## the highest average sales, the largest range and IQR; 50% of platforms have 
## outliers. For EU sales per platform: PS4 has the highest average sales; Wii
## has the largest range and IQR; 55% of platforms have outliers.

###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
view(turtle_sales2)

# Check output: Determine the min, max, and mean values.
## Min.
sales_min <- data.frame(apply(turtle_sales2[, 3:5], 2, min))
colnames(sales_min) <- 'min'
sales_min

## Max.
sales_max <- data.frame(apply(turtle_sales2[, 3:5], 2, max))
colnames(sales_max) <- 'max'
sales_max

## Mean.
sales_mean <- data.frame(apply(turtle_sales2[, 3:5], 2, mean))
colnames(sales_mean) <- 'mean'
sales_mean

# View the descriptive statistics.
summary(turtle_sales2)

###############################################################################

# 2. Determine the impact on sales per product_id.

# View the frequency in Product column. 
turtle_sales2 %>% count(Product, sort = TRUE)

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
turtle_sales2_pr <- turtle_sales2 %>% group_by(Product) %>%
  summarise(sum_NA_Sales = sum(NA_Sales),
            sum_EU_Sales = sum(EU_Sales),
            sum_Global_Sales = sum(Global_Sales),
            .groups='drop')

# View the data frame.
turtle_sales2_pr <- data.frame(turtle_sales2_pr) 
view(turtle_sales2_pr)

# Explore the data frame.
summary(turtle_sales2_pr)

# Save summary output at CSV file.
sink('summary.csv')
summary(turtle_sales2_pr)
sink()

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
## Relationship between sales in North America and Europe.
ggplot(data = turtle_sales2_pr, mapping = aes(x = sum_NA_Sales, 
                                              y = sum_EU_Sales)) +
  geom_point(colour = 'blue') +
  geom_smooth(method = 'lm', se=F, col = 'red')
  labs(x = 'Sales in North America, £M', y = 'Sales in Europe, £M', 
       title = 'Relationship between sales in North America and Europe 
       for Turtle Games') +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Create histograms.
## Frequency of sales in North America.
ggplot(data = turtle_sales2_pr, mapping = aes(x = sum_NA_Sales)) +
  geom_histogram(bins = 20, colour = 'blue', fill = 'lightblue') +
  scale_y_continuous(breaks = seq(0, 60, 10)) +
  labs(x = 'Sales in North America, £M', y = 'Number of sales',
       title = 'Sales in North America', 
       subtitle = 'Number of bins = 20') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

### Sales in North America (histogram + density plot).
ggplot(data = turtle_sales2_pr, mapping = aes(x = sum_NA_Sales)) +
  geom_histogram(aes(y = ..density..), bins = 20, colour = 'blue', 
                 fill = 'lightblue') +
  geom_density(color = 'black', fill = 'orange', alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  labs(x = 'Sales in North America, £M', y = 'Density',
       title = 'Density of sales in North America', 
       subtitle = 'Number of bins = 20') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## Frequency of sales in Europe.
ggplot(data = turtle_sales2_pr, mapping = aes(x = sum_EU_Sales)) +
  geom_histogram(bins = 20, colour = 'blue', fill = 'lightblue') +
  scale_y_continuous(breaks = seq(0, 60, 10)) +
  labs(x = 'Sales in Europe, £M', y = 'Number of sales',
       title = 'Sales in Europe', 
       subtitle = 'Number of bins = 20') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

### Sales in Europe (histogram + density plot).
ggplot(data = turtle_sales2_pr, mapping = aes(x = sum_EU_Sales)) +
  geom_histogram(aes(y = ..density..), bins = 20, colour = 'blue', 
                 fill = 'lightblue') +
  geom_density(color = 'black', fill = 'orange', alpha = 0.6) +
  labs(x = 'Sales in Europe, £M', y = 'Density',
       title = 'Density of sales in Europe', 
       subtitle = 'Number of bins = 20') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## Frequency of Global sales (NA sales + EU sales + 'other' sales).
ggplot(data = turtle_sales2_pr, mapping = aes(x = sum_Global_Sales)) +
  geom_histogram(bins = 20, colour = 'blue', fill = 'lightblue') +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  labs(x = 'Global sales, £M', y = 'Number of sales',
       title = 'Global sales', 
       subtitle = 'Number of bins = 20') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

### Global sales (histogram + density plot).
ggplot(data = turtle_sales2_pr, mapping = aes(x = sum_Global_Sales)) +
  geom_histogram(aes(y = ..density..), bins = 20, colour = 'blue', 
                 fill = 'lightblue') +
  geom_density(color = 'black', fill = 'orange', alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  labs(x = 'Global sales, £M', y = 'Density',
       title = 'Density of Global sales', 
       subtitle = 'Number of bins = 20') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Create boxplots.
## Install and import 'reshape2' package.
## install.packages('reshape2')
library('reshape2')

## Convert data frame to long format.
data_long <- melt(turtle_sales2_pr)

## View new data frame.
glimpse(data_long)

## Change names in 'variable' column.
### Convert 'variable' column from factor to character data type.
data_long$variable <- as.character(data_long$variable)

### Replace values in 'variable' column.
data_long['variable'][data_long['variable'] == 'sum_NA_Sales'] <- '2. North America'
data_long['variable'][data_long['variable'] == 'sum_EU_Sales'] <- '1. Europe'
data_long['variable'][data_long['variable'] == 'sum_Global_Sales'] <- '3. Global'

### Convert 'variable' column from character to factor data type.
data_long$variable <- as.factor(data_long$variable)

## Display three boxplots (for NA, EU and Global sales) on one plot.
ggplot(data = data_long, mapping = aes(x = variable, y = value)) +
  geom_boxplot(notch = TRUE, fill = 'blue', outlier.color = 'blue') +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  labs(x = 'Regions', y = 'Sales, £M',
       title = 'Sales per region') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
  

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# North America.
ggplot(turtle_sales2_pr, aes(sample = sum_NA_Sales))+
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  scale_y_continuous(breaks = seq(0, 35, 5)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for sales in North America') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Europe.
ggplot(turtle_sales2_pr, aes(sample = sum_EU_Sales))+
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for sales in Europe') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Global.
ggplot(turtle_sales2_pr, aes(sample = sum_Global_Sales))+
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for Global sales') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test. 
## Sales in North America: p-value < 2.2e-16 => reject H0 => the data is not
## normally distributed.
shapiro.test(turtle_sales2_pr$sum_NA_Sales)

## Sales in Europe: p-value = 2.987e-16 => reject H0 => the data is not
## normally distributed.
shapiro.test(turtle_sales2_pr$sum_EU_Sales)

## Global Sales: p-value < 2.2e-16 => reject H0 => the data is not
## normally distributed.
shapiro.test(turtle_sales2_pr$sum_Global_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
## Sales in North America: skewness = 3.048198 >1 => highly skewed distribution
## (positive); kurtosis = 15.6026 >3 => leptokurtic distribution (is more peaked 
## and has fatter tails than normal distribution).
skewness(turtle_sales2_pr$sum_NA_Sales)
kurtosis(turtle_sales2_pr$sum_NA_Sales)

## Sales in Europe: skewness = 2.886029 >1 => highly skewed distribution
## (positive); kurtosis = 16.22554 >3 => leptokurtic distribution (is more peaked 
## and has fatter tails than normal distribution).
skewness(turtle_sales2_pr$sum_EU_Sales)
kurtosis(turtle_sales2_pr$sum_EU_Sales)

## Global sales: skewness = 3.066769 >1 => highly skewed distribution
## (positive); kurtosis = 17.79072 >3 => leptokurtic distribution (is more peaked 
## and has fatter tails than normal distribution).
skewness(turtle_sales2_pr$sum_Global_Sales)
kurtosis(turtle_sales2_pr$sum_Global_Sales)

## 3d) Determine correlation
# Determine correlation: correlation between NA and EU sales = 0.6209317 =>
# moderate positive correlation between variables.
cor(turtle_sales2_pr[,2:4])

## Remove outliers sum_NA_Sales and check for normalisation.
quartiles_sum_NA <- quantile(turtle_sales2_pr$sum_NA_Sales, probs=c(.25, .75), 
                       na.rm = FALSE)

IQR_sum_NA <- IQR(turtle_sales2_pr$sum_NA_Sales)

Lower_sum_NA <- quartiles_sum_NA[1] - 1.5*IQR_sum_NA
Upper_sum_NA <- quartiles_sum_NA[2] + 1.5*IQR_sum_NA

outliers_sum_NA <- subset(turtle_sales2_pr, sum_NA_Sales <= Lower_sum_NA | 
                      sum_NA_Sales >= Upper_sum_NA)
data_no_outlier_sum_NA <- subset(turtle_sales2_pr, sum_NA_Sales > Lower_sum_NA & 
                             sum_NA_Sales < Upper_sum_NA)

## Reset index.
rownames(data_no_outlier_sum_NA) <- NULL 
rownames(outliers_sum_NA) <- NULL

## View data.
outliers_sum_NA
head(data_no_outlier_sum_NA)

## Check number of rows in two new data frames vs the original data frame.
dim(outliers_sum_NA)
dim(data_no_outlier_sum_NA)
dim(turtle_sales2)

## Perform Shapiro-Wilk test. 
## Sales in North America (without outliers): p-value = 1.557e-07 
## => reject H0 => the data is not normally distributed.
shapiro.test(data_no_outlier_sum_NA$sum_NA_Sales)

## Remove outliers sum_EU_Sales and check for normalisation.
quartiles_sum_EU <- quantile(turtle_sales2_pr$sum_EU_Sales, probs=c(.25, .75), 
                         na.rm = FALSE)

IQR_sum_EU <- IQR(turtle_sales2_pr$sum_EU_Sales)

Lower_sum_EU <- quartiles_sum_EU[1] - 1.5*IQR_sum_EU
Upper_sum_EU <- quartiles_sum_EU[2] + 1.5*IQR_sum_EU

outliers_sum_EU <- subset(turtle_sales2_pr, sum_EU_Sales <= Lower_sum_EU | 
                        sum_EU_Sales >= Upper_sum_EU)
data_no_outlier_sum_EU <- subset(turtle_sales2_pr, sum_EU_Sales > Lower_sum_EU & 
                               sum_EU_Sales < Upper_sum_EU)

## Reset index.
rownames(data_no_outlier_sum_EU) <- NULL 
rownames(outliers_sum_EU) <- NULL

## View data.
outliers_sum_EU
head(data_no_outlier_sum_EU)

## Check number of rows in two new data frames vs the original data frame.
dim(outliers_sum_EU)
dim(data_no_outlier_sum_EU)
dim(turtle_sales2_pr)

## Perform Shapiro-Wilk test. 
## Sales in Europe (without outliers): p-value = 7.44e-10 => reject H0 => the 
## data is not normally distributed.
shapiro.test(data_no_outlier_sum_EU$sum_EU_Sales)

## Remove outliers sum_Global_Sales and check for normalisation.
quartiles_sum_Global <- quantile(turtle_sales2_pr$sum_Global_Sales, probs=c(.25, .75), 
                         na.rm = FALSE)

IQR_sum_Global <- IQR(turtle_sales2_pr$sum_Global_Sales)

Lower_sum_Global <- quartiles_sum_Global[1] - 1.5*IQR_sum_Global
Upper_sum_Global <- quartiles_sum_Global[2] + 1.5*IQR_sum_Global

outliers_sum_Global <- subset(turtle_sales2_pr, 
                              sum_Global_Sales <= Lower_sum_Global | 
                                sum_Global_Sales >= Upper_sum_Global)
data_no_outlier_sum_Global <- subset(turtle_sales2_pr, 
                                 sum_Global_Sales > Lower_sum_Global & 
                                   sum_Global_Sales < Upper_sum_Global)

## Reset index.
rownames(data_no_outlier_sum_Global) <- NULL 
rownames(outliers_sum_Global) <- NULL

## View data.
outliers_sum_Global
head(data_no_outlier_sum_Global)

## Check number of rows in two new data frames vs the original data frame.
dim(outliers_sum_Global)
dim(data_no_outlier_sum_Global)
dim(turtle_sales2_pr)

## Perform Shapiro-Wilk test. 
## Global sales (without outliers): p-value = 2.266e-11 => reject H0 => the data 
## is not normally distributed.
shapiro.test(data_no_outlier_sum_Global$sum_Global_Sales)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

## Normalise the data: sum_NA_Sales.
## Calculates the mathematical square root of sum_NA_Sales.
turtle_sales2_pr$sqrt_NA_Sales <- sqrt(turtle_sales2_pr$sum_NA_Sales)

## Create a Q-Q plot.
sqrt_NA_Sales <- ggplot(turtle_sales2_pr, aes(sample = sqrt_NA_Sales)) +
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for square root of sales in North America') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## View the plot using ggplotly().
library(plotly)
ggplotly(sqrt_NA_Sales)

## Perform Shapiro-Wilk test. 
## Square root of sales in North America: p-value = 6.521e-10 => reject H0 => 
## the data is not normally distributed.
shapiro.test(turtle_sales2_pr$sqrt_NA_Sales)

## Remove all outliers.
quartiles0 <- quantile(turtle_sales2_pr$sqrt_NA_Sales, probs=c(.25, .75), 
                       na.rm = FALSE)

IQR0 <- IQR(turtle_sales2_pr$sqrt_NA_Sales)

Lower0 <- quartiles0[1] - 1.5*IQR0
Upper0 <- quartiles0[2] + 1.5*IQR0

outliers0 <- subset(turtle_sales2_pr, sqrt_NA_Sales <= Lower0 | 
                      sqrt_NA_Sales >= Upper0)
data_no_outlier0 <- subset(turtle_sales2_pr, sqrt_NA_Sales > Lower0 & 
                             sqrt_NA_Sales < Upper0)

## Reset index.
rownames(data_no_outlier0) <- NULL 
rownames(outliers0) <- NULL

## View data.
outliers0
head(data_no_outlier0)

## Check number of rows in two new data frames vs the original data frame.
dim(outliers0)
dim(data_no_outlier0)
dim(turtle_sales2_pr)

## Perform Shapiro-Wilk test. 
## Square root of sales in North America (without outliers): p-value = 0.000119 
## => reject H0 => the data is not normally distributed.
shapiro.test(data_no_outlier0$sqrt_NA_Sales)

## Calculates the log of sum_NA_Sales.
turtle_sales2_pr$log_NA_Sales <- log(turtle_sales2_pr$sum_NA_Sales)

## Create a Q-Q plot.
log_NA_Sales <- ggplot(turtle_sales2_pr, aes(sample = log_NA_Sales)) +
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for log of sales in North America') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## View the plot using ggplotly().
ggplotly(log_NA_Sales)

## Perform Shapiro-Wilk test. 
## Log of sales in North America: p-value = 5.726e-07 => reject H0 => the data 
## is not normally distributed.
shapiro.test(turtle_sales2_pr$log_NA_Sales)

## Remove an obvious outlier (log_NA_Sales = -2.8134107 (row No = 124)).
turtle_sales2_pr1 <- turtle_sales2_pr[-124,]

## Reset index.
rownames(turtle_sales2_pr1) <- NULL 

## Perform Shapiro-Wilk test. 
## Log of sales in North America (without an obvious outlier): p-value = 0.005575
## => reject H0 => the data is not normally distributed.
shapiro.test(turtle_sales2_pr1$log_NA_Sales)

## Create a Q-Q plot.
log_NA_Sales2 <- ggplot(turtle_sales2_pr1, aes(sample = log_NA_Sales)) +
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for log of sales in North America 
       (without an obvious outlier)') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## View the plot using ggplotly().
ggplotly(log_NA_Sales2)

## Remove all outliers.
quartiles <- quantile(turtle_sales2_pr$log_NA_Sales, probs=c(.25, .75), 
                      na.rm = FALSE)

IQR <- IQR(turtle_sales2_pr$log_NA_Sales)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR

outliers <- subset(turtle_sales2_pr, log_NA_Sales <= Lower | 
                     log_NA_Sales >= Upper)
data_no_outlier <- subset(turtle_sales2_pr, log_NA_Sales > Lower & 
                            log_NA_Sales < Upper)

## Reset index.
rownames(data_no_outlier) <- NULL 
rownames(outliers) <- NULL

## View data.
outliers
head(data_no_outlier)

## Check number of rows in two new data frames vs the original data frame.
dim(outliers)
dim(data_no_outlier)
dim(turtle_sales2_pr)

## Perform Shapiro-Wilk test. 
## Log of sales in North America (without outliers): p-value = 0.02838 => reject 
## H0 => the data is not normally distributed.
shapiro.test(data_no_outlier$log_NA_Sales)

## Log of sales in North America (without outliers): skewness = 0.2562564 is 
## between -0.5 and 0.5 => the data is fairly symmetrical; kurtosis = 2.678189 
## is close to 3 => mesokurtic distribution (medium-tailed, so outliers are 
## neither highly frequent, nor highly infrequent).
skewness(data_no_outlier$log_NA_Sales)
kurtosis(data_no_outlier$log_NA_Sales)

## Create a Q-Q plot.
log_NA_Sales3 <- ggplot(data_no_outlier, aes(sample = log_NA_Sales)) +
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for log of sales in North America 
       (without outliers)') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## View the plot using ggplotly().
ggplotly(log_NA_Sales3)

## Inverse for severe skew.
turtle_sales2_pr$inv_NA_Sales <- (1/turtle_sales2_pr$sum_NA_Sales)

## Create a Q-Q plot.
inv_NA_Sales <- ggplot(turtle_sales2_pr, aes(sample = inv_NA_Sales)) +
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for inverse sales in North America') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## View the plot using ggplotly().
ggplotly(inv_NA_Sales)

## Perform Shapiro-Wilk test. 
## Inverse sales in North America: p-value < 2.2e-16 => reject H0 => the data is 
## not normally distributed.
shapiro.test(turtle_sales2_pr$inv_NA_Sales)

## Remove an obvious outlier (inv_NA_Sales = 16.66666667 (row No = 124)).
turtle_sales2_pr1 <- turtle_sales2_pr[-124,]

## Reset index.
rownames(turtle_sales2_pr1) <- NULL 

## Perform Shapiro-Wilk test without an obvious outlier (inv_NA_Sales = 
## 16.66666667 (row No = 124)).
## Inverse sales in North America (without an obvious outlier): p-value < 2.2e-16
## => reject H0 => the data is not normally distributed.
shapiro.test(turtle_sales2_pr1$inv_NA_Sales)

## Create a Q-Q plot.
inv_NA_Sales1 <- ggplot(turtle_sales2_pr1, aes(sample = inv_NA_Sales)) +
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for inverse sales in North America
       (without an obvious outlier)') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## View the plot using ggplotly().
ggplotly(inv_NA_Sales1)

## Remove all outliers.
quartiles1 <- quantile(turtle_sales2_pr$inv_NA_Sales, probs=c(.25, .75), 
                      na.rm = FALSE)

IQR1 <- IQR(turtle_sales2_pr$inv_NA_Sales)

Lower1 <- quartiles1[1] - 1.5*IQR1
Upper1 <- quartiles1[2] + 1.5*IQR1

outliers1 <- subset(turtle_sales2_pr, inv_NA_Sales <= Lower1 | 
                     inv_NA_Sales >= Upper1)
data_no_outlier1 <- subset(turtle_sales2_pr, inv_NA_Sales > Lower1 & 
                            inv_NA_Sales < Upper1)

## Reset index.
rownames(data_no_outlier1) <- NULL 
rownames(outliers1) <- NULL

## View data.
outliers1
head(data_no_outlier1)

## Check number of rows in two new data frames vs the original data frame.
dim(outliers1)
dim(data_no_outlier1)
dim(turtle_sales2_pr)

## Perform Shapiro-Wilk test. 
## Inverse sales in North America (without outliers): p-value = 0.0005633 => 
## reject H0 => the data is not normally distributed.
shapiro.test(data_no_outlier1$inv_NA_Sales)

## Keep log_NA_Sales and drop sqrt_NA_Sales and inv_NA_Sales.
turtle_sales2_pr <- turtle_sales2_pr[, c(1:4, 6)]

## Normalise the data: sum_EU_Sales.
## Calculates the mathematical square root of sum_EU_Sales.
turtle_sales2_pr$sqrt_EU_Sales <- sqrt(turtle_sales2_pr$sum_EU_Sales)

## Create a Q-Q plot.
sqrt_EU_Sales <- ggplot(turtle_sales2_pr, aes(sample = sqrt_EU_Sales)) +
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for square root of sales in Europe') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## View the plot using ggplotly().
ggplotly(sqrt_EU_Sales)

## Perform Shapiro-Wilk test. 
## Square root of sales in Europe: p-value = 4.123e-07 => reject H0 => the data 
## is not normally distributed.
shapiro.test(turtle_sales2_pr$sqrt_EU_Sales)

## Remove all outliers.
quartiles_EU0 <- quantile(turtle_sales2_pr$sqrt_EU_Sales, probs=c(.25, .75), 
                       na.rm = FALSE)

IQR_EU0 <- IQR(turtle_sales2_pr$sqrt_EU_Sales)

Lower_EU0 <- quartiles_EU0[1] - 1.5*IQR_EU0
Upper_EU0 <- quartiles_EU0[2] + 1.5*IQR_EU0

outliers_EU0 <- subset(turtle_sales2_pr, sqrt_EU_Sales <= Lower_EU0 | 
                      sqrt_EU_Sales >= Upper_EU0)
data_no_outlier_EU0 <- subset(turtle_sales2_pr, sqrt_EU_Sales > Lower_EU0 & 
                             sqrt_EU_Sales < Upper_EU0)

## Reset index.
rownames(data_no_outlier_EU0) <- NULL 
rownames(outliers_EU0) <- NULL

## View data.
outliers_EU0
head(data_no_outlier_EU0)

## Check number of rows in two new data frames vs the original data frame.
dim(outliers_EU0)
dim(data_no_outlier_EU0)
dim(turtle_sales2_pr)

## Perform Shapiro-Wilk test. 
## Square root of sales in Europe (without outliers): p-value = 3.581e-05 => 
## reject H0 => the data is not normally distributed.
shapiro.test(data_no_outlier_EU0$sqrt_EU_Sales)

## Calculates the log of sum_EU_Sales.
turtle_sales2_pr$log_EU_Sales <- log(turtle_sales2_pr$sum_EU_Sales)

## Remove non-finite values.
turtle_sales2_pr1 <- turtle_sales2_pr[is.finite(turtle_sales2_pr$log_EU_Sales),]

## Create a Q-Q plot.
log_EU_Sales <- ggplot(turtle_sales2_pr1, aes(sample = log_EU_Sales)) +
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for log of sales in Europe') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## View the plot using ggplotly().
ggplotly(log_EU_Sales)

## Perform Shapiro-Wilk test. 
## Log of sales in Europe: p-value = 1.144e-07 => reject H0 => the data is not
## normally distributed.
shapiro.test(turtle_sales2_pr1$log_EU_Sales)

## Remove all outliers.
quartiles_EU <- quantile(turtle_sales2_pr$log_EU_Sales, probs=c(.25, .75), 
                      na.rm = FALSE)

IQR_EU <- IQR(turtle_sales2_pr$log_EU_Sales)

Lower_EU <- quartiles_EU[1] - 1.5*IQR_EU
Upper_EU <- quartiles_EU[2] + 1.5*IQR_EU

outliers_EU <- subset(turtle_sales2_pr, log_EU_Sales <= Lower_EU | 
                     log_EU_Sales >= Upper_EU)
data_no_outlier_EU <- subset(turtle_sales2_pr, log_EU_Sales > Lower_EU & 
                            log_EU_Sales < Upper_EU)

## Reset index.
rownames(data_no_outlier_EU) <- NULL 
rownames(outliers_EU) <- NULL

## View data.
outliers_EU
head(data_no_outlier_EU)

## Check number of rows in two new data frames vs the original data frame.
dim(outliers_EU)
dim(data_no_outlier_EU)
dim(turtle_sales2_pr)

## Perform Shapiro-Wilk test. 
## Log of sales in Europe (without outliers): p-value = 0.03709 => reject H0 => 
## the data is not normally distributed.
shapiro.test(data_no_outlier_EU$log_EU_Sales)

## Log of sales in Europe (without outliers): skewness = -0.08772597 is between 
## -0.5 and 0.5 => the data is fairly symmetrical; kurtosis = 2.484654 is close 
## to 3 => mesokurtic distribution (medium-tailed, so outliers are neither highly 
## frequent, nor highly infrequent).
skewness(data_no_outlier_EU$log_EU_Sales)
kurtosis(data_no_outlier_EU$log_EU_Sales)

## Create a Q-Q plot.
log_EU_Sales1 <- ggplot(data_no_outlier_EU, aes(sample = log_EU_Sales)) +
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for log of sales in Europe
       (without outliers)') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## View the plot using ggplotly().
ggplotly(log_EU_Sales1)

## Inverse for severe skew.
turtle_sales2_pr$inv_EU_Sales <- (1/turtle_sales2_pr$sum_EU_Sales)

## Remove non-finite values.
turtle_sales2_pr2 <- turtle_sales2_pr[is.finite(turtle_sales2_pr$inv_EU_Sales),]

## Create a Q-Q plot.
inv_EU_Sales <- ggplot(turtle_sales2_pr2, aes(sample = inv_EU_Sales)) +
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for inverse sales in Europe') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## View the plot using ggplotly().
ggplotly(inv_EU_Sales)

## Perform Shapiro-Wilk test. 
## Inverse sales in Europe: p-value < 2.2e-16 => reject H0 => the data is not
## normally distributed.
shapiro.test(turtle_sales2_pr2$inv_EU_Sales)

## Remove an obvious outlier (inv_EU_Sales = 100.000000 (row No = 43)).
turtle_sales2_pr3 <- turtle_sales2_pr2[-43,]

## Reset index.
rownames(turtle_sales2_pr3) <- NULL 

## Perform Shapiro-Wilk test. 
## Inverse sales in Europe (withhout an obvious outlier): p-value = 3.87e-14 => 
## reject H0 => the data is not normally distributed.
shapiro.test(turtle_sales2_pr3$inv_EU_Sales)

## Remove all outliers.
quartiles_EU1 <- quantile(turtle_sales2_pr$inv_EU_Sales, probs=c(.25, .75), 
                       na.rm = FALSE)

IQR_EU1 <- IQR(turtle_sales2_pr$inv_EU_Sales)

Lower_EU1 <- quartiles_EU1[1] - 1.5*IQR_EU1
Upper_EU1 <- quartiles_EU1[2] + 1.5*IQR_EU1

outliers_EU1 <- subset(turtle_sales2_pr, inv_EU_Sales <= Lower_EU1 | 
                      inv_EU_Sales >= Upper_EU1)
data_no_outlier_EU1 <- subset(turtle_sales2_pr, inv_EU_Sales > Lower_EU1 & 
                             inv_EU_Sales < Upper_EU1)

## Reset index.
rownames(data_no_outlier_EU1) <- NULL 
rownames(outliers_EU1) <- NULL

## View data.
outliers_EU1
head(data_no_outlier_EU1)

## Check number of rows in two new data frames vs the original data frame.
dim(outliers_EU1)
dim(data_no_outlier_EU1)
dim(turtle_sales2_pr)

## Perform Shapiro-Wilk test. 
## Inverse sales in Europe (without outliers): p-value = 1.341e-06 => reject H0 
## => the data is not normally distributed.
shapiro.test(data_no_outlier_EU1$inv_EU_Sales)

## Keep log_EU_Sales and drop sqrt_EU_Sales and inv_EU_Sales.
turtle_sales2_pr <- turtle_sales2_pr[, c(1:5, 7)]

## Normalise the data: sum_Global_Sales.
## Calculates the mathematical square root of sum_Global_Sales.
turtle_sales2_pr$sqrt_Global_Sales <- sqrt(turtle_sales2_pr$sum_Global_Sales)

## Create a Q-Q plot.
sqrt_Global_Sales <- ggplot(turtle_sales2_pr, aes(sample = sqrt_Global_Sales)) +
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for square root of Global sales') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## View the plot using ggplotly().
ggplotly(sqrt_Global_Sales)

## Perform Shapiro-Wilk test. 
## Square root of Global sales: p-value = 6.891e-12 => reject H0 => the data is 
## not normally distributed.
shapiro.test(turtle_sales2_pr$sqrt_Global_Sales)

## Remove all outliers.
quartiles_Global0 <- quantile(turtle_sales2_pr$sqrt_Global_Sales, probs=c(.25, .75), 
                          na.rm = FALSE)

IQR_Global0 <- IQR(turtle_sales2_pr$sqrt_Global_Sales)

Lower_Global0 <- quartiles_Global0[1] - 1.5*IQR_Global0
Upper_Global0 <- quartiles_Global0[2] + 1.5*IQR_Global0

outliers_Global0 <- subset(turtle_sales2_pr, sqrt_Global_Sales <= Lower_Global0 | 
                         sqrt_Global_Sales >= Upper_Global0)
data_no_outlier_Global0 <- subset(turtle_sales2_pr, 
                                  sqrt_Global_Sales > Lower_Global0 & 
                                    sqrt_Global_Sales < Upper_Global0)

## Reset index.
rownames(data_no_outlier_Global0) <- NULL 
rownames(outliers_Global0) <- NULL

## View data.
outliers_Global0
head(data_no_outlier_Global0)

## Check number of rows in two new data frames vs the original data frame.
dim(outliers_Global0)
dim(data_no_outlier_Global0)
dim(turtle_sales2_pr)

## Perform Shapiro-Wilk test. 
## Square root of Global sales (without outliers): p-value = 1.792e-09 => reject 
## H0 => the data is not normally distributed.
shapiro.test(data_no_outlier_Global0$sqrt_Global_Sales)

## Calculates the log of sum_Global_Sales.
turtle_sales2_pr$log_Global_Sales <- log(turtle_sales2_pr$sum_Global_Sales)

## Create a Q-Q plot.
log_Global_Sales <- ggplot(turtle_sales2_pr, aes(sample = log_Global_Sales)) +
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for log of Global sales') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## View the plot using ggplotly().
ggplotly(log_Global_Sales)

## Perform Shapiro-Wilk test. 
## Log of Global Sales: p-value = 4.953e-07 => reject H0 => the data is not
## normally distributed.
shapiro.test(turtle_sales2_pr$log_Global_Sales)

## Remove all outliers.
quartiles_Global <- quantile(turtle_sales2_pr$log_Global_Sales, probs=c(.25, .75), 
                         na.rm = FALSE)

IQR_Global <- IQR(turtle_sales2_pr$log_Global_Sales)

Lower_Global <- quartiles_Global[1] - 1.5*IQR_Global
Upper_Global <- quartiles_Global[2] + 1.5*IQR_Global

outliers_Global <- subset(turtle_sales2_pr, log_Global_Sales <= Lower_Global | 
                        log_Global_Sales >= Upper_Global)
data_no_outlier_Global <- subset(turtle_sales2_pr, 
                                 log_Global_Sales > Lower_Global & 
                                   log_Global_Sales < Upper_Global)

## Reset index.
rownames(data_no_outlier_Global) <- NULL 
rownames(outliers_Global) <- NULL

## View data.
outliers_Global
head(data_no_outlier_Global)

## Check number of rows in two new data frames vs the original data frame.
dim(outliers_Global)
dim(data_no_outlier_Global)
dim(turtle_sales2_pr)

## Perform Shapiro-Wilk test. 
## Log of Global Sales (without outliers): p-value = 1.146e-06 => reject H0 => 
## the data is not normally distributed.
shapiro.test(data_no_outlier_Global$log_Global_Sales)

## Inverse for severe skew.
turtle_sales2_pr$inv_Global_Sales <- (1/turtle_sales2_pr$sum_Global_Sales)

## Create a Q-Q plot.
inv_Global_Sales <- ggplot(turtle_sales2_pr, aes(sample = inv_Global_Sales)) +
  stat_qq(size=3, alpha=0.5) +
  stat_qq_line(size=1, color = 'red') +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles',
       title = 'Normal Q-Q Plot for inverse Global sales') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

## View the plot using ggplotly().
ggplotly(inv_Global_Sales)

## Perform Shapiro-Wilk test. 
## Inverse Global sales: p-value = 4.156e-05 => reject H0 => the data is not
## normally distributed.
shapiro.test(turtle_sales2_pr$inv_Global_Sales)

## Remove all outliers.
quartiles_Global1 <- quantile(turtle_sales2_pr$inv_Global_Sales, probs=c(.25, .75), 
                          na.rm = FALSE)

IQR_Global1 <- IQR(turtle_sales2_pr$inv_Global_Sales)

Lower_Global1 <- quartiles_Global1[1] - 1.5*IQR_Global1
Upper_Global1 <- quartiles_Global1[2] + 1.5*IQR_Global1

outliers_Global1 <- subset(turtle_sales2_pr, inv_Global_Sales <= Lower_Global1 | 
                         inv_Global_Sales >= Upper_Global1)
data_no_outlier_Global1 <- subset(turtle_sales2_pr, 
                                  inv_Global_Sales > Lower_Global1 & 
                                    inv_Global_Sales < Upper_Global1)

## Reset index.
rownames(data_no_outlier_Global1) <- NULL 
rownames(outliers_Global1) <- NULL

## View data (no outliers were identified).
outliers_Global1
head(data_no_outlier_Global1)

## Drop sqrt_Global_Sales, log_Global_Sales and inv_Global_Sales.
turtle_sales2_pr <- turtle_sales2_pr[, c(1:6, 8)]

## Scatterplot: Relationship between log of sales in North America and Europe.
ggplot(data = turtle_sales2_pr, mapping = aes(x = log_NA_Sales, 
                                              y = log_EU_Sales)) +
  geom_point(colour = 'blue') +
  labs(x = 'Log of sales in North America', y = 'Log of sales in Europe', 
       title = 'Relationship between log of sales in North America and Europe 
       for Turtle Games') +
  geom_smooth(method = 'lm', se=F, col = 'red')
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Replace '-Inf' values in log_EU_Sales column with 0.
turtle_sales2_pr$log_EU_Sales <- ifelse(is.finite(turtle_sales2_pr$log_EU_Sales), 
                                        turtle_sales2_pr$log_EU_Sales, 0)

# Determine correlation.
cor(turtle_sales2_pr[,2:7])

# Save correlation output at CSV file.
sink('cor.csv')
cor(turtle_sales2_pr[,2:7])
sink()

# Install GGally package.
# install.packages('GGally')

# Load GGally.
library(GGally)

# Apply ggpairs function.
ggpairs(turtle_sales2_pr[,2:7],
        lower=list(continuous='smooth'),
        diag=list(continuous='barDiag')) 

###############################################################################

# 5. Observations and insights

# 41.14% of all unique products were mentioned more than twice in the provided 
# data set. The decision was made to aggregate data by unique product which has 
# reduced the number of observations by 49.72%, from 352 to 175 observations. It 
# also affected values of descriptive statistics for sales columns, in particular
# mean and median of all sales columns have increased.

# The sales columns in the new aggregated data frame were investigated for the
# normalisation. All three variables (sum_NA_Sales, sum_EU_Sales and 
# sum_Global_Sales) have highly skewed (positive) leptokurtic distribution. 
# Removing outliers did not help to normalise data. Three additional methods,  
# square root, log and inverse of data, were employed in order to normalise data.
# Log method in combination with removing outliers gave the best result for 
# sum_NA_Sales and sum_EU_Sales. I was unable to normalise sum_Global_Sales data.

# Correlation between NA and EU sales = 0.6209317 => moderate positive correlation 
# between variables, but it has to be checked further as data is not normally 
# distributed. Correlation between log NA and log EU sales = 0.4420464 => low 
# positive correlation between variables, but it has to be checked further as 
# data has outliers and is not normally distributed.

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
View(turtle_sales2_pr)

# Determine a summary of the data frame.
summary(turtle_sales2_pr)

###############################################################################

# 2. Create a simple linear regression model
### y = sum_NA_Sales and x = sum_EU_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model1 <- lm(turtle_sales2_pr$sum_NA_Sales ~ turtle_sales2_pr$sum_EU_Sales)

# View the result of the model.
summary(model1)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model1))
qqline(residuals(model1), col='red')

### y = sum_NA_Sales (without outliers) and x = sum_EU_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model0 <- lm(data_no_outlier_sum_NA$sum_NA_Sales ~ 
               data_no_outlier_sum_NA$sum_EU_Sales)

# View the result of the model.
summary(model0)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model0))
qqline(residuals(model0), col='red')

### y = log_NA_Sales and x = sum_EU_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model2 <- lm(turtle_sales2_pr$log_NA_Sales ~ turtle_sales2_pr$sum_EU_Sales)

# View the result of the model.
summary(model2)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model2))
qqline(residuals(model2), col='red')

### y = log_NA_Sales (without outliers) and x = sum_EU_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model3 <- lm(data_no_outlier$log_NA_Sales ~ data_no_outlier$sum_EU_Sales)

# View the result of the model.
summary(model3)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model3))
qqline(residuals(model3), col='red')

## Compare four models:
## Arrange four plots (model1, model0, model2 and model3) underneath each other.
par(mfrow=c(4, 1))

## Use the plot() function to create the visualisation for model1.
plot(turtle_sales2_pr$sum_EU_Sales, turtle_sales2_pr$sum_NA_Sales)

## Add a trendline with the abline() function.
abline(coefficients(model1), col = 'red')

## Repeat the above steps for model0, but set the line colour as orange.
plot(data_no_outlier_sum_NA$sum_EU_Sales, data_no_outlier_sum_NA$sum_NA_Sales)
abline(coefficients(model0), col = 'orange')

## Repeat the above steps for model2, but set the line colour as blue.
plot(turtle_sales2_pr$sum_EU_Sales, turtle_sales2_pr$log_NA_Sales)
abline(coefficients(model2), col = 'blue')

## Repeat the above steps for model3, but set the line colour as green.
plot(data_no_outlier$sum_EU_Sales, data_no_outlier$log_NA_Sales)
abline(coefficients(model3), col = 'green')

## Check chosen models for heteroscedasticity (perform a Breusch-Pagan Test).
# Load lmtest library.
library(lmtest)

# Perform Breusch-Pagan Test.
bptest(model1)
bptest(model0)
bptest(model2)
bptest(model3)

# Compare models:

# model1: p value <2e-16; R^2 value = 0.3856; Residual standard error: 3.582 on 
# 173 degrees of freedom; BP test p-value = 0.08526 > 0.05 => we fail to reject 
# the H0 ( => heteroscedasticity is not present);

# model0: p value 1.58e-09; R^2 value = 0.2089; Residual standard error: 1.882 on 
# 156 degrees of freedom; BP test p-value = 9.674e-11 < 0.05 => we reject the 
# H0 ( => heteroscedasticity is present);

# model2: p value 1.54e-11; R^2 value = 0.2318; Residual standard error: 0.6771 
# on 173 degrees of freedom; BP test p-value = 0.5233 > 0.05 => we fail to reject 
# the H0 ( => heteroscedasticity is not present);

# model3: p value 6.05e-14; R^2 value = 0.2915; Residual standard error: 0.4911 
# on 164 degrees of freedom; BP test p-value = 3.369e-05 < 0.05 => we reject the 
# H0 ( => heteroscedasticity is present).

# Model0 and Model3 show the presence of heteroscedasticity, so we will look at 
# other two models. Model1 has higher R^2 value than model2 but Residual standard 
# error is much higher also, meaning that model1 fits the data worse than model2. 
# Therefore model2 is the best out of four models for predicting the 
# log of NA sales (which we can use to predict NA sales as a final output).

### y = sum_EU_Sales and x = sum_NA_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model1_1 <- lm(turtle_sales2_pr$sum_EU_Sales ~ turtle_sales2_pr$sum_NA_Sales)

# View the result of the model.
summary(model1_1)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model1_1))
qqline(residuals(model1_1), col='red')

### y = sum_EU_Sales (without outliers) and x = sum_NA_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model0_1 <- lm(data_no_outlier_sum_EU$sum_EU_Sales ~ 
                 data_no_outlier_sum_EU$sum_NA_Sales)

# View the result of the model.
summary(model0_1)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model0_1))
qqline(residuals(model0_1), col='red')

### y = log_EU_Sales and x = sum_NA_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model2_1 <- lm(turtle_sales2_pr$log_EU_Sales ~ turtle_sales2_pr$sum_NA_Sales)

# View the result of the model.
summary(model2_1)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model2_1))
qqline(residuals(model2_1), col='red')

### y = log_EU_Sales (without outliers) and x = sum_NA_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model3_1 <- lm(data_no_outlier_EU$log_EU_Sales ~ data_no_outlier_EU$sum_NA_Sales)

# View the result of the model.
summary(model3_1)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model3_1))
qqline(residuals(model3_1), col='red')

## Compare four models:

## Use the plot() function to create the visualisation for model1_1.
plot(turtle_sales2_pr$sum_NA_Sales, turtle_sales2_pr$sum_EU_Sales)

## Add a trendline with the abline() function.
abline(coefficients(model1_1), col = 'red')

## Repeat the above steps for model0_1, but set the line colour as orange.
plot(data_no_outlier_sum_EU$sum_NA_Sales, data_no_outlier_sum_EU$sum_EU_Sales)
abline(coefficients(model0_1), col = 'orange')

## Repeat the above steps for model2_1, but set the line colour as blue.
plot(turtle_sales2_pr$sum_NA_Sales, turtle_sales2_pr$log_EU_Sales)
abline(coefficients(model2_1), col = 'blue')

## Repeat the above steps for model3_1, but set the line colour as green.
plot(data_no_outlier_EU$sum_NA_Sales, data_no_outlier_EU$log_EU_Sales)
abline(coefficients(model3_1), col = 'green')

# Perform Breusch-Pagan Test.
bptest(model1_1)
bptest(model0_1)
bptest(model2_1)
bptest(model3_1)

# Compare models:

# model1_1: p value <2e-16; R^2 value = 0.3856; Residual standard error: 2.424 on 
# 173 degrees of freedom; BP test p-value = 4.915e-15 < 0.05 => we reject the 
# H0 ( => heteroscedasticity is present);

# model0_1: p value 4.42e-10; R^2 value = 0.2153; Residual standard error: 1.692 on 
# 161 degrees of freedom; BP test p-value = 2.686e-11 < 0.05 => we reject the 
# H0 ( => heteroscedasticity is present);

# model2_1: p value 8.05e-10; R^2 value = 0.1965; Residual standard error: 0.8044 
# on 173 degrees of freedom; BP test p-value = 0.4609 > 0.05 => we fail to reject 
# the H0 ( => heteroscedasticity is not present);

# model3_1: p value 5.64e-08; R^2 value = 0.1605; Residual standard error: 0.7015 
# on 169 degrees of freedom; BP test p-value = 0.000466 < 0.05 => we reject the 
# H0 ( => heteroscedasticity is present).

# Model2_1 is the only model which shows no presence of heteroscedasticity. It
# also has low Residual standard error => we can use model2_1 to predict log of 
# EU sales (which we can use to predict EU sales as a final output).

# Comparing model2 and model2_1, we can conclude that model2 is stronger and we 
# can better predict NA sales by using EU sales than vice versa.

### y = sum_Global_Sales and x = sum_NA_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model1_2 <- lm(turtle_sales2_pr$sum_Global_Sales ~ turtle_sales2_pr$sum_NA_Sales)

# View the result of the model.
summary(model1_2)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model1_2))
qqline(residuals(model1_2), col='red')

### y = sum_Global_Sales (without outliers) and x = sum_NA_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model0_2 <- lm(data_no_outlier_sum_Global$sum_Global_Sales ~ 
                 data_no_outlier_sum_Global$sum_NA_Sales)

# View the result of the model.
summary(model0_2)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model1_2))
qqline(residuals(model1_2), col='red')

### y = log_Global_Sales and x = sum_NA_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model2_2 <- lm(turtle_sales2_pr$log_Global_Sales ~ turtle_sales2_pr$sum_NA_Sales)

# View the result of the model.
summary(model2_2)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model2_2))
qqline(residuals(model2_2), col='red')

### y = log_Global_Sales (without outliers) and x = sum_NA_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model3_2 <- lm(data_no_outlier_Global$log_Global_Sales ~ 
                 data_no_outlier_Global$sum_NA_Sales)

# View the result of the model.
summary(model3_2)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model3_2))
qqline(residuals(model3_2), col='red')

## Compare four models:

## Use the plot() function to create the visualisation for model1_2.
plot(turtle_sales2_pr$sum_NA_Sales, turtle_sales2_pr$sum_Global_Sales)

## Add a trendline with the abline() function.
abline(coefficients(model1_2), col = 'red')

## Repeat the above steps for model0_2, but set the line colour as orange.
plot(data_no_outlier_sum_Global$sum_NA_Sales, 
     data_no_outlier_sum_Global$sum_Global_Sales)
abline(coefficients(model0_2), col = 'orange')

## Repeat the above steps for model2_2, but set the line colour as blue.
plot(turtle_sales2_pr$sum_NA_Sales, turtle_sales2_pr$log_Global_Sales)
abline(coefficients(model2_2), col = 'blue')

## Repeat the above steps for model3_2, but set the line colour as green.
plot(data_no_outlier_Global$sum_NA_Sales, data_no_outlier_Global$log_Global_Sales)
abline(coefficients(model3_2), col = 'green')

# Perform Breusch-Pagan Test.
bptest(model1_2)
bptest(model0_2)
bptest(model2_2)
bptest(model3_2)

# Compare models:

# model1_2: p value <2e-16; R^2 value = 0.8395; Residual standard error: 3.266 on 
# 173 degrees of freedom; BP test p-value = 3.536e-14 < 0.05 => we reject the 
# H0 ( => heteroscedasticity is present);

# model0_2: p value <2e-16; R^2 value = 0.681; Residual standard error: 2.72 on 
# 161 degrees of freedom; BP test p-value = 1.782e-05 < 0.05 => we reject the 
# H0 ( => heteroscedasticity is present);

# model2_2: p value <2e-16; R^2 value = 0.6731; Residual standard error: 0.3284 
# on 173 degrees of freedom; BP test p-value = 2.174e-07 < 0.05 => we reject the 
# H0 ( => heteroscedasticity is present);

# model3_2: p value <2e-16; R^2 value = 0.6521; Residual standard error: 0.3195 
# on 171 degrees of freedom; BP test p-value = 3.696e-06 < 0.05 => we reject the 
# H0 ( => heteroscedasticity is present).

# Even though model0_2 seems to be the best for predicting log of Global sales
# (which we can use to predict Global sales as a final output), the 
# heteroscedasticity is present in the data and the standard errors that are shown 
# in the output table of the regression may be unreliable.

### y = sum_Global_Sales and x = sum_EU_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model1_3 <- lm(turtle_sales2_pr$sum_Global_Sales ~ turtle_sales2_pr$sum_EU_Sales)

# View the result of the model.
summary(model1_3)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model1_3))
qqline(residuals(model1_3), col='red')

### y = sum_Global_Sales (without outliers) and x = sum_EU_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model0_3 <- lm(data_no_outlier_sum_Global$sum_Global_Sales ~ 
                 data_no_outlier_sum_Global$sum_EU_Sales)

# View the result of the model.
summary(model0_3)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model1_3))
qqline(residuals(model1_3), col='red')

### y = log_Global_Sales and x = sum_EU_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model2_3 <- lm(turtle_sales2_pr$log_Global_Sales ~ turtle_sales2_pr$sum_EU_Sales)

# View the result of the model.
summary(model2_3)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model2_3))
qqline(residuals(model2_3), col='red')

### y = log_Global_Sales (without outliers) and x = sum_EU_Sales.
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model3_3 <- lm(data_no_outlier_Global$log_Global_Sales ~ 
                 data_no_outlier_Global$sum_EU_Sales)

# View the result of the model.
summary(model3_3)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
qqnorm(residuals(model3_3))
qqline(residuals(model3_3), col='red')

## Compare the tree models:

## Use the plot() function to create the visualisation for model1_3.
plot(turtle_sales2_pr$sum_EU_Sales, turtle_sales2_pr$sum_Global_Sales)

## Add a trendline with the abline() function.
abline(coefficients(model1_3), col = 'red')

## Repeat the above steps for model0_3, but set the line colour as orange.
plot(data_no_outlier_sum_Global$sum_EU_Sales, 
     data_no_outlier_sum_Global$sum_Global_Sales)
abline(coefficients(model0_3), col = 'orange')

## Repeat the above steps for model2_3, but set the line colour as blue.
plot(turtle_sales2_pr$sum_EU_Sales, turtle_sales2_pr$log_Global_Sales)
abline(coefficients(model2_3), col = 'blue')

## Repeat the above steps for model3_3, but set the line colour as green.
plot(data_no_outlier_Global$sum_EU_Sales, data_no_outlier_Global$log_Global_Sales)
abline(coefficients(model3_3), col = 'green')

# Perform Breusch-Pagan Test.
bptest(model1_3)
bptest(model0_3)
bptest(model2_3)
bptest(model3_3)

# Compare models:

# model1_3: p value <2e-16; R^2 value = 0.7201; Residual standard error: 4.313 on 
# 173 degrees of freedom; BP test p-value = 0.05021 > 0.05 => we fail to reject 
# the H0 ( => heteroscedasticity is not present);

# model0_3: p value <2e-16; R^2 value = 0.5528; Residual standard error: 3.22 on 
# 161 degrees of freedom; BP test p-value = 0.1186 > 0.05 => we fail to reject 
# the H0 ( => heteroscedasticity is not present);

# model2_3: p value <2e-16; R^2 value = 0.621; Residual standard error: 0.3536 
# on 173 degrees of freedom; BP test p-value = 0.05151 > 0.05 => we fail to reject 
# the H0 ( => heteroscedasticity is not present);

# model3_3: p value <2e-16; R^2 value = 0.6145; Residual standard error: 0.3364 
# on 171 degrees of freedom; BP test p-value = 0.7677 > 0.05 => we fail to reject 
# the H0 ( => heteroscedasticity is not present).

# All four models show no presence of heteroscedasticity in data. Model1_3 has 
# the highest R^2 value out of four models but Residual standard error is much 
# higher also meaning that model1_3 fits the data worse than other three models. 
# Model2_3 and model3_3 have similar (the smallest) Residual standard errors and 
# R^2 values => we can use either models. Possibly model2_3 is better to use as 
# we do not need to remove outliers.

# Comparing model2_2 and model2_3, we can conclude that model2_3 is more reliable 
# as has no heteroscedasticity in data and we can use it to predict Global sales 
# by using EU sales.

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
turtle_sales2_pr4 <- turtle_sales2_pr[, 2:7]

# Multiple linear regression model.
# y = sum_Global_Sales and x = sum_NA_Sales, sum_EU_Sales.
model_ml = lm(sum_Global_Sales ~ sum_NA_Sales + sum_EU_Sales, 
              data = turtle_sales2_pr4)

# Print the summary statistics.
summary(model_ml)

# y = sum_Global_Sales (without outliers) and x = sum_NA_Sales, sum_EU_Sales.
# Select only numeric columns from the original data frame.
data_no_outlier_sum_Global <- data_no_outlier_sum_Global[, 2:4]

# Build model.
model_ml0 = lm(sum_Global_Sales ~ sum_NA_Sales + sum_EU_Sales, 
              data = data_no_outlier_sum_Global)

# Print the summary statistics.
summary(model_ml0)

# y = log_Global_Sales and x = sum_NA_Sales, sum_EU_Sales.
model_ml1 = lm(log_Global_Sales ~ sum_NA_Sales + sum_EU_Sales, 
              data = turtle_sales2_pr4)

# Print the summary statistics.
summary(model_ml1)

# Multiple linear regression model.
# y = sum_Global_Sales and x = log_NA_Sales, log_EU_Sales.
model_ml2 = lm(sum_Global_Sales ~ log_NA_Sales + log_EU_Sales, 
              data = turtle_sales2_pr4)

# Print the summary statistics.
summary(model_ml2)

# Multiple linear regression model.
# y = log_Global_Sales and x = log_NA_Sales, log_EU_Sales.
model_ml3 = lm(log_Global_Sales ~ log_NA_Sales + log_EU_Sales, 
               data = turtle_sales2_pr4)

# Print the summary statistics.
summary(model_ml3)

# Multiple linear regression model.
# Add to data_no_outlier_sum_Global data frame calculated columns for log NA and 
# EU sales.
data_no_outlier_sum_Global$log_NA_Sales <- 
  log(data_no_outlier_sum_Global$sum_NA_Sales)

data_no_outlier_sum_Global$log_EU_Sales <- 
  log(data_no_outlier_sum_Global$sum_EU_Sales)

# Replace '-Inf' values in log_EU_Sales column with 0.
data_no_outlier_sum_Global$log_EU_Sales <- 
  ifelse(is.finite(data_no_outlier_sum_Global$log_EU_Sales), 
         data_no_outlier_sum_Global$log_EU_Sales, 0)

# y = sum_Global_Sales (without outliers) and x = log_NA_Sales, log_EU_Sales.
model_ml4 = lm(sum_Global_Sales ~ log_NA_Sales + log_EU_Sales, 
               data = data_no_outlier_sum_Global)

# Print the summary statistics.
summary(model_ml4)

# Perform Breusch-Pagan Test.
bptest(model_ml)
bptest(model_ml0)
bptest(model_ml1)
bptest(model_ml2)
bptest(model_ml3)
bptest(model_ml4)

# Compare models:

# model_ml: both p values <2e-16; Adj R^2 value = 0.9664; Residual standard 
# error: 1.49 on 172 degrees of freedom; BP test p-value = 0.1718 > 0.05 => we 
# fail to reject the H0 ( => heteroscedasticity is not present);

# model_ml0: both p values <2e-16; Adj R^2 value = 0.9129; Residual standard 
# error: 1.416 on 160 degrees of freedom; BP test p-value = 0.071 > 0.05 => we 
# fail to reject the H0 ( => heteroscedasticity is not present);

# model_ml1: both p values <2e-16; Adj R^2 value = 0.7971; Residual standard 
# error: 0.258 on 172 degrees of freedom; BP test p-value = 2.678e-12 < 0.05 => 
# we reject the H0 ( => heteroscedasticity is present);

# model_ml2: log_NA_Sales p value <2e-16; log_EU_Sales p value 3.62e-12; Adj R^2 
# value = 0.6468; Residual standard error: 4.831 on 172 degrees of freedom; BP 
# test p-value = 0.1986 > 0.05 => we fail to reject the H0 ( => heteroscedasticity 
# is not present);

# model_ml3: both p values <2e-16; Adj R^2 value = 0.773; Residual standard 
# error: 0.2728 on 172 degrees of freedom; BP test p-value = 1.046e-08 < 0.05 => 
# we reject the H0 ( => heteroscedasticity is present);

# model_ml4: log_NA_Sales p value <2e-16; log_EU_Sales p value 1.72e-14; Adj R^2 
# value = 0.6371; Residual standard error: 2.892 on 160 degrees of freedom; BP 
# test p-value = 1.135e-05 < 0.05 => we reject the H0 ( => heteroscedasticity is 
# present).

# model_ml is the best multiple linear regression model which we can use to
# predict Global sales.

# Check for multicollinearity.
# Load the car library.
library(car)

# Calculate the VIF for each predictor variable in the models with no sighns of
# heteroscedasticity.

# model_ml: VIF = 1.627488 which is between 1 and 5 => variables are moderately 
# correlated (but < 3 => is not a cause for concern). 
vif(model_ml)

# model_ml0: VIF = 1.143137 which is between 1 and 5 => variables are moderately 
# correlated (but < 3 => is not a cause for concern). 
vif(model_ml0)

# model_ml2: VIF = 1.242861 which is between 1 and 5 => variables are moderately 
# correlated (but < 3 => is not a cause for concern). 
vif(model_ml2)

###############################################################################

# 4. Predictions based on given values
# model_ml:
# Compare with observed values for a number of records.
# Create a test data frame.
test_data <- data.frame(sum_NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                        sum_EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

# Create a new object and specify the predict function.
predictTest = predict(model_ml, newdata = test_data, interval = 'confidence')

# Print the object.
predictTest

# Join test_data with turtle_sales2 to add actual Global_Sales
test_data1 <- left_join(test_data, turtle_sales2, by=c('sum_NA_Sales'='NA_Sales', 
                                        'sum_EU_Sales'='EU_Sales'))

# Drop unnecessary columns. 
test_data1 <- test_data1[, c(1:2, 5)]

# Merge test_data1 and predictTest to compare results of model_ml.
final_data <- cbind(test_data1, predictTest)

# View output: 1 value out of 5 have been predicted within identified ranges.
final_data

# Save final_data at CSV file.
sink('final_data.csv')
final_data
sink()

# model_ml0:
# Create a new object and specify the predict function.
predictTest_ml0 = predict(model_ml0, newdata = test_data, interval = 'confidence')

# Print the object.
predictTest_ml0

# Merge test_data1 and predictTest_ml0 to compare results of model_ml0.
final_data_ml0 <- cbind(test_data1, predictTest_ml0)

# View output: 1 value out of 5 have been predicted within identified ranges.
final_data_ml0

# model_ml2:
# Create a new data frame and add log_NA_Sales and log_EU_Sales columns.
test_data_ml2 <- test_data
test_data_ml2$log_NA_Sales <- log(test_data_ml2$sum_NA_Sales)
test_data_ml2$log_EU_Sales <- log(test_data_ml2$sum_EU_Sales)

# Create a new object and specify the predict function.
predictTest_ml2 = predict(model_ml2, newdata = test_data_ml2, 
                          interval = 'confidence')

# Print the object.
predictTest_ml2

# Join test_data_ml2 with turtle_sales2 to add actual Global_Sales
test_data_ml21 <- left_join(test_data_ml2, turtle_sales2, 
                            by=c('sum_NA_Sales'='NA_Sales', 
                                 'sum_EU_Sales'='EU_Sales'))

# Drop unnecessary columns. 
test_data_ml21 <- test_data_ml21[, c(1:2, 7)]

# Merge test_data_ml21 and predictTest_ml2 to compare results of model_ml2.
final_data_ml2 <- cbind(test_data_ml21, predictTest_ml2)

# View output: 2 values out of 5 have been predicted within identified ranges.
final_data_ml2

# Simple linear regression model - model2_3 (y = log_Global_Sales and 
# x = sum_EU_Sales):
model2_3 = lm(log_Global_Sales ~ sum_EU_Sales, data = turtle_sales2_pr)

# Create a new object and specify the predict function.
predictTest_ml2_3 = predict(model2_3, newdata = test_data, 
                          interval = 'confidence')

# Print the object.
predictTest_ml2_3

# Merge test_data1 and predictTest_ml2_3.
final_data_ml2_3 <- cbind(test_data1, predictTest_ml2_3)

# Add log of actual Global sales to compare with predicted log of Global sales 
# by model.
final_data_ml2_3$log_Global_Sales <- log(final_data_ml2_3$Global_Sales)

# View output: no values have been predicted correctly within identified ranges.
final_data_ml2_3

# Simple linear regression model - model1_3 (y = sum_Global_Sales and 
# x = sum_EU_Sales):
model1_3 = lm(sum_Global_Sales ~ sum_EU_Sales, data = turtle_sales2_pr)

# Create a new object and specify the predict function.
predictTest_ml1_3 = predict(model1_3, newdata = test_data, 
                            interval = 'confidence')

# Print the object.
predictTest_ml1_3

# Merge test_data1 and predictTest_ml1_3.
final_data_ml1_3 <- cbind(test_data1, predictTest_ml1_3)

# View output: 1 value out of 5 has been predicted correctly within identified 
# range.
final_data_ml1_3

# model_ml, model_ml0, model_ml2 and model1_3 have the best output. 
# Let's calculate RMSE (Root Mean Squared Error) and R^2 for all four models.

# Importing the required package
# install.packages('caret')
library(caret)

## model_ml: RMSE = 1.911992 (representing an error rate of 
## 1.911992/mean(final_data$Global_Sales) = 1.911992/20.99 = 9.11%, which is 
## good); R^2 = 0.9980341 (meaning that the observed and the predicted outcome 
## values are highly correlated, which is very good).
RMSE(predictTest, final_data$Global_Sales)
R2(predictTest, final_data$Global_Sales)
mean(final_data$Global_Sales)

## model_ml0: RMSE = 2.064926 (representing an error rate of 
## 2.064926/mean(final_data_ml0$Global_Sales) = 2.064926/20.99 = 9.84%, which is 
## good); R^2 = 0.998349 (meaning that the observed and the predicted outcome 
## values are highly correlated, which is very good).
RMSE(predictTest_ml0, final_data_ml0$Global_Sales)
R2(predictTest_ml0, final_data_ml0$Global_Sales)
mean(final_data_ml0$Global_Sales)

## model_ml2: RMSE = 16.62366 (representing an error rate of 
## 16.62366/mean(final_data_ml2$Global_Sales) = 16.62366/20.99 = 79.20%, which is 
## bad); R^2 = 0.9680629 (meaning that the observed and the predicted outcome 
## values are highly correlated, which is very good).
RMSE(predictTest_ml2, final_data_ml2$Global_Sales)
R2(predictTest_ml2, final_data_ml2$Global_Sales)
mean(final_data_ml2$Global_Sales)

## model1_3: RMSE = 9.963695 (representing an error rate of 
## 9.963695/mean(final_data_ml1_3$Global_Sales) = 9.963695/20.99 = 47.47%, which 
## is bad); R^2 = 0.9002199 (meaning that the observed and the predicted outcome 
## values are highly correlated, which is very good).
RMSE(predictTest_ml1_3, final_data_ml1_3$Global_Sales)
R2(predictTest_ml1_3, final_data_ml1_3$Global_Sales)
mean(final_data_ml1_3$Global_Sales)

# After comparing RMSE and R^2 for four best models we can conclude that 
# model_ml is the best model to use when predicting Global_Sales.

###############################################################################

# 5. Observations and insights

# We've created various simple linear regression models in order to determine the 
# correlation between the sales columns. 

# Model2 (y = log_NA_Sales and x = sum_EU_Sales) was identified as the best out 
# of four simple linear regression models for predicting sales in North America:
# p value = 1.54e-11; R^2 value = 0.2318; Residual standard error: 0.6771 on 173 
# degrees of freedom.

# Model2_1 (y = log_EU_Sales and x = sum_NA_Sales) is the best out of four 
# created simple linear regression models for predicting sales in Europe:
# p value = 8.05e-10; R^2 value = 0.1965; Residual standard error: 0.8044 on 173 
# degrees of freedom.

# Comparing model2 and model2_1, we can conclude that model2 is stronger and we 
# can better predict NA sales by using EU sales than vice versa.

# Eight simple linear regression and six multiple linear regression models were
# created to predict Global sales. After the first round of evaluations two best 
# simple linear regression and three multiple linear regression models have been
# chosen to test be tested by the test data set. One simple linear regression and 
# two multiple linear regression models produced the best output and were 
# compared by RMSE and R2 values. As a result, model_ml (y = sum_Global_Sales and
# X = sum_NA_Sales, sum_EU_Sales) was identified as the strongest with error 
# rate 9.11% and R^2 = 0.998 (observed and the predicted outcome values are 
# highly correlated).

###############################################################################
###############################################################################




