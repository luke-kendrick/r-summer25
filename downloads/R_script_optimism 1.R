# Your dataset - correlations

##### GETTING EVERYTHING READY #####

# Before doing any analyses, you need to get everything set up and ready...

# Set the working directory -WD- so R knows where the data lives. Do this by going Session > Set working directory > Choose directory

# You can check the working directory...

getwd()

#Before doing anything, need to make sure the right packages are installed and open. We will use all of these......

install.packages(tidyverse)
install.packages(correlation)
install.packages(gridExtra)
install.packages(ppcor)
install.packages(cocor)
install.packages(car)

library(tidyverse)
library(correlation)
library(gridExtra)
library(ppcor)
library(cocor)
library(car)

# Once you have done this, you should see them ticked in the "Packages" list - you can also manually tick the boxes.
# If you need to install any of these, the code is install.packages("name of package")
# Make sure you can then see it in the "Packages" list, and call it using library("name of package") or just tick it in the "Packages" list

# Now get R to open our dataset

mydata <- read_csv("WS_data_R_optimism.csv")

# To check the data have opened ok, you can view the data...

view(mydata)

# You can also check the number of participants (obs) and the number of variables in the "Environment" tab.

# Next, we need to tell R which variables are continuous (as.numeric) and which are categorical (as.factor).

# You can check the names of the variables with this...

names(mydata)

mydata$p_num <- as.numeric(mydata$p_num)
mydata$optimism <- as.numeric(mydata$optimism)
mydata$positive_SC <- as.numeric(mydata$positive_SC)
mydata$negative_SC <- as.numeric(mydata$negative_SC)
mydata$age_years <- as.numeric(mydata$age_years)
mydata$extra_curr <- as.factor(mydata$extra_curr)
mydata$reading_age <- as.numeric(mydata$reading_age)

##### RUNNING DESCRIPTIVE STATISTICS #####

# Before getting into correlations, we might want a summary of our variables. For the continuous variables, we get descriptives. For the categorical/binary variables, we get frequencies.

summary(mydata)

# If we want to see the descriptives split for different groups, for example, we want to see the descriptives for optimism for extracurricular activities status separately..

descriptives_bygroup <- mydata %>% # Tell R which data set to use.  %>% means "and then" so tells R to move on and do something else
  group_by(extra_curr) %>% # group_by is telling R to split the data file - put the variable to split by in brackets
  summarise(mean_optimism = mean(optimism), sd_optimism = sd(optimism)) # Ask for the mean and standard deviation. statistic_calculated = statistic(variable)

# You then need R to "print" - or display - the calculated descriptives in the console window.

print(descriptives_bygroup)

##### CREATING SCATTERPLOTS #####

# First, let's graph the correlations between "optimism" with life, and the three other continuous variables. You won't see them until after you make them and then ask R to display them. We will make four scatterplots...

# Optimism and positive self compassion

plot1 <- ggplot(mydata, aes(x = positive_SC, y = optimism)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# Optimism and negative self compassion

plot2 <- ggplot(mydata, aes(x = negative_SC, y = optimism)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# Optimism and age in years

plot3 <- ggplot(mydata, aes(x = age_years, y = optimism)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# To see what the plots look like, we need to arrange them in the "Plot" window. Make sure "gridextra" is ticked in the "Packages" window

grid.arrange(plot1, plot2, plot3, ncol = 3)  

# ncol = 3 tells R to put three next to each other

##### RUNNING PEARSON'S CORRELATIONS #####

# Now we can look at the correlations between these four continuous variables - but we want to mainly focus on the correlations with optimism - our main variable of interest.

mydata %>%
  dplyr::select(optimism, positive_SC, negative_SC, age_years) %>%
  correlation(p_adjust = "none")

# In addition to giving you the r and p values, it gives the N, so check that this is correct. To write up the correlation, remember that df = N-2.

##### RUNNING PARTIAL CORRELATIONS #####

# Next, let's look at partial correlations, so the three main correlations of interest we just ran, but now controlling for reading age.

# You need to have one set of code for each partial correlation, and make sure "ppcor" is ticked in the "Packages" window.

pcor.test(mydata$optimism, mydata$positive_SC,
          mydata$reading_age,
          method = "pearson")

pcor.test(mydata$optimism, mydata$negative_SC,
          mydata$reading_age,
          method = "pearson")

pcor.test(mydata$optimism, mydata$age_years,
          mydata$reading_age,
          method = "pearson")

##### COMPARING CORRELATIONS #####

# Final thing is comparing correlations across different groups. For example, is the correlation between satisfaction with life and negative life experiences different when comparing people who are single or in a relationship?

# First, we need to tell R which subgroups within our dataset we want to look at - so identify 0 and 1 from the "relationship" variable, and name each one.

None <- mydata[mydata$extra_curr == "0", ]
Activities <- mydata[mydata$extra_curr == "1", ]

# Now we run the two correlations - we need to tell R first which subgroup to use from the naming we just did, and which continuous variable to correlate.

cor.test(None$optimism, None$positive_SC,
         method = "pearson")
cor.test(Activities$optimism, Activities$positive_SC,
         method = "pearson")

# To compare the correlations statistically, we need the N and the r for each group. The r value is the final value in the output we just created - the final line, under corr.
# To get the N for each group, we ran the "summary" earlier, but you can do it again to save scrolling. 

summary(mydata)

# Now we can statistically compare our r values. Make a note of which group you consider to be "1" and which is "2". For this, it will be 1 is single and 2 is in a relationship.
# First, make sure "cocor" is ticked in the "Packages" tab.
# r1 is the first r-value, in this case 0.2009678
# r2 is the second r-value, in this case 0.6345603
# n1 is the first sample size, in this case 73
# n2 is the second sample size, in this case 77
# the code looks like this, so just replace the values as needed... cocor.indep.groups(r1, r2, n1, n2)

cocor.indep.groups(0.2009678, 0.6345603, 73, 77)

# Final thing to do - graph these two correlations on the same plot to aid interpretation.

plot_cc <- ggplot(mydata, aes(x = positive_SC, y = optimism, colour = extra_curr)) +
  geom_point(aes(shape = extra_curr)) +
  geom_smooth(aes(linetype = extra_curr), method = "lm", se = FALSE) +
  labs(title = "Positive self compassion vs Optimism by Extra curricular activities",
       x = "Positive self compassion",
       y = "Optimism") +
  theme_classic() +
  scale_color_manual(values = c("0" = "grey", "1" = "black ")) +
  scale_linetype_manual(values = c("0" = "solid", "1" = "dashed")) +
  scale_shape_manual(values = c("0" = 16, "1" = 3))

print(plot_cc)

##### MULTIPLE REGRESSION WITH CONTINUOUS PREDICTORS #####

# Now, let's run a multiple regression.
# We have optimism as the outcome variable that we want to predict
# Then the three continuous predictor variables.

model <- lm(optimism ~ positive_SC + negative_SC + age_years, data = mydata)
summary(model)

# We then want to create scatterplots to graphically represent any significant predictors (so three)

# Optimism and positive self compassion

plot1 <- ggplot(mydata, aes(x = positive_SC, y = optimism)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# Optimism and negative self compassion

plot2 <- ggplot(mydata, aes(x = negative_SC, y = optimism)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# Optimism and age in years

plot3 <- ggplot(mydata, aes(x = age_years, y = optimism)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# To see what the plots look like, we need to arrange them in the "Plot" window. Make sure "gridextra" is ticked in the "Packages" window

grid.arrange(plot1, plot2, plot3, ncol = 3)

# ncol = 3 tells R to put two next to each other, nrow = 1 tells R to put one above the other

##### HIERARCHICAL REGRESSION #####

# Now, let's move on to hierarchical regression - exactly what we just did, but adding age as a control variable.

# First, let's see if the control variable is significant by building model 1. Make sure it is called "model 1" and you need to run the summary to see the output.

model1 <- lm(optimism ~ reading_age, data = mydata)
summary(model1)

# Next, build our final model that has all the variables (control and predictor variables). This is "model 2", and again, use the summary to see the output.

model2 <- lm(optimism ~ reading_age + positive_SC + negative_SC + age_years, data = mydata)
summary(model2)

# Finally, we want to see if adding the predictor variables is significantly "better" than the control variable alone, this has two parts.

# First - how much does the variance explained (adjusted R sq) increase?

r2_control <- summary(model1)$adj.r.squared  # Adj Rsq of control model
r2_full <- summary(model2)$adj.r.squared     # Adj Rsq of full model

r2_change <- r2_full - r2_control  

print(r2_change)  # Print the Adj Rsq change

# Does the model significantly improve?

anova(model1,model2)

# We then want to create scatterplots to graphically represent any significant predictors (so two)

# Optimism and positive self compassion

plot1 <- ggplot(mydata, aes(x = positive_SC, y = optimism)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# Optimism and negative self compassion

plot2 <- ggplot(mydata, aes(x = negative_SC, y = optimism)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# To see what the plots look like, we need to arrange them in the "Plot" window. Make sure "gridextra" is ticked in the "Packages" window

grid.arrange(plot1, plot2, nrow = 1, ncol = 2)  

##### REGRESSION WITH BINARY AND INTERACTIVE PREDICTORS #####

# Now, let's run a multiple regression, but this time with continuous, binary and interactive predictors
# We have optimism as the outcome variable that we want to predict
# Then two continuous predictors (positive and negative SC), one binary (extra curricular) and two interactions (each SC measure interacting with extra curricular).

model <- lm(optimism ~ positive_SC + negative_SC + extra_curr + positive_SC*extra_curr + negative_SC*extra_curr, data = mydata)
summary(model)

# Then create a scatterplot for any significant continuous predictors.

plot1 <- ggplot(mydata, aes(x = negative_SC, y = optimism)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

print(plot1)

# For any significant binary predictors, create a graph and descriptives to aid interpretation

# First, calculate the descriptive statistics (mean and SD) for each of the groups.

descriptives_bygroup <- mydata %>% # Tell R which data set to use.  %>% means "and then" so tells R to move on and do something else
  group_by(extra_curr ) %>% # group_by is telling R to split the data file - put the variable to split by in brackets
  summarise(mean_optimism = mean(optimism), sd_optimism = sd(optimism)) # Ask for the mean and standard deviation. statistic_calculated = statistic(variable)

# You then need R to "print" - or display - the calculated descriptives in the console window.

print(descriptives_bygroup)

# Next, we want to create a boxplot for each significant binary predictor. We do this in exactly the same way as for graphing an independent t test, so you can go back to that lecture/workshop if needed.

ggplot(mydata, aes(x = extra_curr, y = optimism)) +
  geom_boxplot() +
  labs(title = "Optimism by doing extracurricular activities",
       x = "Extracurricular activitiies",
       y = "Mean optimism score") +
  theme_classic()

# STEP FIVE - interpreting the interactive predictors

# First: We need to tell R which subgroups within our dataset we want to look at - so identify 0 and 1 from the "extra curricular" variable, and name each one.

None <- mydata[mydata$extra_curr == "0", ]
Activities <- mydata[mydata$extra_curr == "1", ]

# Second: Now we run the two correlations - we need to tell R first which subgroup to use from the naming we just did, and which continuous variable to correlate.

cor.test(None$optimism, None$positive_SC,
         method = "pearson")
cor.test(Activities$optimism, Activities$positive_SC,
         method = "pearson")

# Third: To compare the correlations statistically, we need the N and the r for each group. The r value is the final value in the output we just created - the final line, under corr.
# To get the N for each group, we ran the "summary" earlier, but you can do it again to save scrolling. 

summary(mydata)

# Now we can statistically compare our r values. Make a note of which group you consider to be "1" and which is "2". For this, it will be 1 is single and 2 is in a relationship.
# First, make sure "cocor" is ticked in the "Packages" tab.
# r1 is the first r-value, in this case 0.2009678
# r2 is the second r-value, in this case 0.6345603
# n1 is the first sample size, in this case 73
# n2 is the second sample size, in this case 77
# the code looks like this, so just replace the values as needed... cocor.indep.groups(r1, r2, n1, n2)

cocor.indep.groups(0.2009678, 0.6345603, 73, 77)

# Finally: graph these two correlations on the same plot to aid interpretation.

plot_cc <- ggplot(mydata, aes(x = positive_SC, y = optimism, colour = extra_curr)) +
  geom_point(aes(shape = extra_curr)) +
  geom_smooth(aes(linetype = extra_curr), method = "lm", se = FALSE) +
  labs(title = "Positive self compassion vs Optimism by Extra curricular activities",
       x = "Positive self compassion",
       y = "Optimism") +
  theme_classic() +
  scale_color_manual(values = c("0" = "grey", "1" = "black ")) +
  scale_linetype_manual(values = c("0" = "solid", "1" = "dashed")) +
  scale_shape_manual(values = c("0" = 16, "1" = 3))

print(plot_cc)

##### EVALUATING THE ASSUMPTIONS #####

# First, run the multiple regression. We will not report it yet, but some of the assumptions need the "model" for its calculations.

model <- lm(optimism ~ positive_SC + negative_SC + extra_curr, data = mydata)
summary(model)

##### MULTICOLLINEARITY #####

# Multicollinearity, looking at r values across all predictor variabless.

mydata %>%
  dplyr::select(positive_SC, negative_SC) %>%
  correlation(p_adjust = "none")

# Multicollinearity, calculate VIF.

vif_values <- vif(model)
print(vif_values)

# Multicollinearity, calculate tolerance. This is, essentially 1 - the R2 (so variance explained).

tolerance_value <- 1 - summary(model)$r.squared
print(tolerance_value)

##### DISTRIBUTION OF RESIDUALS #####

# create a histograme to show the distribution of residuals.

ggplot(mydata, aes(x = model$residuals)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

##### HOMOSCEDASTICITY #####

# Create a scatterplot to show homoscedasticity.

ggplot(mydata, aes(x = model$fitted.values, y = model$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

##### EVALUATING OUTLIERS #####

# Identify outliers using standardized residuals
standardized_residuals <- rstandard(model)
# Print standardized residuals
print(standardized_residuals)

# Determine the number of outliers (absolute value greater than 2)
outliers <- sum(abs(standardized_residuals) > 2)
print(outliers)

# Calculate the percentage of outliers
percentage_outliers <- (outliers / nrow(mydata)) * 100
print(percentage_outliers)

##### Yay - regression in R finished!!! #####
