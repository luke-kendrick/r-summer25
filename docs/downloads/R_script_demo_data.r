# Staff training - my demo script for the lecture slides

##### SETTING UP #####

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

mydata <- read_csv("Lecture_data_R_sat_life.csv")

# To check the data have opened ok, you can view the data...

view(mydata)

# You can also check the number of participants (obs) and the number of variables in the "Environment" tab.

# Next, we need to tell R which variables are continuous (as.numeric) and which are categorical (as.factor).

# You can check the names of the variables with this...

names(mydata)

mydata$p_num <- as.numeric(mydata$p_num)
mydata$sat_life <- as.numeric(mydata$sat_life)
mydata$psych_wellbeing <- as.numeric(mydata$psych_wellbeing)
mydata$physical_wellbeing <- as.numeric(mydata$physical_wellbeing)
mydata$relationship_wellbeing <- as.numeric(mydata$relationship_wellbeing)
mydata$neg_life_experiences <- as.numeric(mydata$neg_life_experiences)
mydata$occ_status <- as.factor(mydata$occ_status)
mydata$relationship_status <- as.factor(mydata$relationship_status)
mydata$home_location <- as.factor(mydata$home_location)
mydata$years_edu <- as.numeric(mydata$years_edu)

##### RUNNING THE DESCRIPTIVE STATISTICS #####

# Before getting into correlations, we might want a summary of our variables. For the continuous variables, we get descriptives. For the categorical/binary variables, we get frequencies.

summary(mydata)

# If we want to see the descriptives split for different groups, for example, we want to see the descriptives for satisfaction with life for occupational status separately..

descriptives_bygroup <- mydata %>% # Tell R which data set to use.  %>% means "and then" so tells R to move on and do something else
  group_by(occ_status) %>% # group_by is telling R to split the data file - put the variable to split by in brackets
  summarise(mean_sat_life = mean(sat_life), sd_sat_life = sd(sat_life)) # Ask for the mean and standard deviation. statistic_calculated = statistic(variable  )

# You then need R to "print" - or display - the calculated descriptives in the console window.

print(descriptives_bygroup)

##### CREATING SCATTERPLOTS #####

# First, let's graph the correlations between "satisfaction" with life, and the four other continuous variables. You won't see them until after you make them and then ask R to display them. We will make four scatterplots...

# Satisfaction with life and psychological wellbeing

plot1 <- ggplot(mydata, aes(x = psych_wellbeing, y = sat_life)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# Satisfaction with life and physical wellbeing

plot2 <- ggplot(mydata, aes(x = physical_wellbeing, y = sat_life)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# Satisfaction with life and relationship wellbeing

plot3 <- ggplot(mydata, aes(x = relationship_wellbeing, y = sat_life)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# Satisfaction with life and negative life events

plot4 <- ggplot(mydata, aes(x = neg_life_experiences, y = sat_life)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# To see what the plots look like, we need to arrange them in the "Plot" window. Make sure "gridextra" is ticked in the "Packages" window

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)  

# ncol = 2 tells R to put two next to each other, nrow = 2 tells R to put one above the other

# To just print one of the plots...

print(plot1)

##### RUNNING PEARSON'S CORRELATIONS #####

# Now we can look at the correlations between these four continuous variables - but we want to mainly focus on the correlations with satisfaction with life - our main variable of interest.

mydata %>%
  dplyr::select(sat_life, psych_wellbeing, physical_wellbeing, relationship_wellbeing, neg_life_experiences) %>%
  correlation(p_adjust = "none")

# In addition to giving you the r and p values, it gives the N, so check that this is correct. To write up the correlation, remember that df = N-2.

##### RUNNING PARTIAL CORRELATIONS #####

# Next, let's look at partial correlations, so the four main correlations of interest we just ran, but now controlling for years of education.

# You need to have one set of code for each partial correlation, and make sure "ppcor" is ticked in the "Packages" window.

pcor.test(mydata$sat_life, mydata$psych_wellbeing,
          mydata$years_edu,
          method = "pearson")

pcor.test(mydata$sat_life, mydata$physical_wellbeing,
          mydata$years_edu,
          method = "pearson")

pcor.test(mydata$sat_life, mydata$relationship_wellbeing,
          mydata$years_edu,
          method = "pearson")

pcor.test(mydata$sat_life, mydata$neg_life_experiences,
          mydata$years_edu,
          method = "pearson")

##### COMPARING TWO CORRELATIONS #####

# Final thing is comparing correlations across different groups. For example, is the correlation between satisfaction with life and negative life experiences different when comparing people who are single or in a relationship?

# First, we need to tell R which subgroups within our dataset we want to look at - so identify 0 and 1 from the "relationship" variable, and name each one.

single <- mydata[mydata$relationship_status == "0", ]
relationship <- mydata[mydata$relationship_status == "1", ]

# Now we run the two correlations - we need to tell R first which subgroup to use from the naming we just did, and which continuous variable to correlate.

cor.test(single$sat_life, single$neg_life_experiences,
         method = "pearson")
cor.test(relationship$sat_life, relationship$neg_life_experiences,
         method = "pearson")

# To compare the correlations statistically, we need the N and the r for each group. The r value is the final value in the output we just created - the final line, under corr.
# To get the N for each group, we ran the "summary" earlier, but you can do it again to save scrolling. 

summary(mydata)

# Now we can statistically compare our r values. Make a note of which group you consider to be "1" and which is "2". For this, it will be 1 is single and 2 is in a relationship.
# First, make sure "cocor" is ticked in the "Packages" tab.
# r1 is the first r-value, in this case -0.5218863 
# r2 is the second r-value, in this case -0.1741089
# n1 is the first sample size, in this case 92
# n2 is the second sample size, in this case 108
# the code looks like this, so just replace the values as needed... cocor.indep.groups(r1, r2, n1, n2)

cocor.indep.groups(-0.5218863, -0.1741089, 92, 108)

# Final thing to do - graph these two correlations on the same plot to aid interpretation.

plot_cc <- ggplot(mydata, aes(x = neg_life_experiences, y = sat_life, colour = relationship_status)) +
  geom_point(aes(shape = relationship_status)) +
  geom_smooth(aes(linetype = relationship_status), method = "lm", se = FALSE) +
  labs(title = "Negative life experiences vs Satisfaction with life by Relationship status",
       x = "Negative life experiences",
       y = "Satisfaction with life") +
  theme_classic() +
  scale_color_manual(values = c("0" = "grey", "1" = "black ")) +
  scale_linetype_manual(values = c("0" = "solid", "1" = "dashed")) +
  scale_shape_manual(values = c("0" = 16, "1" = 3))


print(plot_cc)

##### MULTIPLE REGRESSION WITH CONTINUOUS PREDICTOR VARIABLES #####

# Now, let's run a multiple regression.
# We have SWL as the outcome variable that we want to predict
# Then the three wellbeing measures and the number of negative life experiences giving us four continuous predictor variable.

model <- lm(sat_life ~ psych_wellbeing + physical_wellbeing + relationship_wellbeing + neg_life_experiences, data = mydata)
summary(model)

# We then want to create scatterplots to graphically represent any significant predictors (so three)

# Satisfaction with life and psychological wellbeing

plot1 <- ggplot(mydata, aes(x = psych_wellbeing, y = sat_life)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# Satisfaction with life and relationship wellbeing

plot2 <- ggplot(mydata, aes(x = relationship_wellbeing, y = sat_life)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# Satisfaction with life and negative life events

plot3 <- ggplot(mydata, aes(x = neg_life_experiences, y = sat_life)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# To see what the plots look like, we need to arrange them in the "Plot" window. Make sure "gridextra" is ticked in the "Packages" window

grid.arrange(plot1, plot2, plot3, nrow = 1, ncol = 3)  

# ncol = 3 tells R to put two next to each other, nrow = 1 tells R to put one above the other

##### HIERARCHICAL REGRESSION #####

# Now, let's move on to hierarchical regression - exactly what we just did, but adding years of education as a control variable.

# First, let's see if the control variable is significant by building model 1. Make sure it is called "model 1" and you need to run the summary to see the output.

model1 <- lm(sat_life ~ years_edu, data = mydata)
summary(model1)

# Next, build our final model that has all the variables (control and predictor variables). This is "model 2", and again, use the summary to see the output.

model2 <- lm(sat_life ~ years_edu + psych_wellbeing + physical_wellbeing + relationship_wellbeing + neg_life_experiences, data = mydata)
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

# Satisfaction with life and psychological wellbeing

plot1 <- ggplot(mydata, aes(x = psych_wellbeing, y = sat_life)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# Satisfaction with life and negative life events

plot2 <- ggplot(mydata, aes(x = neg_life_experiences, y = sat_life)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# To see what the plots look like, we need to arrange them in the "Plot" window. Make sure "gridextra" is ticked in the "Packages" window

grid.arrange(plot1, plot2, nrow = 1, ncol = 2)

##### MULTIPLE REGRESSION WITH CONTINUOUS AND BINARY PREDICTORS #####

# Now, let's run a multiple regression, but this time adding in the three new binary predictors
# We have SWL as the outcome variable that we want to predict
# Then four continuous predictors (the three wellbeing measures and the number of negative life experiences) and three binary (occupational status, relationship status, home location).

model <- lm(sat_life ~ psych_wellbeing + physical_wellbeing + relationship_wellbeing + neg_life_experiences + occ_status + relationship_status + home_location, data = mydata)
summary(model)

# We then want to create scatterplots to show the significant continuous predictors and boxplots to show the significant binary predictors.

# First, build the two scatterplots (using the code from previous weeks)

# Satisfaction with life and psychological wellbeing

plot1 <- ggplot(mydata, aes(x = psych_wellbeing, y = sat_life)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# Satisfaction with life and negative life events

plot2 <- ggplot(mydata, aes(x = neg_life_experiences, y = sat_life)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_classic()

# To see what the scatterplots look like, we need to arrange them in the "Plot" window. Make sure "gridextra" is ticked in the "Packages" window

grid.arrange(plot1, plot2, nrow = 1, ncol = 2)  

# ncol = 2 tells R to put two next to each other, nrow = 1 tells R to have one "row" of graphs, so puts them next to each other.

# Next, we want to create a boxplot for each significant binary predictor. We do this in exactly the same way as for graphing an independent t test, so you can go back to that lecture/workshop if needed.

ggplot(mydata, aes(x = occ_status, y = sat_life)) +
  geom_boxplot() +
  labs(title = "Satisfaction with life by occupational status",
       x = "Occupational status",
       y = "Mean satisfaction with life score") +
  theme_classic()

ggplot(mydata, aes(x = relationship_status, y = sat_life)) +
  geom_boxplot() +
  labs(title = "Satisfaction with life by relationship status",
       x = "Relationship status",
       y = "Mean satisfaction with life score") +
  theme_classic()

# To interpret the binary predictors, you might also want to look at the descriptive variables for each group separately.

# First, looking by occupational status.

descriptives_bygroup <- mydata %>% # Tell R which data set to use.  %>% means "and then" so tells R to move on and do something else
  group_by(occ_status) %>% # group_by is telling R to split the data file - put the variable to split by in brackets
  summarise(mean_sat_life = mean(sat_life), sd_sat_life = sd(sat_life)) # Ask for the mean and standard deviation. statistic_calculated = statistic(variable  )

# You then need R to "print" - or display - the calculated descriptives in the console window.

print(descriptives_bygroup)

# Next,repeat this, but looking by home location.

descriptives_bygroup <- mydata %>% # Tell R which data set to use.  %>% means "and then" so tells R to move on and do something else
  group_by(home_location) %>% # group_by is telling R to split the data file - put the variable to split by in brackets
  summarise(mean_sat_life = mean(sat_life), sd_sat_life = sd(sat_life)) # Ask for the mean and standard deviation. statistic_calculated = statistic(variable  )

# You then need R to "print" - or display - the calculated descriptives in the console window.

print(descriptives_bygroup)

##### MULTIPLE REGRESSION WITH INTERACTIVE PREDICTORS #####

# Now, let's move on to looking at a very simple interactive predictor. For ease of teaching, we will simplify the model...
# Predictor 1: negative life experiences (continuous predictor)
# Predictor 2: relationship status (binary predictor)
# Predictor 3: NLE by relationship status (interactive predictor)

model <- lm(sat_life ~ neg_life_experiences + relationship_status + 
              neg_life_experiences*relationship_status, data = mydata)
summary(model)

# How do you break down and understand a significant interactive predictor? Remember comparing correlations - go back to that!

# Go back to the code we used to statistically compare correlations, and adapt it (if/where needed) to map onto NLE * relationship status predicting SWL...

# First, we need to tell R which subgroups within our dataset we want to look at - so identify 0 and 1 from the "relationship_status" variable, and name each one.

single <- mydata[mydata$relationship_status == "0", ]
relationship <- mydata[mydata$relationship_status == "1", ]

# Now we run the two correlations - we need to tell R first which subgroup to use from the naming we just did, and which continuous variable to correlate.

cor.test(single$sat_life, single$neg_life_experiences,
         method = "pearson")
cor.test(relationship$sat_life, relationship$neg_life_experiences,
         method = "pearson")

# To compare the correlations statistically, we need the N and the r for each group. The r value is the final value in the output we just created - the final line, under corr.
# To get the N for each group, we ran the "summary" earlier, but you can do it again to save scrolling. 

summary(mydata)

# Now we can statistically compare our r values. Make a note of which group you consider to be "1" and which is "2". For this, it will be 1 is single and 2 is in a relationship.
# First, make sure "cocor" is ticked in the "Packages" tab.
# r1 is the first r-value, in this case -0.5218863 
# r2 is the second r-value, in this case -0.1741089
# n1 is the first sample size, in this case 92
# n2 is the second sample size, in this case 108
# the code looks like this, so just replace the values as needed... cocor.indep.groups(r1, r2, n1, n2)

cocor.indep.groups(-0.5218863, -0.1741089, 92, 108)

# Final thing to do - graph these two correlations on the same plot to aid interpretation.

plot_cc <- ggplot(mydata, aes(x = neg_life_experiences, y = sat_life, colour = relationship_status)) +
  geom_point(aes(shape = relationship_status)) +
  geom_smooth(aes(linetype = relationship_status), method = "lm", se = FALSE) +
  labs(title = "Negative life experiences vs Satisfaction with life by Relationship status",
       x = "Negative life experiences",
       y = "Satisfaction with life") +
  theme_classic() +
  scale_color_manual(values = c("0" = "grey", "1" = "black")) +
  scale_linetype_manual(values = c("0" = "solid", "1" = "dashed")) +
  scale_shape_manual(values = c("0" = 16, "1" = 3))

#Now show the graph in the "Plots" window...

print(plot_cc)

##### ASSUMPTIONS OF MULTIPLE REGRESSION #####

# Finally, time to look at assumptions!

# First, the the regression - you report the regression after the assumptions, but R will need the "model" for the assumptions code.

model <- lm(sat_life ~ psych_wellbeing + physical_wellbeing + relationship_wellbeing + neg_life_experiences + occ_status + relationship_status + home_location, data = mydata)
summary(model)

##### MULTICOLLINEARITY #####

# Multicollinearity, looking at r values across all continuous predictor variables.

mydata %>%
  dplyr::select(psych_wellbeing, physical_wellbeing, relationship_wellbeing, neg_life_experiences) %>%
  correlation(p_adjust = "none")

# Multicollinearity, calculate VIF.

vif_values <- vif(model)
print(vif_values)

# Multicollinearity, calculate tolerance. This is, essentially 1 - the R2 (so variance explained).

tolerance_value <- 1 - summary(model)$r.squared
print(tolerance_value)

##### DISTRIBUTION OF RESIDUALS #####

# Create the histogram of residuals

ggplot(mydata, aes(x = model$residuals)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

##### HOMOSCEDASTICITY #####

# Create the scatterplot for homoscedasticity

ggplot(mydata, aes(x = model$fitted.values, y = model$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

##### EVALUATE THE NUMBER OF OUTLIERS #####

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


##### Yay - R learning finished!!! #####