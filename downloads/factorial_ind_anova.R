########################################################################
######################    PS2010 - Workshop 6     ######################
##################### Factorial ANOVA (Independent) ####################
########################################################################

## 1. import packages and data
# first we need to install and/or load any packages needed for today's analysis.
install.packages("afex")
install.packages("emmeans")
library(tidyverse)
library(afex)
library(emmeans)
library(car)

#next we need to check/set the working directory and then import today's data file. make sure you have downloaded the file
getwd() #change the directory if needed, or save the file to this folder (run the code)
mydata <- read_csv("socialmedia.csv")

## 2. prepare data
#have a quick check of your data file to learn about it.
head(mydata)
names(mydata)
summary(mydata)
view(mydata)

## 3. describe data
#we have a factorial design so we need to group by BOTH independent variables
#we will also ask for n() to tell us about the group sizes
desc <- mydata %>%
  group_by(notification, usage) %>%
  summarise(m = mean(anxiety), 
            sd = sd(anxiety),
            n = n())

view(desc) #we can view our descriptive here. this will open a new table. 

#let's also visualise our data with a box plot to see what is going on.
ggplot(mydata, aes(x = usage, y = anxiety, fill = notification)) +
  geom_boxplot() +
  theme_classic()

ggsave("boxplot.jpg")

## 4. model data
#we can run the ANOVA using aov_ez()
anova_result <- aov_ez(id = "id",
                        dv = "anxiety",
                        between = c("notification", "usage"),
                        type = 3,
                        include_aov = TRUE,
                        data = mydata)

factorial_output <- summary(anova_result)
print(factorial_output)

library(effectsize)
eta_squared(anova_result, partial = TRUE) #ask for partial eta square

#if the interaction is significant we need to break it down!
#let's plot it first
interaction.plot(mydata$usage,          # plot the x-axis
                 mydata$notification,   # plot the different lines
                 mydata$anxiety,        # state the dependent variable
                 fun = mean)            # ask for the mean values

#first we need to get the emmeans
#then we need to ask for cohen's d too
posthoc_factorial <- emmeans(anova_result, 
                             pairwise ~ notification| usage, 
                             adjust = "holm")
output <-  posthoc_factorial$contrasts %>%
  summary() %>% 
  mutate(cohen_d = effectsize::t_to_d(t.ratio, df))

view(output)

#there was a main effect too, we can have a look but really the interaction is most important.

#you can use the same contrasts as previous weeks. 
#we need emmeans to run post-hocs
em_means <- emmeans(anova_result, ~ usage)

# Pairwise comparisons (posthoc)
pairwise_contrasts <- contrast(em_means, method = "pairwise")
print(pairwise_contrasts, adjust = "holm")
print(pairwise_contrasts, adjust = "bonferroni")
print(pairwise_contrasts, adjust = "tukey")

## 5. evaluate model
#testing normality, looking at normal distribution of residuals from the model.
residuals <- residuals(anova_result) #pulls residuals from the model
qqPlot(residuals) #produces a qq-plot of residuals
hist(residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue") #histogram of residuals
shapiro.test(residuals) #Shapiro test for residuals

# arghhhhh A VIOLATION. is this a problem?
# well, Field et al. (2009) notes that an ANOVA is robust to violations of both normality and homogeneity IF...
# ...sample sizes are equal. We can check this:
mydata %>% count(usage, notification)

#testing homogeneity of variance
install.packages("car")
library(car)
leveneTest(anxiety ~ usage*notification, mydata)

###### END OF WORKSHOP ######

# Optional code: try out the code below to produce different types of plots.

