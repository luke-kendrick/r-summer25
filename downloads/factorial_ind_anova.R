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

#now you are working with more complex data sets, we need to make sure variables are set-up correctly. This is quite an important step.
#we need to make sure any categorical variables are coded as a "factor" and any scale variables are coded as "numeric".
mydata$notification = factor(mydata$notification) #turns variable into a factor
mydata$usage = factor(mydata$usage, levels = c("rare", "regular", "problematic")) #turns variable into a factor and orders levels
mydata$anxiety = as.numeric(mydata$anxiety) #turns variable into numeric

## 3. describe data
#we have a factorial design so we need to group by BOTH independent variables
#we will also ask for n() to tell us about the group sizes
descriptives <- mydata %>%
  group_by(notification, usage) %>%
  summarise(m = mean(anxiety), 
            sd = sd(anxiety),
            n = n(),
            se = sd/sqrt(n))

view(descriptives) #we can view our descriptive here. this will open a new table. 

#let's also visualise our data with a box plot to see what is going on.
ggplot(mydata, aes(x = usage, y = anxiety, fill = notification)) +
  geom_boxplot() +
  theme_classic()

ggsave("boxplot.jpg")

#you could create a violin plot too if you prefer!
ggplot(mydata, aes(x = usage, y = anxiety, fill = notification)) +
  geom_violin(trim = FALSE, alpha = 0.1) +  # Violin plot without trimming tails
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5, position = position_dodge(.9)) +  # Optional: Overlay a boxplot for additional summary statistics
  theme_minimal() +  # Classic theme
  labs(title = "Violin Plot of Anxiety Scores by Notification and Use",  # Add a title
       x = "Usage",  # X-axis label
       y = "Anxiety Score")  # Y-axis label


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

# Games-Howell is an option for post-hocs when you have unequal variances
# requires rstatix
install.packages("rstatix")
library(rstatix)
games_howell <- mydata %>%
  games_howell_test(anxiety ~ usage)
print(games_howell)

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
# plot 1
ggplot(mydata, aes(x = usage, y = anxiety, fill = notification)) +
  geom_boxplot(outlier.shape = NA, color = "black", width = 0.6, 
               alpha = 0.8, notch = FALSE) +  # Boxplot with black outline and transparency
  theme_classic(base_size = 16) +  # Classic theme with increased base font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"),  # Centered, bold title
    axis.title.x = element_text(size = 16, face = "bold", color = "black"),  # Bold and larger x-axis title
    axis.title.y = element_text(size = 16, face = "bold", color = "black"),  # Bold and larger y-axis title
    axis.text.x = element_text(size = 14, color = "black"),  # Larger x-axis text
    axis.text.y = element_text(size = 14, color = "black"),  # Larger y-axis text
    legend.title = element_text(size = 16, face = "bold", color = "black"),  # Bold legend title
    legend.text = element_text(size = 14, color = "black"),  # Larger legend text
    panel.grid.major = element_line(color = "grey85", linetype = "dotted"),  # Light grey dotted major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", color = NA),  # White background for the panel
    plot.background = element_rect(fill = "white", color = NA),  # White background for the entire plot
    legend.position = "right",  # Position the legend to the right
    legend.key = element_rect(fill = "white", color = NA)  # White background for legend keys
  ) +
  scale_fill_brewer(palette = "Set3") +  # Use a colorblind-friendly palette
  labs(
    title = "Box Plot of Anxiety Scores by Usage and Notification Type",  # Clear title
    x = "Usage Type",  # X-axis label
    y = "Anxiety Score",  # Y-axis label
    fill = "Notification Type"  # Legend title
  ) +
  coord_flip()  # Optional: Flip coordinates for a horizontal box plot

#plot 2
ggplot(mydata, aes(x = usage, y = anxiety, fill = notification)) +
  geom_boxplot(outlier.shape = NA, color = "black", width = 0.6, alpha = 0.7) +  # Boxplot with no outliers and transparency
  theme_minimal(base_size = 16) +  # Minimal theme with increased base font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold", color = "black"),  # Centered, bold title
    axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold and larger x-axis title
    axis.title.y = element_text(size = 18, face = "bold", color = "black"),  # Bold and larger y-axis title
    axis.text.x = element_text(size = 14, color = "black"),  # Larger x-axis text
    axis.text.y = element_text(size = 14, color = "black"),  # Larger y-axis text
    legend.title = element_text(size = 16, face = "bold", color = "black"),  # Bold legend title
    legend.text = element_text(size = 14, color = "black"),  # Larger legend text
    panel.grid.major = element_line(color = "grey85", linetype = "dotted"),  # Light grey dotted major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", color = NA),  # White background for the panel
    plot.background = element_rect(fill = "white", color = NA),  # White background for the entire plot
    legend.position = "right",  # Position the legend to the right
    legend.key = element_rect(fill = "white", color = NA)  # White background for legend keys
  ) +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73")) +  # Custom color palette for distinction and colorblind friendliness
  labs(
    title = "Anxiety Scores by Usage Type and Notification",  # Clear and concise title
    x = "Usage Type",  # X-axis label
    y = "Anxiety Score",  # Y-axis label
    fill = "Notification Type"  # Legend title
  ) +
  coord_flip()  # Optional: Flip coordinates for a horizontal box plot

# plot 3
ggplot(mydata, aes(x = usage, y = anxiety, fill = notification)) +
  geom_violin(trim = FALSE, alpha = 0.2, color = "black", lwd = 0.1) +  # Smooth violin plot with transparency and black outline
  geom_boxplot(width = 0.15, color = "black", alpha = 0.5, position = position_dodge(0.9)) +  # Overlay a slim boxplot for additional statistics
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black", position = position_dodge(0.9)) +  # Overlay mean points
  theme_minimal(base_size = 16) +  # Minimal theme with increased base font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold", color = "black"),  # Centered, bold title
    axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold and larger x-axis title
    axis.title.y = element_text(size = 18, face = "bold", color = "black"),  # Bold and larger y-axis title
    axis.text.x = element_text(size = 14, color = "black"),  # Larger x-axis text
    axis.text.y = element_text(size = 14, color = "black"),  # Larger y-axis text
    legend.title = element_text(size = 16, face = "bold", color = "black"),  # Bold legend title
    legend.text = element_text(size = 14, color = "black"),  # Larger legend text
    panel.grid.major = element_line(color = "grey85", linetype = "dotted"),  # Light grey dotted major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", color = NA),  # White background for the panel
    plot.background = element_rect(fill = "white", color = NA),  # White background for the entire plot
    legend.position = "right",  # Position the legend to the right
    legend.key = element_rect(fill = "white", color = NA)  # White background for legend keys
  ) +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73")) +  # Custom color palette for distinction and colorblind friendliness
  labs(
    title = "Anxiety Scores by Usage Type and Notification",  # Clear and concise title
    x = "Usage Type",  # X-axis label
    y = "Anxiety Score",  # Y-axis label
    fill = "Notification Type"  # Legend title
  )
