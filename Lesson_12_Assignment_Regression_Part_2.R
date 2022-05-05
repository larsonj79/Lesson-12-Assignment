# Lesson 12 Assignment - Regression Part 2

# Your assignment is to write the commands instructed in the comments below. To run your
# commands, simply hit Ctrl+Enter (command+return on a MAC) when the cursor is on that 
# command line. You can also type commands directly into the Console below, but you must
# save them in this file for your assignment.

# Do not change these eight lines or GradeScope will not work
library(readr)
library(dplyr)
library(ggplot2)
fexppre <- read_csv("fexppre.csv")
fexpdur <- read_csv("fexpdur.csv")
fexpratios <- read_csv("fexpratios.csv")
fexpcondrat <- read_csv("fexpcondrat.csv")
fexppredict <- read_csv("fexppredict.csv")


# You will be working with four different datasets, but they're all derived from the same 
# dataset: the field experiment data you analyzed back in lessons 3, 4, 5, and 9. You 
# will finally be able to learn the outcome of that field experiment.

# The dataset *fexppre* contains the sales and advertising figures for each of 210 
# regions for the 3-month period prior to the field experiment. 

#1. To familiarize yourself a bit with this dataset, create a report of the 10 regions 
# with the highest population (POPN > 2000000), sorted by sales (WebSales) in 
# descending order. Call the report top10sales.
top10sales <- fexppre %>% 
  filter(POPN > 2000000) %>% 
  arrange(desc(WebSales))

#2. Examine whether advertising amounts are predictive of sales. With the fexppre data,
# run a regression model predicting sales (WebSales) from all four advertising channels 
# (Google Prospecting - GP, Google Retargeting - GR, Facebook Prospecting - FP, 
# Facebook Retargeting - FR). Save the regression model as mod1. Output the summary 
# report from this model.
mod1 <- lm(WebSales ~ GP + GR + FP + FR, data = fexppre)
summary(mod1)

#3. Interpret the coefficient for GR (Google Retargeting).
# *ENTER ANSWER HERE*

#4. Using ggplot, create a scatter plot with WebSales on the y-axis and Google 
# Retargeting (GR) on the x-axis. Save the plot as grplot.
grplot <- ggplot(fexppre, aes(x = GR, y = WebSales)) +
  geom_point()

# The negative coefficient on GR from the first regression model implies a negative 
# relationship between Google Retargeting and WebSales. But this plot shows a strong 
# positive relationship. Remember, coefficients of a regression model must be read, 
# "all else being equal . . ." Clearly, not all else is equal in the other advertising 
# channels.

#5. Create that same plot again, but this time, map POPN onto size. Make the points 
# 50% transparent. Save the plot ass grplot2.
grplot2 <- ggplot(fexppre, aes(x = GR, y = WebSales, size = POPN)) +
  geom_point(alpha = .5)

#6. What is the relationship between GR and POPN?
# *ENTER ANSWER HERE*

#7. What is the relationship between WebSales and POPN?
# *ENTER ANSWER HERE*

#8. Remember, correlation is not causation. The plot shows a strong correlation 
# between GR and WebSales. What is the true causal relationship among POPN, GR, 
# and WebSales?
# *ENTER ANSWER HERE*

# Because of this alternative causal relationship, the furniture company ran a 
# field experiment that randomized advertising amounts so that the true causal 
# relationship between advertising and sales could be assessed. 

#9. Run a regression model predicting sales from all four advertising channels, 
# but this time using the *fexpdur* dataset (the sales and advertising amounts 
# *during* the experiment). Save the model as mod2. Output the summary report.
mod2 <- lm(WebSales ~ GP + GR + FP + FR, data = fexpdur)
summary(mod2)

#10. Interpret the coefficient on Facebook Retargeting (FR).
# *ENTER ANSWER HERE*

#11. Is this coefficient statistically significant?
# *ENTER ANSWER HERE*

#12. To verify that the field experiment removed the problematic alternative 
# causality, plot WebSales (on the y-axis) against Facebook Retargeting (on 
# the x-axis). Map POPN onto size, and again make the points 50% transparent.
# Save the plot as frplot.
frplot <- ggplot(fexpdur, aes(x = FR, y = WebSales, size = POPN)) +
  geom_point(alpha = .5)

# The relationship between POPN and advertising has weakened, but there is 
# still a positive correlation, which means we still can't trust the prior 
# regression model.

#13. Look at the first 10 rows of the *fexpratios* data.
fexpratios[1:10,]

# This new dataset contains all the same variables as *fexppre* and *fexpdur*, 
# but the values are not advertising and sales amounts, but ratios. 
# Specifically, they are fexpdur / fexppre. That is, they report the amount 
# spent during the field experiment relative the amount spent in the time 
# period before the field experiment. For example, the WebSales value of 1.61 
# for Lafayette, LA indicates that WebSales during the field experiment was 
# 61% higher during the experiment than it was in the period prior.

#14. Run a regression model (save it as mod3) predicting WebSales from the 
# four advertising channels, using *fexpratios*. Show the summary report.
mod3 <- lm(WebSales ~ GP + GR + FP + FR, data = fexpratios)
summary(mod3)

#15. Which advertising channels are statistically significant?
# *ENTER ANSWER HERE*

# The *fexpratios* data removes the problematic correlation between POPN and 
# advertising/sales amounts, but the ratios from this data introduce another 
# problematic data feature.

#16. To see this problematic feature, plot WebSales (on the y-axis) against 
# POPN. Put POPN on a log10 scale. Save the plot as popnplot.
# (https://campus.datacamp.com/courses/introduction-to-the-tidyverse/types-of-visualizations?ex=9).
popnplot <- ggplot(fexpratios, aes(x = POPN, y = WebSales)) +
  geom_point() +
  scale_x_log10()

# Notice the funnel shape of the points. Variation in WebSales is lower as 
# the population increases. Remember, WebSales in the *fexpratios* data is a 
# ratio of during-field-experiment sales relative to before-field-experiment
# sales.

#17. Why is variation on WebSales higher in the lower-population regions?
# *ENTER ANSWER HERE*
  
# To correct for this last problem, we will now use our final dataset, 
# *fexpcondrat*. 

# 18. Print the whole dataset to the screen.
fexpcondrat

# This dataset is different from *fexpratios* in two ways. (1) The 
# advertising and sales ratios are calculated by field experiment condition 
# instead of by region. Low-population regions exhibit high variance in 
# sales, making estimates unreliable. By pooling sales and advertising 
# figures by field experiment condition, we remove the high level of 
# idiosyncratic variation. (2) The data from the "OUTLIERS" condition has 
# been removed, because those regions were not included in the field 
# experiment.

# Now that these problems have been removed, we can finally run a 
# regression model that we can trust.

#19. Using *fexpcondrat*, run a regression model predicting sales (save it 
# as mod4) from the four advertising channels. Show the summary report.
mod4 <- lm(WebSales ~ GP + GR + FP + FR, data = fexpcondrat)
summary(mod4)

#20. Which channels significantly affect sales?
# *ENTER ANSWER HERE*
  
# Interpretation of these coefficients functions the same as any regression 
# model, but the data is in ratio format instead of a straightforward unit 
# like dollars. With our ratio data, a "one-unit increase" in GP means a 
# 100% increase in Google Prospecting advertising (relative to the amount 
# spent on Google Prospecting in the pre-field-experiment period).

#21. If we increase GP by one unit (a 100% increase in spending), what is 
# the expected effect on WebSales?
# *ENTER ANSWER HERE*
  
#22. If the company spent $100,000 on Google Prospecting in the pre-field-
# experiment period and earned $4 million during the same period, how much 
# would the company expect to earn in sales if it spent $200,000 on Google 
# Prospecting in a similar period? (Spending of $200,000 on Google 
# Prospecting is a "one-unit increase". That should produce a percentage 
# increase, see question 21, in the $4 million in sales.)
# *ENTER ANSWER HERE*
  
#23. If the company spent $200,000 on Facebook Prospecting in the pre-
# field-experiment period and earned $4 million during the same period, 
# how much would the company expect to earn in sales if it spent $300,000 
# on Facebook Prospecting in a similar period? (Spending of $300,000 on 
# Facebook Prospecting is not a one-unit increase. It is a half-unit 
# increase.)
# *ENTER ANSWER HERE*
  
# Because this regression model is based on ratios, the coefficients 
# express an elasticity. You might remember price elasticity from ECON 
# 110. Price elasticity is expressed as: 
# (% change in demand) / (% change in price). Our regression is expressing 
# an advertising elasticity: 
# (% change in sales) / (% percent change in advertising). An elasticity 
# greater than 1 is considered a high elasticity, and lower than 1 is a 
# low elasticity.

#24. Does advertising exhibit high elasticity or low elasticity?
# *ENTER ANSWER HERE*
  
# Advertising can influence sales through two pathways. (1) It can increase 
# the number of purchases made, or (2) it can influence the amount spent 
# during a transaction. 

#25. To investigate which pathway is being influenced by advertising, 
# create a new variable, AOV, for Average Order Value, in our *fexpcondrat* 
# dataset. AOV is WebSales / WebOrders. 
fexpcondrat <- fexpcondrat %>% 
  mutate(AOV = WebSales / WebOrders)

#26. Conduct a regression model (mod5) predicting WebOrders from the four 
# advertising channels. Output the summary report.
mod5 <- lm(WebOrders ~ GP + GR + FP + FR, data = fexpcondrat)
summary(mod5)

#27. Conduct a regression model (mod6) predicting AOV from the four 
# advertising channels. Output the summary report.
mod6 <- lm(AOV ~ GP + GR + FP + FR, data = fexpcondrat)
summary(mod6)


#28. Based on these last two models, which is the pathway by which 
# advertising affects sales?
# *ENTER ANSWER HERE*
  
# Advertising in some channels might not be completely independent. That 
# is, spending on one advertising channel might influence the effect of 
# advertising in a different channel. In other words, there might be an 
# interaction between advertising spending in some channels. The most 
# likely interaction would be between the Prospecting and Retargeting 
# channels on the same advertising platform.

#29. Run a regression model predicting WebOrders from the four 
# advertising channels as well as (1) the interaction between GP and GR 
# and (2) the interaction between FP and FR. Save the model as mod7. 
# Show the summary report.
mod7 <- lm(WebOrders ~ GP + GR + FP + FR + GP:GR + FP:FR, data = fexpcondrat)
summary(mod7)

#30. Is there evidence of an interaction effect of advertising on either 
# Google or Facebook?
# *ENTER ANSWER HERE*
  
# Pretend for a minute that the interaction terms were statistically 
# significant. The company is considering an increase to its Google 
# advertising budget. It can increase Google Prospecting by 20%, Google 
# Retargeting by 20% or both by 20%. 

#31. Apply your last regression model to the *fexppredict* dataframe 
# to find the predicted sales. Save the predicted sales amounts as 
# predsales.
predsales <- predict(mod7, newdata = fexppredict)

# **EXTRA CREDIT** *OPTIONAL*
#32. The company has determined to increase its Google advertising budget 
# by 40%. If we assume the company was spending equal amounts on Google 
# Prospecting and Google Retargeting, then it can increase GP to 1.4, 
# increase GR to 1.4, increase both GP and GR to 1.2, or increase them in 
# some combination that adds up to only a 40% total increase (e.g., 1.1 
# and 1.3). Assuming the last regression model is correct and 
# statistically significant, find the optimal increase across GP and GR
# to produce the highest level of sales. (Assume FP and FR remain at 1.)
# Save your chosen values as admaxsales. (Start by saving admaxsales as
# the first row of fexppredict. Then change the amonts of GP and GR.)
admaxsales <- fexppredict[1, ]
admaxsales$GP <- 1.4
admaxsales$GR <- 1
