
# install.packages("sjmisc")
# install.packages("plyr")
# install.packages("ggplot2")

library(lme4)
library(multcomp)
library(reshape)
library(plyr)
library(ggplot2)
library(lmerTest) 
library(emmeans)
library(sjPlot)
library(sjmisc)




## read the data into R

dataset = read.csv('workshopdata.csv', sep = ',', header = T)




#inspect the data 

str(dataset)

head(dataset)

summary(dataset)

View(dataset)







####################################
#visualisation with simple code ####
####################################


# a simple boxplot and histogram to explore ages of participants 
boxplot(dataset$age, main = "Age")

hist(dataset$age, main = "Age")



# a simple plot of age by postonset data 

plot(dataset$age, dataset$postonset, main = "My Plot")

plot(dataset$age, dataset$postonset, main = "My Plot", col = "red")

plot(dataset$age, dataset$postonset, main = "My Beautiful Plot", col = "purple")




### bar plot for frequency of responses
### you need to created summary/plot table first
### then plot is created based on that table

plot.table = table (dataset$accuracy, dataset$Sentence_Type)

barplot(plot.table)





###### GGPLOT#####
#Let's plot a simple bar graph to visualize our accuracy data
## again we need a summary table to visualize accuracy 

plot.table <- ddply(dataset, c("Sentence_Type", "Question_Type"), summarise,
                    N    = length(accuracy),
                    mean = mean(accuracy, na.rm=TRUE),
                    sd   = sd(accuracy, na.rm=TRUE),
                    se   = sd / sqrt(N))



ggplot(plot.table, aes(x=Sentence_Type, fill =Question_Type, y=mean, label=mean)) + 
  geom_bar(position=position_dodge(), stat="identity", colour="black", size=.3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) 







#############################
### GLMER ###################
#############################

# Now let's run our first logistic regression model
# Practicing with random effects 
# There are many different types of random effect structurs in LME models



# 1 ) simple random intercept 
# here "+ (1 |Item )" takes items as a random factor

model1 = glmer(accuracy ~ Sentence_Type * Question_Type  + (1 |Item ) , 
               family=binomial, data=dataset)


summary(model1)
ranef(model1)





# 2) random intercept with a nested factor
# "(1|participant / gender)" here this piece of codes means that 
# participants are my random intercept but within participants 
# gender is a nested factor which might influence responses differentially
# but we assume they are not independent from each other 

model2 = glmer(accuracy ~ Sentence_Type * Question_Type  +  
                 (1|participant / gender), family=binomial, data=dataset)


summary(model2)




# 3 ) random intercept with two or multiple independent factors 
# here we are taking " (1|Item) + (1|participant)" both items and participants as random intrecepts 
# and we assume these two factors are independent from each other
# this is very much used in linguistics

model3 = glmer(accuracy ~ Sentence_Type * Question_Type  +  
                 (1|Item) + (1|participant), family=binomial, data=dataset)


summary(model3)




#4) random intercepts and random slope
# In a random intercept structure for example, (1| Item) 
# assumes that the source of variability in items are items themselves 
# with " (1+postonset | Item)" we force the model to assume slopes of 
# random intercept for items to vary by items themselves (1+) and (postonset).  

model4 = glmer(accuracy ~ Sentence_Type * Question_Type  + 
                 (1+postonset | Item) + (1|participant), family=binomial, data=dataset)


summary(model4)




#5) random intercepts and random slope
# we forced random intercept for items to vary by items themselves (1+) and (postonset).  
# we can however, remove items as random slopes from this structure and 
# calculate random intercepts for items modulated by (postonset) but not items themselves
# to do this we can try formulate (postonset | Item) without 1
# or  (0+postonset | Item) where random slope for items themselves are forced to removed with 0

model5 = glmer(accuracy ~ Sentence_Type * Question_Type  + 
                 (0+postonset | Item) + (1|participant), family=binomial, data=dataset)


summary(model5)




# checking for a fixed-effect of a co-variate 

model6 = glmer(accuracy ~ Sentence_Type * Question_Type  +  postonset +
                 (1|Item) , family=binomial, data=dataset)


summary(model6)





######################################
##### Interpreting results ###########
######################################


# interpreting fix effects 

fixef(model1)

plogis(fixef(model1) ["(Intercept)"])



#now let's inspect the distribution of residuals 
#This is not normal probably, but normality is not required for logistic regression

qqnorm (resid(model4))
qqline (resid(model4))







#posthoc tests with emmeans 



emmeans(model4, "Question_Type", by = "Sentence_Type")

pairs(emmeans(model4, "Sentence_Type", by = "Question_Type"))




#alternative posthoc tests 
#first subset your data into different question types
data.who <- subset(dataset, Question_Type == 'Who') 
data.which <- subset(dataset, Question_Type == 'Which') 

library(multcomp)

data.who$Factor = interaction(data.who$Sentence_Type)
model = glmer(accuracy ~ Factor + (1|Item) + (1|participant), data=data.who, family = binomial)
summary(model)
summary(glht(model,linfct=mcp(Factor = "Tukey")))








