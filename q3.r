library(likert)
library(reshape2) # contains melt function for converting tables to charts
library(magrittr)
library(haven)
library(ggplot2)
library(dplyr)
setwd("E:/Google Drive/Berkeley MIDS/w203/my_lab1/data")

# Haven reference: https://haven.tidyverse.org/reference/read_dta.html

# This code can be used to access column descriptions/labels:
# attr(d$V200003, 'label') # just an example

# NOTE: Lee said on 2/27/2021 5:26 in the class channel that we can 
# disregard weights.


################################################ 1. IMPORT AND CLEAN DATA ################################################
d <- read_dta("anes_timeseries_2020_stata_20210211.dta")

# NOTE: See code book on ANES' website for more detail on these codes.
d_trim = d[, c('V201018', 'V201507x', 'V201151', 'V201153',
               'V201624', 'V201625', 'V201066', 'V201145', 'V201146')]

# Rename columns
colnames(d_trim) = c('party_of_reg', 'age', 'sentiment_biden', 'sentiment_harris',
                     'covid_test_positive', 'covid_symptoms',
                     'vote_for_governor', 'gov_approval', 'gov_how_much')


#####   Q3: ARE SURVEY RESPONDENTS WHO HAVE HAD SOMEONE IN THEIR HOME INFECTED
#####   BY COVID MORE LIKELY TO DISAPPROVE OF THE WAY THEIR GOVERNOR IS HANDLING
#####   THE PANDEMIC 


# Trim columns relevant to q3
q3 <- d_trim[, c('covid_test_positive', 'covid_symptoms',
               'vote_for_governor', 'gov_approval', 'gov_how_much')]

# Clean the labels from the spss dataset
q3_clean <- zap_labels(q3)


## REFERENCE:
## Intend to vote for governor? 1 = yes, 2 = no, -1 = inapplicable, -8 = don't know
## Governor approve or disapprove: 1 = approve, 2 = disapprove, -8 = don't know, -9 = refused
## How strongly approve/disapprove? 1 = strongly, 2 = not strongly, -1 = inapplicable
##        -8 = don't know, -9 = Refused

################################################ 2. INVESTIGATE/SANITY CHECK DATA ################################################

# 1) Are there any samples where "don't know" is answered for "approve/disapprove" and
# "strongly/not strongly" given for follow up question, or vice versa?
subset(q3_clean, gov_approval < 0)
subset(q3_clean, gov_approval == -8)
subset(q3_clean, gov_approval == -9)
subset(q3_clean, gov_approval == -8 & gov_how_much == -9)
subset(q3_clean, gov_approval == -9 & gov_how_much == -8)
subset(q3_clean, gov_approval == -8 & gov_how_much == -8)

# Observations: all -8 for "gov_approval" have -1 for gov_how_much. Same with -9
# So we're consistent between these two columns.

# 2) How many were refused or interview broken off?
breakoff_count = count(subset(q3_clean, covid_test_positive == -5))
refused_count = count(subset(q3_clean, covid_test_positive == -9))

sprintf('Number of broken off interviews: %s', breakoff_count)
sprintf('Number of refused answers: %s', refused_count)

# Probably going to want to use non-parametric Wilcoxon Ranked sum test
# Maybe write a function to create a new column in rank form of strongly approve,
# approve, neutral (-8?), disapprove, strongly disapprove


# Define function that will rank, from 1 to 5 on the likert scale, based on the two columns
# in which approval data is currently located (one is just approve/disapprove, the next is strongly or not).
# This function will then be applied to those column vectors, and a new column with a 1-5 likert score created,
# which will be used for the Rank-sum hypothesis test and visualizations.

# REFERENCE control flow: https://adv-r.hadley.nz/control-flow.html

# (1,1) = strongly approve, (1,2) = approve, -8 = neutral, 
# (2,2) = disapprove, (2,1) = disapprove strongly
scale_approval <- function(approve_disapprove, how_much){
  if (approve_disapprove == 1 & how_much == 1) {
    # "strongly approve"
    return(1)
  } else if (approve_disapprove == 1 & how_much == 2) {
    # "approve"
    return(2)
  } else if (approve_disapprove == -8){
    # "neutral"
    return(3)
  } else if (approve_disapprove == 2 & how_much == 2){
    # "disapprove"
    return(4)
  } else if (approve_disapprove == 2 & how_much == 1) {
    # "strongly disapprove"
    return(5)
  } else {
    return(-88) # needs this for cases where some version of the above isn't met
  }
}

# NOTE: -88 is returned in the function above for easy subsetting and filtering below based on positive/negative values.
q3_clean$gov_scale <- mapply(scale_approval, approve_disapprove = q3_clean$gov_approval,
       how_much = q3_clean$gov_how_much)
q3_clean <- subset(q3_clean, gov_scale > 0) # remove all entries with gov_scale == -88


#### NOTES ON CLEANED DATASET AFTER ADDING NEW COLUMN
# There are 46 entries that did not fit into one of the 5 conditional statements
# according to my function scale_approval. These were labeled by the function as
# -88. Upon investigation of these, all fall into one of the following categories:
# 1) R refused to provide an answer for either "approve/disapprove" or "how much"
#    If someone answered "don't know" in "approve/disapprove", they are labeled
#    as neutral. If a respondent answered approve/disapprove but then refused
#    the follow up question about "how much," then that sample is lacking the
#    necessary data to fit into the new column
# 2) Refused to respond at all, resulting in a -9 entry for approve/disapprove
# 
# Since the number of samples that fit into one of the above two categories are
# small, compared to the total sample size, and they lack the data needed to calculate
# the new 1-5 likert valuewhich is needed to run Wilcox.test, I decided to drop them. 

## REFERENCE:
## covid_test_positive: 
#### 1 = yes, 2 = no one tested positive, -5 = interview breakoff, -9 = refused

## covid_house_symptoms: 
#### 1 = suspected of having covid, 2 = no one has been suspected of having covid
####-5 = interview breakoff, -9 = refused
#### Observation: No overlap in negative values between these two
subset(q3_clean, covid_test_positive > 0)
subset(q3_clean, covid_test_positive == -5 & covid_symptoms == -9)
subset(q3_clean, covid_test_positive == -9 & covid_symptoms == -5)


################################################ HYPOTHESIS TESTS ################################################
#### NOTE: wilcox.test can't take gov_scale in factor form, since it needs to
#### add the ranks together to perform the hypothesis test. That column
#### is factored below for the visualizations, since the likert package
#### takes factors as inputs.


## HT 1): tested positive vs governor approval
# Subset q3_clean to create two vectors, since Wilcox.test two numeric vectors x, y as input. 
samples_positive_covid_test <- subset(q3_clean, covid_test_positive == 1)
samples_negative_covid_test <- subset(q3_clean, covid_test_positive == 2)

wilcox.test(x=samples_positive_covid_test$gov_scale, 
            y=samples_negative_covid_test$gov_scale,
            alternative="two.side",
            paired = FALSE, conf.int = TRUE, conf.level=0.95)

## HT 2): positive symptoms vs governor approval
# Same logic as with HT 1, except for covid_symptoms instead of positive_covid_test.
samples_covid_symptoms <- subset(q3_clean, covid_symptoms == 1)
samples_no_covid_symptoms <- subset(q3_clean, covid_symptoms == 2)

wilcox.test(x=samples_covid_symptoms$gov_scale, 
            y=samples_no_covid_symptoms$gov_scale,
            alternative="two.side",
            paired = FALSE, conf.int = TRUE, conf.level = 0.95)

################################### PRACTICAL SIGNIFICANCE ################################################
## REFERENCES/READING:
# https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/#correlation-coefficient
# https://stats.stackexchange.com/questions/3730/pearsons-or-spearmans-correlation-with-non-normal-data
# https://stats.stackexchange.com/questions/8071/how-to-choose-between-pearson-and-spearman-correlation
# https://rpubs.com/aaronsc32/spearman-rank-correlation
# https://statistics.laerd.com/statistical-guides/spearmans-rank-order-correlation-statistical-guide-2.php
# NOtation for spearman's rho: https://www.statisticssolutions.com/spearman-rank-correlation/


# Drop samples where covid test is below 0
q3_clean_covid_test <- subset(q3_clean, covid_test_positive > 0)

# This conducts the rank sum test and returns Spearman's Rho
cor.test(q3_clean_covid_test$covid_test_positive, 
                 q3_clean_covid_test$gov_scale, method="spearman",
                 exact=FALSE)

# This just returns Spearman's Rho
cor(q3_clean_covid_test$covid_test_positive, 
         q3_clean_covid_test$gov_scale, method="spearman")

# Drop samples where covid symptoms is below 0
q3_clean_covid_symptoms <- subset(q3_clean, covid_symptoms > 0)

cor.test(q3_clean_covid_symptoms$covid_symptoms, 
         q3_clean_covid_symptoms$gov_scale, method="spearman",
         exact=FALSE)

cor(q3_clean_covid_symptoms$covid_symptoms, 
    q3_clean_covid_symptoms$gov_scale, method="spearman")


################################ VISUALIZATIONS & TABLES ################################################
#### USE q3_clean_noneg in this section, rather than q3_clean. The former filters out negative values. In the prior section,
#### I split out positive and not positive results, which filtered negative numbers by default. The difference is due
#### only to wilcox.test taking a different type of input (array/vectors as x and y) than likert(a dataframe).

# Drop samples where covid test is below 0, then factor gov_scale column
q3_clean_noneg <- subset(q3_clean, covid_test_positive > 0 & 
                         covid_symptoms > 0)

# Recast gov_scale & test_positive columns as factors, and
# recode gov_scale factors from 1-5 to "strongly approve" etc,
# so that they show up in the visualization correctly
q3_clean_noneg$gov_scale <- as.factor(q3_clean_noneg$gov_scale)
q3_clean_noneg$covid_test_positive <- as.factor(q3_clean_noneg$covid_test_positive)

# Logic behind recasting columns with numeric factors to character factors: the likert package provides some nice,
# turnkey graphics for likert-type data. But customizing the plot functionality is a little convoluded, so it's easier
# to just rename/recast the data being fed into the likert plot function.

# This must happen here, rather than above, since the Wilcox.test hypothesis works but calculating numeric
# rank values, then adding them together. If these numeric types are first converted to factors, Wilcox.test fails
q3_clean_noneg$gov_scale <- as.factor(recode(q3_clean_noneg$gov_scale, `1` = "Strongly Approve", `2` = "Approve", 
              `3` = "Neutral", `4` = "Disapprove", `5` ="Strongly Disapprove"))

q3_clean_noneg$covid_test_positive <- as.factor(recode(q3_clean_noneg$covid_test_positive, `1` = "Household tested positive",
                                             `2` = "No positive test"))

q3_clean_noneg$covid_symptoms <- as.factor(recode(q3_clean_noneg$covid_symptoms, `1` = "Person in household had COVID symptoms",
                                                       `2` = "No symptoms in household"))

# Table 1
covid_test_positive_table <- table(q3_clean_noneg$gov_scale, q3_clean_noneg$covid_test_positive)
covid_test_positive_table <- round(prop.table(covid_test_positive_table, 2), 2)
covid_test_positive_table_melt <- melt(covid_test_positive_table)
colnames(covid_test_positive_table_melt) <- c("gov_scale", 
                                              "positive_test",
                                              "percent")
# Reference: https://stackoverflow.com/questions/22305023/how-to-get-a-barplot-with-several-variables-side-by-side-grouped-by-a-factor

# https://stackoverflow.com/questions/14622421/how-to-change-legend-title-in-ggplot
ggplot(covid_test_positive_table_melt, 
       aes(x=gov_scale, y=percent, fill=factor(positive_test))) +
  geom_bar(stat="identity", position="dodge") +
  xlab("Governor's handling of pandemic response") +
  ylab("Percent of respondents") +
  scale_fill_discrete(name = "COVID in household?") +
  theme(axis.text.x = element_text(angle = 90))


# Table 2
covid_symptoms_table <- table(q3_clean_noneg$gov_scale, q3_clean_noneg$covid_symptoms)
covid_symptoms_table <- round(prop.table(covid_symptoms_table, 2), 2)
covid_symptoms_table_melt <- melt(covid_symptoms_table)
colnames(covid_symptoms_table_melt) <- c("gov_scale", 
                                              "symptoms",
                                              "percent")

ggplot(covid_symptoms_table_melt, 
       aes(x=gov_scale, y=percent, fill=factor(symptoms))) +
  geom_bar(stat="identity", position="dodge") +
  xlab("Governor's handling of pandemic response") +
  ylab("Percent of respondents") +
  scale_fill_discrete(name = "COVID symptoms in household?") +
  theme(axis.text.x = element_text(angle = 90))


ggplot(melt(covid_symptoms_table), 
       aes(x=gov_scale, fill=covid_test_positive)) + 
  geom_bar(position="dodge")

# This line is necessary since the "likert" function takes a df as input.
# The original error produced when I tried to pass q3_clean_noneg$gov_scale:
# "Error in likert(q3_clean_noneg$gov_scale) : 
# The items parameter must be a data frame. If trying to subset 
# a data frame to analyze only one column, try: items=mydf[,1, drop=FALSE]." 
itemsq3 <- as.data.frame(q3_clean_noneg[,'gov_scale', drop=FALSE])

lgrq3 <- likert(itemsq3, grouping=q3_clean_noneg$covid_test_positive)
summary(lgrq3) # print out of this command will be useful in presentation

plot(lgrq3) + 
  ggtitle("People living with a family member that has tested positive for COVID-19 
  tend to disapprove of their governor's pandemic response")

group_by(q3_clean, covid_test_positive)

q3_clean %>%
  ggplot() + 
  geom_histogram(x=gov_scale)


require(gridExtra)
all_bar <- ggplot(data=q3_clean_noneg, mapping=aes(y=gov_scale)) +
  geom_bar() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("All")

# samples_positive_covid_test
positive_bar <- ggplot(data=samples_positive_covid_test, mapping=aes(y=gov_scale)) +
  geom_bar() +
  coord_flip() +
  scale_y_continuous(breaks=1:5,
                       labels=c("Strongly Approve","Approve",
                                "Neutral","Disapprove","Strongly Disapprove")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Positive Covid test")
  
# samples_negative_covid_test
no_positive_bar <- ggplot(data=samples_negative_covid_test, mapping=aes(y=gov_scale)) +
  geom_bar() +
  coord_flip() +
  scale_y_continuous(breaks=1:5,
                     labels=c("Strongly Approve","Approve",
                              "Neutral","Disapprove","Strongly Disapprove")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("No positive Covid test")

grid.arrange(all_bar, positive_bar, no_positive_bar, ncol=3)




daily_plot <- daily_plot 








           