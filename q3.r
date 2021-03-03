# Try foreign library again, per Lesley's suggestion
# Wilcox graphic ideas: https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#two-sample-wilcoxon-test

# library(tidyverse)
# library(HH) # Trying the other package
library(likert)
# library(grid) # Necessary for plotting histogram on likert chart

library(magrittr)
library(haven) # Note: this is part of tidyverse
library(ggplot2)
library(dplyr)
setwd("E:/Google Drive/Berkeley MIDS/w203/my_lab1/data")

# Note: Originally tried to import .dta file with read.dta from Foreign library,
# but it only accepted specific types of .dta files. Instead, am using read_dta
# from the Haven library
# Haven reference: https://haven.tidyverse.org/reference/read_dta.html
d <- read_dta("anes_timeseries_2020_stata_20210211.dta")

# This code can be used to access column descriptions/labels:
# attr(d$V200003, 'label') # just an example

# NOTE: Lee said on 2/27/2021 5:26 in the class channel that we can 
# disregard weights.
d_trim = d[, c('V201018', 'V201507x', 'V201151', 'V201153',
               'V201624', 'V201625', 'V201066', 'V201145', 'V201146')]

# There has to be an easier way to do this than lining them all up manually
colnames(d_trim) = c('party_of_reg', 'age', 'sentiment_biden', 'sentiment_harris',
                     'covid_test_positive', 'covid_symptoms',
                     'vote_for_governor', 'gov_approval', 'gov_how_much')


#####   Q3: ARE SURVEY RESPONDENTS WHO HAVE HAD SOMEONE IN THEIR HOME INFECTED
#####   BY COVID MORE LIKELY TO DISAPPROVE OF THE WAY THEIR GOVERNOR IS HANDLING
#####   THE PANDEMIC #####


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

# Investigate/sanity check:
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

# 2)
# How many were refused or interview broken off?
breakoff_count = count(subset(q3_clean, covid_test_positive == -5))
refused_count = count(subset(q3_clean, covid_test_positive == -9))

sprintf('Number of broken off interviews: %s', breakoff_count)
sprintf('Number of refused answers: %s', refused_count)

# Probably going to want to use non-parametric Wilcoxon Ranked sum test
# Maybe write a function to create a new column in rank form of strongly approve,
# approve, neutral (-8?), disapprove, strongly disapprove

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

#### NOTES: WHY DOES THE ABOVE FUNCTION RETURN -88 IN ELSE?
#    Originally, I did not have this final else clause. However, since 46 entries
#    (more detail on this below) did not fit into the conditional statements of
#    my scale_approval function, R was returning that column as a list, some of which
#    had entries of length 0. This was making it difficult to use unlist(), which
#    was necessary to convert that column into numeric type data. Using -88
#    as a catchall fixes this problem.

# This also seems to work, in addition to Vectorize()
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
# small, compared to the total sample size, and they lack the data needed to plug
# into that final column for our Wilcoxon rank-test, I decided to drop them. We need
# numeric vector data for easy plotting later on.

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


######################## HYPOTHESIS TESTS ########################
#### NOTE: wilcox.test can't take gov_scale in factor form, since it needs to
#### add the ranks together to perform the hypothesis test. That column
#### is factored below for the visualizations, since the likert package
#### takes factors as inputs.

# HT 1): tested positive vs governor approval
samples_positive_covid_test <- subset(q3_clean, covid_test_positive == 1)
samples_negative_covid_test <- subset(q3_clean, covid_test_positive == 2)

# NOTE: unlist is used here to convert samples_positive_covid_test$gov_scale from
# list into a vector with atomic components (in this case, numeric)
# For whatever reason, as.numeric didn't work when I tried to do this
# THIS MAY NO LONGER BE NECESSARY
wilcox.test(x=unlist(samples_positive_covid_test$gov_scale), 
            y=unlist(samples_negative_covid_test$gov_scale),
            alternative="two.side",
            paired = FALSE)

wilcox.test(x=samples_positive_covid_test$gov_scale, 
            y=samples_negative_covid_test$gov_scale,
            alternative="two.side",
            paired = FALSE)

# HT 2): positive symptoms vs governor
samples_covid_symptoms <- subset(q3_clean, covid_symptoms == 1)
samples_no_covid_symptoms <- subset(q3_clean, covid_symptoms == 2)

wilcox.test(x=unlist(samples_covid_symptoms$gov_scale), 
            y=unlist(samples_no_covid_symptoms$gov_scale),
            alternative="two.side",
            paired = FALSE)

######################## VISUALIZATIONS ########################
#### USE q3_clean_noneg in this section, rather than q3_clean. The former filters out negative values. In the prior section,
#### I split out positive and not positive results, which filtered negative numbers by default. The difference is due
#### only to wilcox.test taking a different type of input (array/vectors as x and y) than likert(a dataframe).

# Drop samples where covid test is below 0, then factor
# gov_scale column
q3_clean_noneg <- subset(q3_clean, covid_test_positive > 0)

# Recast gov_scale & test_positive columns as factors
q3_clean_noneg$gov_scale <- as.factor(q3_clean_noneg$gov_scale)
q3_clean_noneg$covid_test_positive <- as.factor(q3_clean_noneg$covid_test_positive)

gov_scale_levels <- c("Strongly Approve", "Approve", "Neutral",
                      "Disapprove", "Strongly Disapprove")

covid_test_levels <- c("Household tested positive", "No positive test")

# Recode gov_scale factors from 1-5 to "strongly approve" etc,
# so that they show up in the visualization correctly

# Note: there is a recode function in dplyr and likert packages
# likert::recode seems to be recoding 1 - 5 incorrectly. Check out recode with dplyr
q3_clean_noneg$gov_scale <- likert::recode(q3_clean_noneg$gov_scale,
                                     from=c(1, 2, 3, 4, 5), 
                                     to=gov_scale_levels)



# Recast columns with numeric factors to character factors. Reasoning: the likert package does provide some nice,
# turnkey graphs for likert type data. But customizing the plot functionality is a little convoluded, so it's easier
# to just rename/recast the data being fed into the likert plot function.

# This must happen here, rather than above, since the Wilcox.test hypothesis works but calculating numeric
# rank values, then adding them together. If these numeric types are first converted to factors, Wilcox.test fails

q3_clean_noneg$gov_scale <- recode(q3_clean_noneg$gov_scale, `1` = "Strongly Approve", `2` = "Approve", 
              `3` = "Neutral", `4` = "Disapprove", `5` ="Strongly Disapprove")

q3_clean_noneg$covid_test_positive <- recode(q3_clean_noneg$covid_test_positive, `1` = "Household tested positive",
                                             `2` = "No positive test")

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


count(subset(q3_clean, covid_test_positive == 1))
count(subset(q3_clean_noneg, covid_test_positive == 2))

# NOTES: to add/change to the above plot:
# Legend must include "approve/disapprove etc"
# Y-axis labels should be "test positive" or "test negative"
# Useful to get a total count of people in each category

# KEEP THIS BLOCK OF CODE
group_by(q3_clean, covid_test_positive)


  
  
  







           