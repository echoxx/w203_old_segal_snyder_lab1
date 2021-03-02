# Try foreign library again, per Lesley's suggestion
# Wilcox graphic ideas: https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#two-sample-wilcoxon-test

library(tidyverse)
library(HH) # Trying the other package
library(likert)

# library(magrittr)
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

# Some reference on slicing
# NOTE: Lee said on 2/27/2021 5:26 in the class channel that we can disregard weights
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

#### NOTES ON WHY -88
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
q3_clean$gov_scale <- as.factor(q3_clean$gov_scale)

plot.likert(q3_clean$gov_scale)

table(sapply(q3_clean$gov_scale, length)) # this USED TO reveal that some entries have length 0

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




# https://stackoverflow.com/questions/42935178/vector-different-length-after-unlist
table(sapply(q3_clean$gov_scale, length)) # this reveals that some entries have length 0
View(subset(q3_clean, gov_scale == -88))


#### THIS CODE BLOCK DOESN'T APPEAR NECESSARY ANYMORE
# Vectorizing functions: https://stackoverflow.com/questions/34682109/apply-custom-function-to-two-columns-for-every-row-in-data-frame-in-r
scale_approval_v <- Vectorize(scale_approval)
q3_clean$gov_scale <- scale_approval_v(q3_clean$gov_approval, q3_clean$gov_how_much)
q3_clean$gov_scale <- lapply(q3_clean$gov_scale, as.numeric)
scale_approval(q3_clean$gov_approval, q3_clean$gov_how_much)


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


#### HYPOTHESIS TESTS
# HT 1): tested positive vs governor
samples_positive_covid_test <- subset(q3_clean, covid_test_positive == 1)
samples_negative_covid_test <- subset(q3_clean, covid_test_positive == 2)

# Note: unlist is to convert samples_positive_covid_test$gov_scale from
# list into a vector with atomic components (in this case, numeric)
# For whatever reason, as.numeric didn't work when I tried to do this
wilcox.test(x=unlist(samples_positive_covid_test$gov_scale), 
            y=unlist(samples_negative_covid_test$gov_scale),
            alternative="two.side",
            paired = FALSE)

# HT 2): positive symptoms vs governor
samples_covid_symptoms <- subset(q3_clean, covid_symptoms == 1)
samples_no_covid_symptoms <- subset(q3_clean, covid_symptoms == 2)

wilcox.test(x=unlist(samples_covid_symptoms$gov_scale), 
            y=unlist(samples_no_covid_symptoms$gov_scale),
            alternative="two.side",
            paired = FALSE)

# Probably also worth testing between covid symptoms & test positive somehow

#### VISUALIZATIONS
#ggplot(q3_clean, aes(x="Testing positive?", y=gov_scale), fill=covid_test_positive) +
  #geom_bar(stat='identity')

samples_covid_symptoms %>%
  ggplot(aes(x = gov_scale, y = covid_test_positive)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Following line of code converts gov_scale to numeric, then from
# numeric to factor (because the likert package takes factors as 
# inputs. )
q3_clean$gov_scale <- as.factor(as.numeric(q3_clean$gov_scale))


items <- as.data.frame(q3_clean[,'gov_scale', drop=FALSE])
class(items)
items

as.factor(items)
class(as.data.frame(items))
as.factor(items)
likert(items)
sapply(items, class)


View(items)
likert(as.data.frame(items))
likert.bar.plot()


# plot.likert(data=q3_clean$gov_scale)

# KEEP THIS BLOCK OF CODE
group_by(q3_clean, covid_test_positive)

subset(q3_clean, covid_test_positive > 0) %>% 
  group_by(covid_test_positive) %>% 
  count(gov_scale) %>%
  ggplot(aes(x=covid_test_positive, y=fill=gov_scale)) +
  geom_col() +
  coord_flip()


likert(q3_clean, grouping = q3_clean$covid_test_positive)




  
  
  







           