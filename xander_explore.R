install.packages('haven')
library(haven)
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
      'V201624', 'V201625', 'V201066', 'V201066', 'V201147x')]

# There has to be an easier way to do this than lining them all up manually
colnames(d_trim) = c('party_of_reg', 'age', 'sentiment_biden', 'sentiment_harris',
                     'covid_in_house_test_postive', 'covid_in_house_symptoms',
                     'vote_for_governor?', 'gov_handling_covid_amt', 'gov_handling_approve')

