###########################################################
### Class to compute IRR for nominal or ordinal data
### If nominal, consider Cohens Kappa 
### If ordinal need to use ICC
### http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3402032/
### http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3402032/table/T7/
###########################################################
    
library(irr)

## Example data for ICC IRR
ex1data <- read.table( text = "Subject	Dep_Rater1	Dep_Rater2	Dep_Rater3
1	1	0	1
2	0	0	0
3	1	1	1
4	0	0	0
5	0	0	0
6	1	1	2
7	0	1	1
8	0	2	0
9	1	0	1
10	0	0	0
11	2	2	2
12	2	2	2", header = TRUE)

# first check for violations in normality for each rater
hist(ex1data$Dep_Rater1)
hist(ex1data$Dep_Rater2)
hist(ex1data$Dep_Rater3)

# Satisfied, proceed with ICC
icc(ex1data, model='t', type='consistency', unit='average')

# yields
# Average Score Intraclass Correlation
# 
# Model: twoway 
# Type : consistency 
# 
# Subjects = 12 
# Raters = 4 
# ICC(C,4) = 0.562
# 
# F-Test, H0: r0 = 0 ; H1: r0 > 0 
# F(11,33) = 2.28 , p = 0.0331 
# 
# 95%-Confidence Interval for ICC Population Values:
#     -0.056 < ICC < 0.859

# model interpretation
# IRR was assessed using a two-way mixed, consistency, average-measures 
# ICC (McGraw & Wong, 1996) to assess the degree that coders provided consistency 
# in their ratings of empathy across subjects. The resulting ICC was in the 
# excellent range, ICC = 0.96 (Cicchetti, 1994), indicating that coders had 
# a high degree of agreement and suggesting that empathy was rated similarly 
# across coders. The high ICC suggests that a minimal amount of measurement error 
# was introduced by the independent coders, and therefore statistical power for 
# subsequent analyses is not substantially reduced. Empathy ratings were therefore 
# deemed to be suitable for use in the hypothesis tests of the present study.

