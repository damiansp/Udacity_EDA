# Personality data from: http://personality-testing.info/_rawdata/
# BIG 5
rm(list = ls())

library(GGally)
library(ggplot2)
library(scales)

big5 <- read.csv('~/Desktop/Classes/Udacity/EDA_withR/Project/BIG5/data.csv', sep='\t')
names(big5)
str(big5)

# For all columns a score of 0 indicates 'NA' ...change:
big5[big5 == 0] <- NA

# There are 5 main personality traits being examined-- group them:
# "E" columns ('extroversion') 
E <- 8:17

# "N" columns ('nervousness')
N <- 18:27

# "A" columns ('affinity' or 'empathy')
A <- 28:37

# "C" columns ('conscientiousness')
C <- 38:47

# "O" columns ('cognition')
O <- 48:57

# The scoring is inconsistent.. for example for the 'extroversion' category, sometimes a 5 ('strongly 
# agree) is the most extroverted ('I am the life of the party'), and sometimes a 5 is the least 
# extroverted ('I have little to say').  If we flip the scoring for some columns, then the measurements
# will be consistent (e.g., 5 always represents the "most..." {extroverted, nervouse, empathetic...})
reversed <- c( 'E2', 'E4', 'E6', 'E8', 'E10', 'N2', 'N4', 'A1', 'A3','A5','A7','C2','C4','C6','C8',
			   'O2', 'O4','O6' )
			   
# check the original scores in those columns
head(big5[,reversed])

# Now reverse them
big5[, reversed] <- 6 - big5[reversed]

# And compare...
head(big5[,reversed])

# Now we can create "overall" scores for each category by summing the individual columns:
big5$EScore <- rowSums(big5[, E], na.rm=T)
big5$NScore <- rowSums(big5[, N], na.rm=T)
big5$AScore <- rowSums(big5[, A], na.rm=T)
big5$CScore <- rowSums(big5[, C], na.rm=T)
big5$OScore <- rowSums(big5[, O], na.rm=T)

# Check:
head(cbind(big5[, E], big5$EScore)) # sums look good!

# ...but remember there may be some values missing (NA), in which case we will have to adjust the 
# scores
summary(big5)

# We see there is just 1 NA value in most columns... could it all be a single row?
which(is.na(big5$O5))  
# Indicates that the NA is in row 19065-- examine this row:
big5[19065, ]
# Indeed, all personality scores are 'NA' ... let's just remove this useless datum
big5 <- big5[-19065, ]

# And now... 
summary(big5) # ... no more missing data in the personality variables



# Clean the remaining variables:
# The variable "race" is a categorical variable with numerical value codings; change class and label
big5$race <- as.factor(big5$race)
levels(big5$race) <- c('Mixed', 'Arctic', 'European', 'Indian', 'MidEastern', 'NAfrican', 
					   'AustrNative', 'NatAmerican', 'NEAsian', 'Pacific', 'SEAsian', 'WAfrican',
					   'Other')

# The variable "engnat" (native speaker of English?) is also categorical; convert
big5$engnat <- as.factor(big5$engnat)
levels(big5$engnat) <- c('yes', 'no')

# ...same with the variable "gender"
big5$gender <- as.factor(big5$gender)
levels(big5$gender) <- c('male', 'female', 'other')

# ...same with "hand" (handedness)
big5$hand <- as.factor(big5$hand)
levels(big5$hand) <- c('right', 'left', 'ambi')

# ...and same with "source" 
big5$source <- as.factor(big5$source)
levels(big5$source) <- c('otherPage', 'Google', 'Facebook', 'Edu', 'other')

# Check data again:
summary(big5)

# At a glance, most variables look ok, except that there are some clearly problematic data in the 
# "age" variable (Max == 999999999)... examine the age distribution more closely:
hist(big5$age)

# Ok, not very useful....
table(big5$age > 100) 

# It looks like 83 people "claim" to be more than 100... let's take a closer look at those values
sort(big5$age, decreasing = T)[1:100]

# It seems that, in a number of these cases, the respondent entered their year of birth rather than
# their age, but to err on the side of caution, lets replace all ages > 100 with NA:
big5$age[big5$age > 100] <- NA



# Now that we have a reasonably clean data set, examine pairs-- to minimize the number of variables, 
# use summed scores for each of the "big 5" personality components, and omit 'country':
pairData <- c(1:6, 58:62)
ggpairs(big5[pairData])

# There are a number of interesting observations here already... lets consider a few, row by row:
# Race:  evidentally data are not evenly distributed among races and one value is especially high:
table(big5$race)  # [Caucasians of] European descent are the largest group by far
# Age is a long-tailed distribution: most respondents are relatively young:
quantile(big5$age, probs=seq(0, 1, 0.1), na.rm=T) 
# Half the respondents are age 13 - 22; 90% are 44 or under
# Age correlates most strongly with "C" score-- older people are more likely to be more conscientious
# Gender: more female respondents than male:
table(big5$gender)
table(big5$gender) / sum(table(big5$gender)) # appx. 61% female
# There are some interesting gender-based variations in personality traits... to be explored further
# Handedness: righties dominate (unsurprisingly):
table(big5$hand)
table(big5$hand) / sum(table(big5$hand)) # appx. 89% right-handed
# No easily observable difference in personality based on handedness
# Personality traits:
# The scatterplots are all too overplotted to make much sense of in this graphic, so we'll return to 
# those later. Interestingly, 'extroversion (E)', 'nervousness (N)', and 'conscientiousness (C)' all 
# appear to be roughly normal, whereas both 'affinity (A)' and 'cognitive (O)' are both left-skewed, 
# which also seems to be a good thing: there are more empathetic people than non-empathetic, and more
# cognitive/imagintive people than not... or at least that's how people think of themselves.

# Now let's examine some of these in more detail:

# Race
plot(age ~ race, data=big5, cex.axis=0.6)
tapply(big5$age, big5$race, summary, na.rm=T)
# While median and lower quantile ages are similar for most groups, respondents that were European, N. 
# African, and Native American had larger IQRs with the 3rd quantile being older than in other groups.

table(big5$race, big5$gender) # convert to proportions:
round(table(big5$race, big5$gender) / rowSums(table(big5$race, big5$gender)), 3)
# In most ethnic groups, more females responded than males.  The reverse in true in the Arctic, and
# Indians and W. Africans were closest to 50/50.  Also of note, Arctic and Australian Native peoples 
# had the highest incidence of people identifying as "other" gender.



# Age
plot(age ~ gender, data=big5)  
# Age distributions between male and female are similar... 'other' tends to be younger overall... 
# there are multiple possible interpretations:  younger people may be more uncertain about their 
# gender; they may be more open to questioning; or they may be more likely to jokingly falsify their
# responses



# Gender
plot(hand ~ gender, data=big5)
# Males and females show similar proportions of left- and right-handers; interestingly, people 
# identifying as 'other' gender, had higher proportions of ambidextrous people, though this may also 
# be a result of people being dishonest or joking about gender also being more likely to be dishonest
# or joke about handedness






# Now onto the main categories of interest: personality traits
personality <- 58:62
round(cor(big5[personality]), 3)
# Interestingly, all of the personality traits are fairly weakly correlated with each other.  The 
# strongest correlation is between extroversion and affinity: more extroverted people tend to be more
# empathetic (or at least view themselves as such).


# Extroversion:

plot(EScore ~ race, data=big5, las=3, xlab='', cex.axis=0.8)
# From the looks of this, Arctic peoples seem to be more extroverted than any others.  However, given 
# their small numbers (n = 14), we should be cautious about the significance of this.
# To explore further, we can do an analysis of variance to see how well race predicts EScore:
eRace <- aov(EScore ~ race, data=big5)

# We can then look at the significance of pairwise differences.  Because we are making many 
# comparisons simultaneously, the p values have to be adjusted (recall that a with a p value of 0.05, 
# you might expect a "false positive" about one time in 20, here we are making 13 * 12 = 156 
# comparisons--Tukey's "Honest Significant Differences" can be applied to adjust the p value in such 
# cases:
?TukeyHSD	# for further information
TukeyHSD(eRace)

# This indicates that Arctic peoples are not necessarily more extroverted than other populations 
# (p values are all rather high).  Interestingly though, it does suggest that SE Asians are less 
# extroverted (very low p values), something not readily apparent in the boxplots.

plot(jitter(EScore) ~ age, data=big5, col=rgb(0,0,0, alpha=0.2), pch=16)
# Look at different smoothers-- a linear model:
abline(lm(EScore ~ age, data=big5), col=2, lwd=3)
# A Lowess line (local polynomial regression):
lines(lowess(big5$EScore[!is.na(big5$age)] ~ big5$age[!is.na(big5$age)]), col=4, lwd=3)

# Since age was so skewed, we might try to transform this variable to potentially detect other 
# relationships:


# Try a range of exponential transformations
# Test how much the transformed distribution diverges from normal with a Shapiro-Wilk test.
# shapiro.test() won't accept more than 5000 values, so we take a random sample
expVals <- seq(-0.5, -1.5, -0.01)
ps <- c()
samp <- sample(big5$age, size=5000)

for(i in expVals) {
	shapiroP <- shapiro.test(samp^i)$p.value 
	ps <- c(ps, shapiroP)
}

# Plot the p value as a function of the exponent used to transform:
plot(ps ~ expVals, type='l')
abline(v=-1.11, col=2)

# Even at the optimal exponential transformation (~x^-1.11), the data are still far from normal, but 
# much closer than what we started with:
hist(big5$age^-1.11)
big5$ageTrans <- big5$age^-1.11

# Examine the scatterplot with the tranformed data:
plot(jitter(EScore) ~ ageTrans, data=big5, col=rgb(0,0,0, alpha=0.2), pch=16)
abline(lm(EScore ~ ageTrans, data=big5), col=2, lwd=3)
lines(lowess(big5$EScore[!is.na(big5$age)] ~ big5$ageTrans[!is.na(big5$age)]), col=4, lwd=3)


# With ggplot
# Create the transfomation function:
expTrans <- function() { 
				trans_new('exponential', 
						  transform=function(x) x^-1.11,
						  inverse=function(x) x^(1 / 1.11))
	
			}

ggplot(aes(x=age, y=jitter(EScore)), data=subset(big5, !is.na(ageTrans))) +
	geom_point(alpha=0.2) + 
	scale_x_continuous(trans=expTrans(), breaks=seq(10, 100, 10)) + 
	geom_smooth(col=4) +	# note this uses a different method than the "lowess" above
	geom_smooth(method='lm', col=2)

# In short, we see nothing particularly striking here, the relationship is weak if any at all
# Since the transformation of the age variable has the unfortunate consequence of reversing the
# slope of the lines, the remaining graphs will used the untransformed age, for easier interpretation


# Now combine variables:
plot(jitter(EScore) ~ age, col=race, data=big5)
for (r in levels(big5$race)) {
	rnum <- which(levels(big5$race) == r)
#	abline(lm(EScore ~ age, data=subset(big5, race==r)), col=rnum, lwd=rnum/3)
	lines(lowess(big5$EScore[big5$race==r & !is.na(big5$race) & !is.na(big5$age)] ~ 
				 big5$age[big5$race==r & !is.na(big5$race) & !is.na(big5$age)]), 
		  col=rnum, lwd=rnum/3)
}
nraces <- length(levels(big5$race))
legend('topright', pch=1, col=1:nraces, lwd=(1:nraces) / 3, legend=levels(big5$race), 
	   bg=rgb(1,1,1, 0.8))
# NOTE: even the the lowess line may typically be better than a linear smoother, as it makes fewer 
# assumptions about the data, graphics using the default graphics package will typically use just the 
# linear models, as the syntax is so much briefer
# Similarly in ggplot graphics, there default smoother is 'loess', and there are frequently too few 
# data points when subdividing the data to allow for this method, so 'lm' is used here when necessary
# as well

## With ggplot
ggplot(aes(x=age, y=jitter(EScore)), data=subset(big5, !is.na(ageTrans))) +
	geom_point(alpha=0.2, aes(color=race)) + 
	geom_smooth(method='lm', aes(col=race))

# While the graphs also show the higher value for Arctic peoples, and a seemingly opposite trend in 
# Australian Natives (less extoverted with age), all lines are close to being flat, and the confidence
# intervals plotted in the ggplot all overlap with each other, indicating no difference by race

plot(EScore ~ gender, data=big5)
plot(jitter(EScore) ~ age, col=gender, data=big5, pch=16, cex=0.6)
abline(lm(EScore ~ age, data=subset(big5, gender=='male')))
abline(lm(EScore ~ age, data=subset(big5, gender=='female')), col=2)
abline(lm(EScore ~ age, data=subset(big5, gender=='other')), col=3)
legend('topright', pch=16, col=1:3, lty=1, legend=c('male', 'female', 'other'), 
	   title='Gender', bg=rgb(1,1,1, 0.8))

## With ggplot
ggplot(aes(x=jitter(age), y=jitter(EScore)), data=subset(big5, !is.na(ageTrans) & !is.na(gender))) +
	geom_point(alpha=0.2, aes(color=gender)) + 
	geom_smooth(aes(col=gender))

# Here we get evidence that suggests gender does have an effect on extroversion, with young males and 
# females being similarly extroverted, but females becoming moreso than males with age.  Young others
# have lower rates of extroversion than males and females, but become increasingly similar with age. 
# The non-linear smoother in the ggplot suggests that extroversion only increases until about age 55, 
# and then begins to decrease again, except, in other gender, where the trend is constantly upward. 
# Do we see the same phenomenon in each racial group?
ggplot(aes(x=jitter(age), y=jitter(EScore)), data=subset(big5, !is.na(ageTrans) & !is.na(gender))) +
	geom_point(alpha=0.2, aes(color=gender)) + 
	geom_smooth(method='lm', aes(col=gender)) +
	facet_wrap(~ race, nrow=3) + 
	ylim(0, 50)

# No, in fact, the results appear to be driven by people of European descent.  In other ethnic groups,
# males and females do not appear to be as different (overlapping CIs), though this may again be due 
# to smaller numbers.  In nearly all races though, we see the same tendency to increased extroversion
# with age.

# What about differences by country within the European group?  (Because there are too many countries
# to plot nicely in a single window, we take only those 9 countries with the most data points):
sort(table(big5$country[big5$race == 'European']), decreasing=T)
eurSample <- c('US', 'GB', 'AU', 'CA', 'IT', 'DE', 'SE', 'NO', 'RO')
ggplot(aes(x=jitter(age), y=jitter(EScore)), 
	   data=subset(big5, !is.na(ageTrans) & !is.na(gender) & race=='European' & 
	   			   country %in% eurSample)) +
	geom_point(alpha=0.2, aes(color=gender)) + 
	geom_smooth(method='lm', aes(col=gender)) +
	facet_wrap(~ country, nrow=3) + 
	ylim(0, 50)

# Here we see that it is predominately respondents from the US that are driving the trend.
# There are not significant differences in the other countries (overlapping CIs), though again 
# possibly due to numbers.  In all countries though, the absolute scores of females are higher than 
# for males at all ages.

plot(EScore ~ hand, data=big5)  # No apparent relationship








# Try similar exploration with other personality scores:  (Commentary mostly omitted.)
# Nervousness:

plot(NScore ~ race, data=big5, las=3, xlab='', cex.axis=0.8)
nRace <- aov(NScore ~ race, data=big5)
TukeyHSD(nRace)

plot(jitter(NScore) ~ jitter(age), data=big5, col=rgb(0,0,0, alpha=0.2), pch=16)
abline(lm(NScore ~ age, data=big5), col=2, lwd=3)
lines(lowess(big5$NScore[!is.na(big5$age)] ~ big5$age[!is.na(big5$age)]), col=4, lwd=3)
# For those of you who are nervous, it may be reassuring that the effect will likely diminish with age

# With ggplot
ggplot(aes(x=age, y=jitter(NScore)), data=subset(big5, !is.na(ageTrans))) +
	geom_point(alpha=0.2) + 
	geom_smooth(col=4) +	
	geom_smooth(method='lm', col=2)
# note this uses a different method for smoothing, and may suggest an increase again in nervousness 
# after about age 60

# Now combine variables:
plot(jitter(NScore) ~ age, col=race, data=big5)
for (r in levels(big5$race)) {
	rnum <- which(levels(big5$race) == r)
	abline(lm(NScore ~ age, data=subset(big5, race==r)), col=rnum, lwd=rnum/3)
}
nraces <- length(levels(big5$race))
legend('topleft', pch=1, col=1:nraces, lwd=(1:nraces) / 3, legend=levels(big5$race), 
	   bg=rgb(1,1,1, 0.8))

## With ggplot
ggplot(aes(x=age, y=jitter(NScore)), data=subset(big5, !is.na(ageTrans))) +
	geom_point(alpha=0.2, aes(color=race)) + 
	geom_smooth(method='lm', aes(col=race))
# Perhaps some pairwise differences, but generally the effect is the same across races

plot(NScore ~ gender, data=big5)
# Females appear more nervous than males, and others more nervous still

plot(jitter(NScore) ~ jitter(age), col=gender, data=big5, pch=16, cex=0.6)
abline(lm(NScore ~ age, data=subset(big5, gender=='male')))
abline(lm(NScore ~ age, data=subset(big5, gender=='female')), col=2)
abline(lm(NScore ~ age, data=subset(big5, gender=='other')), col=3)
legend('topright', pch=16, col=1:3, lty=1, legend=c('male', 'female', 'other'), 
	   title='Gender', bg=rgb(1,1,1, 0.8))
# Males and females both decrease in nervousness with age, though the change is more dramatic in 
# females. Others unfortunately, remain at constant relatively high levels throughout life

## With ggplot
ggplot(aes(x=jitter(age), y=jitter(NScore)), data=subset(big5, !is.na(gender))) +
	geom_point(alpha=0.2, aes(color=gender)) + 
	geom_smooth(aes(col=gender))
# This shows a similar trend... all groups generally decreasing in nervousness with age.... until 
# about 60 when females and others may begin to increase again; males continue to decrease throughout
# the lifespan

ggplot(aes(x=jitter(age), y=jitter(NScore)), data=subset(big5, !is.na(gender))) +
	geom_point(alpha=0.2, aes(color=gender)) + 
	scale_x_continuous(trans=expTrans(), breaks=seq(10, 100, 10)) +
	geom_smooth(method='lm', aes(col=gender)) +
	facet_wrap(~ race, nrow=3) + 
	ylim(0, 50)
# Surpisingly, when we subdivide the data in this way, women show a tendency toward more nervousness 
# with age in all races.  Men are less consistent... in many cases they remain at a roughly constant
# level througout life, though in some cases they may show a slight increase with age as well, with 
# W. Africans showing the strongest increase in nervousness with age.  

# What about differences by country within the European group?  (Because there are too many countries
# to plot nicely in a single window, we take only those 9 countries with the most data points):
ggplot(aes(x=jitter(age), y=jitter(NScore)), 
	   data=subset(big5, !is.na(ageTrans) & !is.na(gender) & race=='European' & 
	   			   country %in% eurSample)) +
	geom_point(alpha=0.2, aes(color=gender)) + 
	geom_smooth(method='lm', aes(col=gender)) +
	facet_wrap(~ country, nrow=3) + 
	ylim(0, 50)
# At the country level, the trend reversed again!  Generally a decrease in nervousness with age, or
# constant rates.  In many cases, the differences between men and women diminish with age.  Clearly 
# the relationship between nervousness and these other variables is complicated, though we won't 
# explore it further here.

plot(NScore ~ hand, data=big5)  # No apparent relationship



# Empathy/Affinity
# ...Similar tests could be done for each of the personality traits, but are omitted here for brevity.



# As one last study, lets suppose that a company is able to infer most personality traits based on 
# other evidence (cognition based on test scores, conscientiousness based on social activities, empathy 
# based on responses on Facebook, or whatever...), but introversion/extroversion cannot be easily 
# estimated.  However, certain types of work may be more appropriate for introverts, and other types 
# for extroverts, so the company wishes to estimate extroversion/introversion based on the other 
# available data.  How well can this goal be achieved with the tools we have learned so far?

# First, to simplify, let's try to normalize all of our numeric variables
# To acheive this, write a simple function that transforms the data with a range of exponent values, 
# checks the normality for each, and reports on the optimal transformation
# NOTE: sometimes other functions may be more appropriate for normalizing, but exponents work in many 
# cases... in some cases, no (continous) normalizing function can be easily found.
optimumExponent <- function(x, minExp=-1, maxExp=2, step=0.01, samples=20) {
	exps <- seq(minExp, maxExp, step)
	# remove 0 as a base taken to the 0th power is undefined:
	if (0 %in% exps) {
		exps <- exps[-which(exps==0)]
	}
	

	# initialize a vector, ps, that will store the p values of Shapiro-Wilk tests for normality for
	# each transformation
	ps <- numeric(length(exps))
	
	# loop through each exp value, transform, test for normality, and write results to ps
	for (e in 1:length(exps)) {
		xTrans <- x^exps[e]
		
		# Unfortunately, the shapiro.test function will allow a maximum sample size of 5000, so we 
		# take random samples, and average the results
		pSets <- numeric(samples)
		
		for (s in 1:samples) {
			pSets[s] <- shapiro.test(sample(xTrans, size=5000))$p.value
		}
		
		ps[e] <- mean(pSets)
		
	}
	
	plot(ps ~ exps, type='l')
	# because of the random sampling, the output is jagged, rather than the expected smooth, continous 
	# function... (smoothness improves as 'samples' increases)
	
	bestP <- max(ps)
	bestPIndex <- which(ps == bestP)
	bestExp <- exps[bestPIndex]

	abline(v=bestExp, col=2)
	legend('topleft', pch='', legend=paste('Best Exp: ', bestExp, ' (p = ', round(bestP, 3), ')'))
	return (bestExp)
}

# Apply transformations, and visualize transformed data as histograms
names(big5)

ageExp <- optimumExponent(big5$age, minExp=-2, maxExp=-0.5, samples=50)
big5$ageTrans <- big5$age^ageExp
hist(big5$ageTrans)

EExp <- optimumExponent(big5$EScore, minExp=0, maxExp=1, samples=50)
big5$ETrans <- big5$EScore^EExp
hist(big5$ETrans)

NExp <- optimumExponent(big5$NScore, minExp=0.5, maxExp=1.5, samples=50)
big5$NTrans <- big5$NScore^NExp
hist(big5$NTrans)

AExp <- optimumExponent(big5$AScore, minExp=0.5, maxExp=1.5, samples=50)
big5$ATrans <- big5$AScore^AExp
hist(big5$ATrans)

CExp <- optimumExponent(big5$CScore, minExp=0.3, maxExp=1.5, samples=50)
big5$CTrans <- big5$CScore^CExp
hist(big5$CTrans)

OExp <- optimumExponent(big5$OScore, minExp=1, maxExp=2, samples=50)
big5$OTrans <- big5$OScore^OExp
hist(big5$OTrans)

# Note in all cases of optimumExponent() above, the minExp and maxExp values were determined by first
# selecting a broad range (e.g. [-2, 2], with smaller samples (20)), then decreasing the range to a 
# to the area where the p value is most improved, and increasing the sample size for better inference

# NOTE: although we improved the normality of the distributions, in NO case were we truly able to 
# normalize very well (all p values were still highly significant for Shapiro-Wilk tests, indicating
# significantly non-normal distributions)... hence we might typically prefer non-parametric tests, but 
# for the sake of demonstration, I proceed with the assumption that the distributions are normal


# MODEL BUILDING (Approach #1)
# We now build a linear model to predict the (transformed) E (Extroversion) Score, using the other 
# (transformed) personality scores, race, (transformed) age, engnat, gender, hand, source, and country 
# as the predictors, but first look at the relationship of E scores to other variables to look for 
# potential non-linear relationships:
variablesOfInterest <- names(big5)[63:68]
pairs(big5[, variablesOfInterest], panel=panel.smooth)
# All relationships with E as the response (the second row of the pairs graphs) appear more or less
# linear, so we omit exponents of the predictors in our model, though we may still be interested in 
# interaction terms between 2 or more predictors:
# Also: to simplify, I remove entries with missing data:
complete <- big5[complete.cases(big5), ]
fullModel <- lm( ETrans ~ race + engnat + gender + hand + source + country + 
				 (age + NTrans + ATrans + CTrans + OTrans)^5, data=complete )
summary(fullModel)

# Not all predictors are significant, so we remove predictors that are not informative using the 
# step() function.  (This function adds and/or removes predictors iteratively and calcualtes the
# AIC or Akaike Information Criterion at each iteration, and returns the model with the best (lowest)
# AIC value)
reducedModel <- step(fullModel, direction='both')
summary(reducedModel)

# Also, look at model diagnostic plots to see how well the model fits assumptions:
par(mfrow=c(2, 2))
plot(reducedModel)
par(mfrow=c(1, 1))
# In general, the model is "well-behaved"... residuals are roughly normally distributed (upper right), 
# and more or less constant (upper and lower left), and there are no outliers that are having a 
# disproportionate effect on the model (lower right)... so we can be reasonably confident that are 
# model is not systematically biased.

# BUT... it would be nice to be able to determine how well the model does at predicting new data so....
# MODEL BUILDING (Approach #2)
# The approach here is to use 80% of the data to build the model, and then use that model to predict
# the values of the remaining 20% of the data, and measure how well the predicted E scores match up
# with the observed E scores
records <- dim(complete)[1]
nSamples <- as.integer(0.8 * records)
sampleRecords <- sample(1:records, nSamples)

devSet <- complete[sampleRecords, ]
testSet <- complete[-sampleRecords, ]

fullModel <- lm( ETrans ~ race + engnat + gender + hand + source + 
				 (age + NTrans + ATrans + CTrans + OTrans)^5, data=devSet )
reducedModel <- step(fullModel, direction='both')
summary(reducedModel)

# Before we even put our model to the test, notice the multiple R-squared value of [0.1827] indicating
# that the model explains [18.27%] of the variance... which means the remaining variance is not 
# explained by any of our variables... Hence, even before testing we might not expect the model to 
# perform very well.
predictions <- predict.lm(fullModel, newdata=testSet)
modErrors <- testSet$ETrans - predictions

range(complete$ETrans); range(predictions)
max(complete$ETrans) - min(complete$ETrans)
perfSummary <- cbind( quantile( complete$ETrans, prob=c( 0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 
														 0.95, 0.99, 1 ) ),
	   				  quantile( predictions, prob=c( 0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 
	   				  								 0.99, 1 ) ),
	   				  quantile( modErrors, prob=c( 0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 
	   				  							   0.99, 1 ) ))
colnames(perfSummary) <- c('Observed', 'Predicted', 'Error')
perfSummary
hist(modErrors)


# In general the predictions are not too bad.  The range of transformed E scores ranged from 9.77 to 
# 48.08, and the range of predictions ran frorm 12.11 to 44.49.  The median prediction was only 0.22 
# more than the observed median, and the middle 50% of the predictions were off by just under 6 points 
# (15% of the observed range of values). The middle 90% were off by about 10.6 points (28% of the 
# obseved range).  In other words, using this model, we could predict with 90% confidence that a 
# person's actual extroversion score would be within 10.6 points from their predicted score.  Given that the range of (transformed) scores is about 38 points, might not be too bad, and would at least serve to accurately categorize most people as more-introverted, more-extroverted, or somewhere in between.