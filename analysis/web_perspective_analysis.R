## Feb 1, 2017
## Bodo Winter
## Analysis of underspecification web experiment

##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Load stuff:

options(stringsAsFactors = F)

setwd('/Users/winterb/Research/gesture_categorization/new_paper_new_analyses/data/')
xdata <- read.csv('web_gesture_perspective.csv', fileEncoding = 'latin1')

library(dplyr)
library(stringr)
library(tidyr)

## Rename columns:

xdata <- rename(xdata, Response = The.gesture.you.just.saw.characterized.an.object..Ê.What.do.you.think.the.gesture.was.about.,
	ObjectCondition = Display.Order..Block.Randomizer.FL_3,
	Control = What.is.4...17..Please.type.in.your.answer.,
	Age = What.is.your.age.,
	Gender = What.is.your.gender.,
	Handedness = Are.you.left.or.right.handed.,
	Language = What.is.your.native.language...your.native.languages..Your.native.language.s..is.what.you.spoke.a...)

## Number of data points:

nrow(xdata)		# 200

## Create a response column:

xdata$Response <- str_extract(xdata$Response, 'shape')
xdata[is.na(xdata$Response), ]$Response <- 'height'

## Create a numerical offshoot of this:

xdata$Resp01 <- ifelse(xdata$Response == 'height', 1, 0)

## Those where the fingers are clearly in the way:

xdata$Occlusion <- 'no'
occluded <- c('OnIndex-Bottom_0008', 'OnIndex-Bottom_0017',
	'OnIndex-Bottom_0026', 'OnIndex-Bottom_0035', 'OnIndex-Bottom_0044',
	'OnIndex-Bottom_0053', 'OnIndex-Bottom_0062', 'OnIndex-Bottom_0071',
	'OnIndex-Middle_0008', 'OnIndex-Middle_0017', 'OnIndex-Middle_0026',
	'OnIndex-Middle_0035', 'OnIndex-Middle_0044', 'OnIndex-Middle_0053',
	'OnIndex-Top_0008', 'OnIndex-Top_0017', 'OnIndex-Top_0026',
	'OnIndex-Top_0035', 'OnIndex-Top_0044', 'OnPinkie-Bottom_0008',
	'OnPinkie-Bottom_0017', 'OnPinkie-Bottom_0026', 'OnPinkie-Bottom_0035',
	'OnPinkie-Middle_0008', 'OnPinkie-Middle_0017', 'OnPinkie-Middle_0026',
	'OnPinkie-Middle_0035', 'OnPinkie-Middle_0044', 'OnPinkie-Middle_0053',
	'OnPinkie-Top_0008', 'OnPinkie-Top_0035', 'OnPinkie-Top_0044',
	'OnPinkie-Top_0053', 'OnPinkie-Top_0062')
xdata[xdata$ObjectCondition %in% occluded, ]$Occlusion <- 'yes'

## Create a perspective variable and extract pinkie curl:

xdata <- separate(xdata, col = ObjectCondition, into = c('Perspective', 'Tilt', 'PinkieCurl'))

## Create a numerical pinkie_curl variable:

xdata$PinkieCurl <- as.numeric(xdata$PinkieCurl, '[0-9]+')
uniques <- unique(xdata$PinkieCurl)
myranks <- rank(unique(xdata$PinkieCurl))
xdata$PinkieCurl <- myranks[match(xdata$PinkieCurl, uniques)]

## Center this variable:

xdata <- mutate(xdata,
	PinkieCurl_c = PinkieCurl - mean(PinkieCurl))

## Select only those columns that are needed:

xdata <- select(xdata,
	Occlusion,
	Age:Language, PinkieCurl, PinkieCurl_c, Perspective, Tilt,
	Response, Resp01)

## Check randomization:

table(xdata$Perspective, xdata$PinkieCurl)
table(xdata$Perspective, xdata$Tilt)
table(xdata$PinkieCurl, xdata$Tilt)


## PinkieCurl is coded : small values = high curl, large values = big extension
## 1 is maximally curled in, 9 is maximally extended = this is an EXTENSION variable



##------------------------------------------------------------------
## Analysis:
##------------------------------------------------------------------

## Tabulate shape responses and height responses as a function of occlusion:

(xtab <- table(xdata$Response, xdata$Occlusion))
round(prop.table(xtab, 2), 2)
chisq.test(xtab)

(xtab2 <- table(xdata$Response, xdata$Occlusion, xdata$Perspective))
chisq.test(xtab2[, , 2])

## Check pinkiecurl separately:

xagr <- aggregate(Resp01 ~ Occlusion * Perspective, xdata, mean)
xagr %>% mutate(Resp01 = round(Resp01, 2))
xmdl <- glm(Resp01 ~ Occlusion, xdata, family = 'binomial')
summary(xmdl)

## Tabulate shape responses and height responses as a function of pinkie curl:

table(xdata$Response, xdata$PinkieCurl)

## Check pinkiecurl separately:

aggregate(Resp01 ~ PinkieCurl, xdata, mean) %>% round(2)
xmdl <- glm(Resp01 ~ PinkieCurl, xdata, family = 'binomial')
summary(xmdl)

## Check perspective separately:

aggregate(Resp01 ~ Perspective, xdata, mean)
xmdl <- glm(Resp01 ~ Perspective, xdata, family = 'binomial')
summary(xmdl)

## Check tilt:

aggregate(Resp01 ~ Tilt, xdata, mean)
xmdl <- glm(Resp01 ~ Tilt, xdata, family = 'binomial')
xmdl.null <- glm(Resp01 ~ 1, xdata, family = 'binomial')
anova(xmdl.null, xmdl, test = 'Chisq')

## Sum code perspective:

xdata$Perspective_c <- as.factor(xdata$Perspective)
contrasts(xdata$Perspective_c) <- contr.sum(2)

## Make tilt numeric:

xdata$Tilt_c <- as.numeric(as.factor(xdata$Tilt)) - 2

## Make an analysis of this:

xmdl <- glm(Resp01 ~ PinkieCurl_c + Perspective_c + Tilt_c, xdata, family = 'binomial')
summary(xmdl)

## Make an analysis with two-way interactions:

xmdl <- glm(Resp01 ~ PinkieCurl_c + Perspective_c + Tilt_c +
	PinkieCurl_c:Perspective_c + PinkieCurl_c:Tilt_c,
	xdata, family = 'binomial')
summary(xmdl)


