## Feb 1, 2017
## Bodo Winter
## Analysis of underspecification web experiment

##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Load stuff:

options(stringsAsFactors = F)

setwd('/Users/winterb/Research/gesture_categorization/new_paper_new_analyses/data/')
xdata <- read.csv('web_gesture_specification_explicit_question.csv')

library(dplyr)
library(stringr)

## Rename columns:

xdata <- rename(xdata, Condition = Display.Order..Block.Randomizer.FL_5,
	Control = What.is.4.plus.5.)

## Number of data points:

nrow(xdata)		# 56

## Create a "shape response" column:

xdata$ShapeResponse <- NA
xdata$HeightResponse <- NA

shape_colnames <- grep('shape', colnames(xdata))
height_colnames <- grep('height', colnames(xdata))

for (i in 1:nrow(xdata)) {
	xdata[i, ]$ShapeResponse <- ifelse(any(unlist(xdata[i, shape_colnames]) == 'Yes'), 'yes', 'no')
	xdata[i, ]$HeightResponse <- ifelse(any(unlist(xdata[i, height_colnames]) == 'Yes'), 'yes', 'no')
	xdata[i, ]$ShapeResponse <- ifelse(any(unlist(xdata[i, shape_colnames]) == 'No'), 'no', 'yes')
	xdata[i, ]$HeightResponse <- ifelse(any(unlist(xdata[i, height_colnames]) == 'No'), 'no', 'yes')
	}

## Create a numerical offshot of this:

xdata$ShapeNum <- ifelse(xdata$ShapeResponse == 'yes', 1, 0)
xdata$HeightNum <- ifelse(xdata$HeightResponse == 'yes', 1, 0)

## Create a numerical pinkie_curl variable:

xdata$PinkieCurl <- as.numeric(str_extract(xdata$Condition, '[0-9]+'))
uniques <- unique(xdata$PinkieCurl)
myranks <- rank(unique(xdata$PinkieCurl))
xdata$PinkieCurl <- myranks[match(xdata$PinkieCurl, uniques)]

## Center this variable:

xdata <- mutate(xdata,
	PinkieCurl_c = PinkieCurl - mean(PinkieCurl))

## Rename other columns:

xdata <- rename(xdata,
	Age = What.is.your.age.,
	Gender = What.is.your.gender.,
	Handedness = Are.you.left.or.right.handed.,
	Language = What.is.your.native.language...your.native.languages..Your.native.language.s..is.what.you.spoke.a...
	)

## Select only those columns that are needed:

xdata <- select(xdata,
	Age:Language, ShapeResponse:PinkieCurl_c)

## 1 is maximally curled in, 9 is maximally extended = this is an EXTENSION variable



##------------------------------------------------------------------
## Analysis:
##------------------------------------------------------------------

## Tabulate shape responses and height responses as a function of pinkie curl:

table(xdata$ShapeResponse, xdata$PinkieCurl)
table(xdata$HeightResponse, xdata$PinkieCurl)

prop.table(table(xdata$ShapeResponse, xdata$PinkieCurl), 2) %>% round(2)
prop.table(table(xdata$HeightResponse, xdata$PinkieCurl), 2) %>% round(2)

## Insgesamt aber:

x <- table(xdata$ShapeResponse)
y <- table(xdata$HeightResponse)
(xtab <- rbind(x, y))
chisq.test(xtab)

## Make an analysis of this:

xmdl.shape <- glm(ShapeNum ~ PinkieCurl, xdata, family = 'binomial')
summary(xmdl.shape)

## Make an analysis of this:

xmdl.height <- glm(HeightNum ~ PinkieCurl, xdata, family = 'binomial')
summary(xmdl.height)

## Look at those that are 'any shape but not any height' and 'any height but not any shape':

xdata$Both <- 'both'
shape_only <- which(xdata$ShapeResponse == 'yes' & xdata$HeightResponse == 'no')
xdata[shape_only, ]$Both <- 'shape_only'

height_only <- which(xdata$ShapeResponse == 'no' & xdata$HeightResponse == 'yes')
xdata[height_only, ]$Both <- 'height_only'

## Explicit question:

table(xdata$Both)

## Aggregate:

aggregate(PinkieCurl ~ Both, xdata, mean)
with(filter(xdata, Both != 'both'),
	t.test(PinkieCurl ~ Both, var.equal = T))




