## Feb 1, 2017
## Bodo Winter
## Analysis of underspecification web experiment

##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Load stuff:

options(stringsAsFactors = F)

setwd('/Users/winterb/Research/gesture_categorization/new_paper_new_analyses/data/')
xdata <- read.csv('web_gesture_specification_implicit_object.csv')

library(dplyr)
library(stringr)
library(tidyr)

## Get response column:

# xdata$Response <- NA
these_cols <- grep('match', colnames(xdata))[1:4]

xdata$Response <- NA
for (i in 1:nrow(xdata)) {
	this_list <- unlist(xdata[i, these_cols])
	xdata[i, ]$Response <- this_list[grep('small|big|square|circle', this_list)]
	}

## Clean response column:

xdata$RespClean <- str_extract(xdata$Response, '(small|big) (circle|square)')

## Make a shape and a size response column:

xdata$SizeChoice <- str_extract(xdata$Response, '(small|big)')
xdata$ShapeChoice <- str_extract(xdata$Response, '(circle|square)')

## Rename columns:

xdata <- rename(xdata, DisplayCondition = Display.Order..Block.Randomizer.FL_3,
	ObjectCondition = Display.Order..Block.Randomizer.FL_4,
	Control = What.is.45.minus.15.,
	Age = What.is.your.age.,
	Gender = What.is.your.gender.,
	Handedness = Are.you.left.or.right.handed.,
	Language = What.is.your.native.language...your.native.languages..Your.native.language.s..is.what.you.spoke.a...)

## Select:

xdata <- select(xdata,
	DisplayCondition,
	ObjectCondition,
	Control:Language,
	RespClean, SizeChoice, ShapeChoice)

## Number of data points:

nrow(xdata)		# 6

## Create a numerical pinkie_curl variable:

xdata$PinkieCurl <- as.numeric(str_extract(xdata$DisplayCondition, '[0-9]+'))
uniques <- unique(xdata$PinkieCurl)
myranks <- rank(unique(xdata$PinkieCurl))
xdata$PinkieCurl <- myranks[match(xdata$PinkieCurl, uniques)]

## Center this variable:

xdata <- mutate(xdata,
	PinkieCurl_c = PinkieCurl - mean(PinkieCurl))



##------------------------------------------------------------------
## Analysis:
##------------------------------------------------------------------

## Tabulate shape responses and height responses as a function of pinkie curl:

table(xdata$SizeChoice, xdata$PinkieCurl)
table(xdata$ShapeChoice, xdata$PinkieCurl)




