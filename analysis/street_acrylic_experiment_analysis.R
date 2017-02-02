## Feb 1, 2017
## Bodo Winter
## Analysis of underspecification web experiment

##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Load stuff:

options(stringsAsFactors = F)

setwd('/Users/winterb/Research/gesture_categorization/new_paper_new_analyses/data/')
x <- dget('street_acrylic_objects.txt')

## Explanation:

# first letter is condition, second response
# O = shows circle
# I = shows stick
# i = response expected of thin form, i.e., dice stay in hand
# o = response expected of round form, i.e., dice are taken out of hand

## Cleaning:

splitted <- strsplit(x, split = '')
xdf <- data.frame(condition = sapply(splitted, function(x) x[1]),
    response = sapply(splitted, function(x)x[2]))

## How many data points?

nrow(xdf)    # 21



##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Tabulate:

xtab <- table(xdf$condition, xdf$response)
fisher.test(xtab)




