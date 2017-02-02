## Bodo Winter
## June 3, 2015; Edited July 18, 2016; Edited Feb 1, 2017
## Dice task analysis

##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Load in data:

setwd('/Users/winterb/Research/gesture_categorization/new_paper_new_analyses/data/')
d <- read.csv('street_dice_task.csv', stringsAsFactors = F)

## Check:

table(d$question.order)

## Combine:

d[d$dice.removal == 'removed dice before 1st prompt', ]$dice.removal <- 'removed before 1st'

## Rename:

d[d$dice.removal == 'removed dice before 1st task', ]$dice.removal <- 'removed before 1st'
d[d$dice.removal == 'removed dice before 2nd task', ]$dice.removal <- 'removed dice before 2nd'



##------------------------------------------------------------------
## Analysis:
##------------------------------------------------------------------

## Check association between removal and question type:

table(d$question.order, d$dice.removal)

## Let's look at the subset of "removers":

(xtab <- table(d$question.order,d$dice.removal)[1:2, 3:4])

## Or Fisher's exact test:

fisher.test(xtab)

## For Porto, make a nice barplot out of this:

quartz('', 10, 7)
par(mai = c(2, 0, 1, 0))
plot(1, 1, type = 'n', bty = 'n',
	xaxt = 'n', yaxt = 'n',
	xlim = c(0, 2), ylim = c(0, 1),
	xaxs = 'i', yaxs = 'i',
	xlab = '', ylab = '')
axis(at = c(0.75, 1.45)[1], side = 1,
	labels = '',
	line = 2.5, tick = F, font = 2, cex.axis = 5)
# First bar:
rect(xleft = 0.6, xright = 0.9, ybottom = 0, ytop = p1,
	col = 'darkred',
	border = F)
text(x = 0.75, y = p1 + 0.11,
	col = 'darkred',
	labels = paste0(round(p1, 2) * 100, '%'),
	font = 2, cex = 4.5)
# Second bar:
text(x = 1.45, y = p2 + 0.11,
	col = 'darkred',
	labels = paste0(round(p2, 2) * 100, '%'),
	font = 2, cex = 4.5, xpd = NA)
rect(xleft = 1.3, xright = 1.6, ybottom = 0, ytop = p2,
	col = 'darkred',
	border = F)




