## Feb 1, 2017
## Bodo Winter
## Analysis of underspecification web experiment

##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Load packages:

library(tidyverse)
library(stringr)
library(jpeg)

## Load data:

setwd('/Users/winterb/Research/gesture_categorization/new_paper_new_analyses/data/')
xdata <- read.csv('web_gesture_perspective2.csv', fileEncoding = 'latin1')

## Rename columns:

xdata <- rename(xdata, Response = The.gesture.you.just.saw.characterized.an.object..Ê.What.do.you.think.the.gesture.was.about.,
	ObjectCondition = Display.Order..Block.Randomizer.FL_3,
	Control = What.is.4...17..Please.type.in.your.answer.,
	Age = What.is.your.age.,
	Gender = What.is.your.gender.,
	Handedness = Are.you.left.or.right.handed.,
	Language = What.is.your.native.language...your.native.languages..Your.native.language.s..is.what.you.spoke.a...)

## Number of data points:

nrow(xdata)		# 362

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

## Get rid of that one extra object condition:

xdata <- filter(xdata, ObjectCondition != '')

## Get rid of Ju's response & wrong responses to control question:

xdata <- filter(xdata, Control != 'JuTest')
nrow(xdata)	# 360
xdata <- filter(xdata, Control == '21')
nrow(xdata)	# 353

## Take only those that are native speakers:

these_langs <- c('Italian', 'Japanese, English', 'Russian',
	'Serbian', 'Spanish and English', 'tamil, english')
xdata <- filter(xdata, !(Language %in% these_langs))
nrow(xdata)

## Create a perspective variable and extract pinkie curl:

xdata <- separate(xdata, col = ObjectCondition,
	into = c('Perspective', 'Tilt', 'PinkieCurl'),
	remove = F)

## Create a numerical pinkie_curl variable:

xdata$PinkieCurl <- as.numeric(xdata$PinkieCurl, '[0-9]+')
uniques <- unique(xdata$PinkieCurl)
myranks <- rank(unique(xdata$PinkieCurl))
xdata$PinkieCurl <- myranks[match(xdata$PinkieCurl, uniques)]

## Center this variable:

xdata <- mutate(xdata,
	PinkieCurl_c = PinkieCurl - mean(PinkieCurl))

## Select only those columns that are needed:

xdata <- dplyr::select(xdata,
	ObjectCondition,
	Occlusion,
	Age:Language, PinkieCurl, PinkieCurl_c, Perspective, Tilt,
	Response, Resp01)

## Check demographics:

table(xdata$Gender)
table(xdata$Language)
table(xdata$Control)

## Check randomization:

table(xdata$Perspective)
table(xdata$Tilt)
table(xdata$PinkieCurl)
table(xdata$Perspective, xdata$PinkieCurl)
table(xdata$Perspective, xdata$Tilt)
table(xdata$PinkieCurl, xdata$Tilt)


## PinkieCurl is coded : small values = high curl, large values = big extension
## 1 is maximally curled in, 9 is maximally extended = this is an EXTENSION variable



##------------------------------------------------------------------
## Analysis of main condition effects:
##------------------------------------------------------------------

## Sum code perspective:

xdata$Perspective_c <- (as.numeric(as.factor(xdata$Perspective)) - 2)

## Make tilt numeric:

xdata$Tilt_c <- as.numeric(as.factor(xdata$Tilt)) - 2

## Descriptive means suggest a quadratic pattern:

xdata$PinkieCurl_c2 <- xdata$PinkieCurl_c ^ 2

## Make an analysis of this:

xmdl <- glm(Resp01 ~ PinkieCurl_c + PinkieCurl_c2 + Perspective_c +
	Tilt_c, xdata, family = 'binomial')
summary(xmdl)

## With interactions:

xmdl.interact <- glm(Resp01 ~ (PinkieCurl_c + PinkieCurl_c2) * (Perspective_c +
	Tilt_c) + PinkieCurl_c:Perspective_c:Tilt_c + PinkieCurl_c2:Perspective_c:Tilt_c,
	xdata, family = 'binomial')
summary(xmdl.interact)

## Test all interactions:

anova(xmdl, xmdl.interact, test = 'Chisq')

## Null-models:

xmdl.nopinkie <- glm(Resp01 ~ Perspective_c + Tilt_c,
	xdata, family = 'binomial')
xmdl.nopinkie.noquadr <- glm(Resp01 ~ Perspective_c + Tilt_c + PinkieCurl_c,
	xdata, family = 'binomial')
xmdl.nopersp <- glm(Resp01 ~ PinkieCurl_c + PinkieCurl_c2 + Tilt_c,
	xdata, family = 'binomial')
xmdl.notilt <- glm(Resp01 ~ Perspective_c + PinkieCurl_c + PinkieCurl_c2,
	xdata, family = 'binomial')
anova(xmdl.nopinkie, xmdl, test = 'Chisq')		# both quadratic and linear
anova(xmdl.nopinkie.noquadr, xmdl, test = 'Chisq')	# only quadratic
anova(xmdl.nopinkie, xmdl.nopinkie.noquadr, test = 'Chisq')	# only linear
anova(xmdl.nopersp, xmdl, test = 'Chisq')
anova(xmdl.notilt, xmdl, test = 'Chisq')

## R-squared comparisons:

library(MuMIn)
r.squaredGLMM(xmdl)
r.squaredGLMM(xmdl)[1] - r.squaredGLMM(xmdl.nopinkie)[1]
r.squaredGLMM(xmdl)[1] - r.squaredGLMM(xmdl.nopersp)[1]
r.squaredGLMM(xmdl)[1] - r.squaredGLMM(xmdl.notilt)[1]

## Get predictions for perspective and tilt:

newdata <- data.frame(PinkieCurl_c = 0, PinkieCurl_c2 = 0, Tilt_c = 0,
	Perspective_c = c(-1, 1))
plogis(predict(xmdl, newdata))

## Make a plot of this model:

newdata <- data.frame(PinkieCurl_c = sort(unique(xdata$PinkieCurl_c)),
	Tilt_c = 0, Perspective_c = 0)
newdata$PinkieCurl_c2 <- newdata$PinkieCurl_c ^ 2
newdata <- cbind(newdata,
	as.data.frame(predict.glm(xmdl, newdata, se.fit = T)[1:2]))
newdata$LB <- plogis(newdata$fit - 1.96 * newdata$se.fit)
newdata$UB <- plogis(newdata$fit + 1.96 * newdata$se.fit)
newdata$fit <- plogis(newdata$fit)

## Get jpegs to plot:

setwd('/Users/winterb/Research/gesture_categorization/new_paper_new_analyses/figures/')
left <- readJPEG('OnPinkie-Middle_0008.jpeg')
right <- readJPEG('OnPinkie-Middle_0080.jpeg')

## Make a plot of this:

quartz('', 9, 6)
par(mai = c(1.5, 2, 0.25, 0.75))
plot(1, 1, type = 'n', xlim = c(0.5, 9.15), ylim = c(0, 1),
	xlab = '', ylab = '', bty = 'n', xaxt = 'n', yaxt = 'n')
# images along x-axis
rasterImage(left,
	xleft = -0.5, xright = 1.5,
	ybottom = 0 - 0.38, ytop = 0.28 - 0.38, xpd = NA)
rasterImage(right,
	xleft = 8.5, xright = 10.5,
	ybottom = 0 - 0.38, ytop = 0.28 - 0.38, xpd = NA)
# axis
axis(side = 1, at = 1:9, lwd = 2, labels = F, lwd.ticks = 2)
axis(side = 1, at = 1:9, lwd = 2, tick = F, line = 0.20,
	font = 2, cex.axis = 1.8)
axis(side = 2, at = seq(0, 1, 0.25),
	labels = paste0(seq(0, 1, 0.25) * 100, '%'),
	las = 2, cex.axis = 1.8, font = 2, lwd = 2, lwd.ticks = 2)
mtext(side = 1, text = 'Pinkie curl continuum', cex = 2, font = 2,
	line = 3.5)
mtext(side = 2, text = '% height responses', cex = 2, font = 2,
	line = 5.5)
abline(h = 0.5, lty = 2, lwd = 2)
# predictions
points(1:9, newdata$fit, pch = 15, cex = 2)
arrows(x0 = 1:9, y0 = newdata$LB, y1 = newdata$UB,
	code = 3, angle = 90, lwd = 2, length = 0.15)

## Make a plot of this, reversed:

quartz('', 9, 6)
par(mai = c(1.5, 2, 0.25, 0.75))
plot(1, 1, type = 'n', xlim = c(0.5, 9.15), ylim = c(0, 1),
	xlab = '', ylab = '', bty = 'n', xaxt = 'n', yaxt = 'n')
# images along x-axis
rasterImage(right,
	xleft = -0.5, xright = 1.5,
	ybottom = 0 - 0.38, ytop = 0.28 - 0.38, xpd = NA)
rasterImage(left,
	xleft = 8.5, xright = 10.5,
	ybottom = 0 - 0.38, ytop = 0.28 - 0.38, xpd = NA)
# axis
axis(side = 1, at = 1:9, lwd = 2, labels = F, lwd.ticks = 2)
axis(side = 1, at = 1:9, lwd = 2, tick = F, line = 0.20,
	font = 2, cex.axis = 1.8)
axis(side = 2, at = seq(0, 1, 0.25),
	labels = paste0(seq(0, 1, 0.25) * 100, '%'),
	las = 2, cex.axis = 1.8, font = 2, lwd = 2, lwd.ticks = 2)
mtext(side = 1, text = 'Pinkie curl continuum', cex = 2, font = 2,
	line = 3.5)
mtext(side = 2, text = 'Height responses', cex = 2, font = 2,
	line = 5.5)
abline(h = 0.5, lty = 2, lwd = 2)
# predictions
points(1:9, rev(newdata$fit), pch = 15, cex = 2)
arrows(x0 = 1:9, y0 = rev(newdata$LB), y1 = rev(newdata$UB),
	code = 3, angle = 90, lwd = 2, length = 0.15)

## Ju requested changes:

quartz('', 9, 6)
par(mai = c(1.5, 2, 0.25, 0.75))
plot(1, 1, type = 'n', xlim = c(0.5, 9.15), ylim = c(0, 1),
	xlab = '', ylab = '', bty = 'n', xaxt = 'n', yaxt = 'n')
# images along x-axis
rasterImage(right,
	xleft = -0.7, xright = 1.7,
	ybottom = 0 - 0.42 + 0.05, ytop = 0.32 - 0.42 + 0.05, xpd = NA)
rasterImage(left,
	xleft = 8.3, xright = 10.7,
	ybottom = 0 - 0.42 + 0.05, ytop = 0.32 - 0.42 + 0.05, xpd = NA)
# axis
axis(side = 1, at = 1:9, lwd = 2, labels = F, lwd.ticks = 2)
axis(side = 1, at = 1:9, lwd = 2, tick = F, line = -0.1,
	font = 2, cex.axis = 1.15)
axis(side = 2, at = seq(0, 1, 0.25),
	labels = paste0(seq(0, 1, 0.25) * 100, '%'),
	las = 2, cex.axis = 1.35, font = 2, lwd = 2, lwd.ticks = 2)
mtext(side = 1, text = 'Pinkie curl continuum', cex = 1.6, font = 2,
	line = 3.5)
mtext(side = 2, text = 'Height responses', cex = 1.75, font = 2,
	line = 5.5)
abline(h = 0.5, lty = 2, lwd = 2)
# predictions
points(1:9, rev(newdata$fit), pch = 15, cex = 2)
arrows(x0 = 1:9, y0 = rev(newdata$LB), y1 = rev(newdata$UB),
	code = 3, angle = 90, lwd = 2, length = 0.15)



##------------------------------------------------------------------
## Analysis of alternative variables, Ju 2017:
##------------------------------------------------------------------

# # Entweder Du beschreibst es nicht richtig oder die Berechnung ist nicht richtig (sogar entgegengesetzt!). Denn die BOTTOM-Kategorie ist in der „on pinkie“ Kondition zwar die die am wenigsten finger3-5-intrusion hat, ABER in der „on-index“-Kondition ist es die TOP Kondition. (Finger 1 ist ja bei onPinkie hinten, aber bei onIndex vorn => von oben bzw. von unten gucken hat den entgegengesetzten Effekt wenn man von der einen viewing direction zur anderen wechselt.)
# Du musst also die drei Gruppen vergleichen:
# 1.    Onpinkiebottom+onindextop
# 2.    Onpinkiemiddle+onindexmiddle
# 3.    Onpinkietop+onindexbottom

# Da wäre dann der erwartete Effekt: bei (1.) weniger height, bei (3.) mehr height

# ODER(leichter): Du erfasst die viewing angles separat für onPinkie und onIndex (wie in der graphSum im Mail-Anhang). Die Predictions wären dann entgegengesetzt für diese beiden viewing directions (wie in der graphSum im Mail-Anhang grob angedeutet).

## Doing the second thing first:

summary(xmdl.onPinkie <- glm(Resp01 ~ PinkieCurl_c + Tilt_c + 
	Tilt_c, filter(xdata, Perspective == 'OnPinkie'), family = 'binomial'))
summary(xmdl.onPinkie)

summary(xmdl.onIndex <- glm(Resp01 ~ PinkieCurl_c + Tilt_c + 
	Tilt_c, filter(xdata, Perspective == 'OnIndex'), family = 'binomial'))
summary(xmdl.onIndex)

## Create three groups:

xdata$PerspectiveGroup <- NA
group1 <- xdata$Perspective == 'OnPinkie' & xdata$Tilt == 'Bottom'
group1 <- group1 | (xdata$Perspective == 'OnIndex' & xdata$Tilt == 'Top')
xdata[group1, ]$PerspectiveGroup <- 'group1'

group2 <- xdata$Perspective == 'OnPinkie' & xdata$Tilt == 'Middle'
group2 <- group2 | (xdata$Perspective == 'OnIndex' & xdata$Tilt == 'Middle')
xdata[group2, ]$PerspectiveGroup <- 'group2'

group3 <- xdata$Perspective == 'OnPinkie' & xdata$Tilt == 'Top'
group3 <- group3 | (xdata$Perspective == 'OnIndex' & xdata$Tilt == 'Bottom')
xdata[group3, ]$PerspectiveGroup <- 'group3'

## Check:

table(xdata$PerspectiveGroup)	# roughly equal data points

## Make a model of this, without pinkie curl:

summary(xmdl.persp_group <- glm(Resp01 ~ PerspectiveGroup,
	xdata, family = 'binomial'))
anova(xmdl.persp_group, test = 'Chisq')	# neither

## Make a model of this, with pinkie curl:

summary(xmdl.persp_group <- glm(Resp01 ~ PerspectiveGroup + PinkieCurl_c,
	xdata, family = 'binomial'))
anova(xmdl.persp_group, test = 'Chisq')	# neither

summary(xmdl.persp_group_int <- glm(Resp01 ~ PerspectiveGroup * PinkieCurl_c,
	xdata, family = 'binomial'))
anova(xmdl.persp_group, xmdl.persp_group_int, test = 'Chisq')



##------------------------------------------------------------------
## Analysis of alternative variables (occlusion etc.):
##------------------------------------------------------------------

# The view that shows most of fingers 3-5 in the on pinkie condition is “top”. The view that shows most of fingers 3-5 in the on index condition is “bottow”.
# So IF you wanna group, you have to group onpinkieTop with onindexBottomthat and onpinkieMiddle with onindexMiddle and onpinkieBottom with onindexTop. 

## Follow Ju's suggestions:

xdata$mostPinkie <- 'lessFingers3-5'
these <- xdata$Perspective == 'OnIndex' & xdata$Tilt == 'Bottom'
these <- these | xdata$Perspective == 'OnPinkie' & xdata$Tilt == 'Top'
xdata[these, ]$mostPinkie <- 'moreFingers3-5'

## Model this:

summary(xmdl.new <- glm(Resp01 ~ mostPinkie, xdata, family = 'binomial'))

## Tabulate shape responses and height responses as a function of occlusion:

(xtab <- table(xdata$Response, xdata$Occlusion))
round(prop.table(xtab, 2), 2)
chisq.test(xtab)

(xtab2 <- table(xdata$Response, xdata$Occlusion, xdata$Perspective))
chisq.test(xtab2[, , 2])

## Compare pinkie curl versus occlusion:

xmdl.occl <- glm(Resp01 ~ Occlusion, xdata, family = 'binomial')
xmdl.null <- glm(Resp01 ~ 1, xdata, family = 'binomial')
xmdl.pinkie <- glm(Resp01 ~ PinkieCurl_c + PinkieCurl_c2, xdata, family = 'binomial')
anova(xmdl.null, xmdl.pinkie, test = 'Chisq')
anova(xmdl.null, xmdl.occl, test = 'Chisq')
library(qpcR)
evidence(xmdl.pinkie, xmdl.occl)	# more evidence for pinkie model




##------------------------------------------------------------------
## Analysis of alternative variables (occlusion etc.):
##------------------------------------------------------------------

## Load in area file:

setwd('/Users/winterb/Research/gesture_categorization/new_paper_new_analyses/data/')
areas <- read_csv('web_gesture_perspective_condition_area.csv')

## Calculate percentage overlap:

areas <- mutate(areas,
	PercentOverlap = FingerArea / CShapeArea)

## Merge:

xdata$PercentOverlap <- areas[match(xdata$ObjectCondition, areas$Condition), ]$PercentOverlap
xdata$Diff <- areas[match(xdata$ObjectCondition, areas$Condition), ]$Diff

## Center variables:

xdata <- mutate(xdata,
	PercentOverlap_c = PercentOverlap - mean(PercentOverlap),
	Diff_c = Diff - mean(Diff))

## Make a model with area:

summary(xmdl.area <- glm(Resp01 ~ PercentOverlap,
	xdata, family = 'binomial'))
summary(xmdl.diff <- glm(Resp01 ~ Diff,
	xdata, family = 'binomial'))
summary(xmdl.diff.quadr <- glm(Resp01 ~ Diff_c + I(Diff_c ^ 2),
	xdata, family = 'binomial'))
summary(xmdl.diff2 <- glm(Resp01 ~ Diff_c + PinkieCurl_c + PinkieCurl_c2,
	xdata, family = 'binomial'))
summary(xmdl.pinkie <- glm(Resp01 ~ PinkieCurl_c + PinkieCurl_c2,
	xdata, family = 'binomial'))

## Check VIFs:

library(car)
vif(xmdl.diff2)

## Model comparison:

anova(xmdl.diff, xmdl.diff2, test = 'Chisq')
anova(xmdl.null, xmdl.diff, test = 'Chisq')
anova(xmdl.pinkie, xmdl.diff2, test = 'Chisq')
anova(xmdl.null, xmdl.area, test = 'Chisq')
r.squaredGLMM(xmdl.area)
r.squaredGLMM(xmdl.pinkie)
r.squaredGLMM(xmdl.diff)
r.squaredGLMM(xmdl.diff2)
BIC(xmdl.pinkie)
BIC(xmdl.diff.quadr)
BIC(xmdl.diff)
AIC(xmdl.diff) - AIC(xmdl.pinkie)
library(qpcR)
evidence(xmdl.pinkie, xmdl.diff)
evidence(xmdl.diff.quadr, xmdl.pinkie)

