
## April 3, 2017
## Bodo Winter
## Analysis of underspecification web experiment

##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Load in libraries:

library(tidyverse)
library(tidytext)
library(stringr)
library(wordcloud)
library(lme4)
library(afex)

## Text experiments:

setwd('/Users/winterb/Research/gesture_categorization/new_paper_new_analyses/data/')
textexp <- read.csv('web_gesture_specification_text_entry.csv',
	skip = 1, stringsAsFactors = F, fileEncoding = 'latin1')

## Rename columns:

textexp <- rename(textexp,
	ID = ResponseID,
	Condition = Display.Order..Block.Randomizer.FL_5,
	DisplayOrder = Display.Order..Block.Randomizer.FL_31,
	Language = What.is.your.native.language...your.native.languages..Your.native.language.s..is.what.you.spoke.a...,
	Control = What.is.4.plus.5.,
	Age = What.is.your.age.,
	Gender = What.is.your.gender.,
	Handedness = Are.you.left.or.right.handed.,
	Comments = Please.share.any.comments..questions..thoughts.or.ideas.you.have.on.the.gesture.you.just.saw.or.t...,
	Shape1 = In.your.mind..what.was.the.object.s.SHAPE..Describe.,
	Size1 = In.your.mind..what.was.the.object.s.SIZE..Describe.,
	Shape2 = In.your.mind..what.was.the.object.s.SHAPE..Describe..1,
	Size2 = In.your.mind..what.was.the.object.s.SIZE..Describe..1)

## Select subfields:

textexp <- select(textexp,
	ID, Condition, DisplayOrder, Language, Control,
	Age, Gender, Handedness, Shape1, Shape2,
	Size1, Size2, Comments)

## Make into tibble:

textexp <- as_tibble(textexp)

## Combine shape1 and shape2 etc.:

textexp <- unite(textexp, Shape, Shape1, Shape2)
textexp <- unite(textexp, Size, Size1, Size2)

## Exclude those who got the control question NA (partial responses):

print(old.N <- nrow(textexp))
textexp <- filter(textexp, !is.na(Control))
print(new.N <- nrow(textexp))
new.N / old.N

## Exclude non-native speakers or those who listed other language first:

these_langs <- c('Russian, English ', 'Arabic', 'Indonesian, English.')
textexp <- filter(textexp, !(Language %in% these_langs))



##------------------------------------------------------------------
## Text processing:
##------------------------------------------------------------------

## Make into lower case:

textexp <- mutate(textexp,
	Shape = str_to_lower(Shape),
	Size = str_to_lower(Size))

## Replace underscores (which are spaces):

textexp <- mutate(textexp,
	Shape = str_replace_all(Shape, '_', ''),
	Size = str_replace_all(Size, '_', ''))

## Replace extra characters for quotation marks and also dots:

textexp <- mutate(textexp,
	Shape = str_replace_all(Shape, '\"', ''),
	Size = str_replace_all(Size, '\"', ''),
	Shape = str_replace_all(Shape, '\\.', ''),
	Size = str_replace_all(Size, '\\.', ''))

## Extract word count:

size_counts <- unnest_tokens(textexp, Size, Size) %>%
	count(ID) %>% rename(SizeCount = n)
shape_counts <- unnest_tokens(textexp, Shape, Shape) %>%
	count(ID) %>% rename(ShapeCount = n)

## Merge into table:

textexp <- left_join(textexp, size_counts)
textexp <- left_join(textexp, shape_counts)

## Check:

filter(textexp, Condition == 'LowCurl')$Size
filter(textexp, Condition == 'HighCurl')$Size

filter(textexp, Condition == 'LowCurl')$Shape
filter(textexp, Condition == 'HighCurl')$Shape

## Write to table:

text_red <- select(textexp, ID, Condition, Shape, Size, Comments)
write_csv(text_red, 'text_experiment_to_be_coded_full.csv')

## Analyze counts and whether shape / size counts are related:

quartz('', 9, 6)
plot(jitter(log(textexp$SizeCount)), jitter(log(textexp$ShapeCount)),
	pch = textexp$Condition, col = rgb(0, 0, 0, 0.5), cex = 2)
# some people write more, others less

## For ggplot:

textexp <- mutate(textexp,
	LogSizeCount = log10(SizeCount),
	LogShapeCount = log10(ShapeCount),
	LogRatio = LogSizeCount / LogShapeCount)
boxplot(LogRatio ~ Condition, textexp)
# no overall difference in length

## Make a ggplot of this:

ggplot(data = textexp, aes(x = LogSizeCount, y = LogShapeCount, fill = Condition)) +
	geom_point() + geom_smooth(method = 'lm')



##------------------------------------------------------------------
## Analysis of word counts:
##------------------------------------------------------------------

## Analyze log counts:

textexp <- mutate(textexp,
	LogSize = log10(SizeCount),
	LogShape = log10(ShapeCount))
summary(lm(LogSize ~ Condition, textexp))
summary(lm(LogShape ~ Condition, textexp))

## Analyze counts:

summary(glm(SizeCount ~ Condition + DisplayOrder,
	data = textexp, family = 'poisson'))
summary(glm(ShapeCount ~ Condition + DisplayOrder,
	data = textexp, family = 'poisson'))

## Plot both together:

bothcounts <- select(textexp, ID, Condition, SizeCount) %>%
	rename(Count = SizeCount)
bothcounts$Question <- 'Size'
bothcounts2 <- select(textexp, ID, Condition, ShapeCount) %>%
	rename(Count = ShapeCount)
bothcounts2$Question <- 'Shape'
bothcounts <- rbind(bothcounts, bothcounts2)
rm(bothcounts2)

## Get likelihood ratio tests:

xmdl.lrt <- mixed(Count ~ Condition * Question + (1|ID),
	bothcounts, family = 'poisson', method = 'LRT')

## Check model:

summary(xmdl <- glmer(Count ~ Condition * Question + (1|ID),
	bothcounts, family = 'poisson'))

## To facilitate model interpretation, sum code predictors:

bothcounts <- mutate(bothcounts,
	Condition = as.factor(Condition),
	Question = as.factor(Question))
contrasts(bothcounts$Condition) <- contr.sum(2)
contrasts(bothcounts$Question) <- contr.sum(2)

## Fit model:

summary(xmdl <- glmer(Count ~ Condition * Question + (1|ID),
	bothcounts, family = 'poisson'))

## Descriptive averages:

bothcounts %>% group_by(Condition, Question) %>% summarize(Count = mean(Count))



##------------------------------------------------------------------
## Analysis of coded data:
##------------------------------------------------------------------

## Load in coded data:

labels <- read_csv('text_experiment_manual_codings.csv')

## Presence absence of C-shape or semicircle mentioning:

with(labels, print(xtab <<- table(Condition, CShape)))
round(prop.table(xtab, 1), 2)
fisher.test(xtab)

## Object type:

with(labels, print(xtab <<- table(Condition, Round)))

## Exclude "sign":

xtab <- xtab[, colnames(xtab) != 'sign']

## Analyze those that are clearly rectangular versus clearly round
## (ignoring mixed cases):

print(xtab.clear <<- xtab[, colnames(xtab) %in% c('rectangular', 'yes')])
fisher.test(xtab.clear)
round(prop.table(xtab.clear, 1), 2)

## Interestingly, 4 people said they couldn't determine size,
## which only happened in the "HighCurl" condition:

print(xtab.hard <<- matrix(c(c(4, 0), rowSums(xtab)), nrow = 2, byrow = F))
fisher.test(xtab.hard)

## Test shape vagueness determiners:

with(labels, print(xtab <<- table(Condition, Vagueness)))
round(prop.table(xtab, 1), 2)
fisher.test(xtab)

## Split 'type' apart by focusing on sub-constituents:
## Take those that mention 'number' or 'hand' (even i they have multiple):

labels <- mutate(labels,
	AnyNumber = ifelse(str_detect(Type, 'number'), 'yes', 'no'),
	AnyHand = ifelse(str_detect(Type, 'hand'), 'yes', 'no'),
	AnyDimension = ifelse(str_detect(Type, 'dimension'), 'yes', 'no'),
	AnySource = ifelse(str_detect(Type, 'source'), 'yes', 'no'))

## Look for number mentioning:

with(labels, print(xtab <<- table(Condition, AnyNumber)))
round(prop.table(xtab, 1), 2)
fisher.test(xtab)

## Look for hand-sized mentioning:

with(labels, print(xtab <<- table(Condition, AnyHand)))
round(prop.table(xtab, 1), 2)
fisher.test(xtab)

## Look for dimension-word mentioning:

with(labels, print(xtab <<- table(Condition, AnyDimension)))
round(prop.table(xtab, 1), 2)
fisher.test(xtab)

## Look for source mentioning:

with(labels, print(xtab <<- table(Condition, AnySource)))
round(prop.table(xtab, 1), 2)
fisher.test(xtab)

## Look for diameter mentioning:

with(labels, print(xtab <<- table(Condition, Diameter)))
round(prop.table(xtab, 1), 2)
fisher.test(xtab)

## Look for type of dimension word:

with(labels, print(xtab <<- table(Condition, TypeOfSize)))
# results wouldn't change much if 'medium to large' is counted as large or medium
# same for 'small to medium'; so let's count both as medium (most agnostic)
meds <- xtab[, 'medium_to_large'] + xtab[, 'small_to_medium'] + xtab[, 'medium']
xtab <- cbind(xtab[, 'small'], meds, xtab[, 'large'])
colnames(xtab) <- c('small', 'medium', 'large')
xtab
round(prop.table(xtab, 1), 2)
chisq.test(xtab)

## Look for precise numerical values, those that are not diameters:

nodiam <- filter(labels, Diameter == 'no',
	Inches != 'cant_tell')
nodiam <- mutate(nodiam,
	Inches = as.numeric(Inches))
boxplot(Inches ~ Condition, data = nodiam)
wilcox.test(Inches ~ Condition, data = nodiam, paired = F)



##------------------------------------------------------------------
## Keyword analysis:
##------------------------------------------------------------------

## Make wordles:

set.seed(42)
par(mfrow = c(2, 2))
wordcloud(filter(textexp, Condition == 'LowCurl')$Size)
wordcloud(filter(textexp, Condition == 'HighCurl')$Size)

wordcloud(filter(textexp, Condition == 'LowCurl')$Shape)
wordcloud(filter(textexp, Condition == 'HighCurl')$Shape)


