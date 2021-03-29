## Project Name: TransOver
## Analysis 2: Children's use of generic language

## Written by: Hanna Schleihauf
## Date: 22 March 2021

## load necessary packages and functions
source("./functions/diagnostic_fcns.r")
source("./functions/glmm_stability.r")
source("./functions/glmmTMB_stability.r")
source("./functions/drop1_para_glmmtmb.r")

library(lme4)
library(glmmTMB)
library(effects)
library(dplyr)
library(tidyverse)
library(readxl)
library(car)

xdata <-
  read_xls(path = "./data/R-Transover.xls", col_names = T, na = "NA")
str(xdata)

## making sure that factors are factors
xdata$Fidelity.type <- as.factor(xdata$Fidelity.type)
xdata$Sex <- as.factor(xdata$Sex)
xdata$Study.population <- as.factor(xdata$Study.population)
xdata$Game <- as.factor(xdata$Game)
xdata$Study.phase <- as.factor(xdata$Study.phase)
xdata$id <- as.factor(xdata$id)

## since we only measured generic language during game transmission, we will clean the data frame accordingly
# sort by id
GSdata <- xdata[order(xdata$id, xdata$Study.phase), ]
# delete every second row, because we only have one GS value for the transmission phase
GSdata <- subset(GSdata, GSdata$Fidelity.type == "adoption")
GSdata$Study.population <- factor(GSdata$Study.population, levels(GSdata$Study.population)[c(2, 1)])
levels(GSdata$Study.population)
# create binary GS.score for data exploration
GSdata$GS.binary <- ifelse(GSdata$Generic.Language.Score != 0, 1, 0)
# count number of words total
GSdata$number.of.words <-
  sapply(gregexpr("[[:alpha:]]+", GSdata$Language.Transcription.complete), function(x) sum(x > 0))
GSdata$number.of.words[is.na(GSdata$number.of.words)] <- 0
# count number of words in generic sentences
GSdata$number.of.words.in.generic.sentences <-
  sapply(gregexpr("[[:alpha:]]+", GSdata$Language.Transcription.generic), function(x) sum(x > 0))
GSdata$number.of.words.in.generic.sentences[is.na(GSdata$number.of.words.in.generic.sentences)] <- 0

## checking the distribution of the generic language scores and word counts
# Generic language score
hist(GSdata$Generic.Language.Score)
# Binary score
## checking how many children  show in 0, 1 or both Study.phases GS
table(GSdata$GS.binary)
xx <- ftable(GS.binary ~ id + Study.phase, GSdata)
xx <- xx[, 2]
yy <- zoo::rollapply(xx, width = 2, sum, by = 2)
table(yy)
# create new data frame to store the difference scores
sum.of.GS <- data.frame(GSdata$id, GSdata$Study.population)
names(sum.of.GS) <- c("id", "Study.population")
# delete every second row
toDelete <- seq(0, nrow(sum.of.GS), 2)
toDelete
sum.of.GS <- sum.of.GS[-toDelete, ]
sum.of.GS$GS.sum <- yy
table(sum.of.GS$Study.population, sum.of.GS$GS.sum)
# word counts complete
hist(GSdata$number.of.words, breaks = 20)
hist(GSdata$number.of.words.in.generic.sentences, breaks = 20)

## Starting analyses
xx.fe.re <- fe.re.tab(
  fe.model = "number.of.words.in.generic.sentences ~ Study.population + Study.phase + Sex + Game",
  re = "(1|id)", other.vars = c("number.of.words", "Generic.Language.Score", "GS.binary"), data = GSdata
)
t.data <- xx.fe.re$data
# centering variables that might go in random slopes
t.data$c.Study.phase.second <- t.data$Study.phase.second - mean(t.data$Study.phase.second)
xx.fe.re$summary # we only have one observation per level, therefore we cannot include study phase in the random slopes

## Analyzing the DV: Generic Language Score
## Pre-analysis: Does game type affect the children's Generic Language Score?
contr <-
  glmmTMBControl(
    optCtrl = list(iter.max = 200000, eval.max = 200000),
    profile = FALSE, collect = FALSE
  )
game <- glmmTMB(Generic.Language.Score ~ Game +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
game.null <- glmmTMB(Generic.Language.Score ~ 1 +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
test <- drop1p(
  model.res = game, para = F, data = t.data, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL, return.model.results = F
)
test$drop1.res
r.squaredLR(game, null = game.null) # effect size for game

## Main-analysis: Do study population and study phase affect the children's normative language use?
GL.score <- glmmTMB(Generic.Language.Score ~ Study.population * Study.phase + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
# checking assumptions
# check distribution of random effects
ranef.diagn.plot(GL.score)
summary(GL.score)$varcor
## check absence of colliniarity
xx <- lm(Generic.Language.Score ~ Study.population + Study.phase + Sex, data = t.data)
vif(xx)
## check model stability
words.stab <- glmmTMB.stab(
  model.res = GL.score, contr = contr, ind.cases = F, para = F, data = t.data, use = "id",
  n.cores = c("all-1"), save.path = NULL, load.lib = T, lib.loc = .libPaths()
)
words.stab$detailed$warnings
xx <- round(words.stab$summary[, -1], 3)
dev.off()
m.stab.plot(words.stab$summary[, -1])
write.table(xx, "GL.Score.stab.stability.txt", quote = FALSE, sep = "\t")
xx ## model is stable

# look at the fixed effects
xx <- round(summary(GL.score)$coefficients$cond, 3)
write.table(xx, "GL.score.model.coefficients.txt", quote = FALSE, sep = "\t")

# null model
GL.score.null <- glmmTMB(Generic.Language.Score ~ 1 + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
round(anova(GL.score, GL.score.null, test = "Chisq"), 3)
r.squaredLR(GL.score, null = GL.score.null) # effect size for full-null model comparison

# reduced model comparison
test <- drop1p(
  model.res = GL.score, para = F, data = t.data, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL, return.model.results = F
)
test$drop1.res

# without two-way interaction
GL.score.red <- glmmTMB(Generic.Language.Score ~ Study.population + Study.phase + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
test <- drop1p(
  model.res = GL.score.red, para = F, data = t.data, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL, return.model.results = F
)
test$drop1.res
# quick look at pattern of effects
plot(effect("Study.population", GL.score.red))
plot(effect("Study.phase", GL.score.red))

# calculate effect sizes
r.squaredLR(GL.score, null = GL.score.red) # effect size for two-way interaction

GL.score.red.w.Study.population <- glmmTMB(Generic.Language.Score ~ Study.phase + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
r.squaredLR(GL.score.red, null = GL.score.red.w.Study.population) # effect size for Study population

GL.score.red.w.Study.phase <- glmmTMB(Generic.Language.Score ~ Study.population + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
r.squaredLR(GL.score.red, null = GL.score.red.w.Study.phase) # effect size for Study phase



## Analyzing the DV: Number of words used in normative sentences (as asked by a reviewer).
## Pre-analysis: Does game type affect the children's normative word use?
game.normative.words <- glmmTMB(number.of.words.in.generic.sentences ~ Game +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
game.normative.words.null <- glmmTMB(number.of.words.in.generic.sentences ~ 1 +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
test <- drop1p(
  model.res = game.normative.words, para = F, data = t.data, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL, return.model.results = F
)
test$drop1.res
r.squaredLR(game.normative.words, null = game.normative.words.null) # effect size for game

## Main Analysis: Do study population and study phase affect the number of words used in normative sentences?
normative.words <- glmmTMB(number.of.words.in.generic.sentences ~ Study.population * Study.phase + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
# checking assumptions
# check distribution of random effects
summary(normative.words)$varcor
ranef.diagn.plot(normative.words)
# check absence of colliniarity
xx <- lm(number.of.words.in.generic.sentences ~ Study.population + Study.phase + Sex, data = t.data)
vif(xx)
# check model stability
words.stab <- glmmTMB.stab(
  model.res = normative.words, contr = contr, ind.cases = F, para = F, data = t.data, use = "id",
  n.cores = c("all-1"), save.path = NULL, load.lib = T, lib.loc = .libPaths()
)
words.stab$detailed$warnings
xx <- round(words.stab$summary[, -1], 3)
dev.off()
m.stab.plot(words.stab$summary[, -1])
write.table(xx, "normative.words.stab.stability.txt", quote = FALSE, sep = "\t")
xx ## model is stable

# look at the fixed effects
xx <- round(summary(normative.words)$coefficients$cond, 3)
write.table(xx, "normative.words.model.coefficients.txt", quote = FALSE, sep = "\t")

# null model
normative.words.null <- glmmTMB(number.of.words.in.generic.sentences ~ 1 + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
round(anova(normative.words, normative.words.null, test = "Chisq"), 3)
r.squaredLR(normative.words, null = normative.words.null) # effect size for full-null model comparison

# reduced model comparison
test <- drop1p(
  model.res = normative.words, para = F, data = t.data, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL, return.model.results = F
)
test$drop1.res

# without two-way interaction
normative.words.red <- glmmTMB(number.of.words.in.generic.sentences ~ Study.population + Study.phase + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
test <- drop1p(
  model.res = normative.words.red, para = F, data = t.data, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL, return.model.results = F
)
test$drop1.res

# calculate effect sizes
r.squaredLR(normative.words, null = normative.words.red) # effect size for interaction

normative.words.red.w.Study.population <- glmmTMB(number.of.words.in.generic.sentences ~ Study.phase + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
r.squaredLR(normative.words.red, null = normative.words.red.w.Study.population) # effect size for Study population

normative.words.red.w.Study.phase <- glmmTMB(number.of.words.in.generic.sentences ~ Study.population + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
r.squaredLR(normative.words.red, null = normative.words.red.w.Study.phase) # effect size for Study phase



## Analyzing the DV: Number of words used in non-normative sentences (as asked by a reviewer).
# caluculate the number of words in non-normative utterances
t.data$difference <- t.data$number.of.words - t.data$number.of.words.in.generic.sentences

## Pre-analysis: Does game type affect the children's non-normative language use?
difference.game <- glmmTMB(difference ~ Game +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
difference.game.null <- glmmTMB(difference ~ 1 +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
test <- drop1p(
  model.res = difference.game, para = F, data = t.data, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL, return.model.results = F
)
test$drop1.res
r.squaredLR(difference.game, null = difference.game.null) # effect size for game

## Main Analysis: Do study population and study phase affect the number of words used in normative sentences?
other.words <- glmmTMB(difference ~ Study.population * Study.phase + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
# checking assumptions
# check distribution of random effects
summary(other.words)$varcor
ranef.diagn.plot(other.words)
# check absence of colliniarity
xx <- lm(difference ~ Study.population + Study.phase + Sex, data = t.data)
vif(xx)
# check model stability
words.stab <- glmmTMB.stab(
  model.res = other.words, contr = contr, ind.cases = F, para = F, data = t.data, use = "id",
  n.cores = c("all-1"), save.path = NULL, load.lib = T, lib.loc = .libPaths()
)
words.stab$detailed$warnings
xx <- round(words.stab$summary[, -1], 3)
dev.off()
m.stab.plot(words.stab$summary[, -1])
write.table(xx, "words.stab.stability.txt", quote = FALSE, sep = "\t")
xx ## model is stable

# look at the fixed effects
xx <- round(summary(other.words)$coefficients$cond, 3)
write.table(xx, "other.words.model.coefficients.txt", quote = FALSE, sep = "\t")

# null model
other.words.null <- glmmTMB(difference ~ 1 + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
round(anova(other.words, other.words.null, test = "Chisq"), 3) # the full model is not significantly different from the null model
r.squaredLR(other.words, null = other.words.null) # effect size for full-null model comparison

# reduced model comparison
test <- drop1p(
  model.res = other.words, para = F, data = t.data, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL, return.model.results = F
)
test$drop1.res

# without two-way interaction
other.words.red <- glmmTMB(difference ~ Study.population + Study.phase + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
test <- drop1p(
  model.res = other.words.red, para = F, data = t.data, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL, return.model.results = F
)
test$drop1.res

# calculate effect sizes
r.squaredLR(other.words, null = other.words.red) # effect size for interaction

other.words.red.w.Study.phase <- glmmTMB(difference ~ Study.population + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
r.squaredLR(other.words.red, null = other.words.red.w.Study.phase) # effect size for Study phase

other.words.red.w.Study.population <- glmmTMB(difference ~ Study.phase + Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)
r.squaredLR(other.words.red, null = other.words.red.w.Study.population) # effect size for Study population

### Descriptive Statistics
my_range <- function(x, na.rm = TRUE) {
  max(x, na.rm = na.rm) - min(x, na.rm = na.rm)
}
round(tapply(t.data$Generic.Language.Score, list(t.data$Study.population, t.data$Study.phase), mean), 3)
round(tapply(t.data$Generic.Language.Score, list(t.data$Study.population, t.data$Study.phase), sd), 3)
round(tapply(t.data$Generic.Language.Score, list(t.data$Study.population, t.data$Study.phase), median), 3)
round(tapply(t.data$Generic.Language.Score, list(t.data$Study.population, t.data$Study.phase), my_range), 3)

round(tapply(t.data$number.of.words.in.generic.sentences, list(t.data$Study.population, t.data$Study.phase), mean), 3)
round(tapply(t.data$number.of.words.in.generic.sentences, list(t.data$Study.population, t.data$Study.phase), sd), 3)
round(tapply(t.data$number.of.words.in.generic.sentences, list(t.data$Study.population, t.data$Study.phase), median), 3)
round(tapply(t.data$number.of.words.in.generic.sentences, list(t.data$Study.population, t.data$Study.phase), my_range), 3)

round(tapply(t.data$difference, list(t.data$Study.population, t.data$Study.phase), mean), 3)
round(tapply(t.data$difference, list(t.data$Study.population, t.data$Study.phase), sd), 3)
round(tapply(t.data$difference, list(t.data$Study.population, t.data$Study.phase), median), 3)
round(tapply(t.data$difference, list(t.data$Study.population, t.data$Study.phase), my_range), 3)

save.image("./Analysis.2.generic.language/Analysis.2.generic.language.RData")
