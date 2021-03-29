## Project Name: TransOver
## Analysis 3: Children's game modification

## Written by: Hanna Schleihauf
## Date: 22 March 2021

## load necessary packages and functions
## load necessary packages and functions
source("./functions/diagnostic_fcns.r")
source("./functions/glmm_stability.r")
source("./functions/drop1_para.r")
library("MuMIn")
library("effects")
library("tidyverse")
library("readxl")
library("car")

xdata <-
  read_xls(path = "./data/R-Transover.xls", col_names = T, na = "NA")
str(xdata)

## making sure that factors are factors
xdata$Fidelity.type <- as.factor(xdata$Fidelity.type)
xdata$Sex <- as.factor(xdata$Sex)
xdata$Study.population <- as.factor(xdata$Study.population)
xdata$Game <- as.factor(xdata$Game)
xdata$Study.phase <- as.factor(xdata$Study.phase)

## look at response and predictor
hist(xdata$Innovation)
range(xdata$Innovation)
table(xdata$Sex)
table(xdata$Study.population)
table(xdata$Game)
table(xdata$Study.phase)

## creating a data frame with dummy variables
xx.fe.re <- fe.re.tab(
  fe.model = "Innovation ~ Study.population + Fidelity.type + Study.phase + Sex + Game",
  re = "(1|id)", data = xdata
)
xx.fe.re$summary # I am able to include the random slopes of Study.phase and Fidelity.type, because I have two observations per level.
t.data <- xx.fe.re$data
str(t.data)

# center dummy variables included in random slopes
t.data$Study.phase.code <- as.numeric(t.data$Study.phase.second) - mean(as.numeric(t.data$Study.phase.second))
t.data$Fidelity.type.code <- as.numeric(t.data$Fidelity.type.transmission) - mean(as.numeric(t.data$Fidelity.type.transmission))

## create second variable for response matrix
t.data$No.Innovation <- 3 - t.data$Innovation
resp.mat <- cbind(t.data$Innovation, t.data$No.Innovation)

## Pre-analysis: Does game type affect the children's game modification?
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
Game <- glmer(resp.mat ~ Game +
  (1 + Study.phase.code + Fidelity.type.code | id),
data = t.data, control = contr, family = binomial
)

Game.null <- glmer(resp.mat ~ 1 +
  (1 + Study.phase.code + Fidelity.type.code | id),
data = t.data, control = contr, family = binomial
)

tests <- drop1p(model.res = Game, para = F, data = t.data, contr = contr, n.cores = c("all-1", "all"), to.del = NULL)
round(tests$drop1.res, 3)
r.squaredLR(Game, null = Game.null)
## Game type does not have an influence on children's game modification.

## Main-analysis: Do study population and study type affect the children's game modification?
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
full <- glmer(resp.mat ~ (Study.population + Study.phase + Fidelity.type)^3 + Sex +
  (1 + Study.phase.code + Fidelity.type.code | id),
data = t.data, control = contr, family = binomial
)
logLik(full)

# checking assumptions
# check distribution of random effects
summary(full)$varcor
ranef.diagn.plot(full)
# check absence of overdispersion
overdisp.test(full)
# check absence of colliniarity
xx <- lm(Innovation ~
Study.population + Fidelity.type + Study.phase + Sex,
data = t.data
)
vif(xx)
# check model stability
m.stab.b <- glmm.model.stab(model.res = full, contr = contr, use = c("id"))
m.stab.b$detailed$warnings
xx <- as.data.frame(round(m.stab.b$summary[, -1], 3))
m.stab.plot(round(m.stab.b$summary[, -1], 3))
write.table(xx, "fidelity.model.stability.txt", quote = FALSE, sep = "\t")

# look at the fixed effects
xx <- round(summary(full)$coefficients, 3)
write.table(xx, "game.modification.model.coefficients.txt", quote = FALSE, sep = "\t")

# null model
null <- glmer(resp.mat ~ Sex +
  (1 + Study.phase.code + Fidelity.type.code | id),
data = t.data, control = contr, family = binomial
)
round(anova(full, null, test = "Chisq"), 3)
r.squaredLR(full, null = null) # effect size for full-null model comparison

# reduced model comparison
tests <- drop1p(model.res = full, para = F, data = NULL, contr = contr, n.cores = c("all-1", "all"), to.del = NULL)
round(tests$drop1.res, 3)

# without three-way interaction
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
full2 <- glmer(resp.mat ~ (Study.population + Study.phase + Fidelity.type)^2 + Sex +
  (1 + Study.phase.code + Fidelity.type.code | id),
data = t.data, control = contr, family = binomial
)
tests <- drop1p(model.res = full2, para = F, data = NULL, contr = contr, n.cores = c("all-1", "all"), to.del = NULL)
round(tests$drop1.res, 3)

round(r.squaredLR(full, null = full2), 3) # effect size for three-way interaction

# fit reduced models to be able to calculate effect sizes
full.w.Study.population.Gen <- glmer(resp.mat ~ Study.population + Study.phase + Fidelity.type +
  Study.population:Fidelity.type +
  Study.phase:Fidelity.type +
  Sex +
  (1 + Study.phase.code + Fidelity.type.code | id),
data = t.data, control = contr, family = binomial
)
r.squaredLR(full2, null = full.w.Study.population.Gen) # effect size for Study.population:Fidelity type

full.w.Study.population.Fidelity.type <- glmer(resp.mat ~ Study.population + Study.phase + Fidelity.type +
  Study.population:Study.phase +
  Study.phase:Fidelity.type +
  Sex +
  (1 + Study.phase.code + Fidelity.type.code | id),
data = t.data, control = contr, family = binomial
)
r.squaredLR(full2, null = full.w.Study.population.Fidelity.type) # effect size for Study.population:Fidelity type

full.w.Gen.Fidelity.type <- glmer(resp.mat ~ Study.population + Study.phase + Fidelity.type +
  Study.population:Study.phase +
  Study.population:Fidelity.type +
  Sex +
  (1 + Study.phase.code + Fidelity.type.code | id),
data = t.data, control = contr, family = binomial
)
r.squaredLR(full2, null = full.w.Gen.Fidelity.type) # effect size for Generation:Fidelity type


# without the non-significant two-way interactions
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
full3 <- glmer(resp.mat ~ Study.population * Study.phase + Fidelity.type + Sex +
  (1 + Study.phase.code + Fidelity.type.code | id),
data = t.data, control = contr, family = binomial
)
tests <- drop1p(model.res = full3, para = F, data = NULL, contr = contr, n.cores = c("all-1", "all"), to.del = NULL)
round(tests$drop1.res, 3)

## Post-hoc analysis of both study phases separately
## Study Phase 1
t.data.phase.1 <- subset(t.data, t.data$Study.phase == "first")
resp.mat.1 <- cbind(t.data.phase.1$Innovation, t.data.phase.1$No.Innovation)

full.phase.1 <- glmer(resp.mat.1 ~ (Study.population) +
  (1 | id),
data = t.data.phase.1, control = contr, family = binomial
)

null.phase.1 <- glmer(resp.mat.1 ~ 1 +
  (1 | id),
data = t.data.phase.1, control = contr, family = binomial
)
round(anova(full.phase.1, null.phase.1, test = "Chisq"), 3)
r.squaredLR(full.phase.1, null = null.phase.1) # effect size for full-null model comparison
round(summary(full.phase.1)$coefficients, 3)

# reduced model comparison
tests <- drop1p(model.res = full.phase.1.red, para = F, data = NULL, contr = contr, n.cores = c("all-1", "all"), to.del = NULL)
round(tests$drop1.res, 3)

## Study Phase 2
t.data.phase.2 <- subset(t.data, t.data$Study.phase == "second")
resp.mat.2 <- cbind(t.data.phase.2$Innovation, t.data.phase.2$No.Innovation)

full.phase.2 <- glmer(resp.mat.2 ~ (Study.population) +
  (1 | id),
data = t.data.phase.2, control = contr, family = binomial
)

null.phase.2 <- glmer(resp.mat.2 ~ 1 +
  (1 | id),
data = t.data.phase.2, control = contr, family = binomial
)
round(anova(full.phase.2, null.phase.2, test = "Chisq"), 3)
r.squaredLR(full.phase.2, null = null.phase.2) # effect size for full-null model comparison
round(summary(full.phase.2)$coefficients, 3)


save.image("./Analysis.3.game.modification/Analysis.3.game.modification.RData")
