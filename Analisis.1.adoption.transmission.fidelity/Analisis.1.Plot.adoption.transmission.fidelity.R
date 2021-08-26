## Project Name: TransOver
## Plot 1: Children's adoption and transmission fidelity

## Written by: Hanna Schleihauf
## Date: 22 March 2021

##set your working directory using setwd 

load("./Analisis.1.adoption.transmission.fidelity/Analisis.1.adoption.transmission.fidelity.RData")

library("lme4")
source("./functions/diagnostic_fcns.r")
source("./functions/boot_glmm.r")

# I need to center the predictor box fo the model and then model it again
t.data$z.Sex <- as.numeric(t.data$Sex) - mean(as.numeric(t.data$Sex))

t.data$NoFidelity <- 11 - t.data$Fidelity
resp.mat <- cbind(t.data$Fidelity, t.data$NoFidelity)

contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
plot.model <- glmer(resp.mat ~ Study.population + Study.phase + Fidelity.type + z.Sex +
  (1 + Study.phase.code + Fidelity.type.code | id),
data = t.data, control = contr, family = binomial
)

# boot.res.plot=boot.glmm.pred(model.res=plot.model, excl.warnings=T,
#                            nboots=1000, para=F, level=0.95,
#                           use=c("Study.population", "Study.phase", "Fidelity.type"))
round(boot.res.plot$ci.estimates, 3)
boot.res.plot$ci.predicted

xx <- summary(plot.model)$coefficients
xx <- xx[, 1]
xx

# define the space around the margins of each of the plots per subject
par(mar = c(5, 3.5, 1, 0.5), mgp = c(1.8, 0.5, 0)) # set margins etc.

t.data$color.id <- paste(t.data$Fidelity.type, t.data$Study.population, sep = "_")
t.data$color.id <- as.factor(t.data$color.id)
levels(t.data$color.id)

t.data$xcats <- paste(t.data$Study.phase, t.data$Fidelity.type, t.data$Study.population, sep = "_")
t.data$xcats <- as.factor(t.data$xcats)
levels(t.data$xcats)
t.data$xcats <- as.numeric(t.data$xcats)
t.data$xcats[t.data$xcats == 1] <- 1
t.data$xcats[t.data$xcats == 2] <- 1.4
t.data$xcats[t.data$xcats == 3] <- 2
t.data$xcats[t.data$xcats == 4] <- 2.4
t.data$xcats[t.data$xcats == 5] <- 3.5
t.data$xcats[t.data$xcats == 6] <- 3.9
t.data$xcats[t.data$xcats == 7] <- 4.5
t.data$xcats[t.data$xcats == 8] <- 4.9

# wahrscheinlichkeit pro person ein fidelity point zu bekommen
t.data$prob <- t.data$Fidelity / (t.data$Fidelity + t.data$NoFidelity)

range(t.data$prob)

# empty plot
plot(
  x = 1, y = 1, type = "n", 
  xlim = c(0.7, 5.3), ylim = c(-0.05, 1.05),
  bty = "l", tcl = -0.25,
  xaxs = "i", xaxt = "n",
  las = 1,
  xlab = "", ylab = "", 
  cex.axis = 0.7
)
box(lty = "solid", col = "grey")

# calculate jitter x values
t.data$xvals <- t.data$xcats + runif(n = nrow(t.data), min = -0.14, max = 0.14)
# calculate jitter y values
t.data$yvals <- t.data$prob + runif(n = nrow(t.data), min = -0.015, max = 0.015)

mycolors <- c("red", "blue", "red4", "blue4")[as.numeric(t.data$color.id)]
mycolors.t <- adjustcolor(mycolors, alpha.f = 0.3)
myshape <- c(16, 16, 15, 15)

points(t.data$xvals, t.data$yvals, col = mycolors.t, pch = 16)

## add model line
# OI - Adoption - Study.phase1
pred.yvals <- boot.res.plot$ci.predicted$fitted[boot.res.plot$ci.predicted$Study.population == "OI" &
  boot.res.plot$ci.predicted$Study.phase == "first" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
lower.cl <- boot.res.plot$ci.predicted$lower.cl[boot.res.plot$ci.predicted$Study.population == "OI" &
  boot.res.plot$ci.predicted$Study.phase == "first" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
upper.cl <- boot.res.plot$ci.predicted$upper.cl[boot.res.plot$ci.predicted$Study.population == "OI" &
  boot.res.plot$ci.predicted$Study.phase == "first" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
segments(
  x0 = 0.8, x1 = 1.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "red"
)
arrows(
  x0 = 1, y0 = lower.cl, x1 = 1, y1 = upper.cl, length = 0.1, angle = 90,
  code = 3, col = "red", lty = 1, lwd = 1.5
)

# No-OI - Adoption - Study.phase1
pred.yvals <- boot.res.plot$ci.predicted$fitted[boot.res.plot$ci.predicted$Study.population == "non-OI" &
  boot.res.plot$ci.predicted$Study.phase == "first" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
lower.cl <- boot.res.plot$ci.predicted$lower.cl[boot.res.plot$ci.predicted$Study.population == "non-OI" &
  boot.res.plot$ci.predicted$Study.phase == "first" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
upper.cl <- boot.res.plot$ci.predicted$upper.cl[boot.res.plot$ci.predicted$Study.population == "non-OI" &
  boot.res.plot$ci.predicted$Study.phase == "first" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
segments(
  x0 = 1.2, x1 = 1.6,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "blue"
)
arrows(
  x0 = 1.4, y0 = lower.cl, x1 = 1.4, y1 = upper.cl, length = 0.1, angle = 90,
  code = 3, col = "blue", lty = 1, lwd = 1.5
)

# OI - Transmission - Study.phase1
pred.yvals <- boot.res.plot$ci.predicted$fitted[boot.res.plot$ci.predicted$Study.population == "OI" &
  boot.res.plot$ci.predicted$Study.phase == "first" &
  boot.res.plot$ci.predicted$Fidelity.type == "transmission"]
lower.cl <- boot.res.plot$ci.predicted$lower.cl[boot.res.plot$ci.predicted$Study.population == "OI" &
  boot.res.plot$ci.predicted$Study.phase == "first" &
  boot.res.plot$ci.predicted$Fidelity.type == "transmission"]
upper.cl <- boot.res.plot$ci.predicted$upper.cl[boot.res.plot$ci.predicted$Study.population == "OI" &
  boot.res.plot$ci.predicted$Study.phase == "first" &
  boot.res.plot$ci.predicted$Fidelity.type == "transmission"]
segments(
  x0 = 1.8, x1 = 2.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "red4"
)
arrows(
  x0 = 2, y0 = lower.cl, x1 = 2, y1 = upper.cl, length = 0.1, angle = 90,
  code = 3, col = "red4", lty = 1, lwd = 1.5
)

# No-OI - Transmission - Study.phase 1
pred.yvals <- boot.res.plot$ci.predicted$fitted[boot.res.plot$ci.predicted$Study.population == "non-OI" &
  boot.res.plot$ci.predicted$Study.phase == "first" &
  boot.res.plot$ci.predicted$Fidelity.type == "transmission"]
lower.cl <- boot.res.plot$ci.predicted$lower.cl[boot.res.plot$ci.predicted$Study.population == "non-OI" &
  boot.res.plot$ci.predicted$Study.phase == "first" &
  boot.res.plot$ci.predicted$Fidelity.type == "transmission"]
upper.cl <- boot.res.plot$ci.predicted$upper.cl[boot.res.plot$ci.predicted$Study.population == "non-OI" &
  boot.res.plot$ci.predicted$Study.phase == "first" &
  boot.res.plot$ci.predicted$Fidelity.type == "transmission"]
segments(
  x0 = 2.2, x1 = 2.6,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "blue4"
)
arrows(
  x0 = 2.4, y0 = lower.cl, x1 = 2.4, y1 = upper.cl, length = 0.1, angle = 90,
  code = 3, col = "blue4", lty = 1, lwd = 1.5
)

# OI - Adoption - Study.phase2
pred.yvals <- boot.res.plot$ci.predicted$fitted[boot.res.plot$ci.predicted$Study.population == "OI" &
  boot.res.plot$ci.predicted$Study.phase == "second" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
lower.cl <- boot.res.plot$ci.predicted$lower.cl[boot.res.plot$ci.predicted$Study.population == "OI" &
  boot.res.plot$ci.predicted$Study.phase == "second" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
upper.cl <- boot.res.plot$ci.predicted$upper.cl[boot.res.plot$ci.predicted$Study.population == "OI" &
  boot.res.plot$ci.predicted$Study.phase == "second" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
segments(
  x0 = 3.3, x1 = 3.7,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "red"
)
arrows(
  x0 = 3.5, y0 = lower.cl, x1 = 3.5, y1 = upper.cl, length = 0.1, angle = 90,
  code = 3, col = "red", lty = 1, lwd = 1.5
)

# No-OI - Adoption - Study.phase2
pred.yvals <- boot.res.plot$ci.predicted$fitted[boot.res.plot$ci.predicted$Study.population == "non-OI" &
  boot.res.plot$ci.predicted$Study.phase == "second" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
lower.cl <- boot.res.plot$ci.predicted$lower.cl[boot.res.plot$ci.predicted$Study.population == "non-OI" &
  boot.res.plot$ci.predicted$Study.phase == "second" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
upper.cl <- boot.res.plot$ci.predicted$upper.cl[boot.res.plot$ci.predicted$Study.population == "non-OI" &
  boot.res.plot$ci.predicted$Study.phase == "second" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
segments(
  x0 = 3.7, x1 = 4.1,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "blue"
)
arrows(
  x0 = 3.9, y0 = lower.cl, x1 = 3.9, y1 = upper.cl, length = 0.1, angle = 90,
  code = 3, col = "blue", lty = 1, lwd = 1.5
)

# OI - Transmission - Study.phase2
pred.yvals <- boot.res.plot$ci.predicted$fitted[boot.res.plot$ci.predicted$Study.population == "OI" &
  boot.res.plot$ci.predicted$Study.phase == "second" &
  boot.res.plot$ci.predicted$Fidelity.type == "transmission"]
lower.cl <- boot.res.plot$ci.predicted$lower.cl[boot.res.plot$ci.predicted$Study.population == "OI" &
  boot.res.plot$ci.predicted$Study.phase == "second" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
upper.cl <- boot.res.plot$ci.predicted$upper.cl[boot.res.plot$ci.predicted$Study.population == "OI" &
  boot.res.plot$ci.predicted$Study.phase == "second" &
  boot.res.plot$ci.predicted$Fidelity.type == "transmission"]
segments(
  x0 = 4.3, x1 = 4.7,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "red4"
)
arrows(
  x0 = 4.5, y0 = lower.cl, x1 = 4.5, y1 = upper.cl, length = 0.1, angle = 90,
  code = 3, col = "red4", lty = 1, lwd = 1.5
)

# No-OI - Transmission - Study.phase2
pred.yvals <- boot.res.plot$ci.predicted$fitted[boot.res.plot$ci.predicted$Study.population == "non-OI" &
  boot.res.plot$ci.predicted$Study.phase == "second" &
  boot.res.plot$ci.predicted$Fidelity.type == "transmission"]
lower.cl <- boot.res.plot$ci.predicted$lower.cl[boot.res.plot$ci.predicted$Study.population == "non-OI" &
  boot.res.plot$ci.predicted$Study.phase == "second" &
  boot.res.plot$ci.predicted$Fidelity.type == "adoption"]
upper.cl <- boot.res.plot$ci.predicted$upper.cl[boot.res.plot$ci.predicted$Study.population == "non-OI" &
  boot.res.plot$ci.predicted$Study.phase == "second" &
  boot.res.plot$ci.predicted$Fidelity.type == "transmission"]
segments(
  x0 = 4.7, x1 = 5.1,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "blue4"
)
arrows(
  x0 = 4.9, y0 = lower.cl, x1 = 4.9, y1 = upper.cl, length = 0.1, angle = 90,
  code = 3, col = "blue4", lty = 1, lwd = 1.5
)

# t.data$xcats[t.data$xcats == 1] = 1
# t.data$xcats[t.data$xcats == 2] = 1.4
# t.data$xcats[t.data$xcats == 3] = 2
# t.data$xcats[t.data$xcats == 4] = 2.4
# t.data$xcats[t.data$xcats == 5] = 3.5
# t.data$xcats[t.data$xcats == 6] = 3.9
# t.data$xcats[t.data$xcats == 7] = 4.5
# t.data$xcats[t.data$xcats == 8] = 4.9

# add labels at x-axis
axis(1,
  at = c(1, 1.4, 2, 2.4, 3.5, 3.9, 4.5, 4.9), NULL,
  labels = c("", "", "", "", "", "", "", ""), tick = TRUE, las = 1, tck = -0.01, col = "grey"
)
mtext(text = c("OIs", "non-OIs"), side = 1, line = 0.5, at = c(1, 1.4, 2, 2.4, 3.5, 3.9, 4.5, 4.9), cex = 0.7)
mtext(text = c("Game Adaption", "Game Transmission"), side = 1, line = 1.75, at = c(1.2, 2.2, 3.7, 4.7), cex = 0.7)
mtext(text = c("Study Phase 1", "Study Phase 2"), side = 1, line = 3, at = c(1.7, 4.2), cex = 0.7)

mtext(text = "Fidelity proportion", side = 2, line = 2.3, at = 0.5, cex = 0.7)

save.image("./Analisis.1.adoption.transmission.fidelity/Analisis.1.Plot.adoption.transmission.fidelity.RData")

