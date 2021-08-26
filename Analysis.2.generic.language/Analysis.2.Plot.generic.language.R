## Project Name: TransOver
## Plot 2: Children's use of generic language

## Written by: Hanna Schleihauf
## Date: 22 March 2021

##set your working directory using setwd 

load("./Analysis.2.generic.language/Analysis.2.generic.language.RData")
source("./functions/boot_glmmTMB.r")

## Plot for number of normative and non-normative words
word.data <- data.frame(
  t.data$id, t.data$Study.population, t.data$Study.phase,
  t.data$number.of.words, t.data$number.of.words.in.generic.sentences
)
colnames(word.data) <- c(
  "id", "Study.population",
  "Study.phase", "number.of.words",
  "number.of.words.in.generic.sentences"
)
word.data$id <- paste(word.data$id, word.data$Study.population, sep = ".")
word.data$difference <-
  word.data$number.of.words - word.data$number.of.words.in.generic.sentences
word.data <-
  word.data[order(-word.data$number.of.words, -word.data$number.of.words.in.generic.sentences), ]

word.data <- word.data %>%
  group_by(Study.phase, Study.population) %>% # As a precaution / handle in a separate .grouped_df method
  arrange(-number.of.words) %>% # arrange by facet variables and continuous values
  mutate(.r = row_number())

word.data <-
  word.data %>%
  pivot_longer(!c(id, .r, Study.population, Study.phase), names_to = "category", values_to = "count")
word.data <-
  subset(word.data, word.data$category != "number.of.words")

library(ggplot2)
# New facet label names for dose variable
Study.population.labs <- c("non-OIs", "OIs")
names(Study.population.labs) <- c("non-OI", "OI")
Gen.labs <- c("Phase 1", "Phase 2")
names(Gen.labs) <- c("first", "second")

p <- ggplot(word.data, aes(fill = category, y = count, x = .r)) +
  geom_bar(position = "stack", stat = "identity", width = 0.9)

p <- p + facet_wrap(Study.phase ~ Study.population,
  labeller = labeller(Study.phase = Gen.labs, Study.population = Study.population.labs)
)

p <- p + xlab("participant id")
p <- p + ylab("word count")
p <- p + scale_fill_manual(
  labels =
    c(
      "number of words\nin non-normative\nutterances",
      "number of words\nin normative\nutterances"
    ),
  values = c("#FFB90F", "#CD6600")
)

p <- p + theme(
  legend.title = element_blank(),
  legend.key = element_rect(size = 6),
  legend.key.size = unit(1, "cm"),
  legend.key.height = unit(4, "line"),
  legend.key.width = unit(1, "line")
)
p <- p + theme_test()
p

#### Plots for the Generic Language Score model, reported in the paper so far
contr <-
  glmmTMBControl(
    optCtrl = list(iter.max = 200000, eval.max = 200000),
    profile = FALSE, collect = FALSE
  )

# I need to center the predictor box fo the model and then model it again
t.data$z.Sex <- as.numeric(t.data$Sex) - mean(as.numeric(t.data$Sex))
plot.model <- glmmTMB(Generic.Language.Score ~ Study.population + Study.phase + z.Sex +
  (1 | id),
data = t.data, control = contr,
family = nbinom1(link = "log")
)

boot.plot.model <- boot.glmmTMB(model.res=plot.model, data=t.data, excl.non.conv=T,
              nboots=1000, para=T, level=0.95, use=c("z.trial"),
              contr=contr, circ.var.name=NULL, circ.var=NULL, n.cores=c("Study.population", "Study.phase"),
             save.path=NULL, load.lib=T, lib.loc=.libPaths(), set.all.effects.2.zero=F)
boot.plot.model$ci.fitted
# library(emmeans)
# emm = emmeans(GL.score.red,  ~ Study.population + Study.phase)
# xx = summary(emm, type = "response")

# define the space around the margins of each of the plots per subject
par(mar = c(5, 3.5, 1, 0.5), mgp = c(1.8, 0.5, 0)) # set margins etc.

t.data$color.id <- paste(t.data$Study.population, sep = "_")
t.data$color.id <- as.factor(t.data$color.id)
levels(t.data$color.id)
t.data$color.id <- factor(t.data$color.id, levels(t.data$color.id)[c(2, 1)])

t.data$xcats <- paste(t.data$Study.phase, t.data$Study.population, sep = "_")
t.data$xcats <- as.factor(t.data$xcats)
t.data$xcats <- factor(t.data$xcats, levels(t.data$xcats)[c(2, 1, 4, 3)])
levels(t.data$xcats)
t.data$xcats <- as.numeric(t.data$xcats)
t.data$xcats[t.data$xcats == 1] <- 1
t.data$xcats[t.data$xcats == 2] <- 1.4
t.data$xcats[t.data$xcats == 3] <- 2.4
t.data$xcats[t.data$xcats == 4] <- 2.8

range(t.data$Generic.Language.Score)

# empty plot
plot(
  x = 1, y = 1, type = "n", xlim = c(0.5, 3.5), ylim = c(-1.2, 11.2),
  bty = "l", tcl = -0.25,
  xaxs = "i", xaxt = "n", # yaxs="i", yaxt="n",
  las = 1,
  xlab = "", ylab = "", cex.axis = 0.7
)
box(lty = "solid", col = "grey")

# calculate jitter x values
t.data$xvals <- t.data$xcats + runif(n = nrow(t.data), min = -0.17, max = 0.17)
# calculate jitter y values
t.data$yvals <- t.data$Generic.Language.Score + runif(n = nrow(t.data), min = -0.45, max = 0.45)

mycolors <- c("red4", "blue4")[as.numeric(t.data$color.id)]
mycolors.t <- adjustcolor(mycolors, alpha.f = 0.3)
myshape <- c(16, 16) # , 15, 15)

points(t.data$xvals, t.data$yvals, col = mycolors.t, pch = 16)

## fitted values and confidence intervals
# I need to center the predictor box fo the model and then model it again
t.data$z.Sex <- as.numeric(t.data$Sex) - mean(as.numeric(t.data$Sex))

## add model line


pred.yvals <- boot.plot.model$ci.fitted$fitted[boot.plot.model$ci.fitted$Study.population == "OI" &
  boot.plot.model$ci.fitted$Study.phase == "first"]
lower.cl <- boot.plot.model$ci.fitted$lower.cl[boot.plot.model$ci.fitted$Study.population == "OI" &
  boot.plot.model$ci.fitted$Study.phase == "first"]
upper.cl <- boot.plot.model$ci.fitted$upper.cl[boot.plot.model$ci.fitted$Study.population == "OI" &
  boot.plot.model$ci.fitted$Study.phase == "first"]
segments(
  x0 = 0.8, x1 = 1.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 2.5, col = "red4"
)
arrows(
  x0 = 1, y0 = lower.cl, x1 = 1, y1 = upper.cl, length = 0.1, angle = 90,
  code = 3, col = "red4", lty = 1, lwd = 1.5
)

pred.yvals <- boot.plot.model$ci.fitted$fitted[boot.plot.model$ci.fitted$Study.population == "non-OI" &
  boot.plot.model$ci.fitted$Study.phase == "first"]
lower.cl <- boot.plot.model$ci.fitted$lower.cl[boot.plot.model$ci.fitted$Study.population == "non-OI" &
  boot.plot.model$ci.fitted$Study.phase == "first"]
upper.cl <- boot.plot.model$ci.fitted$upper.cl[boot.plot.model$ci.fitted$Study.population == "non-OI" &
  boot.plot.model$ci.fitted$Study.phase == "first"]
segments(
  x0 = 1.2, x1 = 1.6,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 2.5, col = "blue4"
)
arrows(
  x0 = 1.4, y0 = lower.cl, x1 = 1.4, y1 = upper.cl, length = 0.1, angle = 90,
  code = 3, col = "blue4", lty = 1, lwd = 1.5
)

pred.yvals <- boot.plot.model$ci.fitted$fitted[boot.plot.model$ci.fitted$Study.population == "OI" &
  boot.plot.model$ci.fitted$Study.phase == "second"]
lower.cl <- boot.plot.model$ci.fitted$lower.cl[boot.plot.model$ci.fitted$Study.population == "OI" &
  boot.plot.model$ci.fitted$Study.phase == "second"]
upper.cl <- boot.plot.model$ci.fitted$upper.cl[boot.plot.model$ci.fitted$Study.population == "OI" &
  boot.plot.model$ci.fitted$Study.phase == "second"]
segments(
  x0 = 2.2, x1 = 2.6,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 2.5, col = "red4"
)
arrows(
  x0 = 2.4, y0 = lower.cl, x1 = 2.4, y1 = upper.cl, length = 0.1, angle = 90,
  code = 3, col = "red4", lty = 1, lwd = 1.5
)

pred.yvals <- boot.plot.model$ci.fitted$fitted[boot.plot.model$ci.fitted$Study.population == "non-OI" &
  boot.plot.model$ci.fitted$Study.phase == "second"]
lower.cl <- boot.plot.model$ci.fitted$lower.cl[boot.plot.model$ci.fitted$Study.population == "non-OI" &
  boot.plot.model$ci.fitted$Study.phase == "second"]
upper.cl <- boot.plot.model$ci.fitted$upper.cl[boot.plot.model$ci.fitted$Study.population == "non-OI" &
  boot.plot.model$ci.fitted$Study.phase == "second"]
segments(
  x0 = 2.6, x1 = 3.0,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 2.5, col = "blue4"
)
arrows(
  x0 = 2.8, y0 = lower.cl, x1 = 2.8, y1 = upper.cl, length = 0.1, angle = 90,
  code = 3, col = "blue4", lty = 1, lwd = 1.5
)

# add labels at x-axis
axis(1, at = c(1, 1.4, 2.4, 2.8), NULL, labels = c("", "", "", ""), tick = TRUE, las = 1, tck = -0.01, col = "grey")
mtext(text = c("OIs", "non-OIs"), side = 1, line = 0.5, at = c(1, 1.4, 2.4, 2.8), cex = 0.7)
mtext(text = c("Study Phase 1", "Study Phase 2"), side = 1, line = 2, at = c(1.2, 2.6), cex = 0.7)

mtext(text = "Normative Language Score", side = 2, line = 2.3, at = 5, cex = 0.7)

save.image("./Analysis.2.generic.language/Analysis.2.Plot.generic.language.RData")
