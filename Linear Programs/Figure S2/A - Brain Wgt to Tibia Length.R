#################################################
### Figure S2A - Brain Weight to Tibia Length ###
#################################################


### Load Libraries ###
library(emmeans)
library(ggplot2)
library(ggResidpanel)
library(readxl)
library(zoo)



### Load and Clean Dataset ###
source("zFigure S2 - Clean Data.R")



### Analysis ###
Outcome <- "Brain_Tibia"

MF1 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Exposure + Genotype*Sex")
MF2 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Sex")
MF3 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Exposure")
MF4 <- paste(Outcome, "~ Genotype + Exposure + Sex")
MF5 <- paste(Outcome, "~ Genotype + Exposure + Genotype*Exposure")
MF6 <- paste(Outcome, "~ Genotype + Exposure")

model1.A1 <- lm(as.formula(MF1), data = fig.s2.data.1)
model1.A2 <- lm(as.formula(MF2), data = fig.s2.data.1)
model1.A3 <- lm(as.formula(MF3), data = fig.s2.data.1)
model1.A4 <- lm(as.formula(MF4), data = fig.s2.data.1)
model1.A5 <- lm(as.formula(MF5), data = fig.s2.data.1)
model1.A6 <- lm(as.formula(MF6), data = fig.s2.data.1)

F1.Table <- data.frame(Predictors = c("Intercept", "Genotype", "Exposure", "Sex", "Genotype:Exposure", "Genotype:Sex"),
                       Model1 = c(ifelse(round(coef(summary(model1.A1))[1:6,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A1))[1:6,4], 4))),
                       Model2 = c(ifelse(round(coef(summary(model1.A2))[1:4,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A2))[1:4,4], 4)),
                                  NA,
                                  ifelse(round(coef(summary(model1.A2))[5,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A2))[5,4], 4))),
                       Model3 = c(ifelse(round(coef(summary(model1.A3))[1:5,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A3))[1:5,4], 4)),
                                  NA),
                       Model4 = c(ifelse(round(coef(summary(model1.A4))[1:4,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A4))[1:4,4], 4)),
                                  rep(NA,2)),
                       Model5 = c(ifelse(round(coef(summary(model1.A5))[1:3,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A5))[1:3,4], 4)),
                                  NA,
                                  ifelse(round(coef(summary(model1.A5))[4,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A5))[4,4], 4)),
                                  NA),
                       Model6 = c(ifelse(round(coef(summary(model1.A6))[1:3,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A6))[1:3,4], 4)),
                                  rep(NA,3)))
rownames(F1.Table) <- NULL



# Create Diagnostic Graph for Model 1A
Figure6A.RP <- resid_panel(model1.A6)



# Test Pairwise Comparisons (Adjusted for Multiple Comparisons)
Est.Marg.Means <- emmeans(model1.A6, ~ Genotype + Exposure)
Pairwise.Comp <- pairs(Est.Marg.Means)
summary(Pairwise.Comp)



# Test Sensitivity of Outliers
small.pmb <- fig.s2.data.1[,which(colnames(fig.s2.data.1) %in% c("Genotype", "Sex", "Exposure", Outcome))]
small.pmb.mean <- mean(as.matrix(small.pmb[,Outcome]), na.rm = TRUE)
small.pmb.sd <- sd(as.matrix(small.pmb[,Outcome]), na.rm = TRUE)
small.pmb$KR <- ifelse(as.matrix(small.pmb[,Outcome]) < (small.pmb.mean - 2.5*small.pmb.sd) | as.matrix(small.pmb[,Outcome]) > (small.pmb.mean + 2.5*small.pmb.sd), "Remove", "Keep")
table(small.pmb$KR)

small.pmb1 <- small.pmb[which(small.pmb$KR == "Keep"),]
model1.A1.S <- lm(as.formula(MF1), data = small.pmb1)
model1.A2.S <- lm(as.formula(MF2), data = small.pmb1)
model1.A3.S <- lm(as.formula(MF3), data = small.pmb1)
model1.A4.S <- lm(as.formula(MF4), data = small.pmb1)
model1.A5.S <- lm(as.formula(MF5), data = small.pmb1)
model1.A6.S <- lm(as.formula(MF6), data = small.pmb1)

F2.Table <- data.frame(Predictors = c("Intercept", "Genotype", "Exposure", "Sex", "Genotype:Exposure", "Genotype:Sex"),
                       Model1 = c(ifelse(round(coef(summary(model1.A1.S))[1:6,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A1.S))[1:6,4], 4))),
                       Model2 = c(ifelse(round(coef(summary(model1.A2.S))[1:4,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A2.S))[1:4,4], 4)),
                                  NA,
                                  ifelse(round(coef(summary(model1.A2.S))[5,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A2.S))[5,4], 4))),
                       Model3 = c(ifelse(round(coef(summary(model1.A3.S))[1:5,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A3.S))[1:5,4], 4)),
                                  NA),
                       Model4 = c(ifelse(round(coef(summary(model1.A4.S))[1:4,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A4.S))[1:4,4], 4)),
                                  rep(NA,2)),
                       Model5 = c(ifelse(round(coef(summary(model1.A5.S))[1:3,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A5.S))[1:3,4], 4)),
                                  NA,
                                  ifelse(round(coef(summary(model1.A5.S))[4,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A5.S))[4,4], 4)),
                                  NA),
                       Model6 = c(ifelse(round(coef(summary(model1.A6.S))[1:3,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A6.S))[1:3,4], 4)),
                                  rep(NA,3)))
rownames(F2.Table) <- NULL



# Create Graph(s)

# Group Sex
AD.FA.Max <- max(as.matrix(fig.s2.data.1[,Outcome])[which(fig.s2.data.1$Exposure == "FA" & fig.s2.data.1$Genotype == "AD")], na.rm = TRUE)
AD.PM.Max <- max(as.matrix(fig.s2.data.1[,Outcome])[which(fig.s2.data.1$Exposure == "PM" & fig.s2.data.1$Genotype == "AD")], na.rm = TRUE)
WT.FA.Max <- max(as.matrix(fig.s2.data.1[,Outcome])[which(fig.s2.data.1$Exposure == "FA" & fig.s2.data.1$Genotype == "WT")], na.rm = TRUE)
WT.PM.Max <- max(as.matrix(fig.s2.data.1[,Outcome])[which(fig.s2.data.1$Exposure == "PM" & fig.s2.data.1$Genotype == "WT")], na.rm = TRUE)

seg.max.1 <- 1.5
seg.max.2 <- 0.5
text.above.1 <- 1.6
text.corner.1 <- 2
text.corner.2 <- 1
ylab.statement <- "Brain Weight : Tibia Length (mg : mm)"
yaxis.min <- 0
yaxis.max <- 50
yaxis.breaks <- 2.5

FigureS2A.1 <- ggplot(data = fig.s2.data.1, aes(x = Genotype, y = !!sym(Outcome), fill = Exposure)) +
  stat_boxplot(position = position_dodge(width = 0.7), geom = "errorbar", width = 0.2, size = 0.8) + 
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.5, size = 0.8, outlier.shape = NA, show.legend = FALSE) + 
  geom_point(aes(fill = factor(Exposure, labels = c("FA1", "PM1"))), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.7), color = "black", shape = 21, size = 2, show.legend = FALSE) +
  scale_y_continuous(breaks = seq(yaxis.min, yaxis.max, yaxis.breaks)) +
  scale_fill_manual(name = "Exposure", breaks = c("FA", "PM"), values = c("FA" = "indianred4", "PM" = "dodgerblue4",
                                                                          "FA1" = "indianred", "PM1" = "dodgerblue")) + 
  theme_bw()  + theme(panel.spacing = unit(0.8, "lines")) +
  theme(plot.title = element_text(size = rel(1.2))) + theme(strip.text.x = element_text()) + 
  xlab("Genotype") + ylab(ylab.statement) + 
  theme(axis.text = element_text(size = 12, face = "bold")) +
  theme(axis.title = element_text(size = 13, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(size = 13, face = "bold")) +
  geom_text(aes(x = 2.13, y = 32.2, label = "* Genotype"), color = "black", size = 4, hjust = 0, vjust = 0) +
  geom_text(aes(x = 1.73, y = 32.5, label = ""))



# Create Final Model
final.model.results <- cbind(c("Intercept", "Genotype", "Exposure"),
                             round(summary(model1.A6)$coef[,1:3], 3),
                             round(confint(model1.A6), 3),
                             ifelse(summary(model1.A6)$coef[,4] < 0.0001, "< 0.0001", round(summary(model1.A6)$coef[,4], 3)))
rownames(final.model.results) <- NULL





