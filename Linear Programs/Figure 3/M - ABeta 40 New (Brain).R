########################################
### Figure 3M - ABeta 40 New (Brain) ###
########################################


### Load Libraries ###
library(cowplot)
library(emmeans)
library(ggplot2)
library(ggpubr)
library(ggResidpanel)
library(readxl)
library(rstatix)
library(zoo)



### Load and Clean Dataset ###
source("zFigure 3 - Clean Data.R")



### Analysis ###
Outcome <- "Abeta.40.New"

MF1 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Exposure + Genotype*Sex")
MF2 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Sex")
MF3 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Exposure")
MF4 <- paste(Outcome, "~ Genotype + Exposure + Sex")
MF5 <- paste(Outcome, "~ Genotype + Exposure + Genotype*Exposure")
MF6 <- paste(Outcome, "~ Genotype + Exposure")

model1.A1 <- lm(as.formula(MF1), data = fig.3.data.2[which(fig.3.data.2$Organ == "Brain"),])
model1.A2 <- lm(as.formula(MF2), data = fig.3.data.2[which(fig.3.data.2$Organ == "Brain"),])
model1.A3 <- lm(as.formula(MF3), data = fig.3.data.2[which(fig.3.data.2$Organ == "Brain"),])
model1.A4 <- lm(as.formula(MF4), data = fig.3.data.2[which(fig.3.data.2$Organ == "Brain"),])
model1.A5 <- lm(as.formula(MF5), data = fig.3.data.2[which(fig.3.data.2$Organ == "Brain"),])
model1.A6 <- lm(as.formula(MF6), data = fig.3.data.2[which(fig.3.data.2$Organ == "Brain"),])

F1.Table <- data.frame(Predictors = c("Intercept", "Genotype", "Exposure", "Sex", "Genotype:Exposure", "Genotype:Sex"),
                       Model1 = rep(NA,6),
                       Model2 = rep(NA,6),
                       Model3 = rep(NA,6),
                       Model4 = rep(NA,6),
                       Model5 = c(ifelse(round(coef(summary(model1.A5))[1:3,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A5))[1:3,4], 4)),
                                  NA,
                                  ifelse(round(coef(summary(model1.A5))[4,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A5))[4,4], 4)),
                                  NA),
                       Model6 = c(ifelse(round(coef(summary(model1.A6))[1:3,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A6))[1:3,4], 4)),
                                  rep(NA,3)))
rownames(F1.Table) <- NULL



# Create Diagnostic Graph for Model 1A
Figure3A.RP <- resid_panel(model1.A5)



# Test Pairwise Comparisons (Adjusted for Multiple Comparisons)
Est.Marg.Means <- emmeans(model1.A5, ~ Genotype + Exposure)
Pairwise.Comp <- pairs(Est.Marg.Means)
summary(Pairwise.Comp)[c(2,5),]



# Test Sensitivity of Outliers
small.pmb <- fig.3.data.2[which(fig.3.data.2$Organ == "Brain"),which(colnames(fig.3.data.2) %in% c("Genotype", "Sex", "Exposure", Outcome))]
small.pmb.mean <- mean(as.matrix(small.pmb[,Outcome]), na.rm = TRUE)
small.pmb.sd <- sd(as.matrix(small.pmb[,Outcome]), na.rm = TRUE)
small.pmb$KR <- ifelse(as.matrix(small.pmb[,Outcome]) < (small.pmb.mean - 2.5*small.pmb.sd) | as.matrix(small.pmb[,Outcome]) > (small.pmb.mean + 2.5*small.pmb.sd), "Remove", "Keep")
table(small.pmb$KR)

# Create Graph(s)

# Group Sex
AD.FA.Max <- max(as.matrix(fig.3.data.2[,Outcome])[which(fig.3.data.2$Exposure == "FA" & fig.3.data.2$Genotype == "AD" & fig.3.data.2$Organ == "Brain")], na.rm = TRUE)
AD.PM.Max <- max(as.matrix(fig.3.data.2[,Outcome])[which(fig.3.data.2$Exposure == "PM" & fig.3.data.2$Genotype == "AD" & fig.3.data.2$Organ == "Brain")], na.rm = TRUE)
WT.FA.Max <- max(as.matrix(fig.3.data.2[,Outcome])[which(fig.3.data.2$Exposure == "FA" & fig.3.data.2$Genotype == "WT" & fig.3.data.2$Organ == "Brain")], na.rm = TRUE)
WT.PM.Max <- max(as.matrix(fig.3.data.2[,Outcome])[which(fig.3.data.2$Exposure == "PM" & fig.3.data.2$Genotype == "WT" & fig.3.data.2$Organ == "Brain")], na.rm = TRUE)

seg.max.1 <- 0.002
seg.max.2 <- 0.003
text.above.1 <- 0.00202
text.corner.1 <- 0.0024
text.corner.2 <- 0.005
ylab.statement <- expression(paste("A", beta, "-40 Integrated Density (signal : total protein)", sep = ""))
yaxis.min <- 0
yaxis.max <- 1
yaxis.breaks <- 0.005

Figure3A.11 <- ggplot(data = fig.3.data.2[which(fig.3.data.2$Organ == "Brain"),], aes(x = Genotype, y = !!sym(Outcome), fill = Exposure)) +
  stat_boxplot(position = position_dodge(width = 0.7), geom = "errorbar", width = 0.2, size = 3) + 
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.5, size = 3, outlier.shape = NA, show.legend = FALSE) + 
  geom_point(aes(fill = factor(Exposure, labels = c("FA1", "PM1"))), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.7), color = "black", shape = 21, size = 6, show.legend = FALSE) +
  scale_y_continuous(breaks = seq(yaxis.min, yaxis.max, yaxis.breaks)) +
  scale_fill_manual(name = "Exposure", breaks = c("FA", "PM"), values = c("FA" = "indianred4", "PM" = "dodgerblue4",
                                                                          "FA1" = "indianred", "PM1" = "dodgerblue")) + 
  theme_bw() + theme(panel.spacing = unit(0.8, "lines")) +
  theme(plot.title = element_text(size = rel(1.2))) + theme(strip.text.x = element_text()) + 
  xlab("Genotype") + ylab(ylab.statement) + 
  theme(axis.text = element_text(size = 30, face = "bold")) +
  theme(axis.title = element_text(size = 18, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(legend.text = element_text(size = 30, face = "bold")) +
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(size = 31, face = "bold")) +
  geom_segment(aes(x = 0.825, xend = 1.175, y = max(as.matrix(fig.3.data.2[which(fig.3.data.2$Organ == "Brain"),Outcome]), na.rm = TRUE) + seg.max.1, yend = max(as.matrix(fig.3.data.2[which(fig.3.data.2$Organ == "Brain"),Outcome]), na.rm = TRUE) + seg.max.1), 
               color = "black", size = 3, lineend = "round") +
  geom_text(aes(x = 1, y = max(as.matrix(fig.3.data.2[which(fig.3.data.2$Organ == "Brain"),Outcome]), na.rm = TRUE) + text.above.1, label = "**"), color = "black", size = 8, vjust = 0) +
  geom_text(aes(x = 1, y = 0.02, label = ""))



# Create Final Model
final.model.results <- cbind(c("Intercept", "Genotype", "Exposure", "Genotype:Exposure"),
                             round(summary(model1.A5)$coef[,1:3], 3),
                             round(confint(model1.A5), 3),
                             ifelse(summary(model1.A5)$coef[,4] < 0.0001, "< 0.0001", round(summary(model1.A5)$coef[,4], 3)))
rownames(final.model.results) <- NULL

