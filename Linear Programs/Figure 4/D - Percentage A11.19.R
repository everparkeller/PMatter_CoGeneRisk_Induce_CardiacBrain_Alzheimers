#####################################
### Figure 4D - Percentage A11-19 ###
#####################################


### Load Libraries ###
library(car)
library(emmeans)
library(ggplot2)
library(ggResidpanel)
library(lme4)
library(readxl)
library(zoo)



### Load and Clean Dataset ###
source("zFigure 4 - Clean Data.R")



### Analysis ###
Outcome <- "Percentage.A11.19"

fig.4b.data.1$Exposure.New <- factor(ifelse(fig.4b.data.1$Exposure == "FA", 0, 1))

MF1 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Exposure + Genotype*Sex + (1|Mouse.ID)")
MF2 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Sex + (1|Mouse.ID)")
MF3 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Exposure + (1|Mouse.ID)")
MF4 <- paste(Outcome, "~ Genotype + Exposure + Sex + (1|Mouse.ID)")
MF5 <- paste(Outcome, "~ Genotype + Exposure + Genotype*Exposure + (1|Mouse.ID)")
MF6 <- paste(Outcome, "~ Genotype + Exposure + (1|Mouse.ID)")

model1.A1 <- lmer(as.formula(MF1), data = fig.4b.data.1)
model1.A2 <- lmer(as.formula(MF2), data = fig.4b.data.1)
model1.A3 <- lmer(as.formula(MF3), data = fig.4b.data.1)
model1.A4 <- lmer(as.formula(MF4), data = fig.4b.data.1)
model1.A5 <- lmer(as.formula(MF5), data = fig.4b.data.1, contrasts = list(Genotype = "contr.sum", Exposure = "contr.sum"))
model1.A6 <- lmer(as.formula(MF6), data = fig.4b.data.1, contrasts = list(Genotype = "contr.sum", Exposure = "contr.sum"))

F1.Table <- data.frame(Predictors = c("Intercept", "Genotype", "Exposure", "Sex", "Genotype:Exposure", "Genotype:Sex"),
                       Model1 = rep(NA,6),
                       Model2 = rep(NA,6),
                       Model3 = rep(NA,6),
                       Model4 = rep(NA,6),
                       Model5 = c(ifelse(round(Anova(model1.A5, test = "F", type = "III")[1:3,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A5, test = "F", type = "III")[1:3,4], 4)),
                                  NA,
                                  ifelse(round(Anova(model1.A5, test = "F", type = "III")[4,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A5, test = "F", type = "III")[4,4], 4)),
                                  NA),
                       Model6 = c(ifelse(round(Anova(model1.A6, test = "F", type = "III")[1:3,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A6, test = "F", type = "III")[1:3,4], 4)),
                                  rep(NA,3)))
rownames(F1.Table) <- NULL



# Create Diagnostic Graph for Model 1A
Figure4A.RP <- resid_panel(model1.A6)



# Test Pairwise Comparisons (Adjusted for Multiple Comparisons)
Est.Marg.Means <- emmeans(model1.A6, ~ Genotype + Exposure)
Pairwise.Comp <- pairs(Est.Marg.Means)
summary(Pairwise.Comp)



# Test Sensitivity of Outliers
small.pmb <- fig.4b.data.1[,which(colnames(fig.4b.data.1) %in% c("Mouse.ID", "Genotype", "Sex", "Exposure", Outcome))]
small.pmb.mean <- mean(as.matrix(small.pmb[,Outcome]), na.rm = TRUE)
small.pmb.sd <- sd(as.matrix(small.pmb[,Outcome]), na.rm = TRUE)
small.pmb$KR <- ifelse(as.matrix(small.pmb[,Outcome]) < (small.pmb.mean - 2.5*small.pmb.sd) | as.matrix(small.pmb[,Outcome]) > (small.pmb.mean + 2.5*small.pmb.sd), "Remove", "Keep")
table(small.pmb$KR)

small.pmb1 <- small.pmb[which(small.pmb$KR == "Keep"),]
model1.A1.S <- lmer(as.formula(MF1), data = small.pmb1)
model1.A2.S <- lmer(as.formula(MF2), data = small.pmb1)
model1.A3.S <- lmer(as.formula(MF3), data = small.pmb1)
model1.A4.S <- lmer(as.formula(MF4), data = small.pmb1)
model1.A5.S <- lmer(as.formula(MF5), data = small.pmb1)
model1.A6.S <- lmer(as.formula(MF6), data = small.pmb1)

F2.Table <- data.frame(Predictors = c("Intercept", "Genotype", "Exposure", "Sex", "Genotype:Exposure", "Genotype:Sex"),
                       Model1 = rep(NA,6),
                       Model2 = rep(NA,6),
                       Model3 = rep(NA,6),
                       Model4 = rep(NA,6),
                       Model5 = c(ifelse(round(Anova(model1.A5.S, test = "F", type = "III")[1:3,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A5.S, test = "F", type = "III")[1:3,4], 4)),
                                  NA,
                                  ifelse(round(Anova(model1.A5.S, test = "F", type = "III")[4,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A5.S, test = "F", type = "III")[4,4], 4)),
                                  NA),
                       Model6 = c(ifelse(round(Anova(model1.A6.S, test = "F", type = "III")[1:3,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A6.S, test = "F", type = "III")[1:3,4], 4)),
                                  rep(NA,3)))
rownames(F2.Table) <- NULL



# Create Graph(s)

# Group Sex
AD.FA.Max <- max(as.matrix(fig.4b.data.1[,Outcome])[which(fig.4b.data.1$Exposure == "FA" & fig.4b.data.1$Genotype == "AD")], na.rm = TRUE)
AD.PM.Max <- max(as.matrix(fig.4b.data.1[,Outcome])[which(fig.4b.data.1$Exposure == "PM" & fig.4b.data.1$Genotype == "AD")], na.rm = TRUE)
WT.FA.Max <- max(as.matrix(fig.4b.data.1[,Outcome])[which(fig.4b.data.1$Exposure == "FA" & fig.4b.data.1$Genotype == "WT")], na.rm = TRUE)
WT.PM.Max <- max(as.matrix(fig.4b.data.1[,Outcome])[which(fig.4b.data.1$Exposure == "PM" & fig.4b.data.1$Genotype == "WT")], na.rm = TRUE)

seg.max.1 <- 4
seg.max.2 <- 2
text.above.1 <- 4.5
text.corner.1 <- 2
text.corner.2 <- 1
ylab.statement <- "Percent Area of A11-19\n(Pixel Units / Tissue Area)"
yaxis.min <- 0
yaxis.max <- 70
yaxis.breaks <- 5

Figure4A.3 <- ggplot(data = fig.4b.data.1, aes(x = Genotype, y = !!sym(Outcome), fill = Exposure)) +
  stat_boxplot(position = position_dodge(width = 0.7), geom = "errorbar", width = 0.2, size = 0.4) + 
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.5, size = 0.4, outlier.shape = NA, show.legend = FALSE) + 
  geom_point(aes(fill = factor(Exposure, labels = c("FA1", "PM1"))), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.7), color = "black", shape = 21, size = 1, show.legend = FALSE) +
  scale_y_continuous(breaks = seq(yaxis.min, yaxis.max, yaxis.breaks)) +
  scale_fill_manual(name = "Exposure", breaks = c("FA", "PM"), values = c("FA" = "indianred4", "PM" = "dodgerblue4",
                                                                          "FA1" = "indianred", "PM1" = "dodgerblue")) + 
  theme_bw()  + theme(panel.spacing = unit(0.8, "lines")) +
  theme(plot.title = element_text(size = rel(1.2))) + theme(strip.text.x = element_text()) + 
  xlab("Genotype") + ylab(ylab.statement) + 
  theme(axis.text = element_text(size = 6, face = "bold")) +
  theme(axis.title = element_text(size = 7, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0))) +
  theme(legend.text = element_text(size = 6, face = "bold")) +
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(size = 7, face = "bold")) 



# Create Final Model
final.model.results <- cbind(c("Intercept", "Genotype", "Exposure"),
                             round(summary(model1.A6)$coef[,1:3], 3),
                             round(confint(model1.A6)[3:5,], 3),
                             ifelse(Anova(model1.A6, test = "F", type = "III")[,4] < 0.0001, "< 0.0001", round(Anova(model1.A6, test = "F", type = "III")[,4], 3)))
rownames(final.model.results) <- NULL


