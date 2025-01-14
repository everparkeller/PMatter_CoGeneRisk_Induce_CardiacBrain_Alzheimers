#########################
### Figure 2A: LAD/TL ###
#########################


### Load Libraries ###
library(emmeans)
library(ggplot2)
library(ggResidpanel)
library(readxl)
library(zoo)



### Load and Clean Dataset ###
source("zFigure 2 - Clean Data.R")



### Analysis ###
Outcome <- "LAD.TL"

MF1 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Exposure + Genotype*Sex")
MF2 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Sex")
MF3 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Exposure")
MF4 <- paste(Outcome, "~ Genotype + Exposure + Sex")
MF5 <- paste(Outcome, "~ Genotype + Exposure + Genotype*Exposure")
MF6 <- paste(Outcome, "~ Genotype + Exposure")

model1.A1 <- lm(as.formula(MF1), data = fig.2.data.1)
summary(model1.A1)

model1.A2 <- lm(as.formula(MF2), data = fig.2.data.1)
summary(model1.A2)

model1.A3 <- lm(as.formula(MF3), data = fig.2.data.1)
summary(model1.A3)

model1.A4 <- lm(as.formula(MF4), data = fig.2.data.1)
summary(model1.A4)

model1.A5 <- lm(as.formula(MF5), data = fig.2.data.1)
summary(model1.A5)

model1.A6 <- lm(as.formula(MF6), data = fig.2.data.1)
summary(model1.A6)

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
Figure2A.RP <- resid_panel(model1.A2)



# Test Pairwise Comparisons (Adjusted for Multiple Comparisons)
Est.Marg.Means <- emmeans(model1.A2, ~ Genotype + Sex)
Pairwise.Comp <- pairs(Est.Marg.Means)
summary(Pairwise.Comp)

Est.Marg.Means <- emmeans(model1.A2, ~ Genotype + Exposure + Sex)
Pairwise.Comp <- pairs(Est.Marg.Means)
summary(Pairwise.Comp)



# Test Sensitivity of Outliers
small.pmb <- fig.2.data.1[,which(colnames(fig.2.data.1) %in% c("Genotype", "Sex", "Exposure", Outcome))]
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
AD.FA.Max <- max(as.matrix(small.pmb[,Outcome])[which(fig.2.data.1$Exposure == "FA" & fig.2.data.1$Genotype == "AD")], na.rm = TRUE)
AD.PM.Max <- max(as.matrix(small.pmb[,Outcome])[which(fig.2.data.1$Exposure == "PM" & fig.2.data.1$Genotype == "AD")], na.rm = TRUE)
WT.FA.Max <- max(as.matrix(small.pmb[,Outcome])[which(fig.2.data.1$Exposure == "FA" & fig.2.data.1$Genotype == "WT")], na.rm = TRUE)
WT.PM.Max <- max(as.matrix(small.pmb[,Outcome])[which(fig.2.data.1$Exposure == "PM" & fig.2.data.1$Genotype == "WT")], na.rm = TRUE)

Figure2A.1 <- ggplot(data = fig.2.data.1, aes(x = Genotype, y = !!sym(Outcome), fill = Exposure)) +
  stat_boxplot(position = position_dodge(width = 0.7), geom = "errorbar", width = 0.2, size = 0.4) + 
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.5, size = 0.4, outlier.shape = NA) + 
  geom_point(aes(fill = factor(Exposure, labels = c("FA1", "PM1"))), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.7), color = "black", shape = 21, size = 1, show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0, 1, 0.05)) +
  scale_fill_manual(name = "Exposure", breaks = c("FA", "PM"), values = c("FA" = "indianred4", "PM" = "dodgerblue4",
                                                                          "FA1" = "indianred", "PM1" = "dodgerblue")) + 
  theme_bw()  + theme(panel.spacing = unit(0.8, "lines")) +
  theme(plot.title = element_text(size = rel(1.2))) + theme(strip.text.x = element_text()) + 
  xlab("Genotype") + ylab("LAD/TL (mm:mm)") + 
  theme(axis.text = element_text(size = 6, face = "bold")) +
  theme(axis.title = element_text(size = 7, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(legend.text = element_text(size = 6, face = "bold")) +
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(size = 7, face = "bold")) +
  geom_segment(aes(x = 0.824, xend = 1.175, y = max(!!sym(Outcome), na.rm = TRUE) + 0.02, yend = max(!!sym(Outcome), na.rm = TRUE) + 0.02), 
               color = "black", size = 0.4, lineend = "round") + 
  geom_segment(aes(x = 0.825, xend = 0.825, y = max(!!sym(Outcome), na.rm = TRUE) + 0.02, yend = AD.FA.Max + 0.01), 
               color = "black", size = 0.4, lineend = "round") +
  geom_segment(aes(x = 1.175, xend = 1.175, y = max(!!sym(Outcome), na.rm = TRUE) + 0.02, yend = AD.PM.Max + 0.01), 
               color = "black", size = 0.4, lineend = "round") +
  geom_segment(aes(x = 1.824, xend = 2.175, y = max(!!sym(Outcome), na.rm = TRUE) + 0.02, yend = max(!!sym(Outcome), na.rm = TRUE) + 0.02), 
               color = "black", size = 0.4, lineend = "round") + 
  geom_segment(aes(x = 1.825, xend = 1.825, y = max(!!sym(Outcome), na.rm = TRUE) + 0.02, yend = WT.FA.Max + 0.01), 
               color = "black", size = 0.4, lineend = "round") +
  geom_segment(aes(x = 2.175, xend = 2.175, y = max(!!sym(Outcome), na.rm = TRUE) + 0.02, yend = WT.PM.Max + 0.01), 
               color = "black", size = 0.4, lineend = "round") + 
  geom_text(aes(x = 1, y = max(!!sym(Outcome), na.rm = TRUE) + 0.023, label = "NS"), color = "black", size = 2.5, vjust = 0) +
  geom_text(aes(x = 2, y = max(!!sym(Outcome), na.rm = TRUE) + 0.023, label = "NS"), color = "black", size = 2.5, vjust = 0) +
  geom_text(aes(x = 2.32, y = max(!!sym(Outcome), na.rm = TRUE) + 0.03, label = "*** Sex"), color = "black", size = 2.5, hjust = 0, vjust = 0) +
  geom_text(aes(x = 2.32, y = max(!!sym(Outcome), na.rm = TRUE) + 0.02, label = "** Genotype:Sex"), color = "black", size = 2.5, hjust = 0, vjust = 0)



# Stratified Sex
fig.2.data.1$Concat.G.S <- ifelse(fig.2.data.1$Genotype == "WT" & fig.2.data.1$Sex == "M", "Male\nWT",
                                  ifelse(fig.2.data.1$Genotype == "WT" & fig.2.data.1$Sex == "F", "Female\nWT",
                                         ifelse(fig.2.data.1$Genotype == "AD" & fig.2.data.1$Sex == "M", "Male\nAD",
                                                ifelse(fig.2.data.1$Genotype == "AD" & fig.2.data.1$Sex == "F", "Female\nAD", "Error"))))

AD.FA.Max.M <- max(as.matrix(fig.2.data.1[,Outcome])[which(fig.2.data.1$Exposure == "FA" & fig.2.data.1$Genotype == "AD" & fig.2.data.1$Sex == "M")], na.rm = TRUE)
AD.PM.Max.M <- max(as.matrix(fig.2.data.1[,Outcome])[which(fig.2.data.1$Exposure == "PM" & fig.2.data.1$Genotype == "AD" & fig.2.data.1$Sex == "M")], na.rm = TRUE)
WT.FA.Max.M <- max(as.matrix(fig.2.data.1[,Outcome])[which(fig.2.data.1$Exposure == "FA" & fig.2.data.1$Genotype == "WT" & fig.2.data.1$Sex == "M")], na.rm = TRUE)
WT.PM.Max.M <- max(as.matrix(fig.2.data.1[,Outcome])[which(fig.2.data.1$Exposure == "PM" & fig.2.data.1$Genotype == "WT" & fig.2.data.1$Sex == "M")], na.rm = TRUE)

AD.FA.Max.F <- max(as.matrix(fig.2.data.1[,Outcome])[which(fig.2.data.1$Exposure == "FA" & fig.2.data.1$Genotype == "AD" & fig.2.data.1$Sex == "F")], na.rm = TRUE)
AD.PM.Max.F <- max(as.matrix(fig.2.data.1[,Outcome])[which(fig.2.data.1$Exposure == "PM" & fig.2.data.1$Genotype == "AD" & fig.2.data.1$Sex == "F")], na.rm = TRUE)
WT.FA.Max.F <- max(as.matrix(fig.2.data.1[,Outcome])[which(fig.2.data.1$Exposure == "FA" & fig.2.data.1$Genotype == "WT" & fig.2.data.1$Sex == "F")], na.rm = TRUE)
WT.PM.Max.F <- max(as.matrix(fig.2.data.1[,Outcome])[which(fig.2.data.1$Exposure == "PM" & fig.2.data.1$Genotype == "WT" & fig.2.data.1$Sex == "F")], na.rm = TRUE)

Figure2A.1T <- ggplot(data = fig.2.data.1, aes(x = Concat.G.S, y = !!sym(Outcome), fill = Exposure)) +
  stat_boxplot(position = position_dodge(width = 0.7), geom = "errorbar", width = 0.2, size = 0.4) + 
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.5, size = 0.4, outlier.shape = NA, show.legend = FALSE) + 
  geom_point(aes(fill = factor(Exposure, labels = c("FA1", "PM1"))), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.7), color = "black", shape = 21, size = 1, show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0, 1, 0.05)) +
  scale_fill_manual(name = "Exposure", breaks = c("FA", "PM"), values = c("FA" = "indianred4", "PM" = "dodgerblue4",
                                                                          "FA1" = "indianred", "PM1" = "dodgerblue")) + 
  theme_bw()  + theme(panel.spacing = unit(0.8, "lines")) +
  theme(plot.title = element_text(size = rel(1.2))) + theme(strip.text.x = element_text()) + 
  xlab("Genotype") + ylab("LAD/TL (mm:mm)") + 
  theme(axis.text = element_text(size = 6, face = "bold")) +
  theme(axis.title = element_text(size = 7, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(legend.text = element_text(size = 6, face = "bold")) +
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(size = 7, face = "bold")) +
  geom_text(aes(x = 3.36, y = max(as.matrix(fig.2.data.1[,Outcome]), na.rm = TRUE) + 0.028, label = "*** Sex"), color = "black", size = 2, hjust = 0, vjust = 0) +
  geom_text(aes(x = 3.36, y = max(as.matrix(fig.2.data.1[,Outcome]), na.rm = TRUE) + 0.015, label = "** Genotype:Sex"), color = "black", size = 2, hjust = 0, vjust = 0) +
  geom_text(aes(x = 3.36, y = max(as.matrix(fig.2.data.1[,Outcome]), na.rm = TRUE) + 0.035, label = ""))



# Create Final Model
final.model.results <- cbind(c("Intercept", "Genotype", "Exposure", "Sex", "Genotype:Sex"),
                             round(summary(model1.A2)$coef[,1:3], 3),
                             round(confint(model1.A2), 3),
                             ifelse(summary(model1.A2)$coef[,4] < 0.0001, "< 0.0001", round(summary(model1.A2)$coef[,4], 3)))
rownames(final.model.results) <- NULL


