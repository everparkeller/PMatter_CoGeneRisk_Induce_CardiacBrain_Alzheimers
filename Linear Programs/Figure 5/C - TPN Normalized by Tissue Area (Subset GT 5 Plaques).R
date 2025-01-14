##########################################################################
### Figure 5C - TPN Normalized by Tissue Area (Subset of >= 5 Plaques) ###
##########################################################################


### Load Libraries ###
library(car)
library(emmeans)
library(ggplot2)
library(ggResidpanel)
library(lme4)
library(readxl)
library(zoo)



### Load and Clean Dataset ###
source("zFigure 5 - Clean Data.R")



### Analysis ###
Outcome <- "TPN.Normalized.by.Tissue.Area"

MF1 <- paste(Outcome, "~ Exposure + Sex + Exposure*Sex")
MF2 <- paste(Outcome, "~ Exposure + Sex")
MF3 <- paste(Outcome, "~ Exposure")

model1.A1 <- lm(as.formula(MF1), data = fig.5b.data.1[which(fig.5b.data.1$LT.5.Plaques == "No"),])
model1.A2 <- lm(as.formula(MF2), data = fig.5b.data.1[which(fig.5b.data.1$LT.5.Plaques == "No"),])
model1.A3 <- lm(as.formula(MF3), data = fig.5b.data.1[which(fig.5b.data.1$LT.5.Plaques == "No"),])

F1.Table <- data.frame(Predictors = c("Intercept", "Exposure", "Sex", "Exposure:Sex"),
                       Model1 = c(ifelse(round(coef(summary(model1.A1))[1:4,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A1))[1:4,4], 4))),
                       Model2 = c(ifelse(round(coef(summary(model1.A2))[1:3,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A2))[1:3,4], 4)),
                                  NA),
                       Model3 = c(ifelse(round(coef(summary(model1.A3))[1:2,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A3))[1:2,4], 4)),
                                  rep(NA,2)))
rownames(F1.Table) <- NULL



# Create Diagnostic Graph for Model 1A
Figure4A.RP <- resid_panel(model1.A3)



# Test Pairwise Comparisons (Adjusted for Multiple Comparisons)
Est.Marg.Means <- emmeans(model1.A3, ~ Exposure)
Pairwise.Comp <- pairs(Est.Marg.Means)
summary(Pairwise.Comp)



# Test Sensitivity of Outliers
small.pmb <- fig.5b.data.1[which(fig.5b.data.1$LT.5.Plaques == "No"),which(colnames(fig.5b.data.1) %in% c("Genotype", "Sex", "Exposure", Outcome))]
small.pmb.mean <- mean(as.matrix(small.pmb[,Outcome]), na.rm = TRUE)
small.pmb.sd <- sd(as.matrix(small.pmb[,Outcome]), na.rm = TRUE)
small.pmb$KR <- ifelse(as.matrix(small.pmb[,Outcome]) < (small.pmb.mean - 2.5*small.pmb.sd) | as.matrix(small.pmb[,Outcome]) > (small.pmb.mean + 2.5*small.pmb.sd), "Remove", "Keep")
table(small.pmb$KR)

small.pmb1 <- small.pmb[which(small.pmb$KR == "Keep"),]
model1.A1.S <- lm(as.formula(MF1), data = small.pmb1)
model1.A2.S <- lm(as.formula(MF2), data = small.pmb1)
model1.A3.S <- lm(as.formula(MF3), data = small.pmb1)

F2.Table <- data.frame(Predictors = c("Intercept", "Exposure", "Sex", "Exposure:Sex"),
                       Model1 = c(ifelse(round(coef(summary(model1.A1.S))[1:4,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A1.S))[1:4,4], 4))),
                       Model2 = c(ifelse(round(coef(summary(model1.A2.S))[1:3,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A2.S))[1:3,4], 4)),
                                  NA),
                       Model3 = c(ifelse(round(coef(summary(model1.A3.S))[1:2,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A3.S))[1:2,4], 4)),
                                  rep(NA,2)))
rownames(F2.Table) <- NULL



# Create Graph(s)

# Group Sex
FA.Max <- max(as.matrix(fig.5b.data.1[,Outcome])[which(fig.5b.data.1$Exposure == "FA")], na.rm = TRUE)
PM.Max <- max(as.matrix(fig.5b.data.1[,Outcome])[which(fig.5b.data.1$Exposure == "PM")], na.rm = TRUE)

seg.max.1 <- 0.01
seg.max.2 <- 0.005
text.above.1 <- 0.015
text.corner.1 <- 0.015
text.corner.2 <- 0.003
ylab.statement <- expression(paste("Total Plaque Number / Tissue Area   ", mu, M^{2}, sep = ''))
yaxis.min <- 0
yaxis.max <- 0.5
yaxis.breaks <- 0.02

Figure5A.2 <- ggplot(data = fig.5b.data.1, aes(x = Exposure, y = !!sym(Outcome), fill = Exposure)) +
  stat_boxplot(position = position_dodge(width = 0.7), geom = "errorbar", width = 0.2, size = 0.4) + 
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.5, size = 0.4, outlier.shape = NA, show.legend = FALSE) + 
  geom_point(aes(fill = factor(Exposure, labels = c("FA1", "PM1"))), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.7), color = "black", shape = 21, size = 1, show.legend = FALSE) +
  scale_y_continuous(breaks = seq(yaxis.min, yaxis.max, yaxis.breaks)) +
  scale_fill_manual(name = "Exposure", breaks = c("FA", "PM"), values = c("FA" = "indianred4", "PM" = "dodgerblue4",
                                                                          "FA1" = "indianred", "PM1" = "dodgerblue")) + 
  theme_bw()  + theme(panel.spacing = unit(0.8, "lines")) +
  theme(plot.title = element_text(size = rel(1.2))) + theme(strip.text.x = element_text()) + 
  xlab("Exposure") + ylab(ylab.statement) + 
  theme(axis.text = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(legend.text = element_text(size = 6, face = "bold")) +
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(size = 7, face = "bold")) +
  geom_text(aes(x = 1.93, y = max(!!sym(Outcome), na.rm = TRUE) + text.corner.1, label = "* Exposure"), color = "black", size = 2.5, vjust = 0, hjust = 0)



# Create Final Model
final.model.results <- cbind(c("Intercept", "Exposure"),
                             round(summary(model1.A3)$coef[,1:3], 3),
                             round(confint(model1.A3), 3),
                             ifelse(summary(model1.A3)$coef[,4] < 0.0001, "< 0.0001", round(summary(model1.A3)$coef[,4], 3)))
rownames(final.model.results) <- NULL


