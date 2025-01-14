######################################
### Figure 6HI - Difference in ROS ###
######################################


### Load Libraries ###
library(emmeans)
library(ggplot2)
library(ggResidpanel)
library(readxl)
library(zoo)



### Load and Clean Dataset ###
source("zFigure 6 - Clean Data.R")



### Analysis ###
fig.6.epr.data.1$Diff.ROS <- fig.6.epr.data.1$Hrs.2 - fig.6.epr.data$Min.20
Outcome <- "Diff.ROS"

MF1 <- paste(Outcome, "~ Genotype + Exposure + Genotype*Exposure")
MF2 <- paste(Outcome, "~ Genotype + Exposure")

model1.A1 <- lm(as.formula(MF1), data = fig.6.epr.data.1[which(!is.na(fig.6.epr.data.1$Mouse.ID)),])
model1.A2 <- lm(as.formula(MF2), data = fig.6.epr.data.1[which(!is.na(fig.6.epr.data.1$Mouse.ID)),])

F1.Table <- data.frame(Predictors = c("Intercept", "Genotype", "Exposure", "Genotype:Exposure"),
                       Model1 = c(ifelse(round(coef(summary(model1.A1))[1:4,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A1))[1:4,4], 4))),
                       Model2 = c(ifelse(round(coef(summary(model1.A2))[1:3,4], 4) < 0.0001, "< 0.0001", round(coef(summary(model1.A2))[1:3,4], 4)),
                                  NA))
rownames(F1.Table) <- NULL



# Create Diagnostic Graph for Model 1A
Figure6A.RP <- resid_panel(model1.A2)



# Test Pairwise Comparisons (Adjusted for Multiple Comparisons)
Est.Marg.Means <- emmeans(model1.A2, ~ Genotype + Exposure)
Pairwise.Comp <- pairs(Est.Marg.Means)
summary(Pairwise.Comp)



# Test Sensitivity of Outliers
small.pmb <- fig.6.epr.data.1[which(!is.na(fig.6.epr.data.1$Mouse.ID)),which(colnames(fig.6.epr.data.1) %in% c("Genotype", "Sex", "Exposure", Outcome))]
small.pmb.mean <- mean(as.matrix(small.pmb[,Outcome]), na.rm = TRUE)
small.pmb.sd <- sd(as.matrix(small.pmb[,Outcome]), na.rm = TRUE)
small.pmb$KR <- ifelse(as.matrix(small.pmb[,Outcome]) < (small.pmb.mean - 2.5*small.pmb.sd) | as.matrix(small.pmb[,Outcome]) > (small.pmb.mean + 2.5*small.pmb.sd), "Remove", "Keep")
table(small.pmb$KR)



# Create Graph(s)

# Group Sex
AD.FA.Max <- max(as.matrix(fig.6.epr.data.1[,Outcome])[which(fig.6.epr.data.1$Exposure == "FA" & fig.6.epr.data.1$Genotype == "AD")], na.rm = TRUE)
AD.PM.Max <- max(as.matrix(fig.6.epr.data.1[,Outcome])[which(fig.6.epr.data.1$Exposure == "PM" & fig.6.epr.data.1$Genotype == "AD")], na.rm = TRUE)
WT.FA.Max <- max(as.matrix(fig.6.epr.data.1[,Outcome])[which(fig.6.epr.data.1$Exposure == "FA" & fig.6.epr.data.1$Genotype == "WT")], na.rm = TRUE)
WT.PM.Max <- max(as.matrix(fig.6.epr.data.1[,Outcome])[which(fig.6.epr.data.1$Exposure == "PM" & fig.6.epr.data.1$Genotype == "WT")], na.rm = TRUE)

seg.max.1 <- 200
seg.max.2 <- 100
text.above.1 <- 225
text.corner.1 <- 250
text.corner.2 <- 220 
ylab.statement <- expression(paste("Difference in ROS Level (Intensity per   ", mu, "g / ml protein)"))
yaxis.min <- -2000
yaxis.max <- 4000
yaxis.breaks <- 500

Figure6A.14 <- ggplot(data = fig.6.epr.data.1[which(!is.na(fig.6.epr.data.1$Mouse.ID)),], aes(x = Genotype, y = !!sym(Outcome), fill = Exposure)) +
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
  theme(axis.title = element_text(size = 3, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0))) +
  theme(legend.text = element_text(size = 6, face = "bold")) +
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(size = 7, face = "bold")) 



# Create Final Model
final.model.results <- cbind(c("Intercept", "Genotype", "Exposure"),
                             round(summary(model1.A2)$coef[,1:3], 3),
                             round(confint(model1.A2), 3),
                             ifelse(summary(model1.A2)$coef[,4] < 0.0001, "< 0.0001", round(summary(model1.A2)$coef[,4], 3)))
rownames(final.model.results) <- NULL






