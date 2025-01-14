######################################
### Figure 6DF - GSH Total (Heart) ###
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
Outcome <- "GSH.Total"

MF1 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Exposure + Genotype*Sex")
MF2 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Sex")
MF3 <- paste(Outcome, "~ Genotype + Exposure + Sex + Genotype*Exposure")
MF4 <- paste(Outcome, "~ Genotype + Exposure + Sex")
MF5 <- paste(Outcome, "~ Genotype + Exposure + Genotype*Exposure")
MF6 <- paste(Outcome, "~ Genotype + Exposure")

model1.A1 <- lm(as.formula(MF1), data = fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),])
model1.A2 <- lm(as.formula(MF2), data = fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),])
model1.A3 <- lm(as.formula(MF3), data = fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),])
model1.A4 <- lm(as.formula(MF4), data = fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),])
model1.A5 <- lm(as.formula(MF5), data = fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),])
model1.A6 <- lm(as.formula(MF6), data = fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),])

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
Figure6A.RP <- resid_panel(model1.A4)



# Test Pairwise Comparisons (Adjusted for Multiple Comparisons)
Est.Marg.Means <- emmeans(model1.A4, ~ Genotype + Sex)
Pairwise.Comp <- pairs(Est.Marg.Means)
summary(Pairwise.Comp)

Est.Marg.Means <- emmeans(model1.A4, ~ Genotype + Exposure + Sex)
Pairwise.Comp <- pairs(Est.Marg.Means)
summary(Pairwise.Comp)



# Test Sensitivity of Outliers
small.pmb <- fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),which(colnames(fig.6.gsh.data.1) %in% c("Genotype", "Sex", "Exposure", Outcome))]
small.pmb.mean <- mean(as.matrix(small.pmb[,Outcome]), na.rm = TRUE)
small.pmb.sd <- sd(as.matrix(small.pmb[,Outcome]), na.rm = TRUE)
small.pmb$KR <- ifelse(as.matrix(small.pmb[,Outcome]) < (small.pmb.mean - 2.5*small.pmb.sd) | as.matrix(small.pmb[,Outcome]) > (small.pmb.mean + 2.5*small.pmb.sd), "Remove", "Keep")
table(small.pmb$KR)



# Create Graph(s)

# Group Sex
AD.FA.Max <- max(as.matrix(fig.6.gsh.data.1[,Outcome])[which(fig.6.gsh.data.1$Exposure == "FA" & fig.6.gsh.data.1$Genotype == "AD" & fig.6.gsh.data.1$Organ == "Heart")], na.rm = TRUE)
AD.PM.Max <- max(as.matrix(fig.6.gsh.data.1[,Outcome])[which(fig.6.gsh.data.1$Exposure == "PM" & fig.6.gsh.data.1$Genotype == "AD" & fig.6.gsh.data.1$Organ == "Heart")], na.rm = TRUE)
WT.FA.Max <- max(as.matrix(fig.6.gsh.data.1[,Outcome])[which(fig.6.gsh.data.1$Exposure == "FA" & fig.6.gsh.data.1$Genotype == "WT" & fig.6.gsh.data.1$Organ == "Heart")], na.rm = TRUE)
WT.PM.Max <- max(as.matrix(fig.6.gsh.data.1[,Outcome])[which(fig.6.gsh.data.1$Exposure == "PM" & fig.6.gsh.data.1$Genotype == "WT" & fig.6.gsh.data.1$Organ == "Heart")], na.rm = TRUE)

seg.max.1 <- 2
seg.max.2 <- 1
text.above.1 <- 2.2
text.corner.1 <- 2.6
text.corner.2 <- 1.7
ylab.statement <- expression(paste("GSH Total ( ", mu, "M / mg total protein)"))
yaxis.min <- 0
yaxis.max <- 40
yaxis.breaks <- 5

Figure6A.1 <- ggplot(data = fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),], aes(x = Genotype, y = !!sym(Outcome), fill = Exposure)) +
  stat_boxplot(position = position_dodge(width = 0.7), geom = "errorbar", width = 0.2, size = 0.4) + 
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.5, size = 0.4, outlier.shape = NA) + 
  geom_point(aes(fill = factor(Exposure, labels = c("FA1", "PM1"))), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.7), color = "black", shape = 21, size = 1, show.legend = FALSE) +
  scale_y_continuous(breaks = seq(yaxis.min, yaxis.max, yaxis.breaks)) +
  scale_fill_manual(name = "Exposure", breaks = c("FA", "PM"), values = c("FA" = "indianred4", "PM" = "dodgerblue4",
                                                                          "FA1" = "indianred", "PM1" = "dodgerblue")) + 
  theme_bw()  + theme(panel.spacing = unit(0.8, "lines")) +
  theme(plot.title = element_text(size = rel(1.2))) + theme(strip.text.x = element_text()) + 
  xlab("Genotype") + ylab(ylab.statement) + 
  theme(axis.text = element_text(size = 6, face = "bold")) +
  theme(axis.title = element_text(size = 7, face = "bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(legend.text = element_text(size = 6, face = "bold")) +
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(size = 7, face = "bold")) +
  geom_segment(aes(x = 0.825, xend = 1.175, y = max(!!sym(Outcome), na.rm = TRUE) + seg.max.1, yend = max(!!sym(Outcome), na.rm = TRUE) + seg.max.1), 
               color = "black", size = 0.4, lineend = "round") +
  geom_segment(aes(x = 0.825, xend = 0.825, y = max(!!sym(Outcome), na.rm = TRUE) + seg.max.1, yend = AD.FA.Max + seg.max.2), 
               color = "black", size = 0.4, lineend = "round") +
  geom_segment(aes(x = 1.175, xend = 1.175, y = max(!!sym(Outcome), na.rm = TRUE) + seg.max.1, yend = AD.PM.Max + seg.max.2), 
               color = "black", size = 0.4, lineend = "round") +
  geom_segment(aes(x = 1.824, xend = 2.175, y = max(!!sym(Outcome), na.rm = TRUE) + seg.max.1, yend = max(!!sym(Outcome), na.rm = TRUE) + seg.max.1), 
               color = "black", size = 0.4, lineend = "round") + 
  geom_segment(aes(x = 1.825, xend = 1.825, y = max(!!sym(Outcome), na.rm = TRUE) + seg.max.1, yend = WT.FA.Max + seg.max.2), 
               color = "black", size = 0.4, lineend = "round") +
  geom_segment(aes(x = 2.175, xend = 2.175, y = max(!!sym(Outcome), na.rm = TRUE) + seg.max.1, yend = WT.PM.Max + seg.max.2), 
               color = "black", size = 0.4, lineend = "round") + 
  geom_text(aes(x = 1, y = max(!!sym(Outcome), na.rm = TRUE) + text.above.1, label = "*"), color = "black", size = 2.5, vjust = 0) +
  geom_text(aes(x = 2, y = max(!!sym(Outcome), na.rm = TRUE) + text.above.1, label = "*"), color = "black", size = 2.5, vjust = 0) +
  geom_text(aes(x = 2.32, y = max(!!sym(Outcome), na.rm = TRUE) + text.corner.1, label = "# Genotype"), color = "black", size = 2.5, hjust = 0, vjust = 0) +
  geom_text(aes(x = 2.32, y = max(!!sym(Outcome), na.rm = TRUE) + text.corner.2, label = "** Exposure"), color = "black", size = 2.5, hjust = 0, vjust = 0) +
  geom_text(aes(x = 2.32, y = max(!!sym(Outcome), na.rm = TRUE) + 0.8, label = "*** Sex"), color = "black", size = 2.5, hjust = 0, vjust = 0)



# Stratified Sex
fig.6.gsh.data.1$Concat.G.S <- ifelse(fig.6.gsh.data.1$Genotype == "WT" & fig.6.gsh.data.1$Sex == "M", "Male\nWT",
                                      ifelse(fig.6.gsh.data.1$Genotype == "WT" & fig.6.gsh.data.1$Sex == "F", "Female\nWT",
                                             ifelse(fig.6.gsh.data.1$Genotype == "AD" & fig.6.gsh.data.1$Sex == "M", "Male\nAD",
                                                    ifelse(fig.6.gsh.data.1$Genotype == "AD" & fig.6.gsh.data.1$Sex == "F", "Female\nAD", "Error"))))


AD.FA.Max.M <- max(as.matrix(fig.6.gsh.data.1[,Outcome])[which(fig.6.gsh.data.1$Exposure == "FA" & fig.6.gsh.data.1$Genotype == "AD" & fig.6.gsh.data.1$Sex == "M" & fig.6.gsh.data.1$Organ == "Heart")], na.rm = TRUE)
AD.PM.Max.M <- max(as.matrix(fig.6.gsh.data.1[,Outcome])[which(fig.6.gsh.data.1$Exposure == "PM" & fig.6.gsh.data.1$Genotype == "AD" & fig.6.gsh.data.1$Sex == "M" & fig.6.gsh.data.1$Organ == "Heart")], na.rm = TRUE)
WT.FA.Max.M <- max(as.matrix(fig.6.gsh.data.1[,Outcome])[which(fig.6.gsh.data.1$Exposure == "FA" & fig.6.gsh.data.1$Genotype == "WT" & fig.6.gsh.data.1$Sex == "M" & fig.6.gsh.data.1$Organ == "Heart")], na.rm = TRUE)
WT.PM.Max.M <- max(as.matrix(fig.6.gsh.data.1[,Outcome])[which(fig.6.gsh.data.1$Exposure == "PM" & fig.6.gsh.data.1$Genotype == "WT" & fig.6.gsh.data.1$Sex == "M" & fig.6.gsh.data.1$Organ == "Heart")], na.rm = TRUE)

AD.FA.Max.F <- max(as.matrix(fig.6.gsh.data.1[,Outcome])[which(fig.6.gsh.data.1$Exposure == "FA" & fig.6.gsh.data.1$Genotype == "AD" & fig.6.gsh.data.1$Sex == "F" & fig.6.gsh.data.1$Organ == "Heart")], na.rm = TRUE)
AD.PM.Max.F <- max(as.matrix(fig.6.gsh.data.1[,Outcome])[which(fig.6.gsh.data.1$Exposure == "PM" & fig.6.gsh.data.1$Genotype == "AD" & fig.6.gsh.data.1$Sex == "F" & fig.6.gsh.data.1$Organ == "Heart")], na.rm = TRUE)
WT.FA.Max.F <- max(as.matrix(fig.6.gsh.data.1[,Outcome])[which(fig.6.gsh.data.1$Exposure == "FA" & fig.6.gsh.data.1$Genotype == "WT" & fig.6.gsh.data.1$Sex == "F" & fig.6.gsh.data.1$Organ == "Heart")], na.rm = TRUE)
WT.PM.Max.F <- max(as.matrix(fig.6.gsh.data.1[,Outcome])[which(fig.6.gsh.data.1$Exposure == "PM" & fig.6.gsh.data.1$Genotype == "WT" & fig.6.gsh.data.1$Sex == "F" & fig.6.gsh.data.1$Organ == "Heart")], na.rm = TRUE)

seg.max.1 <- 1
seg.max.2 <- 1
text.above.1 <- 0.1
text.corner.1 <- 1
text.corner.2 <- 1.8
ylab.statement <- expression(paste("GSH Total ( ", mu, "M / mg total protein)"))
yaxis.min <- 0
yaxis.max <- 40
yaxis.breaks <- 5

Figure6A.4 <- ggplot(data = fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),], aes(x = Concat.G.S, y = !!sym(Outcome), fill = Exposure)) +
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
  theme(axis.title = element_text(size = 3.5, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0))) +
  theme(legend.text = element_text(size = 6, face = "bold")) +
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(size = 7, face = "bold")) +
  theme(strip.text = element_text(face = "bold")) +
  geom_segment(aes(x = 0.825, xend = 1.175, y = max(as.matrix(fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),Outcome]), na.rm = TRUE) + seg.max.1, yend = max(as.matrix(fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),Outcome]), na.rm = TRUE) + seg.max.1), 
               color = "black", size = 0.4, lineend = "round") + 
  geom_text(aes(x = 1, y = max(as.matrix(fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),Outcome]), na.rm = TRUE) + text.above.1, label = "*"), color = "black", size = 2.5, vjust = 0) +
  geom_segment(aes(x = 1.825, xend = 2.175, y = max(as.matrix(fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),Outcome]), na.rm = TRUE) + seg.max.1, yend = max(as.matrix(fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),Outcome]), na.rm = TRUE) + seg.max.1), 
               color = "black", size = 0.4, lineend = "round") + 
  geom_text(aes(x = 2, y = max(as.matrix(fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),Outcome]), na.rm = TRUE) + text.above.1, label = "*"), color = "black", size = 2.5, vjust = 0) +
  geom_segment(aes(x = 2.825, xend = 3.175, y = max(as.matrix(fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),Outcome]), na.rm = TRUE) + seg.max.1, yend = max(as.matrix(fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),Outcome]), na.rm = TRUE) + seg.max.1), 
               color = "black", size = 0.4, lineend = "round") + 
  geom_text(aes(x = 3, y = max(as.matrix(fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),Outcome]), na.rm = TRUE) + text.above.1, label = "*"), color = "black", size = 2.5, vjust = 0) +
  geom_segment(aes(x = 3.825, xend = 4.175, y = max(as.matrix(fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),Outcome]), na.rm = TRUE) + seg.max.1, yend = max(as.matrix(fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),Outcome]), na.rm = TRUE) + seg.max.1), 
               color = "black", size = 0.4, lineend = "round") + 
  geom_text(aes(x = 4, y = max(as.matrix(fig.6.gsh.data.1[which(fig.6.gsh.data.1$Organ == "Heart"),Outcome]), na.rm = TRUE) + text.above.1, label = "*"), color = "black", size = 2.5, vjust = 0) +
  geom_text(aes(x = 3.83, y = 32, label = "# Genotype"), color = "black", size = 1.5, hjust = 0, vjust = 0) +
  geom_text(aes(x = 3.83, y = 28, label = "** Exposure"), color = "black", size = 1.5, hjust = 0, vjust = 0) +
  geom_text(aes(x = 3.83, y = 24, label = "*** Sex"), color = "black", size = 1.5, hjust = 0, vjust = 0) +
  geom_text(aes(x = 3, y = 34, label = ""))



# Create Final Model
final.model.results <- cbind(c("Intercept", "Genotype", "Exposure", "Sex"),
                             round(summary(model1.A4)$coef[,1:3], 3),
                             round(confint(model1.A4), 3),
                             ifelse(summary(model1.A4)$coef[,4] < 0.0001, "< 0.0001", round(summary(model1.A4)$coef[,4], 3)))
rownames(final.model.results) <- NULL







