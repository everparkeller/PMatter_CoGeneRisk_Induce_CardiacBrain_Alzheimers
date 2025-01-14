#############################################
### Figure 5D - Normalized by Tissue Area ###
#############################################


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
Outcome <- "Normalized.by.Tissue.Area"

fig.5c.data.1$Starting.Plaque.Size <- factor(fig.5c.data.1$Starting.Plaque.Size, levels = c("Less Than 250", "Between 250 to 500", "Between 500 to 1000", "Greater than 1000"),
                                             labels = c("Less Than 250", "Between 250 to 500", "Between 500 to 1000", "Greater than 1000"))

MF1 <- paste(Outcome, "~ Exposure + Sex + Exposure*Sex + Starting.Plaque.Size + (1|Mouse.ID)")
MF2 <- paste(Outcome, "~ Exposure + Sex + Exposure*Sex + (1|Mouse.ID)")
MF3 <- paste(Outcome, "~ Exposure + Sex + Starting.Plaque.Size + (1|Mouse.ID)")
MF4 <- paste(Outcome, "~ Exposure + Sex + (1|Mouse.ID)")
MF5 <- paste(Outcome, "~ Exposure + Starting.Plaque.Size + (1|Mouse.ID)")
MF6 <- paste(Outcome, "~ Exposure + (1|Mouse.ID)")

model1.A1 <- lmer(as.formula(MF1), data = fig.5c.data.1, contrasts = list(Exposure = "contr.sum", Sex = "contr.sum", Starting.Plaque.Size = "contr.sum"))
model1.A2 <- lmer(as.formula(MF2), data = fig.5c.data.1, contrasts = list(Exposure = "contr.sum", Sex = "contr.sum"))
model1.A3 <- lmer(as.formula(MF3), data = fig.5c.data.1, contrasts = list(Exposure = "contr.sum", Sex = "contr.sum", Starting.Plaque.Size = "contr.sum"))
model1.A4 <- lmer(as.formula(MF4), data = fig.5c.data.1, contrasts = list(Exposure = "contr.sum", Sex = "contr.sum"))
model1.A5 <- lmer(as.formula(MF5), data = fig.5c.data.1, contrasts = list(Exposure = "contr.sum", Starting.Plaque.Size = "contr.sum"))
model1.A6 <- lmer(as.formula(MF6), data = fig.5c.data.1, contrasts = list(Exposure = "contr.sum"))

F1.Table <- data.frame(Predictors = c("Intercept", "Exposure", "Sex", "Starting.Plaque.Size", "Exposure:Sex"),
                       Model1 = c(ifelse(round(Anova(model1.A1, test = "F", type = "III")[1:5,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A1, test = "F", type = "III")[1:5,4], 4))),
                       Model2 = c(ifelse(round(Anova(model1.A2, test = "F", type = "III")[1:3,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A2, test = "F", type = "III")[1:3,4], 4)),
                                  NA,
                                  ifelse(round(Anova(model1.A2, test = "F", type = "III")[4,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A2, test = "F", type = "III")[4,4], 4))),
                       Model3 = c(ifelse(round(Anova(model1.A3, test = "F", type = "III")[1:4,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A3, test = "F", type = "III")[1:4,4], 4)),
                                  NA),
                       Model4 = c(ifelse(round(Anova(model1.A4, test = "F", type = "III")[1:3,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A4, test = "F", type = "III")[1:3,4], 4)),
                                  rep(NA,2)),
                       Model5 = c(ifelse(round(Anova(model1.A5, test = "F", type = "III")[1:2,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A5, test = "F", type = "III")[1:2,4], 4)),
                                  NA,
                                  ifelse(round(Anova(model1.A5, test = "F", type = "III")[3,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A5, test = "F", type = "III")[3,4], 4)),
                                  NA),
                       Model6 = c(ifelse(round(Anova(model1.A6, test = "F", type = "III")[1:2,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A6, test = "F", type = "III")[1:2,4], 4)),
                                  rep(NA,3)))
rownames(F1.Table) <- NULL



# Create Diagnostic Graph for Model 1A
Figure4A.RP <- resid_panel(model1.A3)



# Test Pairwise Comparisons (Adjusted for Multiple Comparisons)
Est.Marg.Means <- emmeans(model1.A3, ~ Exposure + Starting.Plaque.Size)
Pairwise.Comp <- pairs(Est.Marg.Means)
summary(Pairwise.Comp)



# Test Sensitivity of Outliers
small.pmb <- fig.5c.data.1[,which(colnames(fig.5c.data.1) %in% c("Mouse.ID", "Genotype", "Sex", "Exposure", "Starting.Plaque.Size", Outcome))]
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

F2.Table <- data.frame(Predictors = c("Intercept", "Exposure", "Sex", "Starting.Plaque.Size", "Exposure:Sex"),
                       Model1 = c(ifelse(round(Anova(model1.A1.S, test = "F", type = "III")[1:5,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A1.S, test = "F", type = "III")[1:5,4], 4))),
                       Model2 = c(ifelse(round(Anova(model1.A2.S, test = "F", type = "III")[1:3,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A2.S, test = "F", type = "III")[1:3,4], 4)),
                                  NA,
                                  ifelse(round(Anova(model1.A2.S, test = "F", type = "III")[4,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A2.S, test = "F", type = "III")[4,4], 4))),
                       Model3 = c(ifelse(round(Anova(model1.A3.S, test = "F", type = "III")[1:4,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A3.S, test = "F", type = "III")[1:4,4], 4)),
                                  NA),
                       Model4 = c(ifelse(round(Anova(model1.A4.S, test = "F", type = "III")[1:3,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A4.S, test = "F", type = "III")[1:3,4], 4)),
                                  rep(NA,2)),
                       Model5 = c(ifelse(round(Anova(model1.A5.S, test = "F", type = "III")[1:2,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A5.S, test = "F", type = "III")[1:2,4], 4)),
                                  NA,
                                  ifelse(round(Anova(model1.A5.S, test = "F", type = "III")[3,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A5.S, test = "F", type = "III")[3,4], 4)),
                                  NA),
                       Model6 = c(ifelse(round(Anova(model1.A6.S, test = "F", type = "III")[1:2,4], 4) < 0.0001, "< 0.0001", round(Anova(model1.A6.S, test = "F", type = "III")[1:2,4], 4)),
                                  rep(NA,3)))
rownames(F2.Table) <- NULL



# Create Graph(s)

# Group Sex
fig.5c.data.1$Concat.E.SPS <- ifelse(fig.5c.data.1$Exposure == "FA" & fig.5c.data.1$Starting.Plaque.Size == "Less Than 250", "FA\n< 250",
                                     ifelse(fig.5c.data.1$Exposure == "PM" & fig.5c.data.1$Starting.Plaque.Size == "Less Than 250", "PM\n< 250",
                                            ifelse(fig.5c.data.1$Exposure == "FA" & fig.5c.data.1$Starting.Plaque.Size == "Between 250 to 500", "FA\n250-500",
                                                   ifelse(fig.5c.data.1$Exposure == "PM" & fig.5c.data.1$Starting.Plaque.Size == "Between 250 to 500", "PM\n250-500",
                                                          ifelse(fig.5c.data.1$Exposure == "FA" & fig.5c.data.1$Starting.Plaque.Size == "Between 500 to 1000", "FA\n500-1000",
                                                                 ifelse(fig.5c.data.1$Exposure == "PM" & fig.5c.data.1$Starting.Plaque.Size == "Between 500 to 1000", "PM\n500-1000",
                                                                        ifelse(fig.5c.data.1$Exposure == "FA" & fig.5c.data.1$Starting.Plaque.Size == "Greater than 1000", "FA\n> 1000",
                                                                               ifelse(fig.5c.data.1$Exposure == "PM" & fig.5c.data.1$Starting.Plaque.Size == "Greater than 1000", "PM\n> 1000", "Error"))))))))
fig.5c.data.1$Concat.E.SPS <- factor(fig.5c.data.1$Concat.E.SPS, labels = c("FA\n< 250", "PM\n< 250", "FA\n250-500", "PM\n250-500", "FA\n500-1000", "PM\n500-1000", "FA\n> 1000", "PM\n> 1000"),
                                     levels = c("FA\n< 250", "PM\n< 250", "FA\n250-500", "PM\n250-500", "FA\n500-1000", "PM\n500-1000", "FA\n> 1000", "PM\n> 1000"))             

FA.SPS1.Max <- max(as.matrix(fig.5c.data.1[,Outcome])[which(fig.5c.data.1$Exposure == "FA" & fig.5c.data.1$Starting.Plaque.Size == "Less Than 250")], na.rm = TRUE)
PM.SPS1.Max <- max(as.matrix(fig.5c.data.1[,Outcome])[which(fig.5c.data.1$Exposure == "PM" & fig.5c.data.1$Starting.Plaque.Size == "Less Than 250")], na.rm = TRUE)
FA.SPS2.Max <- max(as.matrix(fig.5c.data.1[,Outcome])[which(fig.5c.data.1$Exposure == "FA" & fig.5c.data.1$Starting.Plaque.Size == "Between 250 to 500")], na.rm = TRUE)
PM.SPS2.Max <- max(as.matrix(fig.5c.data.1[,Outcome])[which(fig.5c.data.1$Exposure == "PM" & fig.5c.data.1$Starting.Plaque.Size == "Between 250 to 500")], na.rm = TRUE)
FA.SPS3.Max <- max(as.matrix(fig.5c.data.1[,Outcome])[which(fig.5c.data.1$Exposure == "FA" & fig.5c.data.1$Starting.Plaque.Size == "Between 500 to 1000")], na.rm = TRUE)
PM.SPS3.Max <- max(as.matrix(fig.5c.data.1[,Outcome])[which(fig.5c.data.1$Exposure == "PM" & fig.5c.data.1$Starting.Plaque.Size == "Between 500 to 1000")], na.rm = TRUE)
FA.SPS4.Max <- max(as.matrix(fig.5c.data.1[,Outcome])[which(fig.5c.data.1$Exposure == "FA" & fig.5c.data.1$Starting.Plaque.Size == "Greater than 1000")], na.rm = TRUE)
PM.SPS4.Max <- max(as.matrix(fig.5c.data.1[,Outcome])[which(fig.5c.data.1$Exposure == "PM" & fig.5c.data.1$Starting.Plaque.Size == "Greater than 1000")], na.rm = TRUE)

fig.5c.data.1$Starting.Plaque.Size.2 <- factor(fig.5c.data.1$Starting.Plaque.Size, levels = c("Less Than 250", "Between 250 to 500", "Between 500 to 1000", "Greater than 1000"))

seg.max.1 <- 0.01
seg.max.2 <- 0.005
text.above.1 <- 0.012
text.corner.1 <- 0.005
text.corner.2 <- 0.01
ylab.statement <- expression(paste("Total Plaque Number / Tissue Area   ", mu, M^{2}, sep = ''))
yaxis.min <- 0
yaxis.max <- 0.5
yaxis.breaks <- 0.02

Figure5A.3 <- ggplot(data = fig.5c.data.1, aes(x = Concat.E.SPS, y = !!sym(Outcome), fill = Exposure)) +
  stat_boxplot(position = position_dodge(width = 0.7), geom = "errorbar", width = 0.2, size = 0.4) + 
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.5, size = 0.4, outlier.shape = NA, show.legend = FALSE) + 
  geom_point(aes(fill = factor(Exposure, labels = c("FA1", "PM1"))), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.7), color = "black", shape = 21, size = 1, show.legend = FALSE) +
  scale_y_continuous(breaks = seq(yaxis.min, yaxis.max, yaxis.breaks)) +
  scale_fill_manual(name = "Exposure", breaks = c("FA", "PM"), values = c("FA" = "indianred4", "PM" = "dodgerblue4",
                                                                          "FA1" = "indianred", "PM1" = "dodgerblue")) + 
  theme_bw() + theme(panel.spacing = unit(0.8, "lines")) +
  theme(plot.title = element_text(size = rel(1.2))) + theme(strip.text.x = element_text()) + 
  xlab("Exposure") + ylab(ylab.statement) + 
  theme(axis.text = element_text(size = 7, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(legend.text = element_text(size = 6, face = "bold")) +
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(size = 7, face = "bold")) +
  theme(strip.text = element_text(face = "bold")) +
  geom_text(aes(x = 6.18, y = max(as.matrix(fig.5c.data.1[,Outcome]), na.rm = TRUE) + 2*text.corner.1, label = "** Sex"), color = "black", size = 2.5, hjust = 0, vjust = 0) +
  geom_text(aes(x = 6.18, y = max(as.matrix(fig.5c.data.1[,Outcome]), na.rm = TRUE) + text.corner.1, label = "*** Starting Plaque Size"), color = "black", size = 2.5, hjust = 0, vjust = 0) 



# Create Final Model
final.model.results <- cbind(c("Intercept", "Exposure", "Sex", "SPS: 250-500", "SPS: 500-1000", "SPS: >1000"),
                             round(summary(model1.A3)$coef[,1:3], 4),
                             round(confint(model1.A3)[3:8,], 4),
                             c(ifelse(Anova(model1.A3, test = "F", type = "III")[1:3,4] < 0.0001, "< 0.0001", round(Anova(model1.A3, test = "F", type = "III")[1:3,4], 4)),
                               rep(ifelse(Anova(model1.A3, test = "F", type = "III")[4,4] < 0.0001, "< 0.0001", round(Anova(model1.A3, test = "F", type = "III")[4,4], 4)), 3)))
rownames(final.model.results) <- NULL






