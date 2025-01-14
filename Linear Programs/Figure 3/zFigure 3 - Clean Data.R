##########################
### Figure 3: PM Abeta ###
##########################


### Load Libraries ###
library(readxl)        # If data file is an excel document


### Load and Clean Dataset ###
# fig.1.data <- Available Upon Request [PM Behavior Data.xlsx]
# fig.3a.data <- Available Upon Request [PM Abeta40_42 ELISA.xlsx]
# fig.3b.data <- Available Upon Request [Male Amyloid-Beta Western Blot Heart and Brain 01182023.xlsx]

fig.3a.keep <- which(colnames(fig.3a.data) %in% c("Sample ID", "Organ", 
                                                  "Abeta_42 normalized signal (pg/mg)", "Abeta_40 normalized signal (pg/mg)", "42/40 ratio"))
fig.3b.keep <- which(colnames(fig.3b.data) %in% c("Sample ID...2", "Brain Abeta 42", "Brain Abeta 40", "Brain 42:40 Ratio",
                                                  "Sample ID...8", "Heart Abeta 42", "Heart Abeta 40", "Heart 42:40 Ratio"))
fig.1.keep <- which(colnames(fig.1.data) %in% c("ID Number", "Strain", "Exposure", "Sex"))

fig.1.reduced <- fig.1.data[,fig.1.keep]
fig.3a.reduced <- fig.3a.data[,fig.3a.keep]
fig.3b.reduced <- fig.3b.data[,fig.3b.keep]

fig.3b.brain.reduced <- cbind(rep("Brain", (nrow(fig.3b.reduced))), fig.3b.reduced[,1:4])
fig.3b.heart.reduced <- cbind(rep("Heart", (nrow(fig.3b.reduced))), fig.3b.reduced[,5:8])
colnames(fig.3b.brain.reduced) <- colnames(fig.3b.heart.reduced) <- c("Organ", "Sample.ID", "Abeta.42", "Abeta.40", "Ratio.42.40")
fig.3b.reduced.long <- rbind(fig.3b.brain.reduced, fig.3b.heart.reduced)

fig.1.reduced$`ID Number` <- as.numeric(fig.1.reduced$`ID Number`)
fig.3a.reduced$`Sample ID` <- as.numeric(fig.3a.reduced$`Sample ID`)
fig.3b.reduced.long$Sample.ID <- as.numeric(fig.3b.reduced.long$Sample.ID)

fig.3.data.1 <- merge(fig.3a.reduced, fig.1.reduced, by.x = "Sample ID", by.y = "ID Number", all.x = TRUE)
fig.3.data.2 <- merge(fig.3b.reduced.long, fig.1.reduced, by.x = "Sample.ID", by.y = "ID Number", all.x = TRUE)

colnames(fig.3.data.1) <- c("Mouse.ID", "Organ", "Abeta.42.Norm.Sig", "Abeta.40.Norm.Sig", "Ratio.42.40",
                            "Genotype", "Sex", "Exposure")
colnames(fig.3.data.2) <- c("Mouse.ID", "Organ", "Abeta.42", "Abeta.40", "Ratio.42.40",
                            "Genotype", "Sex", "Exposure")


# Factor Predictors 
fig.3.data.1$Genotype <- factor(fig.3.data.1$Genotype) # AD is reference
fig.3.data.1$Sex <- factor(fig.3.data.1$Sex)           # F is reference
fig.3.data.1$Exposure <- factor(fig.3.data.1$Exposure) # FA is reference

fig.3.data.2$Genotype <- factor(fig.3.data.2$Genotype) # AD is reference
fig.3.data.2$Sex <- factor(fig.3.data.2$Sex)           # F is reference
fig.3.data.2$Exposure <- factor(fig.3.data.2$Exposure) # FA is reference

# Fix Character to Numeric Variables
fig.3.data.1$Ratio.42.40 <- as.numeric(fig.3.data.1$Ratio.42.40)
fig.3.data.2$Abeta.42 <- as.numeric(fig.3.data.2$Abeta.42)
fig.3.data.2$Abeta.40 <- as.numeric(fig.3.data.2$Abeta.40)
fig.3.data.2$Ratio.42.40 <- as.numeric(fig.3.data.2$Ratio.42.40)

# Impute Lowest Value (Instead of 0)
fig.3.data.1$Abeta.42.Norm.Sig.New <- ifelse(fig.3.data.1$Organ == "Heart" & fig.3.data.1$Abeta.42.Norm.Sig == 0,
                                             (min(fig.3.data.1$Abeta.42.Norm.Sig[which(fig.3.data.1$Organ == "Heart" & !(fig.3.data.1$Abeta.42.Norm.Sig == 0))], na.rm = TRUE) / 2),
                                             ifelse(fig.3.data.1$Organ == "Brain" & fig.3.data.1$Abeta.42.Norm.Sig == 0,
                                                    (min(fig.3.data.1$Abeta.42.Norm.Sig[which(fig.3.data.1$Organ == "Brain" & !(fig.3.data.1$Abeta.42.Norm.Sig == 0))], na.rm = TRUE) / 2), fig.3.data.1$Abeta.42.Norm.Sig))
fig.3.data.1$Abeta.40.Norm.Sig.New <- ifelse(fig.3.data.1$Organ == "Heart" & fig.3.data.1$Abeta.40.Norm.Sig == 0,
                                             (min(fig.3.data.1$Abeta.40.Norm.Sig[which(fig.3.data.1$Organ == "Heart" & !(fig.3.data.1$Abeta.40.Norm.Sig == 0))], na.rm = TRUE) / 2),
                                             ifelse(fig.3.data.1$Organ == "Brain" & fig.3.data.1$Abeta.40.Norm.Sig == 0,
                                                    (min(fig.3.data.1$Abeta.40.Norm.Sig[which(fig.3.data.1$Organ == "Brain" & !(fig.3.data.1$Abeta.40.Norm.Sig == 0))], na.rm = TRUE) / 2), fig.3.data.1$Abeta.40.Norm.Sig))
fig.3.data.1$Ratio.42.40.New <- fig.3.data.1$Abeta.42.Norm.Sig.New / fig.3.data.1$Abeta.40.Norm.Sig.New

fig.3.data.2$Abeta.42.New <- ifelse(fig.3.data.2$Organ == "Heart" & fig.3.data.2$Abeta.42 == 0,
                                    (min(fig.3.data.2$Abeta.42[which(fig.3.data.2$Organ == "Heart" & !(fig.3.data.2$Abeta.42 == 0))], na.rm = TRUE) / 2),
                                    ifelse(fig.3.data.2$Organ == "Brain" & fig.3.data.2$Abeta.42 == 0,
                                           (min(fig.3.data.2$Abeta.42[which(fig.3.data.2$Organ == "Brain" & !(fig.3.data.2$Abeta.42 == 0))], na.rm = TRUE) / 2), fig.3.data.2$Abeta.42))
fig.3.data.2$Abeta.40.New <- ifelse(fig.3.data.2$Organ == "Heart" & fig.3.data.2$Abeta.40 == 0,
                                    (min(fig.3.data.2$Abeta.40[which(fig.3.data.2$Organ == "Heart" & !(fig.3.data.2$Abeta.40 == 0))], na.rm = TRUE) / 2),
                                    ifelse(fig.3.data.2$Organ == "Brain" & fig.3.data.2$Abeta.40 == 0,
                                           (min(fig.3.data.2$Abeta.40[which(fig.3.data.2$Organ == "Brain" & !(fig.3.data.2$Abeta.40 == 0))], na.rm = TRUE) / 2), fig.3.data.2$Abeta.40))
fig.3.data.2$Abeta.40.New <- ifelse(is.na(fig.3.data.2$Abeta.40), (min(fig.3.data.2$Abeta.40[which(fig.3.data.2$Organ == "Brain" & !(fig.3.data.2$Abeta.40 == 0))], na.rm = TRUE) / 2), fig.3.data.2$Abeta.40.New)
fig.3.data.2$Ratio.42.40.New <- fig.3.data.2$Abeta.42.New / fig.3.data.2$Abeta.40.New

