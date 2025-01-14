#################################
### Figure 6: PM GSH SOD1 EPR ###
#################################


### Load Libraries ###
library(readxl)        # If data file is an excel document


### Load and Clean Dataset ###
# fig.1.data <- Available Upon Request [PM Behavior Data.xlsx]
# fig.6.gsh.data <- Available Upon Request [PM GSH_MDA.xlsx]
# fig.6.sod.data <- Available Upon Request [SOD1 Heart and Brain.xlsx]
# fig.6.epr.data <- Available Upon Request [EPR.xlsx]

colnames(fig.6.gsh.data) <- c("Mouse.ID", "Sex", "Organ", "Group", "GSH.Total", "GSSG", "GSH.Disregard", "MDA.Conc", "MDA.GSH.Ratio")
colnames(fig.6.sod.data) <- c("Mouse.ID", "Sex", "Organ", "Group", "KDA.16", "KDA.32", "Membrane.Date")
colnames(fig.6.epr.data) <- c("Mouse.ID", "Plasma.Group", "Cardiomyocyte", "Min.20", "Hrs.2")

fig.1.keep <- which(colnames(fig.1.data) %in% c("ID Number", "Strain", "Exposure", "Sex"))
fig.1.reduced <- fig.1.data[,fig.1.keep]
colnames(fig.1.reduced) <- c("Genotype", "Sex", "Exposure", "ID Number")

fig.1.reduced$`ID Number` <- as.numeric(fig.1.reduced$`ID Number`)
fig.6.gsh.data$Mouse.ID <- as.numeric(fig.6.gsh.data$Mouse.ID)
fig.6.sod.data$Mouse.ID <- as.numeric(fig.6.sod.data$Mouse.ID)
fig.6.epr.data$Mouse.ID <- as.numeric(fig.6.epr.data$Mouse.ID)

fig.6.gsh.data.1 <- merge(fig.6.gsh.data, fig.1.reduced[,-which(colnames(fig.1.reduced) == "Sex")], by.x = "Mouse.ID", by.y = "ID Number", all.x = TRUE)
fig.6.sod.data.1 <- merge(fig.6.sod.data, fig.1.reduced[,-which(colnames(fig.1.reduced) == "Sex")], by.x = "Mouse.ID", by.y = "ID Number", all.x = TRUE)
fig.6.epr.data.1 <- merge(fig.6.epr.data, fig.1.reduced, by.x = "Mouse.ID", by.y = "ID Number", all.x = TRUE)

# Factor Predictors
fig.6.gsh.data.1$Genotype <- factor(fig.6.gsh.data.1$Genotype)

# Factor Predictors 
fig.6.gsh.data.1$Genotype <- factor(fig.6.gsh.data.1$Genotype) # AD is reference
fig.6.gsh.data.1$Sex <- factor(fig.6.gsh.data.1$Sex)           # F is reference
fig.6.gsh.data.1$Exposure <- factor(fig.6.gsh.data.1$Exposure) # FA is reference
fig.6.gsh.data.1$Organ <- factor(fig.6.gsh.data.1$Organ)
fig.6.gsh.data.1$GSH.Total <- as.numeric(fig.6.gsh.data.1$GSH.Total)
fig.6.gsh.data.1$GSSG <- as.numeric(fig.6.gsh.data.1$GSSG)
fig.6.gsh.data.1$MDA.Conc <- as.numeric(fig.6.gsh.data.1$MDA.Conc)
fig.6.gsh.data.1$MDA.GSH.Ratio <- as.numeric(fig.6.gsh.data.1$MDA.GSH.Ratio)

fig.6.sod.data.1$Genotype <- factor(fig.6.sod.data.1$Genotype) # AD is reference
fig.6.sod.data.1$Sex <- factor(fig.6.sod.data.1$Sex)           # F is reference
fig.6.sod.data.1$Exposure <- factor(fig.6.sod.data.1$Exposure) # FA is reference
fig.6.sod.data.1$Organ <- factor(fig.6.sod.data.1$Organ)
fig.6.sod.data.1$KDA.32 <- as.numeric(fig.6.sod.data.1$KDA.32)

fig.6.epr.data.1$Genotype <- factor(fig.6.epr.data.1$Genotype) # AD is reference
fig.6.epr.data.1$Sex <- factor(fig.6.epr.data.1$Sex)           # F is reference
fig.6.epr.data.1$Exposure <- factor(fig.6.epr.data.1$Exposure) # FA is reference


