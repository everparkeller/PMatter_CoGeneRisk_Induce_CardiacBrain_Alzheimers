###########################
### Figure 4: PM A11-19 ###
###########################


### Load Libraries ###
library(readxl)        # If data file is an excel document


### Load and Clean Dataset ###
# fig.1.data <- Available Upon Request [PM Behavior Data.xlsx]
# fig.4a.data <- Available Upon Request [Heart and Brain Oligomer and Fibril Dot Blot Results.xlsx]
# fig.4b.data <- Available Upon Request [A11_19_Quantification For Stats.xlsx, sheet = "Area Data"]

fig.4a.keep <- which(colnames(fig.4a.data) %in% c("Sample ID", "Organ", "A11_19", "OC", "VIA"))
fig.4b.keep <- which(colnames(fig.4b.data) %in% c("Animal", "Tropomyosin Area", "A11-19 Area", "Percentage A11-19"))
fig.1.keep <- which(colnames(fig.1.data) %in% c("ID Number", "Strain", "Exposure", "Sex"))

fig.1.reduced <- fig.1.data[,fig.1.keep]
fig.4a.reduced <- fig.4a.data[,fig.4a.keep]
fig.4b.reduced <- fig.4b.data[,fig.4b.keep]

fig.1.reduced$`ID Number` <- as.numeric(fig.1.reduced$`ID Number`)
fig.4a.reduced$`Sample ID` <- as.numeric(fig.4a.reduced$`Sample ID`)
fig.4b.reduced$Animal <- as.numeric(fig.4b.reduced$Animal)

fig.4a.data.1 <- merge(fig.4a.reduced, fig.1.reduced, by.x = "Sample ID", by.y = "ID Number", all.x = TRUE)
fig.4b.data.1 <- merge(fig.4b.reduced, fig.1.reduced, by.x = "Animal", by.y = "ID Number", all.x = TRUE)

colnames(fig.4a.data.1) <- c("Mouse.ID", "Organ", "A11.19", "OC", "VIA", "Genotype", "Sex", "Exposure")
colnames(fig.4b.data.1) <- c("Mouse.ID", "Tropomyosin.Area", "A11.19.Area", "Percentage.A11.19", "Genotype", "Sex", "Exposure")


# Factor Predictors 
fig.4a.data.1$Genotype <- factor(fig.4a.data.1$Genotype) # AD is reference
fig.4a.data.1$Sex <- factor(fig.4a.data.1$Sex)           # F is reference
fig.4a.data.1$Exposure <- factor(fig.4a.data.1$Exposure) # FA is reference

fig.4b.data.1$Genotype <- factor(fig.4b.data.1$Genotype) # AD is reference
fig.4b.data.1$Sex <- factor(fig.4b.data.1$Sex)           # F is reference
fig.4b.data.1$Exposure <- factor(fig.4b.data.1$Exposure) # FA is reference

# Confirm Percentage of A11-19
fig.4b.data.1$Percentage.A11.19.Confirm <- (fig.4b.data.1$A11.19.Area / fig.4b.data.1$Tropomyosin.Area) * 100

