#################################
### Figure 5: PM Brain Plaque ###
#################################


### Load Libraries ###
library(readxl)        # If data file is an excel document


### Load and Clean Dataset ###
# fig.1.data <- Available Upon Request [PM Behavior Data.xlsx]
# fig.5a.data <- Available Upon Request [PM Brain Plaque.xlsx, sheet = "Copy of PPG (For Analysis)"]
# fig.5b.data <- Available Upon Request [PM Brain Plaque.xlsx, sheet = sheet = "Copy of PCBS (For Analysis) 1"]
# fig.5c.data <- Available Upon Request [PM Brain Plaque.xlsx, sheet = "Copy of PCBS (For Analysis) 2"]

fig.1.keep <- which(colnames(fig.1.data) %in% c("ID Number", "Sex"))
fig.5b.keep <- which(colnames(fig.5b.data) %in% c("Sample.ID", "Sex", "Genotype", "Exposure", "Total Plaque Number Normalized by Tissue Area", "Less.Than.5.Plaques"))
fig.5c.keep <- which(colnames(fig.5c.data) %in% c("Sample.ID", "Sex", "Genotype", "Exposure", "Normalized by tissue area", "Plaque.Starting.Size", "Less.Than.5.Plaques"))

fig.1.reduced <- fig.1.data[,fig.1.keep]
fig.5b.reduced <- fig.5b.data[,fig.5b.keep]
fig.5c.reduced <- fig.5c.data[,fig.5c.keep]

fig.1.reduced$`ID Number` <- as.numeric(fig.1.reduced$`ID Number`)
fig.5a.data$Mouse.ID <- as.numeric(fig.5a.data$Mouse.ID)
fig.5b.reduced$Sample.ID <- as.numeric(fig.5b.reduced$Sample.ID)
fig.5c.reduced$Sample.ID <- as.numeric(fig.5c.reduced$Sample.ID)

fig.5a.data.1 <- merge(fig.5a.data, fig.1.reduced, by.x = "Mouse.ID", by.y = "ID Number", all.x = TRUE)
fig.5b.data.1 <- fig.5b.reduced
fig.5c.data.1 <- fig.5c.reduced

colnames(fig.5a.data.1) <- c("Mouse.ID", "Full.Mouse.ID", "Genotype", "Exposure", "Starting.Plaque.Size", "Plaque", "M4.Avg", "TR.Avg", "Percent.Increase", "Sex")
colnames(fig.5b.data.1) <- c("Mouse.ID", "Sex", "Genotype", "Exposure", "TPN.Normalized.by.Tissue.Area", "LT.5.Plaques")
colnames(fig.5c.data.1) <- c("Mouse.ID", "Sex", "Genotype", "Exposure", "Normalized.by.Tissue.Area", "Starting.Plaque.Size", "LT.5.Plaques")

# Factor Predictors 
fig.5a.data.1$Genotype <- factor(fig.5a.data.1$Genotype) # AD is reference
fig.5a.data.1$Sex <- factor(fig.5a.data.1$Sex)           # F is reference
fig.5a.data.1$Exposure <- factor(fig.5a.data.1$Exposure) # FA is reference
fig.5a.data.1$Starting.Plaque.Size <- factor(fig.5a.data.1$Starting.Plaque.Size, levels = c("Less Than 250", "Between 250 to 500", "Between 500 to 1000", "Greater than 1000"))

fig.5b.data.1$Genotype <- factor(fig.5b.data.1$Genotype) # AD is reference
fig.5b.data.1$Sex <- factor(fig.5b.data.1$Sex)           # F is reference
fig.5b.data.1$Exposure <- factor(fig.5b.data.1$Exposure) # FA is reference
fig.5b.data.1$LT.5.Plaques <- factor(fig.5b.data.1$LT.5.Plaques)

fig.5c.data.1$Genotype <- factor(fig.5c.data.1$Genotype) # AD is reference
fig.5c.data.1$Sex <- factor(fig.5c.data.1$Sex)           # F is reference
fig.5c.data.1$Exposure <- factor(fig.5c.data.1$Exposure) # FA is reference
fig.5c.data.1$Starting.Plaque.Size <- factor(fig.5c.data.1$Starting.Plaque.Size)
fig.5c.data.1$LT.5.Plaques <- factor(fig.5c.data.1$LT.5.Plaques)

