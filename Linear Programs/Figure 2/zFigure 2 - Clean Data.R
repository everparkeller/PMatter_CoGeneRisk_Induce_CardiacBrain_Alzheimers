#########################
### Figure 2: PM Echo ###
#########################


### Load Libraries ###
library(readxl)        # If data file is an excel document


### Load and Clean Dataset ###
# fig.1.data <- Available Upon Request [PM Behavior Data.xlsx]
# fig.2.data <- Available Upon Request [PM Echo Data.xlsx]
# fig.2.lap.data <- Available Upon Request [Echo Lap Data.xlsx]


fig.2.keep <- which(colnames(fig.2.data) %in% c("Mouse ID", "Strain", "Exposure",
                                                "LAD/TL", "LV Mass: TL", "Ejection Fraction",
                                                "Cardiac Output", "Peak Grad", "Peak Vel",
                                                "MV E/A", "MV Decel Time", "MV Decel"))
fig.1.keep <- which(colnames(fig.1.data) %in% c("ID Number", "Sex"))

fig.1.reduced <- fig.1.data[,fig.1.keep]
fig.2.reduced <- fig.2.data[,fig.2.keep]

fig.1.reduced$`ID Number` <- as.numeric(fig.1.reduced$`ID Number`)
fig.2.reduced$`Mouse ID` <- as.numeric(fig.2.reduced$`Mouse ID`)

fig.2.data.1 <- merge(fig.2.reduced, fig.1.reduced, by.x = "Mouse ID", by.y = "ID Number", all.x = TRUE)
colnames(fig.2.data.1) <- c("Mouse.ID", "Genotype", "Exposure", "LAD.TL", "Pulm.Art.Peak.Vel",
                            "Pulm.Art.Peak.Grad", "SAM.Eject.Frac", "SAM.Cardiac.Output", "SAM.LV.Mass.TL",
                            "MVF.MV.Flow.Decel", "MVF.MV.Flow.Decel.Time", "MVF.E.A", "Sex")

fig.2.data.2 <- cbind(fig.2.lap.data, matrix(unlist(strsplit(unlist(strsplit(fig.2.lap.data$Group, "_")), " ")), ncol = 2, byrow = TRUE))
colnames(fig.2.data.2) <- c("Mouse.ID", "Sex", "Group", "LAP", "Genotype", "Exposure")
fig.2.data.2 <- as.data.frame(fig.2.data.2)

# Factor Predictors 
fig.2.data.1$Genotype <- factor(fig.2.data.1$Genotype) # AD is reference
fig.2.data.1$Sex <- factor(fig.2.data.1$Sex)           # F is reference
fig.2.data.1$Exposure <- factor(fig.2.data.1$Exposure) # FA is reference

fig.2.data.2$Genotype <- factor(fig.2.data.2$Genotype) # AD is reference
fig.2.data.2$Sex <- factor(fig.2.data.2$Sex)           # F is reference
fig.2.data.2$Exposure <- factor(fig.2.data.2$Exposure) # FA is reference

# Fix Character to Numeric Variables
fig.2.data.1$SAM.LV.Mass.TL <- as.numeric(fig.2.data.1$SAM.LV.Mass.TL)
fig.2.data.2$LAP <- as.numeric(fig.2.data.2$LAP)




