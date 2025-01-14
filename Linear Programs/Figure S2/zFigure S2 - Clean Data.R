##################################
### Figure S2: PM Morphometric ###
##################################


### Load Libraries ###
library(readxl)        # If data file is an excel document


### Load and Clean Dataset ###
# fig.s2.data.1 <- Available Upon Request [PM Morphometric Data.xlsx]

colnames(fig.s2.data.1) <- c("Mouse.ID", "Group", "Sex", "Genotype", "Exposure",
                             "Body.Weight", "Heart.Weight", "Brain.Weight", "Liver.Weight", "Tibia.Length",
                             "Body_Tibia", "Heart_Tibia", "Brain_Tibia", "Liver_Tibia")

# Factor Predictors 
fig.s2.data.1$Genotype <- factor(fig.s2.data.1$Genotype) # AD is reference
fig.s2.data.1$Sex <- factor(fig.s2.data.1$Sex)           # F is reference
fig.s2.data.1$Exposure <- factor(fig.s2.data.1$Exposure) # FA is reference
fig.s2.data.1$Mouse.ID <- as.numeric(fig.s2.data.1$Mouse.ID)
