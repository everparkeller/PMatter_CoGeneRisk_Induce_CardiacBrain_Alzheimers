#############################
### Figure 1: PM Behavior ###
#############################


### Load Libraries ###
library(readxl)        # If data file is an excel document


### Load and Clean Dataset ###
# fig.1.data <- Available Upon Request [PM Behavior Data.xlsx]
fig.1.keep <- which(colnames(fig.1.data) %in% c("Strain", "Sex", "Exposure",
                                                "Distance...28", "Total Entries", "Spontaneous alternations",
                                                "Percent Open", "Open : entries", "Open : distance",
                                                "Percent Ab Center", "Absolute Center : entries", "Percent Periphery",
                                                "Total Exploration Time", "Delta Novel Zone", "Novel:Fam Entries"))
fig.1.data.1 <- fig.1.data[,fig.1.keep]
colnames(fig.1.data.1) <- c("Genotype", "Sex", "Exposure", "EOM.Open.Entries", "EOM.Percent.Open", "EOM.Distance.Open",
                            "OF.Periphery", "OF.Abs.Center", "OF.Percent.Abs.Center", "YMaze.Distance", "YMaze.Entries", "YMaze.Spont.Alt",
                            "NO.Exp.Time", "NO.Delta.Novel", "NO.Novel.Fam.Entries")

# Factor Predictors 
fig.1.data.1$Genotype <- factor(fig.1.data.1$Genotype) # AD is reference
fig.1.data.1$Sex <- factor(fig.1.data.1$Sex)           # F is reference
fig.1.data.1$Exposure <- factor(fig.1.data.1$Exposure) # FA is reference