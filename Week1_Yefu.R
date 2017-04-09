setwd("/home/yefu/Test") # Set the working directory contains the payments.csv


data <- read.csv("payments.csv") 
# Read the data file and assign it to "data", please be sure that the payments.csv file is in your working directory

colNames <- colnames(data) # Get the column names of each column so that we know that the variable we need is

#Question 1, get the data for Average.Covered.Charges and assign them to Average.Covered.Charges
# get the data for Average.Total.Payments and assign them to Average.Total.Payments
Average.Covered.Charges <- data[data$Provider.State == "NY", "Average.Covered.Charges"]
Average.Total.Payments <- data[data$Provider.State == "NY", "Average.Total.Payments"]

pdf(file = "Question #1 Plot")  # Creat the pdf document for plot
plot(Average.Covered.Charges,Average.Total.Payments) # plot the Average.Total.Payments vs. Average.Covered.Charges
title(main = "Question #1 Plot") # Set the caption of the plot
LM1 <- lm(Average.Total.Payments ~ Average.Covered.Charges) # Get the linear regression results from those two variables
intersect <- coef(LM1)[1] # Get the intersect of the model
slope <- coef(LM1)[2]  # Get the slope of the model
abline(intersect,slope)
EquationText <- sprintf("y = %.1f + %.2f * x", intersect, slope) 
# Set the equation text for the LM model
text(x = 100000, y = 6000, EquationText) # Print the equation of LM model on the plot
rSquared <- summary(LM1)$r.squared  # Get the R2 (coefficient of determination)
rText <- sprintf("Rsquared = %.3f", rSquared)  # Set the R2 text box
text(x = 100000, y = 4500, rText) # Display the Rsquared on the plot
Cor <- cor(Average.Covered.Charges,Average.Total.Payments) # Calculte the correlation coefficient
CorText <- sprintf("Correlation Coef = %.3f", Cor) # Set the Cor text box
text(x = 100000, y = 3000, CorText) # Display the Cor text box on the plot
dev.off() # Save the pdf document of the plot

# Question 1 was solved above, and the output result is saved in the "Question #1 Plot.pdf"

# Question 2 starts here.
# Get the list of all states and all DRG.Definition. 
stateList <- as.character(unique(data$Provider.State))
DRGList <- as.character(unique(data$DRG.Definition))

# Calculate the Cor of Average.Covered.Charges and Average.Total.Payments for different states and DRG. Then save the results in the data frame, which contains the state, DRG, and Cor.
result <- data.frame(matrix(ncol = 3, nrow = 0)) # Initialize an empty data frame
colnames(result) <- c("state","DRG","Cor") # Set the colnames of the data frame
for (state in stateList){
    for (DRG in DRGList){
        Average.Covered.Charges.Temp <- data[data$Provider.State == state & data$DRG.Definition == DRG, "Average.Covered.Charges"]
        Average.Total.Payments.Temp <- data[data$Provider.State == state & data$DRG.Definition == DRG, "Average.Total.Payments"]
        DRGLabel <- substr(DRG,1,3)
        # Notice, only the first three digits of DRG.Definition were used as DRGLabel.
        CorCoef <- cor(Average.Covered.Charges.Temp, Average.Total.Payments.Temp)
        result[nrow(result)+1,] <- c(state,DRGLabel,CorCoef)
    }
} 
# The data frame contains all the state, DRG and Cor information was created.
# Make the multi-panel plots according to different states
pdf(file = "Question 2 Plot")
par(mfrow = c(3,2))
for (state in stateList){
    barplot(as.numeric(result[result$state == state, "Cor"]), xlab = "DRG Definition", ylab = "Correlation Coefficient", names.arg = result[result$state == state, "DRG"], main = state, ylim = c(0, 0.6))
}

# Make the multi-panel plots according to different DRG Definitions.
par(mfrow = c(3,2))
for (DRG in DRGList){
    DRGLabel <- substr(DRG,1,3)
    barplot(as.numeric(result[result$DRG == DRGLabel, "Cor"]), xlab = "Provider State", ylab = "Correlation Coefficient", names.arg = result[result$DRG == DRGLabel, "state"], main = DRG, ylim = c(0, 0.6),cex.main = 0.65)
}
dev.off()




