#For Plotting a mosaic plot I used R, as mentioned in the final project milestone, I am including the R code in the appendix

getwd()
# Reading the data from the .csv file
read.csv("VAC.csv", TRUE, ",")
# Gave a name Con_Seve to the above file
Con_Seve = read.csv("VAC.csv", TRUE, ",")
Con_Seve
# Installing the required packages for the mosaic plot
install.packages("dplyr")
library(dplyr)
# Installing tidyverse packages for core tidyverse package
library(tidyverse)

#A mosaic plot can be constructed with the mosaic function in the vcd package.
install.packages("vcd")
library(vcd)
install.packages("MASS")
library(MASS)

# Colour can be added using the Rcolorbrewer 
install.packages("RColorBrewer")
library("RColorBrewer")
display.brewer.all()

# Calculates the number of observations and variables involved in the package
str(Con_Seve)
# Below function shows the class of the variable Severity and Contact
class(Con_Seve$Severity)
class(Con_Seve$Contact)
# Below functions shows the names of the variables 
names(Con_Seve)
# Creating a table with below varaibles
table(Con_Seve$Contact, Con_Seve$Severity)
# Naming the above mentioned table with table1
table1 = table(Con_Seve$Contact, Con_Seve$Severity) 
# Creating a bar plot 
barplot(table1, beside=T, legend.text = T)
barplot(table1, beside=T, legend.text =c("Animal_contact", "No_Animal_contact"), main="severity and animal_contact", xlab = "Animal_contact", las=1)
# Creating a Mosaic plot
mosaicplot(table1,  beside=T, legend.text =c("Animal_contact", "no_Animal_contact"), main="Severity and Animal Contact", xlab = "Contact", las=1, col=c(10,102,333),  border = "chocolate",cex.lab=6.5, cex.axis=1.5, cex.main=2.5, cex.sub=2.5)
mosaicplot(table1,  beside=T, legend.text =c("Animal_contact", "no_Animal_contact"), main="Severity with Specimen Contact", xlab = "Contact",ylab = "Level of Severity", col=brewer.pal(n = 3, name = "Pastel1"), border = "chocolate", cex.lab=6.5, cex.axis=1.5, cex.main=2.5, cex.sub=2.5)
