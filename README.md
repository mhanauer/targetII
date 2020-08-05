---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Target II
Figure out ID
merge data on matched pairs (check for people only in discharge and not follow-up)
Identify the variables you want
Check for errors in data entry
Aggregate the composite variables
Check psychometrics
Get participant charactertics at base
Get means and sds at base and discharge
Plot mean changes 
Identify differences in treatments
Create IPW weights
Check weights 
Run regression with weights, non-inferiority, and run sensitivity


```{r}
setwd("C:/Users/Matthew.Hanauer/Desktop/Study 5_RELATE Enhanced Follow-up & Tech/3_Data/Database Exports & GSR Analysis/7.31.20/CSV Files")
base = read.csv("Baseline Merged 7.31.20.csv", header = TRUE)
discharge = read.csv("Discharge Merged 7.31.20.csv", header = TRUE)
## change ID 
colnames(base)[1] = "id"
colnames(discharge) = "id"
### Grab the variables that you want
head(base)
#
#26:112, 131:134


base_demos = base[c("id", "Age", "Gender", "HispanicLatino", "IDAfricanAmerican", "IDAsian", "IDWhiteEuropean", "IDAIAN", "IDHawaiianPacific", "IDMultiracial", "Orientation", "Relationship", "Education", "Employment")]
base_outcomes = base[c(25:104, 107:109)]
base = data.frame(base_demos, base_outcomes)
base$id = gsub("[A-z]", "", base$id)
write.csv(base, "base.csv", row.names = FALSE)
base = read.csv("base.csv", header = TRUE, na.strings = c(""))
base$id
# Remove na's for just the ID column
base$remove = is.na(base$id)
base = subset(base, remove == FALSE)
base = base[order(base$id),]
### Ids are in order.  So if you remove the .1,2,3 then you can delete all instances expect the first one.
base$id = gsub("\\..*" , "",base$id)
base
### Now keep the first unique instance 
library(dplyr)
base = distinct(base, id, .keep_all = TRUE)
base
```

