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
Reverse score the variables
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
base = read.csv("Baseline Merged 7.31.20.csv", header = TRUE, na.strings = c(-7))
discharge = read.csv("Discharge Merged 7.31.20.csv", header = TRUE, na.strings = c(-7))
## change ID 
colnames(base)[1] = "id"
colnames(discharge)[1] = "id"
### Grab the variables that you want
head(base)
#
#26:112, 131:134

base_demos = base[c("id", "Age", "Gender", "HispanicLatino", "IDAfricanAmerican", "IDAsian", "IDWhiteEuropean", "IDAIAN", "IDMultiracial", "Orientation", "Relationship", "Education", "Employment")]
base_outcomes = base[c(25:96, 102:104, 107:109)]

base_outcomes

base = data.frame(base_demos, base_outcomes)
base$id = gsub("[A-z]", "", base$id)
write.csv(base, "base.csv", row.names = FALSE)
base = read.csv("base.csv", header = TRUE, na.strings = c(""))
base$id
# Remove na's for just the ID column
base$remove = is.na(base$id)
base = subset(base, remove == FALSE)
base = base[order(base$id),]
base$remove = NULL
sum(is.na(base$id))
### Ids are in order.  So if you remove the .1,2,3 then you can delete all instances expect the first one.
base$id = gsub("\\..*" , "",base$id)
base
### Now keep the first unique instance 
library(dplyr)
base = distinct(base, id, .keep_all = TRUE)
base

### Now review for errors in all scores
library(prettyR)
#apply(base[,2:95], 2, function(x){describe.factor(x)})
### Make the outcome variables numeric
dim(base)
base[,14:91] = apply(base[,14:91], 2, function(x){as.numeric(x)})

### Now reverse score items
## INQ6, INQ7, INQ10
base$INQ6b = 8-base$INQ6b
base$INQ7b = 8-base$INQ7b
base$INQ10b = 8-base$INQ10b
### Create total score for INQ
base[,14:23]
#PB
#INQ1b: The people in my life would be better off if I were gone 
#INQ2b: The people in my life would be happier without me
#INQ3b: I think my death would be a relief to the people in my life
#INQ4b: I think the people in my life wish they could be rid of me
#INQ5b: I think I make things worse for the people in my life
#TB 
#INQ6b:I feel like I belong
#INQ7b:I am fortunate to have many caring and supportive friends
#INQ8b: I feel disconnected from other people
#INQ9b: I often feel like an outsider in social groups
#INQ10b: I am close to other people
INQ_PB_b = apply(base[,14:18], 1, mean, na.rm = TRUE)
INQ_TB_b = apply(base[,19:23], 1, mean, na.rm = TRUE)
### ACSS make higher scores bad
base[,24:30]
### Reverse score 2,3, and 5
### 0 to 4 scale
base$ACSS2b = 4-base$ACSS2b 
base$ACSS3b = 4-base$ACSS3b
base$ACSS5b = 4-base$ACSS5b
ACCS_b = apply(base[,24:30], 1, mean, na.rm = TRUE)
####
### RAS_GSO_b 1 through 5 and 20
RAS_GSO_b = apply(base[,c(32:36, 51)], 1, mean, na.rm = TRUE)
#### RAS_PCH_b 6 through 13
RAS_PCH_b = apply(base[,37:44], 1, mean, na.rm = TRUE)

### NDS 14 through 16
RAS_NDS_b = apply(base[,45:47], 1, mean, na.rm = TRUE)

#### WAH 17 through 19
RAS_WAH_b = apply(base[,48:50], 1, mean, na.rm = TRUE)

#### SIS_SD

### SD
#SIS5b: I have felt life just isn’t worth living
#SIS6b: Life has been so bad, I’ve felt like giving up
#SIS7b: I’ve just wished my life would end 
#SIS8b: I’ve felt it would be better off for everyone involved if I were to die
SD_SIS_b =  apply(base[,56:59], 1, mean, na.rm = TRUE)


####RPP
#SIS1b: I have been thinking of ways to kill myself
#SIS2b: I have told someone I want to kill myself
#SIS3b: I have believed my life will end in suicide
#SIS4b: I have made attempts to kill myself
#SIS9b: I’ve felt there is no solution to my problems other than to end my own life
#SIS10b:I’ve come close to taking my own life 
RPP_SIS_b = apply(base[,c(52:55,60:61)], 1, mean, na.rm = TRUE)



### To create ""readiness to change" score get average score across all four subscales then sum comtemplation, action, and maintenance and then subtract preconemplation.
#https://habitslab.umbc.edu/urica-readiness-score/
# 1,5,6
Precomp_URICA_b = base[,c(62, 66:67)]
Precomp_URICA_b = apply(Precomp_URICA_b, 1, mean, na.rm  = TRUE)

Contemp_URICA_b = base[c("URICA2b", "URICA8b", "URICA10b")]
Contemp_URICA_b = apply(Contemp_URICA_b, 1, mean, na.rm = TRUE)

Action_URICA_b = base[c("URICA3b", "URICA9b", "URICA11b")]
Action_URICA_b = apply(Action_URICA_b, 1, mean, na.rm = TRUE)

Maintain_URICA_b = base[c("URICA4b", "URICA7b", "URICA12b")]
Maintain_URICA_b = apply(Maintain_URICA_b, 1, mean, na.rm  = TRUE)

RTC_URICA_b = (Contemp_URICA_b+Action_URICA_b+Maintain_URICA_b)-Precomp_URICA_b


SEASA_b = apply(base[,74:85], 1, mean, na.rm = TRUE)

#####
setwd("C:/Users/Matthew.Hanauer/Desktop/Study 5_RELATE Enhanced Follow-up & Tech/3_Data/Database Exports & GSR Analysis/7.31.20/CSV Files")
discharge = read.csv("Discharge Merged 7.31.20.csv", header = TRUE, na.strings = c(-7))
colnames(discharge)[1] = "id"
discharge$id = gsub("[A-z]", " ", discharge$id)
#Remvoe â\200\231 and I donâ\200\231t really have anything. Yall have been great  

discharge = discharge[-c(13,18),]
discharge$id = gsub("\\ *" , "",discharge$id)
discharge$id  = ifelse(discharge$id == ".", "", discharge$id)
write.csv(discharge, "discharge.csv", row.names = FALSE)
discharge = read.csv("discharge.csv", header = TRUE, na.strings = c(""))
discharge$id
# Remove na's for just the ID column
discharge$remove = is.na(discharge$id)
discharge = subset(discharge, remove == FALSE)
discharge = discharge[order(discharge$id),]
discharge$remove = NULL
### Ids are in order.  So if you remove the .1,2,3 then you can delete all instances expect the first one.
discharge
discharge$id = gsub("\\..*" , "",discharge$id)
discharge
### Now keep the first unique instance  
library(dplyr)
discharge = distinct(discharge, id, .keep_all = TRUE)
discharge
### Make numeric
discharge = apply(discharge, 2, function(x){as.numeric(x)})
discharge = data.frame(discharge)
discharge
#### Create total scores
discharge
#INQ1b: The people in my life would be better off if I were gone 
#INQ2b: The people in my life would be happier without me
#INQ3b: I think my death would be a relief to the people in my life
#INQ4b: I think the people in my life wish they could be rid of me
#INQ5b: I think I make things worse for the people in my life
#TB 
#INQ6b:I feel like I belong
#INQ7b:I am fortunate to have many caring and supportive friends
#INQ8b: I feel disconnected from other people
#INQ9b: I often feel like an outsider in social groups
INQ_PB_d = apply(discharge[,2:6], 1, mean, na.rm = TRUE)
INQ_TB_d = apply(discharge[,7:10], 1, mean, na.rm = TRUE)

### RAS_GSO_b 1 through 5 and 20
RAS_GSO_d = apply(discharge[,c(12:16, 31)], 1, mean, na.rm = TRUE)
#### RAS_PCH_b 6 through 13
RAS_PCH_d = apply(discharge[,17:24], 1, mean, na.rm = TRUE)

### NDS 14 through 16
RAS_NDS_d = apply(discharge[,25:27], 1, mean, na.rm = TRUE)

#### WAH 17 through 19
RAS_WAH_d = apply(discharge[,28:30], 1, mean, na.rm = TRUE)


#### SIS_SD

### SD
#SIS5b: I have felt life just isn’t worth living
#SIS6b: Life has been so bad, I’ve felt like giving up
#SIS7b: I’ve just wished my life would end 
#SIS8b: I’ve felt it would be better off for everyone involved if I were to die
SD_SIS_d =  apply(discharge[,36:39], 1, mean, na.rm = TRUE)


####RPP
#SIS1b: I have been thinking of ways to kill myself
#SIS2b: I have told someone I want to kill myself
#SIS3b: I have believed my life will end in suicide
#SIS4b: I have made attempts to kill myself
#SIS9b: I’ve felt there is no solution to my problems other than to end my own life
#SIS10b:I’ve come close to taking my own life 
RPP_SIS_d = apply(discharge[,c(32:35,40:41)], 1, mean, na.rm = TRUE)



Precomp_URICA_d = discharge[c("URICA1d", "URICA5d", "URICA6d")]
Precomp_URICA_d = apply(Precomp_URICA_d, 1, mean, na.rm  = TRUE)

Contemp_URICA_d = discharge[c("URICA2d", "URICA8d", "URICA10d")]
Contemp_URICA_d = apply(Contemp_URICA_d, 1, mean, na.rm = TRUE)

Action_URICA_d = discharge[c("URICA3d", "URICA9d", "URICA11d")]
Action_URICA_d = apply(Action_URICA_d, 1, mean, na.rm = TRUE)

Maintain_URICA_d = discharge[c("URICA4d", "URICA7d", "URICA12d")]
Maintain_URICA_d = apply(Maintain_URICA_d, 1, mean, na.rm  = TRUE)

RTC_URICA_d = (Contemp_URICA_d+Action_URICA_d+Maintain_URICA_d)-Precomp_URICA_d

SEASA_d = apply(discharge[,54:65], 1, mean, na.rm = TRUE)
WAI_d = apply(discharge[,66:69], 1, mean, na.rm = TRUE)

### CSQ-8 4, 5, and 8 reversed scoring
discharge$CSQ4 = 5-discharge$CSQ4
discharge$CSQ5 = 5-discharge$CSQ5
discharge$CSQ8 = 5-discharge$CSQ8
CSQ_d = apply(discharge[,70:77], 1, mean, na.rm = TRUE)

### Clean up base demos
base[,1:13]
#IDAfricanAmerican, IDAsian, IDWhiteEuropean, IDAIAN, IDMultiracial
base_demos = apply(base[,5:9], 2, function(x){as.numeric(x)})
base_demos = data.frame(base_demos)
base_demos[is.na(base_demos)] = 0
base_demos_all = data.frame(base[,c(1:4, 10:13)], base_demos)
base_demos_all

base_psych = base
discharge_psych = discharge

base = data.frame(base_demos_all, INQ_PB_b, INQ_TB_b, RAS_GSO_b, RAS_PCH_b, RAS_NDS_b, RAS_WAH_b, SD_SIS_b, RPP_SIS_b, SEASA_b, RTC_URICA_b)

discharge = data.frame(INQ_PB_d, INQ_TB_d, RAS_GSO_d, RAS_PCH_d, RAS_NDS_d, RAS_WAH_d ,SD_SIS_d, RPP_SIS_d, SEASA_d, WAI_d, CSQ_d, RTC_URICA_d, id = discharge$id)


```
Check pyschometrics 

SEASA_b_psych low alpha .65
```{r}
library(psych)
#########################
# Base psychometrics
INQ_PB_b_psych = base_psych[,14:18]
INQ_TB_b_psych = base_psych[,19:23]
RAS_GSO_b_psych  = base_psych[,c(32:36, 51)]
RAS_PCH_b_psych  = base_psych[,37:44]
RAS_NDS_b_psych  = base_psych[,45:47]
RAS_WAH_b_psych  = base_psych[,48:50]
SD_SIS_b_psych  =  base_psych[,56:59]
RPP_SIS_b_psych  = base_psych[,c(52:55,60:61)]
URICA_b_psych  = base_psych[,62:73]
SEASA_b_psych  = base_psych[,74:85]

base_psych = list(INQ_PB_b_psych, INQ_TB_b_psych, RAS_GSO_b_psych, RAS_PCH_b_psych, RAS_NDS_b_psych, RAS_WAH_b_psych, SD_SIS_b_psych, RPP_SIS_b_psych, RTC_URICA_b, SEASA_b_psych)

base_psych_out = list()
for(i in 1:length(base_psych)){
 base_psych_out[[i]]= summary(omega(base_psych[[i]], poly= TRUE))
}

INQ_PB_d_psych = discharge_psych[,2:6]
INQ_TB_d_psych = discharge_psych[,7:10]
RAS_GSO_d_psych = discharge_psych[,c(12:16, 31)]
RAS_PCH_d_psych = discharge_psych[,17:24]
RAS_NDS_d_psych = discharge_psych[,25:27]
RAS_WAH_d_psych = discharge_psych[,28:30]
SD_SIS_d_psych =  discharge_psych[,36:39]
RPP_SIS_d_psych = discharge_psych[,c(32:35,40:41)]
URICA_d_psych = discharge_psych[,42:53]
SEASA_d_psych = discharge_psych[,54:65]
WAI_d_psych = discharge_psych[,66:69]
CSQ_d_psych = discharge_psych[,70:77]

dis_psych = list(INQ_PB_d_psych, INQ_TB_d_psych, RAS_GSO_d_psych, RAS_PCH_d_psych, RAS_NDS_d_psych, RAS_WAH_d_psych, SD_SIS_d_psych, RPP_SIS_d_psych, RTC_URICA_d, SEASA_d_psych)
dis_psych_out = list()
for(i in 1:length(dis_psych)){
 dis_psych_out[[i]]= summary(omega(dis_psych[[i]]))
}

#### EFA 
library(paran)
map_base_out = list()
for(i in 1:length(base_psych)){
  map_base_out[[i]] = vss(base_psych[[i]], n = 3, rotate = "oblimin", fm = "mle")
}
map_base_out

map_dis_out = list()
for(i in 1:length(dis_psych)){
  map_dis_out[[i]] = vss(dis_psych[[i]], n = 3, rotate = "oblimin", fm = "mle")
}
map_dis_out

paran_base_out = list()
for(i in 1:length(base_psych)){
  paran_base_out[[i]] = na.omit(base_psych[[i]])
  paran_base_out[[i]] = paran(paran_base_out[[i]], centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
}
paran_base_out


paran_dis_out = list()
for(i in 1:length(base_psych)){
  paran_dis_out[[i]] = na.omit(dis_psych[[i]])
  paran_dis_out[[i]] = paran(paran_dis_out[[i]], centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
}
paran_dis_out


```
Merge the data
```{r}
dim(base)
base
dim(discharge)
discharge
target_2_dat = merge(base, discharge, by = "id")
dim(target_2_dat)


```



Get descriptives ready
```{r}
target_2_dat[,9:13] = apply(target_2_dat[,9:13], 2, function(x){as.factor(x)})
target_2_dat$Age = as.numeric(target_2_dat$Age)
part_charac =  prettyR::describe(target_2_dat[-c(1)])
num_charac = data.frame(part_charac$Numeric)
num_charac = num_charac[c(1,4),]
num_charac = t(num_charac)
num_charac = round(num_charac,2) 

write.csv(num_charac, "num_charac.csv")

fac_charac = data.frame(part_charac$Factor)
fac_charac = round(fac_charac,2)
fac_charac = t(fac_charac)
fac_charac
```



Look into missing data and imputation
```{r}
library(naniar)
### Remove hispanic and latino from the analysis to avoid large percentage of missing data
target_2_dat$HispanicLatino = NULL

write.csv(target_2_dat, "target_2_dat.csv", row.names = FALSE)
target_2_dat = read.csv("target_2_dat.csv", header = TRUE, na.strings = c("NA"))
target_2_dat
miss_var_summary(target_2_dat)
prop_miss_case(target_2_dat)

library(MissMech)
TestMCARNormality(target_2_dat[-c(1)])

target_2_dat_complete = na.omit(target_2_dat)
dim(target_2_dat)
dim(target_2_dat_complete)
test1 = data.frame(c = rnorm(5), d = rnorm(5))
write.csv(test1, "test1.csv", row.names = FALSE)
```


